package spread

//  SPREAD: Incremental Authenticated Computations
//
//  Features:

//  1) purely functional
//  2) authenticated traces
//  3) spreadsheet-like incremental computation via (weakly) memoized traces
//
//  Copyright 2016: Robbert van Dalen
//

import scala.collection.immutable.Map
import SplitHash._
import Hashing._
import scala.collection.mutable
import scala.language.existentials

object Spread {

  //
  // A SPREAD expression carries the authenticated trace of computations that leads up to itself.
  // In turn, traces can be (weakly) memoized for re-use.
  //
  // For efficient concatenation and authentication, traces are stored as SplitHashes.
  //

  trait SPREAD[V,SH <: SplitHash[_,SH]] extends Hashable with Hash {
    def trace: SH
  }

  // Concrete default implementation
  trait Expr[V] extends SPREAD[V,SHNode[Expr[_]]] {
    def trace: SHNode[Expr[_]] = ExprSHNode(this)
    def head: Expr[V] = trace.last.asInstanceOf[Expr[V]]
    def parts: Array[Expr[_]]
    def size = 1

    var lHash = 0
    def lazyHash: Int
    override def hashCode = {
      if (lHash == 0) lHash = lazyHash
      lHash
    }
    def hash = this
  }

  // A memoization context that is associated during evaluation
  trait MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]): MemoizationContext
    def mget[X](o1: Expr[X]): Expr[X]
  }

  object EmptyContext extends MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]) = this
    def mget[X](o1: Expr[X]): Expr[X] = null
  }

  case class StrongMemoizationContext(m: Map[Expr[_],Expr[_]]) extends MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]) = StrongMemoizationContext(m + (o1->ev))
    def mget[X](o1: Expr[X]): Expr[X] = m.get(o1) match {
      case None => null
      case Some(x) => x.asInstanceOf[Expr[X]]
    }
  }

  var traceReuse: Boolean = false

  case class WeakMemoizationContext(m: mutable.WeakHashMap[Expr[_],Expr[_]]) extends MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]) = WeakMemoizationContext(m += (o1->ev))
    def mget[X](o1: Expr[X]): Expr[X] = m.get(o1) match {
      case None => null
      case Some(x) => {
        if (traceReuse) { println("REUSED: " + o1) }
        x.asInstanceOf[Expr[X]]
      }
    }
  }

  // Denotes an evaluation
  case class Eval[V](o: Expr[V], distance: Int) extends Expr[V] {
    def lazyHash = siphash24(o.hashCode + magic_p1,distance - magic_p3)
    def hashAt(i: Int) = {
      if (i == 0) lazyHash
      else siphash24(hashAt(i-1) - magic_p3,(magic_p2*distance) ^ o.lazyHash)
    }
    def parts = o.parts
    override def toString = o.toString + "@" + distance
  }

  // Nullary function that MUST evaluate to it's canonical value in O(1)
  trait F0[X] extends Expr[X] with Hashable with Hash {
    def unary_! = value
    def value: X
  }
  // Unary (lazy) function
  trait FA1[A,X] extends (F0[A] => Expr[X]) with CodeHash
  // Binary (lazy) function
  trait FA2[A,B,X] extends ((F0[A],F0[B]) => Expr[X]) with CodeHash

  // Denotes an unary function call
  case class F1[A,X](f: FA1[A,X], v1: Expr[A]) extends Expr[X] {
    override def toString = f+"("+v1+")"
    def lazyHash = siphash24(f.hashCode + magic_p2 ,v1.hashCode - magic_p3)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) (f.hashCode + v1.hashCode) ^ hashCode
      else {
        val nindex = index / 2

        if (hashCode > 0) siphash24(f.hash.hashAt(index - nindex) + magic_p3,v1.hash.hashAt(index) - (magic_p2 * hashCode))
        else siphash24(f.hash.hashAt(nindex) + (magic_p1 * hashCode),v1.hash.hashAt(index - nindex) + magic_p3)
      }
    }
    def parts = Array(v1)
  }

  // Denotes an binary function call
  case class F2[A,B,X](f: FA2[A,B,X], v1: Expr[A], v2: Expr[B]) extends Expr[X] {
    override def toString = "(" + v1 + " " + f + " " + v2 + ")"
    def lazyHash = siphash24(f.hashCode + magic_p1,siphash24(v1.hashCode + magic_p2 ,v2.hashCode - magic_p3))
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) (f.hashCode + v1.hashCode + v2.hashCode) ^ hashCode
      else {
        val nindex = index / 3
        val i2 = nindex * 2
        val i3 = index - i2

        if (hashCode > 0) siphash24(siphash24(f.hash.hashAt(nindex) + magic_p3,v1.hash.hashAt(i2) - (magic_p2 * hashCode)),v2.hash.hashAt(i3))
        else siphash24(v2.hash.hashAt(i3) - magic_p3,siphash24(f.hash.hashAt(i2) + (magic_p1 * hashCode),v1.hash.hashAt(nindex) + magic_p3))
      }
    }
    def parts = Array(v1,v2)
  }

  // Fully evaluate an expression, possibly re-using evaluations held by the MemoizationContext
  def fullEval[V](e: Expr[V], c: MemoizationContext): (Expr[V],MemoizationContext) = {
    val me = c.mget(e)
    if (me != null) (me,c)
    else {
      val (ev,c2) = fullEval2(e,c)
      if (ev != e) (ev,c2.mput(e,ev))
      else (ev,c2)
    }
  }

  def fullEval2[V](e: Expr[V], c: MemoizationContext): (Expr[V],MemoizationContext) = {
    val (e2,c2) = eval(e,c)
    if (e2 != e) fullEval2(e2,c2)
    else (e2,c2)
  }

  case class ExprSHNode[X](o: Expr[_]) extends LeafNode[Expr[_]] {
    def value = o
    def lazyHash = siphash24(o.hashCode - magic_p3, magic_p2 * o.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(o.hashAt(i), o.hashCode)
    }
    override def toString = o.toString
  }

  // A TracedExpr holds the history of computations that leads up to itself.
  case class TracedExpr[V](override val trace: SHNode[Expr[_]]) extends Expr[V] {
    override def size = trace.size
    def lazyHash = siphash24(trace.hashCode - magic_p3, magic_p3 * trace.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(trace.hashAt(i), magic_p2 * trace.hashCode)
    }
    def parts = Array(this)
    override def toString = "[" + trace.toString + "]"
  }

  case class LeafExpr[X <: Hashable](value: X) extends F0[X] {
    def lazyHash = siphash24(value.hash.hashCode * magic_p3, magic_p2 - value.hash.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) lazyHash
      else siphash24(value.hash.hashAt(i) - magic_p2, magic_p3 * value.hashCode)
    }
    def parts = Array(this)
    override def toString = "$("+value+")"
  }

  def node[X](e: Expr[X]) = ExprSHNode(e)
  def expr[X](e: SHNode[X]) = LeafExpr(e)

  // Evaluation via pattern matching - in association with a MemoizationContext
  def eval[X](e: Expr[X], c: MemoizationContext): (Expr[X],MemoizationContext) = e match {
    case f1: F1[_,X] => (f1.v1.head) match {
      case x1: F0[_] => (TracedExpr(node(Eval(f1,1)).concat(node(f1.f(x1)))),c)
      case x1 => {
        val (e1,c2) = fullEval(x1,c)
        var trace: SHNode[Expr[_]] = null

        if (e1 != x1) trace = concat(trace,e1.trace)
        trace = concat(trace,node(F1(f1.f,e1.head)))

        (TracedExpr(node(Eval(f1,trace.size)).concat(trace)),c2)
      }
    }
    case f2: F2[_,_,X] => (f2.v1.head,f2.v2.head) match {
      case (x1: F0[_],x2: F0[_]) => (TracedExpr(node(Eval(f2,1)).concat(node(f2.f(x1,x2)))),c)
      case (x1,x2) => {
        val (e1,c2) = fullEval(x1,c)
        val (e2,c3) = fullEval(x2,c2)
        var trace: SHNode[Expr[_]] = null

        if (e1 != x1) trace = concat(trace,e1.trace)
        if (e2 != x2) trace = concat(trace,e2.trace)
        trace = concat(trace,node(F2(f2.f,e1.head,e2.head)))

        (TracedExpr(node(Eval(f2,trace.size)).concat(trace)),c3)
      }
    }
    case t: TracedExpr[X] => {
      val r1 = t.head
      val (r2,cc) = eval(r1,c)
      if (r1 == r2) (t,cc)
      else (TracedExpr(concat(t.trace,r2.trace)),cc)
    }
    case _ => (e,c)
  }

  // convenience
  def %[A, X](f: FA1[A,X], a: Expr[A]): Expr[X] =  F1(f, a)
  def %[A, B, X](f: FA2[A,B,X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f,a,b)

  // Ideally we should recursively Hash all the java byte code (full dependency graph)
  // For now we just use global constants until we implement that
  trait CodeHash extends Hashable with Hash {
    def hash = this
    def codeID: Int  // MUST be globally unique
    override val hashCode = siphash24(codeID + magic_p1,codeID * magic_p2)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(codeID + magic_p3, hashCode * magic_p2)
      else siphash24(hashCode * magic_p2, hashAt(index-1) - magic_p1)
    }
  }
}