package spread

//  SPREAD: Incremental Authenticated Computational Reuse
//
//  Features:

//  1) purely functional
//  2) authenticated traces
//  3) spreadsheet-like incremental computation via memoized traces
//
//  Copyright 2016: Robbert van Dalen
//

import scala.collection.immutable.Map
import SplitHash._
import Hashing._
import scala.language.existentials

object Spread {

  // An Expr[X] carries the authenticed trace that lead up to itself
  // In turn, this trace can be memoized for re-use
  trait Expr[V] extends Hashable with Hash {
    def trace: SHNode[Expr[_]] = ExprSHNode(this)
    def head: Expr[V] = trace.last.asInstanceOf[Expr[V]]

    var lHash = 0
    def lazyHash: Int
    override def hashCode = {
      if (lHash == 0) lHash = lazyHash
      lHash
    }
    def hash = this
  }

  trait MemoizationContext{
    def mput(o1: Expr[_], ev: Expr[_]): MemoizationContext
    def mget[X](o1: Expr[X]): Expr[X]
  }

  object EmptyContext extends MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]) = this
    def mget[X](o1: Expr[X]): Expr[X] = null
  }

  case class MapMemoizationContext(m: Map[Expr[_],Expr[_]]) extends MemoizationContext {
    def mput(o1: Expr[_], ev: Expr[_]) = MapMemoizationContext(m + (o1->ev))
    def mget[X](o1: Expr[X]): Expr[X] = m.get(o1) match {
      case None => null
      case Some(x) => x.asInstanceOf[Expr[X]]
    }
  }

  case class Eval[V](o: Expr[V], distance: Int) extends Expr[V] {
    def lazyHash = siphash24(o.hashCode + magic_p1,distance - magic_p3)
    def hashAt(i: Int) = {
      if (i == 0) lazyHash
      else siphash24(hashAt(i-1) - magic_p3,(magic_p2*distance) ^ o.lazyHash)
    }
    override def toString = o.toString + "@" + distance
  }

  trait F0[X] extends Expr[X] with Hashable with Hash { def value: X }
  trait FA1[A,X] extends (F0[A] => Expr[X]) with CodeHash
  trait FA2[A,B,X] extends ((F0[A],F0[B]) => Expr[X]) with CodeHash

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
  }

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
  }

  def fullEval[V](e: Expr[V], c: MemoizationContext): (Expr[V],MemoizationContext) = {
    val (e2,c2) = eval(e,c)
    if (e2 != e) fullEval(e2,c2)
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

  case class TracedExpr[V](override val trace: SHNode[Expr[_]]) extends Expr[V] {
    def lazyHash = siphash24(trace.hashCode - magic_p3, magic_p3 * trace.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(trace.hashAt(i), magic_p2 * trace.hashCode)
    }
    override def toString = trace.toString
  }

  def node[X](e: Expr[X]) = ExprSHNode(e)

  def eval[X](e: Expr[X], c: MemoizationContext): (Expr[X],MemoizationContext) ={
    val me = c.mget(e)
    if (me != null) (me,c)
    else {
      val (ev,c2) = tracedEval(e,c)
      if (ev != e) (ev,c2.mput(e,ev))
      else (ev,c2)
    }
  }

  def tracedEval[X](e: Expr[X], c: MemoizationContext): (Expr[X],MemoizationContext) = e match {
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
  def %[A, X](f: FA1[A,X], a: Expr[A]): Expr[X] = F1(f, a)
  def %[A, B, X](f: FA2[A,B,X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f, a, b)

  // Ideally with should recursively hash all the java byte code (full dependency graph)
  // For now we just use global constants until we implement that
  trait CodeHash extends Hashable with Hash {
    def hash = this
    def codeHash: Int  // should be globally unique
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(codeHash + magic_p3, hashCode * magic_p2)
      else siphash24(hashCode * magic_p2, hashAt(index-1) - magic_p1)
    }
  }
}