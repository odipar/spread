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

  trait SPREAD[V] extends Hashable with Hash {
    type E <: SPREAD[_]
    type SH <: SplitHash[E,SH]

    def trace: SH
  }

  // Concrete default implementation
  trait Expr[V] extends SPREAD[V] {
    type E = Expr[_]
    type SH = SHNode[E]

    def trace: SH = ExprSHNode(this)
    def head: Expr[V] = trace.last.asInstanceOf[Expr[V]]
    def size = 1

    def hash = this
    def ===(o: Expr[V]) = this == o
    def !==(o: Expr[V]): Expr[Boolean] = F2(equal2[V],this,o)

    def unary_~ = SignedExpr(this,1)

    def fullEval: Expr[V] = fullEval(EmptyContext)._1
    def fullEval(c: EvaluationContext) = c.fullEval(this)

    def eval(c: EvaluationContext): (Expr[V],EvaluationContext) = c.eval(this)
    def _eval(c: EvaluationContext): (Expr[V],EvaluationContext) = (this,c)
  }

  trait HashedExpr[V] extends Expr[V] {
    var lHash = 0
    def lazyHash: Int
    override def hashCode = {
      if (lHash == 0) lHash = lazyHash
      lHash
    }
  }

  trait EvaluationContext {
    def eval[X](e: Expr[X]) = e._eval(this)
    def fullEval[X](e: Expr[X]): (Expr[X], EvaluationContext) = {
      val (e2,c2) = eval(e)
      if (e2 != e) c2.fullEval(e2)
      else (e2,c2)
    }
  }
  
  // A memoization context that is associated during evaluation
  trait MemoizationContext extends EvaluationContext  {
    override def fullEval[V](e: Expr[V]): (Expr[V],EvaluationContext) = {
      val me = get(e)
      if (me != null) (me,this)
      else {
        val (ev,c2: MemoizationContext) = fullEval2(e)
        if (ev != e) (ev,c2.put(e,ev))
        else (ev,c2)
      }
    }
    def fullEval2[V](e: Expr[V]): (Expr[V],EvaluationContext) ={
      val (e2,c2: MemoizationContext) = eval(e)
      if (e2 != e) c2.fullEval2(e2)
      else (e2,c2)
    }

    def get[X](e: Expr[X]): Expr[X] = null
    def put[X](e1: Expr[X], e2: Expr[X]): EvaluationContext = this
  }

  object EmptyContext extends EvaluationContext

  case class StrongMemoizationContext(m: Map[Expr[_],Expr[_]]) extends MemoizationContext {
    override def get[X](e1: Expr[X]) = {
      m.get(e1) match {
        case None => null
        case Some(x) => (x.asInstanceOf[Expr[X]])
      }
    }
    override def put[X](e1: Expr[X], e2: Expr[X]) = StrongMemoizationContext(m + (e1->e2))
  }

  var traceReuse: Boolean = false

  case class WeakMemoizationContext(m: mutable.WeakHashMap[Expr[_],Expr[_]]) extends MemoizationContext {
    override def get[X](e1: Expr[X]) = {
      m.get(e1) match {
        case None => null
        case Some(x) => {
          if (traceReuse) println("REUSED: "+ e1)
          (x.asInstanceOf[Expr[X]])
        }
      }
    }
    override def put[X](e1: Expr[X], e2: Expr[X]) = WeakMemoizationContext(m += (e1->e2))
  }

  val encoder = java.util.Base64.getEncoder

  case class CryptoSigned[X](a: Seq[Int]) extends Expr[X] {
    def hashAt(i: Int) = a(i % (a.size-1))
    def parts = Array(this)
    override def toString = {
      var s = a.size * 4
      val b: Array[Byte] = new Array(s)
      var i = 0
      while (i < s) {
        var ii = i % 3
        var bb = (a(i >> 2) >>> (8 * ii)).toByte
        b(i) = bb
        i = i + 1
      }
       "#"+javax.xml.bind.DatatypeConverter.printHexBinary(b)
    }
    override def _eval(c: EvaluationContext) = (this,c)
  }

  def copyHash(h: Hash, s: Int): Array[Int] = {
    var i = 0
    var a: Array[Int] = new Array(s)
    while (i < s) {
      a(i) = h.hashAt(i)
      i = i + 1
    }
    a
  }
  // Denotes an evaluation
  case class Eval[V](o: Expr[V], distance: Int) extends HashedExpr[V] {
    def lazyHash = siphash24(o.hashCode + magic_p1,distance - magic_p3)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(hashAt(i-1) - magic_p3,(magic_p2*distance) ^ o.hashCode)
    }
    override def _eval(c: EvaluationContext) = o.eval(c)
    def parts = o.parts
    override def toString = o.toString + "@" + distance
  }

  trait Operator { override def toString = getClass().getSimpleName.replace("$","") }
  trait InfixOperator extends Operator

  // Nullary function that MUST evaluate to its canonical value in O(1)
  trait F0[X] extends Expr[X] with Hashable with Hash {
    def unary_! = value
    def value: X
  }
  // Unary (lazy) function
  trait FA1[A,X] extends (F0[A] => Expr[X]) with CodeHash with Operator
  // Binary (lazy) function
  trait FA2[A,B,X] extends ((F0[A],F0[B]) => Expr[X]) with CodeHash with Operator

  // Ternary (lazy) function
  trait FA3[A,B,C,X] extends ((F0[A],F0[B],F0[C]) => Expr[X]) with CodeHash with Operator

  // Denotes an unary function call
  case class F1[A,X](f: FA1[A,X], v1: Expr[A]) extends HashedExpr[X] {
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
    override def _eval(c: EvaluationContext): (Expr[X], EvaluationContext) = v1.head match {
      case x1: F0[A] => (TracedExpr(node(Eval(this,1)).concat(node(f(x1)))),c)
      case x1 => {
        val (e1,c2) = c.fullEval(x1)
        var trace: SHNode[Expr[_]] = null

        if (e1 != x1) trace = concat(trace,e1.trace)
        trace = concat(trace,node(F1(f,e1.head)))

        (TracedExpr[X](node(Eval(this,trace.size)).concat(trace)),c2)
      }
    }
    def parts = Array(f,v1)
  }

  // Denotes an binary function call
  case class F2[A,B,X](f: FA2[A,B,X], v1: Expr[A], v2: Expr[B]) extends HashedExpr[X] {
    override def toString = f match {
      case i: InfixOperator => "(" + v1 + " " + f + " " + v2 + ")"
      case _ =>  f + "(" + v1 + "," + v2 + ")"
    }
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
    override def _eval(c: EvaluationContext): (Expr[X], EvaluationContext) = (v1.head,v2.head) match {
      case (x1: F0[A],x2: F0[B]) => (TracedExpr(node(Eval(this,1)).concat(node(f(x1,x2)))),c)
      case (x1,x2) => {
        val (e1,c2) = x1.fullEval(c)
        val (e2,c3) = x2.fullEval(c2)
        var trace: SHNode[Expr[_]] = null

        if (e1 != x1) trace = concat(trace,e1.trace)
        if (e2 != x2) trace = concat(trace,e2.trace)
        trace = concat(trace,node(F2(f,e1.head,e2.head)))

        (TracedExpr(node(Eval(this,trace.size)).concat(trace)),c3)
      }

    }
    def parts = Array(f,v1,v2)
  }

  // Denotes an ternary function call
  case class F3[A,B,C,X](f: FA3[A,B,C,X], v1: Expr[A], v2: Expr[B], v3: Expr[C]) extends HashedExpr[X] {
    override def toString = f + "(" + v1 + "," + v2 + "," + v3 + ")"
    def lazyHash = siphash24(siphash24(f.hashCode + magic_p1,siphash24(v1.hashCode + magic_p2 ,v2.hashCode - magic_p3)),v2.hashCode * magic_p1)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) (f.hashCode + v1.hashCode + v2.hashCode ^ v3.hashCode) ^ hashCode
      else {
        val nindex = index / 4
        val i2 = nindex * 2
        val i3 = index * 3
        val i4 = index - nindex

        if (hashCode > 0) siphash24(siphash24(siphash24(f.hash.hashAt(nindex) + magic_p3,v1.hash.hashAt(i2) - (magic_p2 * hashCode)),v2.hash.hashAt(i3)),v3.hashAt(i4))
        else siphash24(v3.hashCode.hashAt(i4),siphash24(v2.hash.hashAt(i3) - magic_p3,siphash24(f.hash.hashAt(i2) + (magic_p1 * hashCode),v1.hash.hashAt(nindex) + magic_p3)))
      }
    }
    override def _eval(c: EvaluationContext): (Expr[X], EvaluationContext) = (v1.head,v2.head,v3.head) match {
      case (x1: F0[A],x2: F0[B], x3: F0[C]) => (TracedExpr(node(Eval(this,1)).concat(node(f(x1,x2,x3)))),c)
      case (x1,x2,x3) => {
        val (e1,c2) = x1.fullEval(c)
        val (e2,c3) = x2.fullEval(c2)
        val (e3,c4) = x3.fullEval(c3)

        var trace: SHNode[Expr[_]] = null

        if (e1 != x1) trace = concat(trace,e1.trace)
        if (e2 != x2) trace = concat(trace,e2.trace)
        if (e3 != x3) trace = concat(trace,e3.trace)

        trace = concat(trace,node(F3(f,e1.head,e2.head,e3.head)))

        (TracedExpr(node(Eval(this,trace.size)).concat(trace)),c4)
      }

    }
    def parts = Array(f,v1,v2,v3)
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
  case class TracedExpr[V](override val trace: SHNode[Expr[_]]) extends HashedExpr[V] {
    override def size = trace.size
    def lazyHash = siphash24(trace.hashCode - magic_p3, magic_p3 * trace.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(trace.hashAt(i), magic_p2 * trace.hashCode)
    }
    override def _eval(c: EvaluationContext): (Expr[V], EvaluationContext) = {
      val r1 = head
      val (r2,cc) = c.eval(r1)
      if (r1 == r2) (this,cc)
      else (TracedExpr(concat(trace,r2.trace)),cc)
    }
    def parts = Array(this)
    override def toString = prettyPrint(trace,0)
  }
  
  // A SignedExpr holds the Expr to be crypto signed with hash size of (a * 128) bits
  case class SignedExpr[X](signed: Expr[X], bits_128: Int) extends HashedExpr[X] {
    override def size = trace.size
    def lazyHash = siphash24(signed.hashCode * magic_p3, magic_p3 - bits_128.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else siphash24(signed.hashAt(i) - magic_p1, magic_p1 * signed.hashCode)
    }
    override def _eval(c: EvaluationContext): (Expr[X],EvaluationContext) = {
      val (ev: Expr[X],_) = EmptyContext.fullEval(signed) // we don't need to memoize the sub-trace
      val crypr: Expr[X] = CryptoSigned(copyHash(ev,bits_128*4))
      val tt = node(Eval(this,2)).concat(node(crypr)).concat(node(ev.head))
      (TracedExpr(tt),c)
    }
    def parts = Array(this)
    override def unary_~ = SignedExpr(signed,bits_128+1)
    override def toString = wsp(bits_128,"~")+signed
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
  def expr[X <: Hashable](e: X) = LeafExpr(e)

  // convenience
  def %[A, X](f: FA1[A,X], a: Expr[A]): Expr[X] =  F1(f, a)
  def %[A, B, X](f: FA2[A,B,X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f,a,b)
  def %[A, B, C, X](f: FA3[A,B,C,X], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = F3(f,a,b,c)
  def ~%[A, X](f: FA1[A,X], a: Expr[A]): Expr[X] = ~(%(f,a))
  def ~%[A, B, X](f: FA2[A,B,X], a: Expr[A], b: Expr[B]): Expr[X] = ~(%(f,a,b))
  def ~%[A, B, C, X](f: FA3[A,B,C,X], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = ~(%(f,a,b,c))

  // Ideally we should recursively Hash all the java byte code (full dependency graph)
  // For now we just use hash the full qualified class name until we implement that.
  trait CodeHash extends Hashable with Hash {
    var lazy_hash: Array[Int] = null

    def bhash: Array[Int] = {
      import java.security.MessageDigest
      if (lazy_hash == null) {
        var md = MessageDigest.getInstance("SHA-256");
        md.update(getClass().toString.getBytes())
        val bb = java.nio.ByteBuffer.wrap(md.digest).asIntBuffer
        val ib: Array[Int] = new Array(bb.limit)
        bb.get(ib)
        lazy_hash = ib
      }
      lazy_hash
    }
    def parts = Array()
    def hash = this
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else bhash(i % (bhash.size-1))
    }
    override def hashCode = bhash(0)
  }

  // Pretty print a trace
  def prettyPrint(e: SHNode[Expr[_]], depth: Int): String = {
    if (e == null) ""
    else if (e.size == 1) prettyAtDepth(e.first,depth)
    else if (e.size == 2) prettyAtDepth(e.first,depth) + " => " + e.last
    else e.first match {
      case Eval(x,i) => {
        val (l,r) = e.split(i + 1)
        val (l2,r2) = l.split(1)

        if ((l2.size == 1) && (r2.size == 1)) prettyPrint(l2 ! r2,depth) + "\n" + prettyPrint(r,depth)
        else prettyPrint(l2,depth) + " =>\n" + prettyPrint(r2,depth + 1) + "\n" + prettyPrint(r,depth)
      }
      case x => prettyAtDepth(x,depth)
    }
  }

  def prettyAtDepth(e: Expr[_], depth: Int): String = e match {
    case Eval(x,_) => wsp(depth) + x
    case TracedExpr(t) => wsp(depth) + "[" + "\n" + prettyPrint(t, depth+1) + "\n" + wsp(depth) + "]"
    case x => wsp(depth) + x
  }

  def wsp(d: Int): String = wsp(d,"\t")

  def wsp(d: Int, v: String): String = {
    if (d == 0) ""
    else if (d == 1) v
    else wsp(d/2,v) + wsp(d - (d/2),v)
  }

  trait Equal2[X] extends FA2[X,X,Boolean] with InfixOperator {
    def apply(x: F0[X], y: F0[X]) = SpreadLogic.bexpr(x.value == y.value)
    override def toString = "!=="
  }

  object Equal3 extends Equal2[Nothing]

  def equal2[X]: FA2[X,X,Boolean] = Equal3.asInstanceOf[Equal2[X]]

}