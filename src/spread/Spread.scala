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
import scala.reflect.runtime.universe.TypeTag

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
    def first: Expr[V] = trace.first.asInstanceOf[Expr[V]]
    def head: Expr[V] = trace.last.asInstanceOf[Expr[V]]
    def size = 1

    def hash = this
    def ===(o: Expr[V]) = this == o
    def !==(o: Expr[V]): Expr[Boolean] = %(equal2[V],this,o)

    def unary_~ : Expr[V] = SignedExpr(this,1)
    def quote : Expr[V] = Quoted(this)
    def fullEval: Expr[V] = fullEval(EmptyContext)._1
    def fullEval(c: EvaluationContext) = c.fullEval(this)

    def eval(c: EvaluationContext): (Expr[V],EvaluationContext) = c.eval(this)
    def _eval(c: EvaluationContext): (Expr[V],EvaluationContext) = (this,c)

    private var lazyProperties = 0   // lazy encoding of properties

    def properties = {
      if (lazyProperties == 0) {
        var p = 1
        p = p | ((_depth & ((1 << 30)-1)) << 1)
        p = p | (toInt(_containsVariable) << 30)
        p = p | (toInt(_containsQuote) << 31)
        lazyProperties = p
      }
      lazyProperties
    }
    def toInt(b: Boolean) = if (b) 1 ; else 0
    def toBool(i: Int) = if (i == 1) true ; else false

    def _containsVariable: Boolean = false
    def _containsQuote: Boolean  = false
    def _depth: Int = 1

    def unquote: Expr[V] = %(unquote2,WrappedExpr(this))
    def _unquote: Expr[V]

    def bind[Y](s: Symbol, x: Expr[Y])(implicit t: TypeTag[Y]): Expr[V] = {
      val e: Expr[Expr[V]] = WrappedExpr(this)
      val v: Expr[Variable[Y]] = expr(VariableImpl(s)(t))
      val b: Expr[Expr[Y]] = WrappedExpr(x)

      %[Expr[V],Variable[Y],Expr[Y],V](bind2,e,v,b)
    }
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]): Expr[V]
    final def containsVariable: Boolean = toBool((properties >>> 30) & 1)
    final def containsQuote: Boolean = toBool((properties >>> 31) & 1)
    final def depth: Int = (properties & ((1 << 30)-1)) >>> 1
  }

  trait HashedExpr[V] extends Expr[V] {
    private var lHash = 0
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
    def _unquote = this
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this
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
    override def _eval(c: EvaluationContext) = (o,c)
    def parts = o.parts
    def _unquote = this
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this
    override def toString = o.toString + "@" + distance
  }

  trait Operator { override def toString = getClass().getSimpleName.replace("$","") }
  trait InfixOperator extends Operator
  trait PostfixOperator extends Operator

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
  private case class F1[A,X](f: FA1[A,X], v1: Expr[A]) extends HashedExpr[X] {
    override def toString = {
      if (f.isInstanceOf[PostfixOperator]) v1 + "." + f
      else f+"("+v1+")"
    }
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
      case x1: F0[A] => {
        var trace: SHNode[Expr[_]] = null
        val n = node(f(x1))

        val t1 = v1.trace.size == 1

        if (!t1) trace = concat(trace,v1.trace)

        if (!t1) {
          // patch the traces
          val (Eval(f1,_)) = v1.first
          val n2 = %(f,x1)
          val n3 = Eval(n2,1)
          trace = node(Eval(%(f,f1),trace.size+1)).concat(trace).concat(node(n2)).concat(node(n3)).concat(n)
          (TracedExpr(trace),c)
        }
        else (TracedExpr(node(Eval(this,1)).concat(n)),c)
      }
      case x1 => {
        val (e1,c2) = c.fullEval(x1)
        var trace: SHNode[Expr[_]] = null

        //val t1 = (e1 == x1)
        val t1 = e1.trace.size == 1

        if (!t1) trace = concat(trace,e1.trace)

        if (!t1) {
          trace = concat(trace,node(%(f,e1.head)))
          (TracedExpr[X](node(Eval(this,trace.size)).concat(trace)),c2)
        }
        else (this,c)
      }
    }
    def _unquote = {
      if (containsQuote) %(f,v1._unquote)
      else this
    }
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) %(f,v1._bindVariable(s,x))
      else this
    }
    override def _depth = v1.depth + 1
    override def _containsQuote = v1.containsQuote
    override def _containsVariable = v1.containsVariable
    def parts = Array(f,v1)
  }

  // Denotes an binary function call
  private case class F2[A,B,X](f: FA2[A,B,X], v1: Expr[A], v2: Expr[B]) extends HashedExpr[X] {
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

    // TODO: compress this code - there is redundancy
    override def _eval(c: EvaluationContext): (Expr[X], EvaluationContext) = (v1.head,v2.head) match {
      case (x1: F0[A],x2: F0[B]) => {
        var trace: SHNode[Expr[_]] = null
        val n = node(f(x1,x2))

        val t1 = v1.trace.size == 1
        val t2 = v2.trace.size == 1

        if (!t1) trace = concat(trace,v1.trace)
        if (!t2) trace = concat(trace,v2.trace)

        if (!t1 || !t2) {
          // patch the traces

          val (Eval(f1,_)) = v1.first
          val (Eval(f2,_)) = v2.first
          val n2 = %(f,x1,x2)
          val n3 = Eval(n2,1)
          trace = node(Eval(%(f,f1,f2),trace.size+1)).concat(trace).concat(node(n2)).concat(node(n3)).concat(n)
          (TracedExpr(trace),c)
        }
        else (TracedExpr(node(Eval(this,1)).concat(n)),c)
      }
      case (x1,x2) => {
        val (e1,c2) = x1.fullEval(c)
        val (e2,c3) = x2.fullEval(c2)
        var trace: SHNode[Expr[_]] = null

        val t1 = e1.trace.size == 1
        val t2 = e2.trace.size == 1

        if (!t1) trace = concat(trace,e1.trace)
        if (!t2) trace = concat(trace,e2.trace)

        if (!t1 || !t2) {
          trace = concat(trace,node(%(f,e1.head,e2.head)))
          (TracedExpr(node(Eval(this,trace.size)).concat(trace)),c3)
        }
        else (this,c)
      }

    }
    override def _depth = (v1.depth max v2.depth) + 1
    override def _containsQuote = v1.containsQuote || v2.containsQuote
    override def _containsVariable = v1.containsVariable || v2.containsVariable

    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) %(f,v1._bindVariable(s,x),v2._bindVariable(s,x))
      else this
    }

    def _unquote = {
      if (containsQuote) %(f,v1._unquote,v2._unquote)
      else this
    }
    def parts = Array(f,v1,v2)
  }

  // Denotes an ternary function call
  private case class F3[A,B,C,X](f: FA3[A,B,C,X], v1: Expr[A], v2: Expr[B], v3: Expr[C]) extends HashedExpr[X] {
    override def toString = {
      if (f.isInstanceOf[PostfixOperator]) v1 + "." + f + "(" + v2 + "," + v3 + ")"
      else f + "(" + v1 + "," + v2 + "," + v3 + ")"
    }
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
      case (x1: F0[A],x2: F0[B], x3: F0[C]) => {
        var trace: SHNode[Expr[_]] = null
        val n = node(f(x1,x2,x3))

        val t1 = v1.trace.size == 1
        val t2 = v2.trace.size == 1
        val t3 = v3.trace.size == 1

        if (!t1) trace = concat(trace,v1.trace)
        if (!t2) trace = concat(trace,v2.trace)
        if (!t3) trace = concat(trace,v3.trace)

        if (!t1 || !t2 || !t3) {
          // patch the traces
          val (Eval(f1,_)) = v1.first
          val (Eval(f2,_)) = v2.first
          val (Eval(f3,_)) = v3.first

          val n2 = %(f,x1,x2,x3)
          val n3 = Eval(n2,1)

          trace = node(Eval(%(f,f1,f2,f3),trace.size+1)).concat(trace).concat(node(n2)).concat(node(n3)).concat(n)
          (TracedExpr(trace),c)
        }
        else (TracedExpr(node(Eval(this,1)).concat(n)),c)
      }
      case (x1,x2,x3) => {
        val (e1,c2) = x1.fullEval(c)
        val (e2,c3) = x2.fullEval(c2)
        val (e3,c4) = x3.fullEval(c3)

        var trace: SHNode[Expr[_]] = null

        val t1 = e1.size == 1
        val t2 = e2.size == 1
        val t3 = e3.size == 1

        if (!t1) trace = concat(trace,e1.trace)
        if (!t2) trace = concat(trace,e2.trace)
        if (!t3) trace = concat(trace,e3.trace)

        if (!t1 || !t2 || !t3) {
          trace = concat(trace,node(%(f,e1.head,e2.head,e3.head)))
          (TracedExpr(node(Eval(this,trace.size)).concat(trace)),c4)
        }
        else (this,c)
      }

    }
    override def _depth = (v1.depth max v2.depth max v3.depth) + 1
    override def _containsQuote = v1.containsQuote || v2.containsQuote || v3.containsQuote
    override def _containsVariable = v1.containsVariable || v2.containsVariable | v3.containsVariable
    def _unquote = {
      if (containsQuote) {
        %(f,v1._unquote,v2._unquote,v3._unquote)
      }
      else this
    }
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) %(f,v1._bindVariable(s,x),v2._bindVariable(s,x),v3._bindVariable(s,x))
      else this
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
    override def _containsQuote = head.containsQuote
    override def _containsVariable = head.containsVariable
    override def _depth = head.depth + 1

    override def _unquote = this
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this

    def parts = Array(this)
    override def toString = prettyPrint(trace,0,"")
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
    def _unquote = {
      if (containsQuote) SignedExpr(signed._unquote,bits_128)
      else this
    }
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) SignedExpr(signed._bindVariable(s,x),bits_128)
      else this
    }
    override def _containsQuote = signed.containsQuote
    override def _containsVariable = signed.containsVariable
    override def _depth = signed.depth + 1

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
    def _unquote = this
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this
    override def toString = "$("+value+")"
  }

  case class WrappedExpr[X](value: Expr[X]) extends F0[Expr[X]] {
    def lazyHash = siphash24(value.hash.hashCode - magic_p3, magic_p2 * value.hash.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) lazyHash
      else siphash24(value.hash.hashAt(i) + magic_p2, magic_p3 - value.hashCode)
    }
    def parts = Array(this)
    override def _containsQuote = value.containsQuote
    override def _containsVariable = value.containsVariable
    override def _depth = value.depth + 1
    def _unquote = {
      if (containsQuote) WrappedExpr(value._unquote)
      else this
    }
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) WrappedExpr(value._bindVariable(s,x))
      else this
    }
    override def toString = "" + value
  }

  def node[X](e: Expr[X]) = ExprSHNode(e)
  def expr[X <: Hashable](e: X) = LeafExpr(e)

  // smart constructors
  def %[A, X](f: FA1[A,X], a: Expr[A]): Expr[X] = F1(f,a.head)
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

  // Pretty print a trace (complicated!)
  def prettyPrint(e: SHNode[Expr[_]], depth: Int, prefix: String): String = {
    if (e == null) ""
    else {
      val s = {
        if (e.size == 1) prettyAtDepth(e.first,depth)
        else if (e.size == 2) prettyAtDepth(e.first,depth) + " => " + e.last
        else e.first match {
          case Eval(x,i) => {
            val (l,r) = e.split(i + 1)
            val (l2,r2) = l.split(1)

            if ((l2.size == 1) && (r2.size == 1)) prettyPrint(l2 ! r2,depth,"") + prettyPrint(r,depth,"\n")
            else prettyPrint(l2,depth,"") + prettyPrint(r2,depth + 1," =>\n") + prettyPrint(r,depth,"\n")
          }
          case x => prettyAtDepth(x,depth)
        }
      }
      prefix + s
    }
  }

  def prettyAtDepth(e: Expr[_], depth: Int): String = e match {
    case Eval(x,_) => wsp(depth) + x
    case TracedExpr(t) => wsp(depth) + "[" + "\n" + prettyPrint(t, depth+1,"") + "\n" + wsp(depth) + "]"
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

  trait Unquote2[X] extends FA1[Expr[X],X] with PostfixOperator {
    def apply(x: F0[Expr[X]]) = x.value._unquote
    override def toString = "unquote"
  }

  object Unquote3 extends Unquote2[Nothing]
  def unquote2[X]: FA1[Expr[X],X] = Unquote3.asInstanceOf[FA1[Expr[X],X]]

  trait Bind2[X,Y] extends FA3[Expr[X],Variable[Y],Expr[Y],X] with PostfixOperator {
    def apply(x: F0[Expr[X]], v: F0[Variable[Y]], b: F0[Expr[Y]]) = x.value._bindVariable(v.value.s,b.value)(v.value.t)
    override def toString = "bind"
  }
  object Bind3 extends Bind2[Nothing,Nothing]
  def bind2[X,Y]: FA3[Expr[X],Variable[Y],Expr[Y],X] = Bind3.asInstanceOf[FA3[Expr[X],Variable[Y],Expr[Y],X]]

  case class Quoted[X](e: Expr[X]) extends HashedExpr[X] {
    def lazyHash = siphash24(e.hashCode * magic_p1, e.hashCode * magic_p2 + e.hashCode)
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else if (i == 1) siphash24(e.hashCode * magic_p1, hashCode * magic_p2)
      else siphash24(hashAt(i-1), e.hashCode ^ hashCode)
    }
    override def _eval(c: EvaluationContext) = (this,c)
    def parts = Array(e)
    override def _depth = e.depth + 1
    override def _containsQuote = true
    override def _containsVariable = false
    def _unquote = e
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this
    override def toString = e + ".quote"
  }

  trait Variable[X] extends Expr[X]{
    def s: Symbol
    def t: TypeTag[X]
    def lazyHash = siphash24(s.hashCode * magic_p1,s.hashCode * magic_p2 + s.hashCode)
    def hashAt(i: Int) ={
      if (i == 0) hashCode
      else if (i == 1) siphash24(s.hashCode * magic_p1,hashCode * magic_p2)
      else siphash24(hashAt(i - 1),s.hashCode ^ hashCode)
    }
    override def _eval(c: EvaluationContext) = (this,c)
    def parts = Array()
    override def _containsVariable = true
    def _bindVariable[Y](ss: Symbol, x: Expr[Y])(implicit ot: TypeTag[Y]) = {
      def evt = t.tpe
      def ovt = ot.tpe
      if ((ss == s) &&( evt <:< ovt)) x.asInstanceOf[Expr[X]]  // checks whether it's a subtype that can be bound
      else this
    }
    override def toString = s.toString
  }

  case class VariableImpl[X](s: Symbol)(implicit t2: TypeTag[X]) extends Variable[X] {
    def t = t2
    def _unquote = this
  }
}