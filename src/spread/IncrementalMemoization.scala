package spread

import spread.Hashing._
import scala.collection.immutable.Map

/*
  SPREAD lib: Incremental Computational Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations and functions
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused hashed-consed functions

  - Requires scala 2.11 and higher (usage of TypeTags)

  Copyright 2014: Robbert van Dalen
*/

/*
 Ideas:
  - (destructively) eraseTrail([((1 ++ 2) ** (3 ++ 4)) ~> [([(1 ++ 2) ~> 3] ** [(3 ++ 4) ~> 7]) ~> [(3 ** 7) ~> 21]]]) =>
          [((1 ++ 2) ** (3 ++ 4)) ~> 21]
  -

 */
object IncrementalMemoization {

  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import scala.collection.mutable.HashMap
  import scala.collection.mutable.HashSet

  import java.lang.ref.WeakReference
  import Hashing._
  import scala.reflect.runtime.universe.TypeTag

  trait Context {
    def ref(parent: Expr[_], child: Expr[_]): Context
    def mput(e: Trace[_]): Context
    def contains(e: Expr[_]): Boolean
    def mget[X](e: Expr[X]): Trace[X]
  }

  trait Expr[V] {
    def eval(c: Context): (Context,Expr[V])
    def backref(c: Context): Context = c

    def unquote: Expr[V]
    def containsQuotes: Boolean

    def eraseTrail: Expr[V] = this

    def quote: Expr[V] = hcons(Quote(this))

    def unary_! : Expr[V] = quote
  }

  trait Infix

  trait Func {
    override def toString = getClass.getSimpleName
  }

  var cachehit: Long = 0

  trait LazyExpr[V] extends Expr[V] {

    def eval(c: Context): (Context,Expr[V]) = {
      if (c.contains(this)) {
        cachehit = cachehit + 1
        println("from cache " + cachehit )
        (c,c.mget(this))
      }
      else {
        val (nc,e) = tracedEval(c)
        val t = trace(this,e)
        (nc.mput(t),t)
      }
    }
    def tracedEval(c: Context): (Context,Expr[V])
  }

  trait Trace[V] extends Expr[V] {
    def containsQuotes = to.containsQuotes
    def unquote = if (containsQuotes) to.unquote ; else this

    def from: Expr[V]
    def to: Expr[V]

    override def backref(c: Context): Context = to.backref(c)

    override def eraseTrail: Expr[V] = trace(getFrom(from).eraseTrail,getTo(to).eraseTrail)

    override val hashCode = jh(jh(jh(from)) ^ jh(to))
  }

  case class Trace0[V](from: Expr[V], to: F0[V]) extends Trace[V] with F0[V] {
    def evalValue = to.evalValue
    override def toString = "[" + getFrom(from) + " ~> " + to + "]"
  }

  case class Trace1[V](from: Expr[V], to: Expr[V]) extends Trace[V] with LazyExpr[V] {
    def tracedEval(c: Context) = {
      val (c1,te) = to.eval(c)
      if (te == to) (c1,this)
      else (c1,te)
    }
    override def toString = "[" + getFrom(from) + " => " + to + "]"
  }

  def getFrom[V](e: Expr[V]): Expr[V] = e match {
    case t: Trace[V] => getFrom(t.from)
    case _ => e
  }

  def getTo[V](e: Expr[V]): Expr[V] = e match {
    case t: Trace[V] => getTo(t.to)
    case _ => e
  }

  def trace[V](from: Expr[V], to: Expr[V]): Trace[V] = {
    to match {
      case f0: F0[V] => Trace0(from,f0)
      case _ => Trace1(from,to)
    }
  }

  case class Quote[V](expr: Expr[V]) extends Expr[V] {
    def containsQuotes = true
    def unquote = expr

    override def backref(c: Context): Context = expr.backref(c)

    def eval(c: Context) = (c,this)
    override def toString = "!" + expr
  }

  val vt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def hcons[V](c: Expr[V]): Expr[V] = {
    vt.get(c) match {
      case None => hcons2(c)
      case Some(x) => {
        val xg = x.get
        if (xg != null) {
          xg.asInstanceOf[Expr[V]]
        }
        else hcons2(c)
      }
    }
  }

  def hcons2[V](c: Expr[V]): Expr[V] = { vt.put(c, new WeakReference(c)) ; c }

  val rt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = hcons(FF1(f, a))
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = hcons(FF2(f, a, b))
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    hcons(FF3(f, a, b, c))
  }

  def evalArg[A](c: Context, e: Expr[A]): (Context,Expr[A]) = e match {
    case t0: Trace[A] => (c,getTo(t0))
    case _ => fullRed(c,e)
  }

  def fullRed[A](c: Context, e: Expr[A]): (Context,Expr[A]) = {
    val (nc,ee) = e.eval(c)
    if (ee == e) (nc,ee)
    else fullRed(nc,ee)
  }

  case class ExprImpl[X](evalValue: X) extends F0[X] {
    def containsQuotes = false
    def unquote = this
    def tracedEval(c: Context) = this
    def normalize = this
    override def toString = "`" + evalValue.toString
  }

  def ei[X](x: X): Expr[X] = ExprImpl(x)

  trait F0[R] extends Expr[R] {
    def eval(c: Context) = (c,this)
    def evalValue: R
    def unary_~ : R = evalValue
  }

  trait F1[A, R] extends LazyExpr[R] {
    def f: Expr[A] => Expr[R]
    def a: Expr[A]

    override val hashCode = jh(jh(f) ^ jh(a))
  }

  trait F2[A, B, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]

    override val hashCode = jh(jh(jh(f) ^ jh(a)) + jh(b))
  }

  trait F3[A, B, C, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B], Expr[C]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]
    def c: Expr[C]

    override val hashCode = jh(jh(jh(jh(f) ^ jh(a)) + jh(b)) - jh(c))
  }

  type F_ = F0[_]

  case class FF1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends F1[A, R] {
    val containsQuotes = a.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote) ; else this
    override def eraseTrail: Expr[R] = %(f,a.eraseTrail)

    override def backref(c: Context): Context = c.ref(this,a)

    def tracedEval(c: Context) = {
      val (c1,aa) = evalArg(c,a)

      if (a == aa) (c1,f(aa))
      else (c1,%(f,aa))
    }

    override def toString = f + "(" + a + ")"
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends F2[A, B, R] {
    val containsQuotes = a.containsQuotes || b.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote,b.unquote) ; else this

    override def backref(c: Context): Context = c.ref(this,a).ref(this,b)

    override def eraseTrail: Expr[R] = %(f,a.eraseTrail,b.eraseTrail)

    def tracedEval(c: Context) = {
      val (ca,aa) = evalArg(c,a)
      val (cb,bb) = evalArg(ca,b)

      if ((a == aa) && (b == bb)) (cb,f(aa,bb))
      else (cb,%(f,aa,bb))
    }
    override def toString = {
      f match {
        case i: Infix => "(" + a + " " + f + " " + b + ")"
        case _ => f + "(" + a + "," + b + ")"
      }
    }
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends F3[A, B, C, R] {
    val containsQuotes = a.containsQuotes || b.containsQuotes || c.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote,b.unquote,c.unquote) ; else this

    override def eraseTrail: Expr[R] = %(f,a.eraseTrail,b.eraseTrail,c.eraseTrail)
    override def backref(cc: Context): Context = cc.ref(this,a).ref(this,b).ref(this,c)

    def tracedEval(co: Context) = {
      val (ca,aa) = evalArg(co,a)
      val (cb,bb) = evalArg(ca,b)
      val (ccc,cc) = evalArg(cb,c)

      if ((a == aa) && (b == bb) && (c == cc)) (ccc,f(aa,bb,cc))
      else (ccc,%(f,aa,bb,cc))
    }
    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }

  trait FA1[A, X] extends (Expr[A] => Expr[X]) with Func {
    def apply(a: Expr[A]): Expr[X] = a match {
      case (af: F0[A]) => apply(af)
      case _ => %(this, a)
    }
    def apply(a: F0[A]): Expr[X]
  }

  trait FA2[A, B, X] extends ((Expr[A], Expr[B]) => Expr[X]) with Func {
    def apply(a: Expr[A], b: Expr[B]): Expr[X] = (a, b) match {
      case (af: F0[A], bf: F0[B]) => apply(af, bf)
      case _ => %(this, a, b)
    }
    def apply(a: F0[A], b: F0[B]): Expr[X]
  }

  trait FA3[A, B, C, X] extends ((Expr[A], Expr[B], Expr[C]) => Expr[X]) with Func {
    def apply(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = (a, b, c) match {
      case (af: F0[A], bf: F0[B], cf: F0[C]) => apply(af, bf, cf)
      case _ => %(this, a, b, c)
    }
    def apply(a: F0[A], b: F0[B], c: F0[C]): Expr[X]
  }


  type B = Expr[Boolean]

  trait BExpr extends Expr[Boolean] {
    def origin: B
    def |||(o: B): B = %(or, origin, o)
    def &&&(o: B): B = %(and, origin, o)
  }

  type F0B = F0[Boolean]

  trait BB extends BExpr with F0B {
    def containsQuotes = false
    def unquote = this

    def contains[X](x: X) = BFalse
    def set[X,O: TypeTag](x: X, e: Expr[O]) = this
    def origin = this
  }

  case object BTrue extends BB {
    def evalValue = true
    override def toString = "true"
  }

  case object BFalse extends BB {
    def evalValue = false
    override def toString = "false"
  }

  implicit def toB(b: Boolean) = if (b) BTrue ; else BFalse

  private case class BWrap(origin: B) extends BExpr {
    def containsQuotes = error

    def eval(c: Context) = error
    def unquote = error
    def error = sys.error("BWrap should not be used directly")
  }

  implicit def toBWrap(b: B): BExpr = b match {
    case bb: BExpr => bb
    case _ => BWrap(b)
  }

  object or extends FA2[Boolean,Boolean,Boolean] with Infix {
    def apply(a: F0B, b: F0B) = a.evalValue || b.evalValue
    override def toString = "|||"
  }

  object and extends FA2[Boolean,Boolean,Boolean] with Infix {
    def apply(a: F0B, b: F0B) = a.evalValue && b.evalValue
    override def toString = "&&&"
  }

  case class WB(i: Boolean) {
    def unary_~ = toB(i)
  }

  implicit def toWB(i: Boolean): WB = WB(i)

  val cc = CC(Map(),Map())

  object NC extends Context {
    def ref(parent: Expr[_], child: Expr[_]): Context = this
    def mput(e: Trace[_]): Context = this
    def contains(e: Expr[_]): Boolean = false
    def mget[X](e: Expr[X]): Trace[X] = sys.error("no")
  }

  case class CC(h: Map[Expr[_],Trace[_]], b: Map[Expr[_],Set[Expr[_]]]) extends Context {
    def ref(parent: Expr[_], child: Expr[_]): Context = {
      if (b.contains(child)) {
        val s = b.get(child).get
        CC(h,b + (child -> (s + parent)))
      }
      else {
        val CC(_,b1) = child.backref(this)
        CC(h,b1 + (child -> Set(parent)))
      }
    }

    def mput(e: Trace[_]): Context = {
      if (!h.contains(e.from)) CC(h + (e.from -> e) ,b)
      else this
    }
    def contains(e: Expr[_]): Boolean = h.contains(e)
    def mget[X](e: Expr[X]): Trace[X] = h.get(e).get.asInstanceOf[Trace[X]]
  }
}