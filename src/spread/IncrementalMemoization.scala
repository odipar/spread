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
  5) automatic garbage collection of unused hconsoized functions

  - Requires scala 2.11 and higher (usage of TypeTags)

  Copyright 2014: Robbert van Dalen
*/


object IncrementalMemoization {

  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference
  import Hashing._

  trait Expr[V] {
    def eval: Expr[V]
    def contains[X](x: X): Expr[Boolean]
    def replace[X,O](x: X, o: Expr[O]): Expr[V] = {
      val c = contains(x)
      val to = set(x,o)
      ContainsReplace(x,o,c,this,to)
    }
    def set[X,O](x: X, o: Expr[O]): Expr[V]

    def \ = ?(this)
    def ?[X](x: X): Expr[Boolean] = contains(x)
  }

  trait LazyExpr[V] extends Expr[V] {
    def reduce: Expr[V]
    def check[X](x: X): Expr[Boolean]
    def contains[X](x: X): Expr[Boolean] = fullRed(hcons(Contains(this, x)))
    def eval: Expr[V] = rt.get(this) match {
      case None => eval2
      case Some(x) => {
        val xg = x.get
        if (xg != null) xg.asInstanceOf[Expr[V]]
        else eval2
      }
    }
    def eval2: Expr[V] = {
      val r = trace(this,reduce)
      rt.put(this, new WeakReference(r))
      return r
    }
  }

  def ?[V](e: Expr[V]) = hcons(Quote(e))

  trait Trace[V] extends Expr[V] {
    def from: Expr[V]
    def to: Expr[V]

    override val hashCode = jh(jh(jh(from)) ^ jh(to))

  }

  case class Trace0[V](from: Expr[V], to: F0[V]) extends Trace[V] with F0[V] {
    def value = to.value
    def set[X,O](x: X, e: Expr[O]): Expr[V] = to.set(x,e)
    def contains[X](x: X) = to.contains(x)
    override def toString = "[" + getFrom(from) + " ~> " + to + "]"
  }

  case class Trace1[V](from: Expr[V], to: Expr[V]) extends Trace[V] with LazyExpr[V] {
    def reduce = to.eval
    def check[X](x: X) = to.contains(x)
    def set[X,O](x: X, e: Expr[O]): Expr[V] = to.set(x,e)
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

  def trace[V](from: Expr[V], to: Expr[V]): Expr[V] = {
    hcons(to match {
      case f0: F0[V] => Trace0(from,f0)
      case _ => Trace1(from,to)
    })
  }

  case class ContainsReplace[X,V,O](x: X, o: Expr[O], contains: Expr[Boolean], from: Expr[V], to: Expr[V]) extends LazyExpr[V] {
    def reduce = trace(this,to.eval)
    def check[X](x: X): Expr[Boolean] = to.contains(x)
    def set[X,O](x: X, o: Expr[O]): Expr[V] = to.set(x,o)
    override def toString = from + "@(" + x + "," + o + ")  => " + to
  }

  case class Contains[V,X](expr: LazyExpr[V], x: X) extends LazyExpr[Boolean] {
    def reduce = trace(this,expr.check(x))
    def check[XX](x: XX) = this
    def set[X,O](x: X, e: Expr[O]) = this
    override def toString = expr + " ? " + x
  }

  case class Quote[V](expr: Expr[V]) extends Expr[V] {
    def eval = this
    def contains[X](x: X) = expr.contains(x)
    def set[X,O](x: X, e: Expr[O]): Expr[V] = expr.set(x,e)
    override def toString = expr +".\\"
  }

  case class Var[X,V](label: X, expr: Expr[V]) extends LazyExpr[V] {
    def reduce = trace(this,expr)
    def check[X](x: X) = {
      if (label == x) BTrue ; else expr.contains(x)
    }
    def set[X,O](x: X, e: Expr[O]): Expr[V] = {
      if (label == x) Var(label,e).asInstanceOf[Expr[V]]
      else Var(label,expr.set(x,e))
    }
    override def toString = label + "~" + expr
  }

  val vt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def hcons[V](c: Expr[V]): Expr[V] = {
    vt.get(c) match {
      case None => hcons2(c)
      case Some(x) => {
        val xg = x.get
        if (xg != null) xg.asInstanceOf[Expr[V]]
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

  def reduceArg[A](e: Expr[A]): Expr[A] = e match {
    case t0: Trace0[A] => getTo(t0)
    case _ => fullRed(e)
  }

  def fullRed[A](e: Expr[A]): Expr[A] = {
    val ee = e.eval
    if (ee == e) ee
    else fullRed(ee)
  }

  case class ExprImpl[X](value: X) extends F0[X] {
    def reduce = this
    def set[X,O](x: X, e: Expr[O]) = this
    def contains[X](x: X) = BFalse
    def normalize = this
    override def toString = "`" + value.toString
  }

  def ei[X](x: X): Expr[X] = hcons(ExprImpl(x))

  trait F0[R] extends Expr[R] {
    def eval = this
    def value: R
    def unary_! : R = value
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
    def reduce = {
      val aa = reduceArg(a)

      if (a == aa) {
        aa match {
          case fa: F0[A] => f(aa)
          case _ => this
        }
      }
      else %(f,aa)
    }

    def check[X](x: X) = a.contains(x)
    def set[X,O](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      if ((aa == a)) this
      else %(f,aa)
    }
    override def toString = f + "(" + a + ")"
  }

  def containsReplace[V,X,O](e: Expr[V], x: X, o: Expr[O]): Expr[V] = {
    e.contains(x) match {
      case f: F0[Boolean] => {
        val b = f.value
        if (b) e.set(x,o)
        else e
      }
      case _ => sys.error("illegal system state")
    }
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends F2[A, B, R] {
    def reduce = {
      val aa = reduceArg(a)
      val bb = reduceArg(b)

      if ((a == aa) && (b == bb)) {
        (aa,bb) match {
          case (fa: F0[A], fb: F0[B]) => f(aa,bb)
          case _ => this
        }
      }
      else %(f,aa,bb)
    }

    def check[X](x: X) = a.contains(x) ||| b.contains(x)
    def set[X,O](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      val bb = containsReplace(b,x,e)
      if ((aa == a) && (bb == b)) this
      else %(f,aa,bb)
    }
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends F3[A, B, C, R] {
    def reduce = {
      val aa = reduceArg(a)
      val bb = reduceArg(b)
      val cc = reduceArg(c)

      if ((a == aa) && (b == bb) && (c == cc)) {
        (aa,bb,cc) match {
          case (fa: F0[A], fb: F0[B], fc: F0[C]) => (f,aa,bb,cc)
          case _ => this
        }
      }
      %(f,aa,bb,cc)
    }

    def check[X](x: X) = a.contains(x) ||| b.contains(x) ||| c.contains(x)
    def set[X,O](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      val bb = containsReplace(b,x,e)
      val cc = containsReplace(c,x,e)

      if ((aa == a) && (bb == b) && (cc == c)) this
      else %(f,aa,bb,cc)
    }
    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }

  trait FA1[A, C] extends (Expr[A] => Expr[C]) {
    def apply(a: Expr[A]): Expr[C] = a match {
      case (af: F0[A]) => apply(af)
      case _ => %(this, a)
    }
    def apply(a: F0[A]): Expr[C]
  }

  trait FA2[A, B, C] extends ((Expr[A], Expr[B]) => Expr[C]) {
    def apply(a: Expr[A], b: Expr[B]): Expr[C] = (a, b) match {
      case (af: F0[A], bf: F0[B]) => apply(af, bf)
      case _ => %(this, a, b)
    }
    def apply(a: F0[A], b: F0[B]): Expr[C]
  }

  def unquote[X](x: Expr[X]): Expr[X] = {
    x match {
      case Quote(xx) => unquote(xx)
      case t: Trace[X] => {
        val tq = unquote(t.to)
        if (tq == t.to) t
        else trace(t.from,tq)
      }
      case f1: F1[_,X] => {
        val aa = unquote(f1.a)
        if (aa == f1.a) f1
        else trace(x,%(f1.f,aa))
      }
      case f2: F2[_,_,X] => {
        val aa = unquote(f2.a)
        val bb = unquote(f2.b)
        if ((aa == f2.a) && (bb == f2.b)) f2
        else trace(x,%(f2.f,aa,bb))
      }
      case f3: F3[_,_,_,X] => {
        val aa = unquote(f3.a)
        val bb = unquote(f3.b)
        val cc = unquote(f3.c)
        if ((aa == f3.a) && (bb == f3.b) && (cc == f3.c)) f3
        else trace(x,%(f3.f,aa,bb,cc))
      }
      case _ => x
    }
  }

  implicit def toL[X,V](x: X): L[X] = L(x)

  case class L[X](x: X) {
    def ~[V](e: Expr[V]): Expr[V] = Var(x,e)
  }

  type B = Expr[Boolean]

  trait BExpr extends Expr[Boolean] {
    def origin: B
    def |||(o: B): B = %(or, origin, o)
    def &&&(o: B): B = %(and, origin, o)
  }

  type F0B = F0[Boolean]

  trait BB extends BExpr with F0B {
    def contains[X](x: X) = BFalse
    def set[X,O](x: X, e: Expr[O]) = this
    def origin = this
  }

  case object BTrue extends BB {
    def value = true
    override def toString = "true"
  }

  case object BFalse extends BB {
    def value = false
    override def toString = "false"
  }

  implicit def toB(b: Boolean) = if (b) BTrue ; else BFalse

  private case class BWrap(origin: B) extends BExpr {
    def contains[X](x: X) = error
    def set[X,O](x: X, e: Expr[O]) = error
    def eval = error
    def unquote = error
    def error = sys.error("BWrap should not be used directly")
  }

  implicit def toBWrap(b: B): BExpr = b match {
    case bb: BExpr => bb
    case _ => BWrap(b)
  }

  object or extends FA2[Boolean,Boolean,Boolean] {
    def apply(a: F0B, b: F0B) = a.value || b.value
    override def toString = "|||"
  }

  object and extends FA2[Boolean,Boolean,Boolean] {
    def apply(a: F0B, b: F0B) = a.value && b.value
    override def toString = "&&&"
  }

  case class WB(i: Boolean) {
    def unary_! = toB(i)
  }

  implicit def toWB(i: Boolean): WB = WB(i)
}