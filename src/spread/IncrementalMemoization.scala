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
  5) automatic garbage collection of unused memoized functions

  - Requires scala 2.11 and higher (usage of TypeTags)

  Copyright 2014: Robbert van Dalen
*/


object IncrementalMemoization {

  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference

  trait Expr[V] {

    def eval: Expr[V]
    def unary_~ = quote(this)
  }

  trait LazyExpr[V] extends Expr[V] {
    var e: Expr[V] = null

    def eval: Expr[V] = {
      if (e == null) e = reduce  // no lock, because duplicate (purely functional) evaluation is allowed
      return e
    }
    def reduce: Expr[V]
  }

  def quote[V](e: Expr[V]) = mem(Quote(e))

  case class Quote[V](expr: Expr[V]) extends Expr[V] {
    def eval = Quote(expr.eval)
    override def toString = "~" + expr.toString
  }

  val vt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def mem[V](c: Expr[V]): Expr[V] = {
    if (!vt.contains(c)) vt.put(c, new WeakReference(c))
    else {  println("hit: " + c) }
    vt.get(c).get.get.asInstanceOf[Expr[V]]
  }

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = mem(FF1(f, a))
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = mem(FF2(f, a, b))
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    mem(FF3(f, a, b, c))
  }

  def fullRed[A](e: Expr[A]): Expr[A] = {
    val ee = e.eval
    if (ee == e) ee
    else fullRed(ee)
  }

  case class ExprImpl[X](value: X) extends F0[X] {
    def reduce = this
    override def toString = "`" + value.toString
  }

  def ei[X](x: X): Expr[X] = mem(ExprImpl(x))

  trait F0[R] extends Expr[R] {
    def eval = this
    def value: R
    def unary_! : R = value
  }

  trait F1[A, R] extends LazyExpr[R] {
    def f: Expr[A] => Expr[R]
    def a: Expr[A]
  }

  trait F2[A, B, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]
  }

  trait F3[A, B, C, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B], Expr[C]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]
    def c: Expr[C]
  }

  type F_ = F0[_]

  case class FF1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends F1[A, R] {
    def reduce = {
      val aa = fullRed(a)

      if (a == aa) {
        aa match {
          case fo: F_ => f(aa)
          case _ => this
        }
      }
      else %(f,aa)
    }
    override def toString = f + "(" + a + ")"
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends F2[A, B, R] {
    def reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)

      if ((a == aa) && (b == bb)) {
        (aa,bb) match {
          case (fa: F_, fb: F_) => f(aa,bb)
          case _ => this
        }
      }
      else %(f,aa,bb)
    }
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends F3[A, B, C, R] {
    def reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)
      val cc = fullRed(c)

      if ((a == aa) && (b == bb) && (c == cc)) {
        (aa,bb,cc) match {
          case (fa: F_, fb: F_, fc: F_) => f(aa,bb,cc)
          case _ => this
        }
      }
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

  def ~![X](x: Expr[X]): Expr[X] = {
    x match {
      case Quote(x) => ~!(x)
      case f1: F1[_,X] => {
        val aa = ~!(f1.a)
        %(f1.f,aa)
      }
      case f2: F2[_,_,X] => {
        val aa = ~!(f2.a)
        val bb = ~!(f2.b)
        %(f2.f,aa,bb)
      }
      case f3: F3[_,_,_,X] => {
        val aa = ~!(f3.a)
        val bb = ~!(f3.b)
        val cc = ~!(f3.c)
        %(f3.f,aa,bb,cc)
      }
      case _ => x
    }
  }
}