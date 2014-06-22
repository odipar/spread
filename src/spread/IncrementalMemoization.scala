package spread
/*
  SPREAD lib: Incremental Computational Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations and functions
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused memoized functions

  Copyright 2013: Robbert van Dalen
*/


object IncrementalMemoization {
  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference

  trait Expr[V] {
    def eval: V
    def finish: Expr[V] = this
    def reduce: Expr[V] = this
    def replace[X](v: X, w: X): Expr[V] = {
      if (this == v) w.asInstanceOf[Expr[V]]
      else this
    }
  }

  val vt = WeakHashMap.empty[Expr[_], WeakReference[Expr[_]]]
  def mem[V](c: Expr[V]): Expr[V] = {
    if (!vt.contains(c)) vt.put(c, new WeakReference(c))
    vt.get(c).get.get.asInstanceOf[Expr[V]]
  }

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = mem(F1(f, a))
  def %%[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = mem(FF1(f, a))
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = mem(F2(f, a, b))
  def %%[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = mem(FF2(f, a, b))
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = mem(F3(f, a, b, c))
  def %%[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = mem(FF3(f, a, b, c))

  def fullRed[A](e: Expr[A]): Expr[A] = {
    var ee = e
    while (ee.reduce != ee) { ee = ee.reduce }
    ee
  }

  case class ExprImpl[X](x: X) extends Expr[X] {
    def eval = x
    override def toString = "`" + x.toString
  }

  implicit def ei[X](x: X): Expr[X] = mem(ExprImpl(x))

  case class F1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      if (aa == a) mem(f(a))
      else %(f, aa)
    }
    override lazy val eval = reduce.eval
    override def toString = f + "(" + a + ")"
  }

  case class FF1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    override lazy val finish = F1(f, a.finish)
    override lazy val eval = f(a).eval
    override def toString = f + "(" + a + ")"
  }

  case class F2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)

      if ((aa == a) && (bb == b)) mem(f(a, b))
      else %(f, aa, bb)
    }
    override lazy val eval = reduce.eval
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    override lazy val finish = F2(f, a.finish, b.finish)
    override lazy val eval = f(a, b).eval
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class F3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)
      val cc = fullRed(c)

      if ((aa == a) && (bb == b) && (cc == c)) mem(f(a, b, c))
      else %(f, aa, bb, cc)
    }
    override lazy val eval = reduce.eval
    override def toString = f + "[" + a + "," + b + "," + c + "]"
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    override lazy val finish = F3(f, a.finish, b.finish, c.finish)
    override lazy val eval = f(a, b, c).eval
    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }
}