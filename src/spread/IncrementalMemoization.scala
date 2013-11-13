package spread

/*
  SPREAD lib: Incremental Computation Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused memoized calls

  Copyright 2013: Robbert van Dalen
*/

object IncrementalMemoization {
  import language.implicitConversions

  var fc: Long = 0

  // An purely functional abstract Function FF that may depend on other Functions
  trait FF[F <: FF[F]] {
    def self: F                                         // the self type
    def force: F = self                                 // force dependency tree
    def contains(x: FF[_]) = false                      // check whether the dependency tree contains Function x
    def replaceable = true                              // check whether this can be replaced by another Function
    def replace[X <: FF[X]](a: FF[X], b: FF[X]) = self  // replaces Function a with Function b in the dependency tree

    synchronized { fc = fc + 1 } // statistics
  }

  // nullary Function
  trait F0[R] extends FF[F0[R]] {
    def apply(): R
  }

  // unary Function
  trait F1[A,R] extends FF[F1[A,R]] {
    def apply(a: A): R
  }

  // binary Function
  trait F2[A,B,R] extends FF[F2[A,B,R]] {
    def apply(a: A, b: B): R
  }

  // convenience types
  type FF1[X] = F1[X,X]
  type FF2[X] = F2[X,X,X]

  // strict evaluation
  def %*[A,R](f: F1[A,R], a: A): R = f(a)
  def %[A,R](f: F1[F0[A],F0[R]], a: F0[A]): F0[R] = f(a)
  def %*[A,B,R](f: F2[A,B,R], a: A, b: B): R = f(a,b)
  def %[A,B,R](f: F2[F0[A],F0[B],F0[R]], a: F0[A], b: F0[B]): F0[R] = f(a,b)

  // lazy memoized evaluation
  def %^[A,R](f: F1[A,R], a: F0[A]): F0[R] = hc(OUna(f,a))
  def %^[A,B,R](f: F2[A,B,R], a: F0[A], b: F0[B]): F0[R] = hc(OBin(f,a,b))
  def %![A,R](f: F1[A,F0[R]], a: A): F0[R] = hc(LUna(f,a))
  def %%[A,R](f: F1[F0[A],F0[R]], a: F0[A]): F0[R] = hc(LUna(f,a))
  def %![A,B,R](f: F2[A,B,F0[R]], a: A, b: B): F0[R] = hc(LBin(f,a,b))
  def %%[A,B,R](f: F2[F0[A],F0[B],F0[R]], a: F0[A], b: F0[B]): F0[R] = hc(LBin(f,a,b))

  // A lazy unary Function
  case class LUna[A,R](f: F1[A,F0[R]], a: A) extends F0[R] {
    def self = this
    lazy val eval: F0[R] = f(a)
    lazy val apply = force()
    override lazy val force = eval.force
    override def contains(x: FF[_]) = { if (x == self) true ; else f.contains(x) }
    override def replace[X <: FF[X]](a: FF[X], b: FF[X]): F0[R] = eval.replace(a,b)
    override def toString = f + "[" + a + "]"
  }

  // A lazy binary Function
  case class LBin[A,B,R](f: F2[A,B,F0[R]], a: A, b: B) extends F0[R] {
    def self = this
    lazy val eval = f(a,b)
    lazy val apply = force()
    override lazy val force = eval.force
    override def contains(x: FF[_]) = { if (x == self) true ; else f.contains(x) }
    override def replace[X <: FF[X]](a: FF[X], b: FF[X]): F0[R] = eval.replace(a,b)
    override def toString = "[" + a + " " + f + " " + b + "]"
  }

  // A unary Function with lazy arguments
  case class OUna[A,R](op: F1[A,R], a1: F0[A]) extends F0[R] {
    def self = this
    lazy val apply = %*(op,arg1)
    lazy val arg1 = a1()
    override def contains(x: FF[_]) = {
      if (x == self) true
      else op.contains(x) || a1.contains(x)
    }
    override def force = hc(OUna(op.force,a1.force))
    override def replace[X <: FF[X]](a: FF[X], b: FF[X]): F0[R] = {
      if (a == self) b.asInstanceOf[F0[R]]
      else hc(OUna(op.replace(a,b),a1.replace(a,b)))
    }
    override def toString = op + "(" + a1 + ")"
  }

  // A binary Function with lazy arguments
  case class OBin[A,B,R](op: F2[A,B,R], a1: F0[A], a2: F0[B]) extends F0[R] {
    def self = this
    lazy val apply = %*(op.force,arg1,arg2)
    lazy val arg1 = a1()
    lazy val arg2 = a2()
    override def contains(x: FF[_]) = {
      if (x == self) true
      else op.contains(x) || a1.contains(x) || a2.contains(x)
    }
    override def force = hc(OBin(op.force,a1.force,a2.force))
    override def replace[X <: FF[X]](a: FF[X], b: FF[X]): F0[R] = {
      if (a == self) b.asInstanceOf[F0[R]]
      else hc(OBin(op.replace(a,b),a1.replace(a,b),a2.replace(a,b)))
    }
    override def toString = "(" + a1 + " " + op + " " + a2 + ")"
  }

  // Any value can be a nullary Function
  case class FI[R](r: R) extends F0[R] {
    def self = this
    def apply() = r
    override def replace[X <: FF[X]](a: FF[X], b: FF[X]): F0[R] = {
      if (a == self) b.asInstanceOf[F0[R]]
      else this
    }
    override def toString = "`"+r.toString
  }

  implicit def to_fi[R](x: R): F0[R] = FI(x)

  // Some default implementations
  case object Add extends FF2[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int], arg2: F0[Int]) = %^(add1,arg1,arg2)
    override def toString = "+"
  }

  case object Mul extends FF2[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int], arg2: F0[Int]) = %^(mul1,arg1,arg2)
    override def toString = "*"
  }

  case object mul1 extends FF2[Int] {
    def self = this
    def apply(arg1: Int, arg2: Int) = arg1 * arg2
    override def toString = "*"
  }
  case object add1 extends FF2[Int] {
    def self = this
    def apply(arg1: Int, arg2: Int) = arg1 + arg2
    override def toString = "+"
  }

  // naive hash-consing, implemented via a weak HashMap
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference
  val mt = WeakHashMap.empty[F0[_], WeakReference[F0[_]]]

  def hc[R](c: F0[R]): F0[R] = synchronized {
    if (!mt.contains(c)) mt.put(c,new WeakReference(c))
    else { println("hit: " + c) }
    mt.get(c).get.get.asInstanceOf[F0[R]]
  }
}