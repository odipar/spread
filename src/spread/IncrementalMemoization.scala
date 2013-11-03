package spread

import java.lang.ref.WeakReference

/*
  SPREAD lib: Incremental Computational Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused memoized calls

  Copyright 2013: Robbert van Dalen
*/

object IncrementalMemoization {

  // Applying a function call FCall[+R] will yield its evaluation R
  trait FCall[+R] {
    def apply(): R
  }

  // A nullary function call with zero arguments
  trait NCall[+R] extends FCall[R]

  // An unary function call with one argument A
  trait UCall[A,+R] extends FCall[R] {
    def arg1: A
  }

  // A binary function call with two arguments A and B
  trait BCall[A,B,+R] extends UCall[A,R] {
    def arg2: B
  }

  // Compressing type signatures for convenience
  type F1[-A,+R] = Function1[A,R]
  type F2[-A,-B,+R] = Function2[A,B,R]
  type FFCall[+R] = FCall[FCall[R]]

  // A lazily applied unary function call
  case class LUCall[A,+R](f: F1[A,R], arg1: A) extends UCall[A,R] {
    lazy val apply = f(arg1)
    override def toString = f + "(" + arg1 + ")"
  }

  // A lazily applied binary function call
  case class LBCall[A,B,+R](f: F2[A,B,R], arg1: A, arg2: B) extends BCall[A,B,R] {
    lazy val apply = f(arg1,arg2)
    override def toString = f + "(" + arg1 + "," + arg2 + ")"
  }

  // A lazily applied unary terminal function call, with one call argument
  case class TUCall[A,B,+R](f: F1[A,R], a1: FFCall[A]) extends UCall[FCall[A],R] {
    lazy val apply = f(arg1())
    lazy val arg1 = a1()
    override def toString = f + "(" + a1 + ")"
  }

  // A lazily applied binary terminal function call, with two call arguments
  case class TBCall[A,B,+R](f: F2[A,B,R], a1: FFCall[A], a2: FFCall[B]) extends BCall[FCall[A], FCall[B],R] {
    lazy val apply = f(arg1(),arg2())
    lazy val arg1 = a1()
    lazy val arg2 = a2()
    override def toString = "(" + a1 + " " + f + " " + a2 + ")"
  }

  // eager evaluation
  def $[A,R](f: F1[A,R], a: A): R = f(a)
  def $[A,B,R](f: F2[A,B,R], a: A, b: B): R = f(a,b)

  // lazy evaluation
  def $$[A,R](f: F1[A,FCall[R]], a: A): FFCall[R] = LUCall(f,a)
  def $$[A,B,R](f: F2[A,B,FCall[R]], a: A, b: B): FFCall[R] = LBCall(f,a,b)
  def $$[A,R](f: F1[A,R], a1: FFCall[A]) = TUCall(f,a1)
  def $$[A,B,R](f: F2[A,B,R], a1: FFCall[A], a2: FFCall[B]) = TBCall(f,a1,a2)

  // hash-consed lazy evaluation (=lazy memoization)
  def $_[A,R](f: F1[A,FCall[R]], a: A): FFCall[R] = hashcons($$(f,a))
  def $_[A,B,R](f: F2[A,B,FCall[R]], a: A, b: B): FFCall[R] = hashcons($$(f,a,b))
  def $_[A,R](f: F1[A,R], a1: FFCall[A]) = hashcons($$(f,a1))
  def $_[A,B,R](f: F2[A,B,R], a1: FFCall[A], a2: FFCall[B]) = hashcons($$(f,a1,a2))

  // naive hash-consing, implemented with a weak HashMap
  val mtable = scala.collection.mutable.WeakHashMap.empty[FCall[_], WeakReference[FCall[_]]]

  def hashcons[R](c: FCall[R]): FCall[R] =  synchronized {
    if (!mtable.contains(c)) mtable.put(c,new WeakReference(c))
    else { println("hit: " + c) }
    mtable.get(c).get.get.asInstanceOf[FCall[R]]
  }
}