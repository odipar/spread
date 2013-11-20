package spread

import scala.Function2

/*
  SPREAD lib: Incremental Computation Reuse

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
  import scala.language.existentials
  import scala.collection.mutable.WeakHashMap
  import scala.collection.mutable.HashMap
  import java.lang.ref.WeakReference

  var totf: Long = 0

  trait FValue[X,O] {
    def apply(): X
    def origin: O
    def force: FValue[X,_] = this

    { totf = totf + 1 }
  }

  trait LazyFValue[A,X] extends FValue[X,(A,A => FValue[X,_])] {
    def a: A
    def f: A => FValue[X,_]

    lazy val apply = force()
    lazy val eval = f(a)
    override lazy val force = $$(FForce(this,eval.force))
    def origin = (a,f)
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode) ^ a.hashCode)
    override def toString = f + "[" + a + "]"
  }

  trait LazyF2Value[A,B,X] extends FValue[X,((A,B),(A,B) => FValue[X,_])] {
    def a: A
    def b: B
    def f: (A,B) => FValue[X,_]

    lazy val apply = force()
    lazy val eval = f(a,b)
    override lazy val force = $$(FForce(this,eval.force))
    def origin = ((a,b),f)
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode + b.hashCode) ^ a.hashCode)
    override def toString =  {
      f + "[" + a + ","  + b + "]"
    }
  }

  trait LazyF3Value[A,B,C,X] extends FValue[X,((A,B,C),(A,B,C) => FValue[X,_])] {
    def a: A
    def b: B
    def c: C
    def f: (A,B,C) => FValue[X,_]

    lazy val apply = force()
    lazy val eval = f(a,b,c)
    override lazy val force = $$(FForce(this,eval.force))
    def origin = ((a,b,c),f)
    override lazy val hashCode = {
      Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode + c.hashCode) ^ Hashing.jenkinsHash(a.hashCode + b.hashCode))
    }
    override def toString = f + "[" + a + "," + b + "," + c + "]"
  }

  case class LazyIValue[A](f: A => FValue[Int,_], a: A) extends IValue[(A,A => FValue[Int,_])] with LazyFValue[A,Int]

  type I = IValue[_]
  type IX = FValue[Int,_]

  trait IValue[O] extends FValue[Int,O] {
    def add(o: IX): IValue[O] = $$(TAdd((this,o))).asInstanceOf[IValue[O]]
    def mul(o: IX): IValue[O] = $$(TMul((this,o))).asInstanceOf[IValue[O]]
    def sub(o: IX): IValue[O] = $$(TSub((this,o))).asInstanceOf[IValue[O]]
    def +(o: IX): IValue[O] = add(o)
    def -(o: IX): IValue[O] = sub(o)
    def *(o: IX): IValue[O] = mul(o)
  }

  case class FI[X](x: X) extends FValue[X,FI[X]] {
    def origin = this
    def apply() = x

    override def toString = "`"+x.toString
  }

  case class TInt(i: Int) extends IValue[TInt] {
    def origin = this
    def apply() = i

    override def toString = "`"+i.toString
  }

  type II = FValue[Int,_]

  def ?> = TInt(0)

  trait TUna[A,X] extends FValue[X,FValue[A,_]] {
    def op: A => X
    def rebuild(r: FValue[A,_]): TUna[A,X]
    lazy val apply = op(origin())
    override lazy val force = $$(FForce(this,rebuild(origin.force)))
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(origin.hashCode) ^ op.hashCode)
    override def toString = op + "(" + origin + ")"
  }

  trait TBin[A,B,X] extends FValue[X,(FValue[A,_],FValue[B,_])] {
    def op: (A,B) => X
    def rebuild(r: (FValue[A,_],FValue[B,_])): TBin[A,B,X]
    lazy val apply = op(origin._1(), origin._2())
    override lazy val force = $$(FForce(this,rebuild((origin._1.force,origin._2.force))))
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(origin.hashCode) ^ op.hashCode)
    override def toString = "(" + origin._1 + " " + op + " " + origin._2 + ")"
  }

  case class FForce[X,O,XX,OO](u: FValue[XX,OO], f: FValue[X,O]) extends FValue[X,FValue[XX,OO]] {
    def self = this
    def origin = u
    def apply() = f.apply()
    override def toString = u + " => " + f
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(u.hashCode) + f.hashCode)
  }

  val add1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x + y ; override def toString = "+" }
  val sub1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x - y ; override def toString = "-" }
  val mul1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x * y ; override def toString = "*" }

  case class TAdd(origin: (IX,IX)) extends TBin[Int,Int,Int] with IValue[(IX,IX)] {
    def self = this
    val op = add1
    def rebuild(r: (IX,IX)) = TAdd(r)
  }

  case class TSub(origin: (IX,IX)) extends TBin[Int,Int,Int] with IValue[(IX,IX)] {
    def self = this
    val op = sub1
    def rebuild(r: (IX,IX)) = TSub(r)
  }

  case class TMul(origin: (IX,IX)) extends TBin[Int,Int,Int] with IValue[(IX,IX)] {
    def self = this
    def op = mul1
    def rebuild(r: (IX,IX)) = TMul(r)
  }

  case class LazyDValue[A,X](f: A => FValue[X,_], a: A) extends LazyFValue[A,X] with FValue[X,(A,A => FValue[X,_])]
  case class LazyD2Value[A,B,X](f: (A,B) => FValue[X,_], a: A, b: B) extends LazyF2Value[A,B,X] with FValue[X,((A,B),(A,B) => FValue[X,_])]
  case class LazyD3Value[A,B,C,X](f: (A,B,C) => FValue[X,_], a: A, b: B, c: C) extends LazyF3Value[A,B,C,X] with FValue[X,((A,B,C),(A,B,C) => FValue[X,_])]


  def ?(i: Int): TInt = $$(TInt(i)).asInstanceOf[TInt]
  def %[A,X](f: A => FValue[X,_],a: A): FValue[X,_] = $$(LazyDValue(f,a))
  def %%[A,B,X](f: (A,B) => FValue[X,_],a: A, b: B): FValue[X,_] = $$(LazyD2Value(f,a,b))
  def %%%[A,B,C,X](f: (A,B,C) => FValue[X,_],a: A, b: B, c: C): FValue[X,_] = $$(LazyD3Value(f,a,b,c))
  def $[A](f: A => IValue[_], a: A): IValue[_] = $$(LazyIValue[A](f,a)).asInstanceOf[IValue[_]]

  /*def %[A,X](f: A => FValue[X,_],a: A): FValue[X,_] = f(a)
  def %%[A,B,X](f: (A,B) => FValue[X,_],a: A, b: B): FValue[X,_] = f(a,b)
  def %%%[A,B,C,X](f: (A,B,C) => FValue[X,_],a: A, b: B, c: C): FValue[X,_] = f(a,b,c)
  def $[A](f: A => IValue[_], a: A): IValue[_] = f(a)                   */

  implicit def ttint(i: Int): IValue[_] = $$(TInt(i)).asInstanceOf[TInt]

  val wt = WeakHashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val st = HashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val vt = wt

  def $$[X,O](c: FValue[X,O]): FValue[X,O] = {
    if (!vt.contains(c)) {
     // println("vput: " + c)
      vt.put(c,new WeakReference(c))
    }
    //else { println("vhit: " + c) }
    vt.get(c).get.get.asInstanceOf[FValue[X,O]]
    //c.hashCode
  }
}