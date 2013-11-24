package spread

import scala.{Function3, Function2}

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
    def eval: FValue[X,_] = this
    def force: FValue[X,_] = this

    { totf = totf + 1 }
  }


  type I = FValue[Int,_]

  case class FI[X](x: X) extends FValue[X,FI[X]] {
    def origin = this
    def apply() = x

    override def toString = "`"+x.toString
  }

  trait IValue extends FValue[Int,I] {
    def +(o: FValue[Int,_]): FValue[Int,_] = %(add,origin,o)
    def *(o: FValue[Int,_]): FValue[Int,_] = %(mul,origin,o)
    def -(o: FValue[Int,_]): FValue[Int,_] = %(sub,origin,o)
  }

  case class IWrap(origin: I) extends IValue {
    def apply() = sys.error("should be factored out")
    override def eval = sys.error("should be factored out")
  }

  case class TInt(i: Int) extends IValue {
    def origin = this
    def apply() = i

    override def toString = "`"+i.toString
  }

  def ?> = TInt(0)

  case class Trace[X,O,XX,OO](u: FValue[XX,OO], f: FValue[X,O]) extends FValue[X,FValue[XX,OO]] {
    def self = this
    def origin = u
    lazy val apply = f.apply()
    override lazy val eval = trace(this,f.eval)
    override lazy val force: FValue[X,_] = trace(this,f.force)
    override def toString = "{" + u + " => " + f + "}"
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(u.hashCode) + f.hashCode)
  }

  implicit def ttint(i: Int): I = $$(TInt(i))

  val wt = WeakHashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val st = HashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val vt = wt

  def $$[X,O](c: FValue[X,O]): FValue[X,O] = {
    if (!vt.contains(c)) {
      //println("vput: " + c)
      vt.put(c,new WeakReference(c))
    }
    else { println("vhit: " + c) }
    vt.get(c).get.get.asInstanceOf[FValue[X,O]]
    //c.hashCode
  }

  def trace[X,O,XX,OO](u: FValue[XX,OO], f: FValue[X,O]): FValue[X,FValue[XX,OO]] = $$(Trace(u,f))
  def %[A,X](f: FValue[A,_] => FValue[X,_], a: FValue[A,_]): FValue[X,_] = $$(LazyF1(f,a))
  def %[A,B,X](f: (FValue[A,_],FValue[B,_]) => FValue[X,_], a: FValue[A,_], b: FValue[B,_]): FValue[X,_] = $$(LazyF2(f,a,b))
  def $[A,X](f: FValue[A,_] => FValue[X,_], a: FValue[A,_]): FValue[X,_] = $$(LazyD1(f,a))
  def $[A,B,X](f: (FValue[A,_],FValue[B,_]) => FValue[X,_], a: FValue[A,_], b: FValue[B,_]): FValue[X,_] = $$(LazyD2(f,a,b))

  implicit def toI(i: FValue[Int,_]): IValue = i match {
    case ii: IValue => ii
    case _ => IWrap(i)
  }

  case class LazyF1[A,X](f: (FValue[A,_]) => FValue[X,_], a: FValue[A,_]) extends FValue[X,((FValue[A,_]) => FValue[X,_],FValue[A,_])] {
    def origin = (f,a)
    lazy val apply = force.apply
    override lazy val force = eval
    override lazy val eval = trace(this,f(a.force)).force
    override def toString = f + "(" + a + ")"
    override lazy val hashCode = Hashing.jenkinsHash(origin.hashCode)
  }

  case class LazyD1[A,X](f: (FValue[A,_]) => FValue[X,_], a: FValue[A,_]) extends FValue[X,((FValue[A,_]) => FValue[X,_],FValue[A,_])] {
    def origin = (f,a)
    lazy val apply = force.apply
    override lazy val force = eval.force
    override lazy val eval = trace(this,f(a))
    override def toString = f + "[" + a + "]"
    override lazy val hashCode = Hashing.jenkinsHash(origin.hashCode)
  }


  case class LazyF2[A,B,X](f: (FValue[A,_],FValue[B,_]) => FValue[X,_], a: FValue[A,_], b: FValue[B,_]) extends FValue[X,((FValue[A,_],FValue[B,_]) => FValue[X,_],FValue[A,_],FValue[B,_])] {
    def origin = (f,a,b)
    lazy val apply = force.apply
    override lazy val force = eval
    override lazy val eval = trace(this,f(a.force,b.force)).force
    override def toString = "(" + a + " " + f + " " + b + ")"
    override lazy val hashCode = Hashing.jenkinsHash(origin.hashCode)
  }

  case class LazyD2[A,B,X](f: (FValue[A,_],FValue[B,_]) => FValue[X,_], a: FValue[A,_], b: FValue[B,_]) extends FValue[X,((FValue[A,_],FValue[B,_]) => FValue[X,_],FValue[A,_],FValue[B,_])] {
    def origin = (f,a,b)
    lazy val apply = force.apply
    override lazy val force = eval.force
    override lazy val eval = trace(this,f(a,b))
    override def toString = "[" + a + " " + f + " " + b + "]"
    override lazy val hashCode = Hashing.jenkinsHash(origin.hashCode)
  }

  val add2: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = i1() - i2()
    override def toString = "+"
  }

  val add: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = %(add2,i1,i2)
    override def toString = "+"
  }

  val sub2: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = i1() - i2()
    override def toString = "-"
  }

  val sub: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = %(sub2,i1,i2)
    override def toString = "-"
  }

  val mul2: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = i1() * i2()
    override def toString = "*"
  }
  val mul: Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] = new Function2[FValue[Int,_],FValue[Int,_],FValue[Int,_]] {
    def apply(i1: FValue[Int,_], i2: FValue[Int,_]) = $(mul2,i1,i2)
    override def toString = "*"
  }

  val fac: Function1[I,I] = new Function1[I,I] {
    def apply(i: I): I  = {
     if (i() <= 1) 1
     else i * $(fac,i() - 1)
    }
    override def toString = "fac"
  }
}