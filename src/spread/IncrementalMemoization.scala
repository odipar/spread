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
  import scala.collection.mutable.WeakHashMap
  import scala.collection.mutable.HashMap
  import java.lang.ref.WeakReference

  var totf: Long = 0

  trait FValue[X] {
    def apply(): X
    def eval: FValue[X] = this
    def force: FValue[X] = this

    { totf = totf + 1 }
  }

  case class FI[X](x: X) extends FValue[X] {
    def apply() = x
    override def toString = "`"+x.toString
  }


  case class Trace[X,XX](origin: FValue[XX], f: FValue[X]) extends FValue[X] {
    lazy val apply = f.apply()
    override lazy val eval = this
    override lazy val force: FValue[X] = trace(this,mem(f.force))
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(origin.hashCode) + f.hashCode)
    override def toString = "{" + origin + " => " + f + "}"
  }

  implicit def toFI[X](x: X): FValue[X] = FI(x)

  val wt = WeakHashMap.empty[FValue[_], WeakReference[FValue[_]]]
  val st = HashMap.empty[FValue[_], WeakReference[FValue[_]]]
  val vt = wt

  def mem[X](c: FValue[X]): FValue[X] = {
    if (!vt.contains(c)) {
      //println("vput: " + c)
      vt.put(c,new WeakReference(c))
    }
    //else { println("vhit: " + c) }
    vt.get(c).get.get.asInstanceOf[FValue[X]]
  }

  type F1[A,X] = FValue[A] => FValue[X]
  type F2[A,B,X] = (FValue[A],FValue[B]) => FValue[X]
  type F3[A,B,C,X] = (FValue[A],FValue[B],FValue[C]) => FValue[X]

  def %[A,X](f: F1[A,X], a: FValue[A]) = mem(LazyF1(f,a))
  def $[A,X](f: F1[A,X], a: FValue[A]) = mem(LazyD1(f,a))
  def %[A,B,X](f: F2[A,B,X], a: FValue[A], b: FValue[B]) = mem(LazyF2(f,a,b))
  def $[A,B,X](f: F2[A,B,X], a: FValue[A], b: FValue[B]) = mem(LazyD2(f,a,b))
  def %[A,B,C,X](f: F3[A,B,C,X], a: FValue[A], b: FValue[B], c: FValue[C]) = mem(LazyF3(f,a,b,c))
  def $[A,B,C,X](f: F3[A,B,C,X], a: FValue[A], b: FValue[B], c: FValue[C]) = mem(LazyD3(f,a,b,c))
  def trace[X,XX](u: FValue[XX], f: FValue[X]): FValue[X] = mem(Trace(u,f))

  trait LazyF[X] extends FValue[X] {
    def h: Any
    def apply = force.apply
    override def force = eval.force
    override lazy val hashCode = Hashing.jenkinsHash(h.hashCode)
  }

  trait Lazy1[A,X] extends LazyF[X] {
    def h = (f,a)
    def f: F1[A,X]
    def a: FValue[A]
    override lazy val eval = trace(this,mem(f(a))) // start tracing: from here everything is captured and memoized.
  }

  trait Lazy2[A,B,X] extends LazyF[X] {
    def h = (f,a,b)
    def f: F2[A,B,X]
    def a: FValue[A]
    def b: FValue[B]
    override lazy val eval = trace(this,mem(f(a,b)))
  }

  trait Lazy3[A,B,C,X] extends LazyF[X] {
    def h = (f,a,b)
    def f: F3[A,B,C,X]
    def a: FValue[A]
    def b: FValue[B]
    def c: FValue[C]
    override lazy val eval = trace(this,mem(f(a,b,c)))
  }

  case class LazyF1[A,X](f: F1[A,X], a: FValue[A]) extends Lazy1[A,X] {
    override def toString = f + "(" + a + ")"
  }

  case class LazyD1[A,X](f: F1[A,X], a: FValue[A]) extends Lazy1[A,X] {
    override def toString = f + "[" + a + "]"
  }

  case class LazyF2[A,B,X](f: F2[A,B,X], a: FValue[A], b: FValue[B]) extends Lazy2[A,B,X] {
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class LazyD2[A,B,X](f: F2[A,B,X], a: FValue[A], b: FValue[B]) extends Lazy2[A,B,X] {
    override def toString = "[" + a + " " + f + " " + b + "]"
  }

  case class LazyF3[A,B,C,X](f: F3[A,B,C,X], a: FValue[A], b: FValue[B], c: FValue[C]) extends Lazy3[A,B,C,X] {
    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }

  case class LazyD3[A,B,C,X](f: F3[A,B,C,X], a: FValue[A], b: FValue[B], c: FValue[C]) extends Lazy3[A,B,C,X] {
    override def toString = f + "[" + a + "," + b + "," + c + "]"
  }

  def force11[A,X](f: F1[A,X]) = new F1[A,X] {
    val force = force12(f)
    def apply(i1: FValue[A]) = $(force,i1.force)
    override def toString = "``"+f
  }

  def force12[A,X](f: F1[A,X]) = new F1[A,X] {
    def apply(i1: FValue[A]) = $(f,i1.force())
    override def toString = "`"+f
  }

  def force21[A,B,X](f: F2[A,B,X]) = new F2[A,B,X] {
    val force = force22(f)
    def apply(i1: FValue[A], i2: FValue[B]) = $(force,i1.force,i2.force)
    override def toString = "``"+f
  }

  def force22[A,B,X](f: F2[A,B,X]) = new F2[A,B,X] {
    def apply(i1: FValue[A], i2: FValue[B]) = $(f,i1.force(),i2.force())
    override def toString = "`"+f
  }

  def force31[A,B,C,X](f: F3[A,B,C,X]) = new F3[A,B,C,X]{
    val force = force32(f)
    def apply(i1: FValue[A], i2: FValue[B], i3: FValue[C]) = $(force,i1.force,i2.force,i3.force)
    override def toString = "``"+f
  }

  def force32[A,B,C,X](f: F3[A,B,C,X]) = new F3[A,B,C,X] {
    def apply(i1: FValue[A], i2: FValue[B], i3: FValue[C]) = $(f,i1.force(),i2.force(),i3.force())
    override def toString = "`"+f
  }

  type I = FValue[Int]

  lazy val add = force21(add3)
  lazy val sub = force21(sub3)
  lazy val mul = force21(mul3)

  trait IValue extends FValue[Int] {
    def origin: I
    def +(o: I): I = %(add,origin,o)
    def -(o: I): I = %(sub,origin,o)
    def *(o: I): I = %(mul,origin,o)
  }

  case class IWrap(origin: I) extends IValue {
    def apply() = sys.error("no")
    override def force = sys.error("no")
    override def eval = sys.error("no")
  }

  implicit def toI(i: I): IValue = i match {
    case ii: IValue => ii ; case _ => IWrap(i)
  }

  implicit def toTInt(i: Int): I = mem(TInt(i))

  case class TInt(i: Int) extends IValue {
    def origin = this
    def apply() = i
    override def toString = "`"+i.toString
  }

  val add3: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() + i2()
    override def toString = "+"
  }

  val sub3: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() - i2()
    override def toString = "-"
  }

  val mul3: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() * i2()
    override def toString = "*"
  }
}