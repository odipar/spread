package spread
/*
  SPREAD lib: Incremental Cofinishtion Reuse

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
    def reduce: FValue[X] = this
    def finish: FValue[X] = this
    def iterate: FValue[X] = this

    { totf = totf + 1 }
  }

  implicit def fi[X](x: X): FValue[X] = mem(FI(x))

  case class FI[X](x: X) extends FValue[X] {
    def apply() = x
    override def toString = "`"+x.toString
  }

  case class Reduce[X,XX](from: FValue[XX], to: FValue[X]) extends Trace[X,XX] {
    override def iterate = this
    override def reduce = this
    override def finish: FValue[X] = this
  }

  case class Finish[X,XX](from: FValue[XX], to: FValue[X]) extends Trace[X,XX] {
    override def iterate = this
    override lazy val reduce: FValue[X] = mem(Reduce(this,mem(to.reduce)))
    override def finish: FValue[X] = this
  }

  case class Iterate[X,XX](from: FValue[XX], to: FValue[X]) extends Trace[X,XX] {
    override def iterate = this
    override def reduce: FValue[X] = finish.reduce
    override lazy val finish: FValue[X] = mem(Finish(this,mem(to.finish)))
  }

  trait Trace[X,XX] extends FValue[X] {
    def from: FValue[XX]
    def to: FValue[X]
    def apply = to.apply

    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(from.hashCode) + to.hashCode)
    override def toString = "{" + from + " => " + to + "}"
  }

  val wt = WeakHashMap.empty[FValue[_], WeakReference[FValue[_]]]
  val st = HashMap.empty[FValue[_], WeakReference[FValue[_]]]
  val vt = wt

  def mem[X](c: FValue[X]): FValue[X] = {
    if (!vt.contains(c)) {
      vt.put(c,new WeakReference(c))
    }
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
  def iter[X,XX](u: FValue[XX], f: FValue[X]): FValue[X] = mem(Iterate(u,f))

  trait LazyF[X] extends FValue[X] {
    def h: Any
    override def apply() = reduce()
    override def reduce = finish.reduce
    override def finish = iterate.finish
    override lazy val hashCode = Hashing.jenkinsHash(h.hashCode)
  }

  trait Lazy1[A,X] extends LazyF[X] {
    def h = (f,a)
    def f: F1[A,X]
    def a: FValue[A]
    override lazy val iterate = iter(this,mem(f(a))) // start tracing: from here everything is captured and memoized.
  }

  trait Lazy2[A,B,X] extends LazyF[X] {
    def h = (f,a,b)
    def f: F2[A,B,X]
    def a: FValue[A]
    def b: FValue[B]
    override lazy val iterate = iter(this,mem(f(a,b)))
  }

  trait Lazy3[A,B,C,X] extends LazyF[X] {
    def h = (f,a,b)
    def f: F3[A,B,C,X]
    def a: FValue[A]
    def b: FValue[B]
    def c: FValue[C]
    override lazy val iterate = iter(this,mem(f(a,b,c)))
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

  def finish1[A,X](f: F1[A,X]) = new F1[A,X] {
    val reduce = reduce1(f)
    def apply(i1: FValue[A]) = $(reduce,i1.finish)
    override def toString = "``"+f
  }

  def reduce1[A,X](f: F1[A,X]) = new F1[A,X] {
    def apply(i1: FValue[A]) = $(f,i1.reduce)
    override def toString = "`"+f
  }

  def finish2[A,B,X](f: F2[A,B,X]) = new F2[A,B,X] {
    val reduce = reduce2(f)
    def apply(i1: FValue[A], i2: FValue[B]) = $(reduce,i1.finish,i2.finish)
    override def toString = "``"+f
  }

  def reduce2[A,B,X](f: F2[A,B,X]) = new F2[A,B,X] {
    def apply(i1: FValue[A], i2: FValue[B]) = $(f,i1.reduce,i2.reduce)
    override def toString = "`"+f
  }

  def finish3[A,B,C,X](f: F3[A,B,C,X]) = new F3[A,B,C,X]{
    val force = reduce3(f)
    def apply(i1: FValue[A], i2: FValue[B], i3: FValue[C]) = $(force,i1.finish,i2.finish,i3.finish)
    override def toString = "``"+f
  }

  def reduce3[A,B,C,X](f: F3[A,B,C,X]) = new F3[A,B,C,X] {
    def apply(i1: FValue[A], i2: FValue[B], i3: FValue[C]) = $(f,i1.reduce,i2.reduce,i3.reduce)
    override def toString = "`"+f
  }
}