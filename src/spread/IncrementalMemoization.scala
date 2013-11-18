package spread

import scala.Function2

/*
  SPREAD lib: Incremental Computation Reuse

  Features:

  1) spreadsheet-like incrementaorigintation
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

  trait FValue[X,O] {
    def apply(): X
    def origin: O
    def force: FValue[X,_] = this

    synchronized { totf = totf + 1 }
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
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode) ^ a.hashCode)
    override def toString =  a + " " + f + " "  + b
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
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode) ^ a.hashCode)
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

  trait TTer[A,B,C,X] extends FValue[X,(FValue[A,_],FValue[B,_],FValue[C,_])] {
    def op: (A,B,C) => X
    def rebuild(r: (FValue[A,_],FValue[B,_],FValue[C,_])): TTer[A,B,C,X]
    lazy val apply = op(origin._1(), origin._2(),origin._3())
    override lazy val force = $$(FForce(this,rebuild((origin._1.force,origin._2.force,origin._3.force))))
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(origin.hashCode) ^ op.hashCode)
    override def toString = "(" + origin._1 + " " + op + " " + origin._2 + ")"
  }

  case class FForce[X,O,XX,OO](u: FValue[XX,OO], f: FValue[X,O]) extends FValue[X,FValue[XX,OO]] {
    def self = this
    def origin = u
    def apply() = f.apply()
    override def toString = u + " => " + f
  }

  val add1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x + y ; override def toString = "+" }
  val sub1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x - y ; override def toString = "-" }
  val mul1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x * y ; override def toString = "*" }

  case class TAdd(origin: (IX,IX)) extends TBin[Int,Int,Int] {
    def self = this
    val op = add1
    def rebuild(r: (IX,IX)) = TAdd(r)
  }

  case class TSub(origin: (IX,IX)) extends TBin[Int,Int,Int] {
    def self = this
    val op = sub1
    def rebuild(r: (IX,IX)) = TSub(r)
  }

  case class TMul(origin: (IX,IX)) extends TBin[Int,Int,Int] {
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
  def $[A](f: A => IValue[_], a: A): IValue[_] = LazyIValue[A](f,a)

  implicit def ttint(i: Int): IValue[_] = $$(TInt(i)).asInstanceOf[TInt]

  val wt = WeakHashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val st = HashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val vt = wt

  def $$[X,O](c: FValue[X,O]): FValue[X,O] = synchronized {
    if (!vt.contains(c)) {
      println("vput: " + c)
      vt.put(c,new WeakReference(c))
    }
    else { println("vhit: " + c) }
    vt.get(c).get.get.asInstanceOf[FValue[X,O]]
  }

  trait FTreap[V,P] {
    def depth: Int
    def isEmpty: Boolean
    def prio: P
    def left: FTreap[V,P]
    def value: V
    def right: FTreap[V,P]
  }

  case class NFTreap[V,P]() extends FTreap[V,P] {
    def depth = 0
    def isEmpty = true
    def prio = sys.error("empty treap has no priority")
    def left = this
    def value = sys.error("empty treap has no value")
    def right = this
  }

  case class LFTreap[V,P](value: V, prio: P) extends FTreap[V,P] {
    def depth = 1
    def isEmpty = false
    def left = NFTreap()
    def right = NFTreap()
    override def toString = value.toString
  }

  case class BFTreap[V,P](left: FTreap[V,P], value: V, prio: P, right: FTreap[V,P]) extends FTreap[V,P] {
    def depth = (left.depth max right.depth) + 1
    def isEmpty = false
    override def toString = left + ";" + value.toString + ";" + right

  }

  case class BLeftTreap[V,P](left: FTreap[V,P], value: V, prio: P) extends FTreap[V,P] {
    def depth = left.depth + 1
    def right = NFTreap()
    def isEmpty = false
    override def toString = left + ";" + value.toString
  }

  case class BRightTreap[V,P](value: V, prio: P, right: FTreap[V,P]) extends FTreap[V,P] {
    def depth = right.depth + 1
    def left = NFTreap()
    def isEmpty = false
    override def toString = value.toString + ";" + right
  }

  def fcreate1[V,P]: Function2[V,P,FTreap[V,P]] = new Function2[V,P,FTreap[V,P]] {
    def apply(x: V, p: P) = LFTreap(x,p)
  }

  def fcreate[V,P]: Function3[FTreap[V,P], (V,P), FTreap[V,P],FTreap[V,P]] = new Function3[FTreap[V,P], (V,P), FTreap[V,P],FTreap[V,P]] {
    def apply(l: FTreap[V,P], xp: (V,P), r: FTreap[V,P]): FTreap[V,P] = {
      if (l.isEmpty && r.isEmpty) LFTreap(xp._1,xp._2)
      else if (l.isEmpty) BRightTreap(xp._1,xp._2,r)
      else if (r.isEmpty) BLeftTreap(l,xp._1,xp._2)
      else BFTreap[V,P](l,xp._1,xp._2,r)
    }
  }

  def T[V,P](v: V)(implicit p: PrioOrdering[V,P]) = fcreate1(v,p.prio(v))

  case object IPrioOrdering extends PrioOrdering[Int,Int] {
    def prio(v: Int): Int = Hashing.jenkinsHash(v.hashCode)
    def orderValue(v1: Int,v2: Int) = if (v1 > v2) 1 ; else if (v1 < v2 ) -1 ; else 0
    def orderPrio(p1: Int,p2: Int) =  if (p1 > p2) 1 ; else if (p1 < p2 ) -1 ; else 0

    override def toString = "o"
  }

  implicit def intord: PrioOrdering[Int,Int] = IPrioOrdering

  trait PrioOrdering[V,P] {
    def prio(v: V): P
    def orderValue(v1: V, v2: V): Int
    def orderPrio(p1: P, p2: P): Int
  }

  type VFT[V,P] = FValue[FTreap[V,P],_]

  implicit def tovtf[V,P](t: FTreap[V,P]): FValue[FTreap[V,P],_] = FI(t)

  def create3[V,P](p: PrioOrdering[V,P]): Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] = new Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] {
    val create: Function3[FTreap[V,P], (V,P), FTreap[V,P],FTreap[V,P]] = fcreate
    def join = this
    def apply(l: FTreap[V,P], vp: (V,P), r: FTreap[V,P]): VFT[V,P] = {
      create(l,vp,r)
    }

    override def toString = "create3"
  }

  def create2[V,P](p: PrioOrdering[V,P]): Function3[VFT[V,P],(V,P),VFT[V,P],VFT[V,P]] = new Function3[VFT[V,P],(V,P),VFT[V,P],VFT[V,P]] {
    val create: Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] = create3(p)
    def join = this
    def apply(l: VFT[V,P], vp: (V,P), r: VFT[V,P]): VFT[V,P] = {
      %%%(create,l(),vp,r())
    }

    override def toString = "create2"
  }

  case class TJoin[V,P](origin: (VFT[V,P],VFT[V,P]),p: PrioOrdering[V,P]) extends TBin[FTreap[V,P],FTreap[V,P],FTreap[V,P]] {
    def self = this
    val op: Function2[FTreap[V,P],FTreap[V,P],FTreap[V,P]] = fjoin(p)
    def rebuild(r: (VFT[V,P],VFT[V,P])) = TJoin(r,p)
  }

  def join1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P],VFT[V,P],VFT[V,P]] = new Function2[VFT[V,P],VFT[V,P],VFT[V,P]] {
    lazy val join = join2(p)
    def apply(a1: VFT[V,P], a2: VFT[V,P]): VFT[V,P] = {
      %%(join,a1(),a2())
    }

    override def toString = "join1"
  }

  def join2[V,P](p: PrioOrdering[V,P]): Function2[FTreap[V,P],FTreap[V,P],VFT[V,P]] = new Function2[FTreap[V,P],FTreap[V,P],VFT[V,P]] {
    lazy val join: Function2[VFT[V,P],VFT[V,P],VFT[V,P]] = join1(p)
    val create: Function3[VFT[V,P],(V,P),VFT[V,P],VFT[V,P]] = create2(p)
    val left: Function1[VFT[V,P],VFT[V,P]] = left2
    val right: Function1[VFT[V,P],VFT[V,P]] = right2

    def apply(a1: FTreap[V,P], a2: FTreap[V,P]): VFT[V,P] = {
      val t1 = a1
      val t2 = a2
      if (t1.isEmpty) t2
      else if (t2.isEmpty) t1
      else if (p.orderPrio(t1.prio,t2.prio) > 0) {
        %%%(create,FI(a1.left),(t1.value,t1.prio),%%(join,FI(a1.right),FI(a2)))
      }
      else {
        %%%(create,%%(join,FI(a1),FI(a2.left)),(t2.value,t2.prio),FI(a2.right))
      }
    }

    override def toString = "join2"
  }

  def left2[V,P]: Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(a1: VFT[V,P]) = a1().left
    override def toString = "left"
  }

  def right2[V,P]: Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(a1: VFT[V,P]) = a1().right
    override def toString = "right"
  }

  def fjoin[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P],FTreap[V,P],FTreap[V,P]] {
    val create: Function3[FTreap[V,P],(V,P),FTreap[V,P],FTreap[V,P]]= fcreate
    def apply (t1: FTreap[V,P], t2: FTreap[V,P]): FTreap[V,P] = {
      if (t1.isEmpty) t2
      else if (t2.isEmpty) t1
      else if (p.orderPrio(t1.prio,t2.prio) > 0) {
        create(t1.left,(t1.value,t1.prio),this(t1.right,t2))
      }
      else {
        create(this(t1,t2.left),(t2.value,t2.prio),t2.right)
      }
    }

    override def toString = "fjoin"
  }

  def first[V,P](t1: FTreap[V,P]): Option[V] = {
    if (t1.isEmpty) None
    else if (t1.left.isEmpty && t1.right.isEmpty) Some(t1.value)
    else first(t1.left)
  }

  def last[V,P](t1: FTreap[V,P]): Option[V] = {
    if (t1.isEmpty) None
    else if (t1.left.isEmpty && t1.right.isEmpty) Some(t1.value)
    else last(t1.right)
  }

  def fsplit[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P], V, (FTreap[V,P], Option[V], FTreap[V,P])] {
    val join: Function2[FTreap[V,P],FTreap[V,P],FTreap[V,P]] = fjoin(p)
    def apply(t1: FTreap[V,P], x: V): (FTreap[V,P], Option[V], FTreap[V,P]) = {
      if (t1.isEmpty) (t1.left,None,t1.right)
      else {
        val e = t1.value
        val cc = p.orderValue(e,x)
        if (cc > 0) {
          val (l,xx,r) = this(t1.left,x)
          val r0 = fcreate1(e,p.prio(e))
          val r1 = join(r,r0)
          val r2 = join(r1,t1.right)
          (l,xx,r2)
        }
        else if (cc < 0) {
          val (l,xx,r) = this(t1.right,x)
          val l0 = fcreate1(e,p.prio(e))
          val l1 = join(t1.left,l0)
          var l2 = join(l1,l)
          (l2,xx,r)
        }
        else (t1.left,Some(e),t1.right)
      }
    }

    override def toString = "split"
  }
}