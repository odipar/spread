package spread

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
  import language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import scala.collection.mutable.HashMap
  import java.lang.ref.WeakReference

  var totf: Long = 0

  trait FValue[X,O] {
    def depends: O
    def apply(): X
    def force: FValue[X,_] = this

    synchronized { totf = totf + 1 }
  }

  case class LazyFValue[A,X](f: A => FValue[X,_], a: A) extends FValue[X,LazyFValue[A,X]] {
    lazy val apply = force()
    lazy val eval = f(a)
    override lazy val force = $$(FForce(this,eval.force))
    def depends = this
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(f.hashCode) ^ a.hashCode)
    override def toString = f + "[" + a + "]"
  }

  type I = FValue[Int,_]

  trait IValue[O] extends FValue[Int,O] {
    def add(o: I): TBin = $$(TAdd((this,o))).asInstanceOf[TBin]
    def mul(o: I): TBin = $$(TMul((this,o))).asInstanceOf[TBin]
    def drp(o: I): TBin = $$(TDrp((this,o))).asInstanceOf[TBin]
    def +(o: I): TBin = add(o)
    def *(o: I): TBin = mul(o)
    def \(o: I): TBin = drp(o)
  }

  case class TInt(i: Int) extends IValue[TInt] {
    def self = this
    def depends = this
    def apply() = i

    override def toString = i.toString
  }

  trait TBin extends IValue[(I,I)] {
    def op: (Int,Int) => Int
    def rebuild(r: (I,I)): TBin
    lazy val apply = op(depends._1(), depends._2())
    override lazy val force: FValue[Int,_] = $$(FForce(this,rebuild((depends._1.force,depends._2.force))))
    override lazy val hashCode = Hashing.jenkinsHash(Hashing.jenkinsHash(depends.hashCode) ^ op.hashCode)
    override def toString = "(" + depends._1 + " " + op + " " + depends._2 + ")"
  }

  case class FForce[X,O,XX,OO](u: FValue[XX,OO], f: FValue[X,O]) extends FValue[X,FValue[XX,OO]] {
    def self = this
    def depends = u
    def apply() = f.apply()
    override def toString = u + " => " + f
  }
  val add1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x + y ; override def toString = "+" }
  val mul1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) = x * y ; override def toString = "*" }
  val drp1 = new Function2[Int,Int,Int] { def apply(x: Int, y: Int) =     y ; override def toString = "\\" }

  case class TAdd(depends: (I,I)) extends TBin {
    def self = this
    val op = add1
    def rebuild(r: (I,I)) = TAdd(r)
  }

  case class TMul(depends: (I,I)) extends TBin {
    def self = this
    def op = mul1
    def rebuild(r: (I,I)) = TMul(r)
  }

  case class TDrp(depends: (I,I)) extends TBin {
    def self = this
    def op = drp1
    def rebuild(r: (I,I)) = TDrp(r)
    override def toString = depends._2.toString

  }

  def ?(i: Int): TInt = $$(TInt(i)).asInstanceOf[TInt]
  def $[A,X](f: A => FValue[X,_], a: A): FValue[X,_] = $$(LazyFValue(f,a)).asInstanceOf[FValue[X,_]]

  implicit def ttint(i: Int): TInt = ?(i)
  val wt = WeakHashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val st = HashMap.empty[FValue[_,_], WeakReference[FValue[_,_]]]
  val vt = wt

  def $$[X,O](c: FValue[X,O]): FValue[X,O] = synchronized {
    if (!vt.contains(c)) {
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

  def create[V,P](x: V, p: P): FTreap[V,P] = LFTreap(x,p)
  def create[V,P](l: FTreap[V,P], x: V, p: P, r: FTreap[V,P]): FTreap[V,P] = {
    if (l.isEmpty && r.isEmpty) create(x,p)
    else if (l.isEmpty) BRightTreap(x,p,r)
    else if (r.isEmpty) BLeftTreap(l,x,p)
    else BFTreap[V,P](l,x,p,r)
  }

  def T[V,P](v: V)(implicit p: PrioOrdering[V,P]) = create(v,p.prio(v))

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

  def join[V,P](t1: FTreap[V,P], t2: FTreap[V,P], p: PrioOrdering[V,P]): FTreap[V,P] = {
    if (t1.isEmpty) t2
    else if (t2.isEmpty) t1
    else if (p.orderPrio(t1.prio,t2.prio) > 0) {
      create(t1.left,t1.value,t1.prio,join(t1.right,t2,p))
    }
    else {
      create(join(t1,t2.left,p),t2.value,t2.prio,t2.right)
    }
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

  def split[V,P](t1: FTreap[V,P], x: V, p: PrioOrdering[V,P]): (FTreap[V,P], Option[V], FTreap[V,P]) = {
    if (t1.isEmpty) (t1.left,None,t1.right)
    else {
      val e = t1.value
      val cc = p.orderValue(e,x)
      if (cc > 0) {
        val (l,xx,r) = split(t1.left,x,p)
        val r0 = create(e,p.prio(e))
        val r1 = join(r,r0,p)
        val r2 = join(r1,t1.right,p)
        (l,xx,r2)
      }
      else if (cc < 0) {
        val (l,xx,r) = split(t1.right,x,p)
        val l0 = create(e,p.prio(e))
        val l1 = join(t1.left,l0,p)
        var l2 = join(l1,l,p)
        (l2,xx,r)
      }
      else (t1.left,Some(e),t1.right)
    }
  }
}