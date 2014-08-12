package spread

/*
  Copyright 2014: Robbert van Dalen
 */

object IncrementalTreap {
  import IncrementalMemoization._
  import Hashing._
  import scala.language.implicitConversions
  import scala.reflect.runtime.universe.TypeTag

  trait FTreap[V, P] extends F0[FTreap[V, P]] {
    def isEmpty: Boolean
    def prio: P
    def left: FTreap[V, P]
    def value: V
    def right: FTreap[V, P]

    def evalValue = this
    def contains[X](x: X): Expr[Boolean] = false
    def set[X,O: TypeTag](x: X, o: Expr[O]) = this
    def unquote = this
    def containsQuotes = false
  }

  case class NFTreap[V, P]() extends FTreap[V, P] {
    def isEmpty = true
    def left = this
    def value = sys.error("empty treap has no value")
    def prio = sys.error("empty treap has no priority")
    def right = this
    override def toString = "'"
  }

  case class LFTreap[V, P](value: V, prio: P) extends FTreap[V, P] {
    def isEmpty = false
    def left = NFTreap()
    def right = NFTreap()
    override def toString = value.toString
  }

  case class BFTreap[V, P](left: FTreap[V, P], value: V, prio: P, right: FTreap[V, P]) extends FTreap[V, P] {
    def isEmpty = false
    override def toString = left + "'" + value.toString + "'" + right
    override lazy val hashCode = {
      jenkinsHash(left.hashCode ^ jenkinsHash(value.hashCode ^ prio.hashCode) ^ jenkinsHash(right.hashCode))
    }
  }

  case class BLeftTreap[V, P](left: FTreap[V, P], value: V, prio: P) extends FTreap[V, P] {
    def right = NFTreap()
    def isEmpty = false
    override def toString = left + "'" + value.toString
    override lazy val hashCode = jenkinsHash(left.hashCode ^ jenkinsHash(value.hashCode ^ prio.hashCode))
  }

  case class BRightTreap[V, P](value: V, prio: P, right: FTreap[V, P]) extends FTreap[V, P] {
    def left = NFTreap()
    def isEmpty = false
    override def toString = value.toString + "'" + right
    override lazy val hashCode = jenkinsHash(value.hashCode ^ jenkinsHash(right.hashCode ^ prio.hashCode))
  }

  type VFT[V, P] = Expr[FTreap[V, P]]
  type FT[V,P] = FTreap[V,P]
  type F0T[V,P] = F0[FT[V,P]]

  def fcreate[V, P]: Function3[FTreap[V, P], (V, P), FTreap[V, P], FTreap[V, P]] = new Function3[FTreap[V, P], (V, P), FTreap[V, P], FTreap[V, P]] {
    def apply(l: FTreap[V, P], xp: (V, P), r: FTreap[V, P]): FTreap[V, P] = {
      if (l.isEmpty && r.isEmpty) LFTreap(xp._1, xp._2)
      else if (l.isEmpty) BRightTreap(xp._1, xp._2, r)
      else if (r.isEmpty) BLeftTreap(l, xp._1, xp._2)
      else BFTreap[V, P](l, xp._1, xp._2, r)
    }
  }
  def T[V, P](v: V)(implicit p: PrioOrdering[V, P]): VFT[V, P] = p.create(v)

  case object IPrioOrdering extends PrioOrdering[Int, Int] {
    def prio(v: Int): Int = jenkinsHash(jenkinsHash(v) + v.hashCode)
    def orderValue(v1: Int, v2: Int) = if (v1 > v2) 1; else if (v1 < v2) -1; else 0
    def orderPrio(p1: Int, p2: Int) = if (p1 > p2) 1; else if (p1 < p2) -1; else 0
    override def toString = "o"
  }

  implicit def intord: PrioOrdering[Int, Int] = IPrioOrdering

  trait PrioOrdering[V, P] {
    def prio(v: V): P
    def orderValue(v1: V, v2: V): Int
    def orderPrio(p1: P, p2: P): Int
    lazy val create_0 = create0[V,P](this)
    lazy val create_1 = create1[V, P](this)
    lazy val join_1 = join1[V, P](this)
    lazy val left_1 = left1[V, P](this)
    lazy val right_1 = right1[V, P](this)
    lazy val lsplit_1 = lsplit1[V, P](this)
    lazy val rsplit_1 = rsplit1[V, P](this)
    lazy val put_1 = put1[V, P](this)
    def create(v: V): VFT[V, P] = create_0(ei(v))
    def create(l: VFT[V, P], v: V, p: P, r: VFT[V, P]): VFT[V, P] = hcons(create_1(l, ei(v, p), r))
    def join(t1: VFT[V, P], t2: VFT[V, P]): VFT[V, P] = join_1(t1, t2)
    def left(t: VFT[V, P]): VFT[V, P] = left_1(t)
    def right(t: VFT[V, P]): VFT[V, P] = right_1(t)
    def lsplit(t: VFT[V, P], v: Expr[V]): VFT[V, P] = %(lsplit_1, t, v)
    def rsplit(t: VFT[V, P], v: Expr[V]): VFT[V, P] = %(rsplit_1, t, v)
    def put(t: VFT[V, P], v: Expr[V]): VFT[V, P] = %(put_1, t, v)
  }

  def create0[V, P](p: PrioOrdering[V, P]): FA1[V, FT[V, P]] = new FA1[V, FT[V, P]]  {
    def apply(v: F0[V]): VFT[V, P] = LFTreap(v.evalValue, p.prio(v.evalValue))
    override def toString = "create0"
  }

  def create1[V, P](p: PrioOrdering[V, P]): FA3[FT[V, P], (V, P), FT[V, P], FT[V, P]] = new FA3[FT[V, P], (V, P), FT[V, P], FT[V, P]] {
    val create: Function3[FTreap[V, P], (V, P), FTreap[V, P], FTreap[V, P]] = fcreate
    def apply(l: F0T[V,P], vp: F0[(V, P)], r: F0T[V, P]): VFT[V, P] =  create(l.evalValue, vp.evalValue, r.evalValue)
    override def toString = "create1"
  }
  def left1[V, P](p: PrioOrdering[V, P]): FA1[FT[V, P], FT[V, P]] = new FA1[FT[V, P], FT[V, P]] {
    def apply(t: F0T[V, P]): VFT[V, P] = t.evalValue.left
    override def toString = "left"
  }
  def right1[V, P](p: PrioOrdering[V, P]): FA1[FT[V, P], FT[V, P]] = new FA1[FT[V, P], FT[V, P]] {
    def apply(t: F0T[V, P]): VFT[V, P] = t.evalValue.right
    override def toString = "right"
  }

  def join1[V, P](p: PrioOrdering[V, P]) = new FA2[FT[V, P], FT[V, P], FT[V, P]] {
    def apply(a1: F0T[V, P], a2: F0T[V, P]): VFT[V, P] = {
      val t1 = a1.evalValue
      val t2 = a2.evalValue
      if (t1.isEmpty) t2
      else if (t2.isEmpty) t1
      else if (p.orderPrio(t1.prio, t2.prio) > 0) {
        p.create(p.left(a1), t1.value, t1.prio, p.join(p.right(a1), a2))
      }
      else {
        p.create(p.join(a1, p.left(a2)), t2.value, t2.prio, p.right(a2))
      }
    }
    override def toString = "join"
  }

  def lsplit1[V, P](p: PrioOrdering[V, P]) = new FA2[FT[V, P], V, FT[V, P]] {
    def apply(a1: F0T[V, P], x: F0[V]): VFT[V, P] = {
      val t1 = a1.evalValue
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e, x.evalValue)
        if (cc > 0) p.lsplit(p.left(a1), x)
        else if (cc < 0) {
          val r = p.lsplit(p.right(a1), x)
          val l0 = p.create(e)
          val l1 = p.join(p.left(a1), l0)
          p.join(l1, r)
        }
        else t1.left
      }
    }
    override def toString = "lsplit"
  }

  def rsplit1[V, P](p: PrioOrdering[V, P]) = new FA2[FT[V, P], V, FT[V, P]] {
    def apply(a1: F0T[V, P], x: F0[V]): VFT[V, P] = {
      val t1 = a1.evalValue
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e, x.evalValue)
        if (cc > 0) {
          val l = p.rsplit(p.left(a1), x)
          val l0 = p.create(e)
          val r = p.join(l0, p.right(a1))
          p.join(l, r)
        }
        else if (cc < 0) p.rsplit(p.right(a1), x)
        else t1.right
      }
    }
    override def toString = "rsplit"
  }

  def put1[V, P](p: PrioOrdering[V, P]) = new FA2[FT[V, P], V, FT[V, P]] {
    def apply(a1: F0T[V, P], x: F0[V]): VFT[V, P] = {
      val r = p.rsplit(a1, x)
      val l = p.lsplit(a1, x)
      val e = p.create(x.evalValue)
      val ll = p.join(l, e)
      p.join(ll, r)
    }
    override def toString = "put"
  }
}
