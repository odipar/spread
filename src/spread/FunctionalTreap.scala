package spread

import scala.Function2

object FunctionalTreap {
  import IncrementalMemoization._
  import scala.language.implicitConversions
  import scala.language.existentials

  trait FTreap[V,P] extends FValue[FTreap[V,P],FTreap[V,P]] {
    def isEmpty: Boolean
    def prio: P
    def left: FTreap[V,P]
    def value: V
    def right: FTreap[V,P]

    def apply() = this
    def origin = this

    case object NFTreap extends FTreap[V,P] {
      def isEmpty = true
      def prio = sys.error("empty treap has no priority")
      def left = this
      def value = sys.error("empty treap has no value")
      def right = this
    }
  }

  case class LFTreap[V,P](value: V, prio: P) extends FTreap[V,P] {
    def isEmpty = false
    def left = NFTreap
    def right = NFTreap
    override def toString = value.toString
  }

  case class BFTreap[V,P](left: FTreap[V,P], value: V, prio: P, right: FTreap[V,P]) extends FTreap[V,P] {
    def isEmpty = false
    override def toString = left + "'" + value.toString + "'" + right
    override lazy val hashCode = {
      Hashing.jenkinsHash(left.hashCode ^ Hashing.jenkinsHash(value.hashCode ^ prio.hashCode) + Hashing.jenkinsHash(right.hashCode))
    }
  }

  case class BLeftTreap[V,P](left: FTreap[V,P], value: V, prio: P) extends FTreap[V,P] {
    def right = NFTreap
    def isEmpty = false
    override def toString = left + "'" + value.toString
    override lazy val hashCode = Hashing.jenkinsHash(left.hashCode ^ Hashing.jenkinsHash(value.hashCode ^ prio.hashCode))
  }

  case class BRightTreap[V,P](value: V, prio: P, right: FTreap[V,P]) extends FTreap[V,P] {
    def left = NFTreap
    def isEmpty = false
    override def toString = value.toString + "'" + right
    override lazy val hashCode = Hashing.jenkinsHash(right.hashCode ^ Hashing.jenkinsHash(value.hashCode ^ prio.hashCode))
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

  def T[V,P](v: V)(implicit p: PrioOrdering[V,P]): FTreap[V,P] = fcreate1(v,p.prio(v))

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

    val create_1 = create1[V,P](this)
    val create_2 = create2[V,P](this)
    val create_3 = create3[V,P](this)
    val join_1 = join1[V,P](this)
    val join_2 = join2[V,P](this)
    val join_3 = join3[V,P](this)
    val left_1 = left1[V,P](this)
    val left_2 = left2[V,P](this)
    val left_3 = left3[V,P](this)
    val right_1 = right1[V,P](this)
    val right_2 = right2[V,P](this)
    val right_3 = right3[V,P](this)
    val lsplit_1 = lsplit1[V,P](this)
    val lsplit_2 = lsplit2[V,P](this)
    val lsplit_3 = lsplit3[V,P](this)
    val rsplit_1 = rsplit1[V,P](this)
    val rsplit_2 = rsplit2[V,P](this)
    val rsplit_3 = rsplit3[V,P](this)
    val put_3 = put3[V,P](this)

    def create(v: V): VFT[V,P] = $$(LFTreap(v,prio(v)))
    def create(l: VFT[V,P], v: V, p: P, r: VFT[V,P]): VFT[V,P] = %(create_1,l,FI(v,p),r)
    def join(t1: VFT[V,P], t2: VFT[V,P]): VFT[V,P] = %(join_1,t1,t2)
    def left(t: VFT[V,P]): VFT[V,P] =  %(left_1,t)
    def right(t: VFT[V,P]): VFT[V,P] = %(right_1,t)
    def lsplit(t: VFT[V,P], v: FValue[V,_]): VFT[V,P] = %(lsplit_1,t,v)
    def rsplit(t: VFT[V,P], v: FValue[V,_]): VFT[V,P] = %(rsplit_1,t,v)
    def put(t: VFT[V,P], v: FValue[V,_]): VFT[V,P] = %(put_3,t,v)

    /* def lsplit(t: VFT[V,P], v: V): VFT[V,P] = lsplit_2(t(),v)
     def rsplit(t: VFT[V,P], v: V): VFT[V,P] = rsplit_2(t(),v)
     def put(t: VFT[V,P], v: V): VFT[V,P] = put_2(t(),v)  */

  }

  type VFT[V,P] = FValue[FTreap[V,P],_]

  def create1[V,P](p: PrioOrdering[V,P]): Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] = new Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] {
    val create: Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] = create2(p)
    def apply(l: VFT[V,P], vp: FValue[(V,P),_], r: VFT[V,P]): VFT[V,P] = %(create,l.force,vp,r.force)
    override def toString = "create1"
  }

  def create2[V,P](p: PrioOrdering[V,P]): Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] = new Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] {
    val create: Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] = create3(p)
    def apply(l: VFT[V,P], vp: FValue[(V,P),_], r: VFT[V,P]): VFT[V,P] = %(create,l.force(),vp,r.force())
    override def toString = "create1"
  }

  def create3[V,P](p: PrioOrdering[V,P]): Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] = new Function3[VFT[V,P],FValue[(V,P),_],VFT[V,P],VFT[V,P]] {
    val create: Function3[FTreap[V,P], (V,P), FTreap[V,P],FTreap[V,P]] = fcreate
    def apply(l: VFT[V,P], vp: FValue[(V,P),_], r: VFT[V,P]): VFT[V,P] = create(l(),vp(),r())
    override def toString = "create2"
  }

  def left1[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    val left = left2(p)
    def apply(t: VFT[V,P]): VFT[V,P] = %(left,t.force)
    override def toString = "left1"
  }

  def left2[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    val left = left3(p)
    def apply(t: VFT[V,P]): VFT[V,P] = %(left,t.force())
    override def toString = "left2"
  }

  def left3[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(t: VFT[V,P]): VFT[V,P] =  {
     // println("left3: " + t)
      t().left
    }
    override def toString = "left2"
  }

  def right1[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    val right = right2(p)
    def apply(t: VFT[V,P]): VFT[V,P] = %(right,t.force)
    override def toString = "right1"
  }

  def right2[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    val right = right3(p)
    def apply(t: VFT[V,P]): VFT[V,P] = %(right,t.force())
    override def toString = "right2"
  }

  def right3[V,P](p: PrioOrdering[V,P]): Function1[VFT[V,P],VFT[V,P]] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(t: VFT[V,P]): VFT[V,P] = {
      //println("right3: " + t)
      t().right
    }
    override def toString = "right2"
  }


  def join1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P],VFT[V,P],VFT[V,P]] = new Function2[VFT[V,P],VFT[V,P],VFT[V,P]] {
    val join = join2(p)
    def apply(l: VFT[V,P], r: VFT[V,P]): VFT[V,P] = %(join,l.force,r.force)
    override def toString = "join1"
  }

  def join2[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P],VFT[V,P],VFT[V,P]] = new Function2[VFT[V,P],VFT[V,P],VFT[V,P]] {
    val join = join3(p)
    def apply(l: VFT[V,P], r: VFT[V,P]): VFT[V,P] = %(join,l.force(),r.force())
    override def toString = "join2"
  }

  def join3[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],VFT[V,P],VFT[V,P]] {
    def apply(a1: VFT[V,P], a2: VFT[V,P]): VFT[V,P] = {
     // println("join3: " + a1 + " , " + a2)

      val t1 = a1()
      val t2 = a2()
      if (t1.isEmpty) t2
      else if (t2.isEmpty) t1
      else if (p.orderPrio(t1.prio,t2.prio) > 0) {
        p.create(p.left(a1),t1.value,t1.prio,p.join(p.right(a1),a2))
      }
      else {
        p.create(p.join(a1,p.left(a2)),t2.value,t2.prio,p.right(a2))
      }
    }
    override def toString = "join2"
  }

  def lsplit1[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
    val split = lsplit2(p)
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = %(split,a1.force,x)
    override def toString = "lsplit1"
  }

  def lsplit2[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
    val split = lsplit3(p)
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = %(split,a1.force(),x)
    override def toString = "lsplit2"
  }

  def lsplit3[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = {
      val t1 = a1()
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e,x())
        if (cc > 0) p.lsplit(p.left(a1),x)
        else if (cc < 0) {
          val r = p.lsplit(p.right(a1),x)
          val l0 = p.create(e)
          val l1 = p.join(p.left(a1),l0)
          p.join(l1,r)
        }
        else t1.left
      }
    }
    override def toString = "lsplit3"
  }


  def rsplit1[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
    val split = rsplit2(p)
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = %(split,a1.force,x)
    override def toString = "rsplit1"
  }

  def rsplit2[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
    val split = rsplit3(p)
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = %(split,a1.force(),x)
    override def toString = "rsplit2"
  }


  def rsplit3[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P],FValue[V,_],VFT[V,P]] {
   def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = {
      val t1 = a1()
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e,x())
        if (cc > 0) {
          val l = p.rsplit(p.left(a1),x)
          val l0 = p.create(e)
          val r = p.join(l0,p.right(a1))
          p.join(l,r)
        }
        else if (cc < 0) p.rsplit(p.right(a1),x)
        else t1.right
      }
    }
    override def toString = "rsplit3"
  }

 def put3[V,P](p: PrioOrdering[V,P]) = new Function2[VFT[V,P], FValue[V,_], VFT[V,P]] {
    def apply(a1: VFT[V,P], x: FValue[V,_]): VFT[V,P] = {
      val r = p.rsplit(a1,x)
      val l = p.lsplit(a1,x)
      val e = p.create(x())
      val ll = p.join(l,e)
      p.join(ll,r)
    }
    override def toString = "put3"
  }
}
