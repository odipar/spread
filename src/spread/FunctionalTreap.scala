package spread

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

  case object IPrioOrdering extends MemoizedPrioOrdering[Int,Int] {
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

    val create_2 = create2[V,P](this)
    val join_2 = join2[V,P](this)
    val lsplit_2 = lsplit2[V,P](this)
    val rsplit_2 = rsplit2[V,P](this)
    val put_2 = put2[V,P](this)

    def create(v: V): VFT[V,P] = LFTreap(v,prio(v))
    def create(l: VFT[V,P], v: V, p: P, r: VFT[V,P]): VFT[V,P] = create_2(l(),(v,p),r())
    def join(t1: VFT[V,P], t2: VFT[V,P]): VFT[V,P] = join_2(t1(),t2())
    def lsplit(t: VFT[V,P], v: V): VFT[V,P] = lsplit_2(t(),v)
    def rsplit(t: VFT[V,P], v: V): VFT[V,P] = rsplit_2(t(),v)
    def put(t: VFT[V,P], v: V): VFT[V,P] = put_2(t(),v)

  }

  trait MemoizedPrioOrdering[V,P] extends PrioOrdering[V,P] {
    val fcreate = fcreate1[V,P]
    val create_1 = create1[V,P](this)
    val join_1 = join1[V,P](this)
    val lsplit_1 = lsplit1[V,P](this)
    val rsplit_1 = rsplit1[V,P](this)

    override def create(v: V): VFT[V,P] = fcreate(v,prio(v)) //$$(LFTreap(v,prio(v)))
    override def create(l: VFT[V,P], v: V, p: P, r: VFT[V,P]): VFT[V,P] = create_1(l,(v,p),r)// $$(create_2(l(),(v,p),r()))
    override def join(t1: VFT[V,P], t2: VFT[V,P]): VFT[V,P] = join_1(t1,t2)
    override def lsplit(t: VFT[V,P], v: V): VFT[V,P] = lsplit_1(t,v)
    override def rsplit(t: VFT[V,P], v: V): VFT[V,P] = rsplit_1(t,v)
  }
  type VFT[V,P] = FValue[FTreap[V,P],_]

  def create1[V,P](p: PrioOrdering[V,P]) = new Function3[VFT[V,P],(V,P),VFT[V,P],VFT[V,P]] {
    val create: Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] = create2(p)
    def join = this
    def apply(l: VFT[V,P], vp: (V,P), r: VFT[V,P]): VFT[V,P] = %%%(create,l(),vp,r())
    override def toString = "create2"
  }

  def create2[V,P](p: PrioOrdering[V,P]): Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] = new Function3[FTreap[V,P],(V,P),FTreap[V,P],VFT[V,P]] {
    val create: Function3[FTreap[V,P], (V,P), FTreap[V,P],FTreap[V,P]] = fcreate
    def join = this
    def apply(l: FTreap[V,P], vp: (V,P), r: FTreap[V,P]): VFT[V,P] = create(l,vp,r)
    override def toString = "create3"
  }

  def join1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P],VFT[V,P],VFT[V,P]] = new Function2[VFT[V,P],VFT[V,P],VFT[V,P]] {
    val join = join2(p)
    def apply(a1: VFT[V,P], a2: VFT[V,P]): VFT[V,P] = %%(join,a1(),a2())
    override def toString = "join1"
  }

  def join2[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P],FTreap[V,P],VFT[V,P]] {
    def apply(a1: FTreap[V,P], a2: FTreap[V,P]): VFT[V,P] = {
      val t1 = a1
      val t2 = a2
      if (t1.isEmpty) t2
      else if (t2.isEmpty) t1
      else if (p.orderPrio(t1.prio,t2.prio) > 0) {
        p.create(a1.left,t1.value,t1.prio,p.join(a1.right,a2))
      }
      else {
        p.create(p.join(a1,a2.left),t2.value,t2.prio,a2.right)
      }
    }
    override def toString = "join2"
  }

  def left2[V,P] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(a1: VFT[V,P]) = a1().left
    override def toString = "left"
  }

  def right2[V,P] = new Function1[VFT[V,P],VFT[V,P]] {
    def apply(a1: VFT[V,P]) = a1().right
    override def toString = "right"
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

  def lsplit1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P], V, VFT[V,P]] = new Function2[VFT[V,P], V, VFT[V,P]] {
    lazy val split = lsplit2(p)
    def apply(t1: VFT[V,P], x: V): VFT[V,P] =  split(t1(),x)
    override def toString = "lsplit1"
  }

  def lsplit2[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P], V, VFT[V,P]] {
    lazy val split = lsplit1(p)
    val join = join1(p)
    def apply(t1: FTreap[V,P], x: V): VFT[V,P] = {
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e,x)
        if (cc > 0) p.lsplit(t1.left,x)
        else if (cc < 0) {
          val r = p.lsplit(t1.right,x)
          val l0 = p.create(e)
          val l1 = p.join(t1.left,l0)
          p.join(l1,r)
        }
        else t1.left
      }
    }
    override def toString = "lsplit2"
  }

  def rsplit1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P], V, VFT[V,P]] = new Function2[VFT[V,P], V, VFT[V,P]] {
    lazy val split = rsplit2(p)
    def apply(t1: VFT[V,P], x: V): VFT[V,P] =  split(t1(),x)
    override def toString = "rsplit1"
  }

  def rsplit2[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P], V, VFT[V,P]] {
    def apply(t1: FTreap[V,P], x: V): VFT[V,P] = {
      if (t1.isEmpty) t1
      else {
        val e = t1.value
        val cc = p.orderValue(e,x)
        if (cc > 0) {
          val l = p.rsplit(t1.left,x)
          val l0 = p.create(e)
          val r = p.join(l0,t1.right)
          p.join(l,r)
        }
        else if (cc < 0) p.rsplit(t1.right,x)
        else t1.right
      }
    }
    override def toString = "rsplit2"
  }

  def put1[V,P](p: PrioOrdering[V,P]): Function2[VFT[V,P], V, VFT[V,P]] = new Function2[VFT[V,P], V, VFT[V,P]] {
    val put = put2(p)
    def apply(t1: VFT[V,P], x: V): VFT[V,P] =  put(t1(),x)
    override def toString = "put1"
  }

  def put2[V,P](p: PrioOrdering[V,P]) = new Function2[FTreap[V,P], V, VFT[V,P]] {
    def apply(t1: FTreap[V,P], x: V): VFT[V,P] = {
      val r = p.rsplit(t1,x)
      val l = p.lsplit(t1,x)
      val e = p.create(x)
      val ll = p.join(l,e)
      p.join(ll,r)
    }
    override def toString = "put2"
  }
}
