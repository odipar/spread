package spread

object WeightBalancedSequence {
  import AbstractImmutableSequence._

  trait AppendWBTreeImpl[N,X,M, SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC] {
    def append(r: SS)(implicit c: CC) = {
      val l = self
      val ss = c.sizing
      val ls = l.size
      val rs = r.size
      if (ss.equals(ls,ss.zero)) (r,c)
      else if (ss.equals(rs,ss.zero)) (l,c)
      else if (ss.compare(ss.mul(ls,c.delta2),rs) < 0) {
        val rl = r.left
        val rr = r.right
        val srr = rr.size
        if (ss.compare(ss.mul(srr,c.ratio2),rl.size) < 0) {
          val (l1,c1) = c.append(l,rl.left)
          val (r1,c2) = c1.create(rl.right,rr)
          c2.create(l1,r1)
        }
        else {
          val (l1,c1) = c.append(l,rl)
          c1.create(l1,rr)
        }
      }
      else if (ss.compare(ss.mul(rs,c.delta2),ls) < 0) {
        val ll = l.left
        val lr = l.right
        val ssl = ll.size
        if (ss.compare(ss.mul(ssl,c.ratio2),lr.size) < 0) {
          val (r1,c1) = c.append(lr.right,r)
          val (l1,c2) = c1.create(ll,lr.left)
          c2.create(l1,r1)
        }
        else {
          val (r1,c1) = c.append(lr,r)
          c1.create(ll,r1)
        }
      }
      else c.create(l,r)
    }
  }

  trait WBTree[N,X,M, SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends ISeq[N,X,M,SS,CC]
  trait WBTreeContext[N,X,M, SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends ISeqContext[N,X,M,SS,CC] {
    final lazy val delta2: N = { // 4
      val four = sizing.add(ratio2,ratio2)
      four
    }
    final lazy val ratio2: N = { // 2
      val one = sizing.one
      val two = sizing.add(one,one)
      two
    }
  }

  trait WBTreeImpl[N,X,M,SS <: WBTreeImpl[N,X,M,SS,CC], CC <: WBTreeContextImpl[N,X,M,SS,CC]]
    extends WBTree[N,X,M,SS,CC] with ISeqImpl[N,X,M,SS,CC]

  trait WBTreeContextImpl[N,X,M,SS <: WBTreeImpl[N,X,M,SS,CC], CC <: WBTreeContextImpl[N,X,M,SS,CC]]
    extends WBTreeContext[N,X,M,SS,CC] with ISeqContextImpl[N,X,M,SS,CC] {
  }

  trait IWBTree[N,X,M] extends WBTreeImpl[N,X,M,IWBTree[N,X,M],IWBTreeContext[N,X,M]] {
    def self = this
    type MM = IWBTree[N,X,M]
    type CC = IWBTreeContext[N,X,M]
  }

  trait IWBTreeContext[N,X,M] extends WBTreeContextImpl[N,X,M,IWBTree[N,X,M],IWBTreeContext[N,X,M]] {
    def self = this
    type MM = IWBTree[N,X,M]
    type CC = IWBTreeContext[N,X,M]
  }

  type SWB[N,X,M] = IWBTree[N,X,M]
  type SWC[N,X,M] = IWBTreeContext[N,X,M]

  trait IWBTreeImpl[N,X,M] extends IWBTree[N,X,M] with AppendWBTreeImpl[N,X,M,SWB[N,X,M],SWC[N,X,M]]

  trait IWBTreeContextImpl[N,X,M] extends IWBTreeContext[N,X,M] {
    val emptyC: MM = EmptyIWBTree()
    def empty = (emptyC,this)
    def create(x: X) = measure(Some(x)) match {
      case None => (VLeafIWBTree(x),this)
      case Some(m) => (MLeafIWBTree(x,m),this)
    }
    def create(l: MM, r: MM) = {
      val ss = c.sizing
      val ns = ss.add(l.size,r.size)
      measure(l.measure,r.measure) match {
        case None => (VBinWBTree(l,ns,r),this)
        case Some(m) => (MBinWBTree(l,ns,m,r),this)
      }
    }
  }

  trait EmptyWBMeasure[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC] {
    def measure(implicit c: CC) = None
  }

  trait LeafWBMeasure[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC] {
    def m: M
    def measure(implicit c: CC) = Some(m)
  }

  trait EmptyWBTree[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC] {
    def first(implicit c: CC): Option[X] = None
    def last(implicit c: CC): Option[X] = None
    def size(implicit c: CC) = c.sizing.zero
    def left(implicit c: CC) = c.empty._1
    def right(implicit c: CC) = c.empty._1
    def measure(implicit c: CC) =  c.measure(None)
    def depth = 0
  }

  trait NonEmptyWBTree[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC]

  trait LeafWBTree[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends NonEmptyWBTree[N,X,M,SS,CC] {
    def x: X
    def first(implicit c: CC): Option[X] = Some(x)
    def last(implicit c: CC): Option[X] = Some(x)
    def left(implicit c: CC): SS = c.empty._1
    def right(implicit c: CC): SS = c.empty._1
    def size(implicit c: CC): N = c.sizing.one
    def depth = 1
  }

  trait BinWBTree[N,X,M,SS <: WBTree[N,X,M,SS,CC], CC <: WBTreeContext[N,X,M,SS,CC]] extends WBTree[N,X,M,SS,CC] {
    def l: SS
    def r: SS
    def s: N
    def first(implicit c: CC): Option[X] = l.first
    def last(implicit c: CC): Option[X] = l.last
    def left(implicit c: CC) = l
    def right(implicit c: CC) = r
    def size(implicit c: CC) = s
    def depth = (l.depth max r.depth) + 1
  }

  type EMS[N,X,M] = EmptyWBMeasure[N,X,M,SWB[N,X,M],SWC[N,X,M]]
  type LMS[N,X,M] = LeafWBMeasure[N,X,M,SWB[N,X,M],SWC[N,X,M]]

  trait NonEmptyIWBTree[N,X,M] extends IWBTreeImpl[N,X,M] with NonEmptyWBTree[N,X,M,SWB[N,X,M],SWC[N,X,M]]
  trait LeafIWBTree[N,X,M] extends NonEmptyIWBTree[N,X,M] with LeafWBTree[N,X,M,SWB[N,X,M],SWC[N,X,M]]
  trait BinIWBTree[N,X,M] extends NonEmptyIWBTree[N,X,M] with BinWBTree[N,X,M,SWB[N,X,M],SWC[N,X,M]]
  case class EmptyIWBTree[N,X,M]() extends IWBTreeImpl[N,X,M] with EmptyWBTree[N,X,M,SWB[N,X,M],SWC[N,X,M]]
  case class VLeafIWBTree[N,X,M](x: X) extends LeafIWBTree[N,X,M] with EMS[N,X,M]
  case class MLeafIWBTree[N,X,M](x: X, m: M) extends LeafIWBTree[N,X,M] with LMS[N,X,M]
  case class VBinWBTree[N,X,M](l: IWBTree[N,X,M], s: N, r: IWBTree[N,X,M]) extends BinIWBTree[N,X,M] with EMS[N,X,M]
  case class MBinWBTree[N,X,M](l: IWBTree[N,X,M], s: N, m: M, r: IWBTree[N,X,M]) extends BinIWBTree[N,X,M] with LMS[N,X,M]

  object IntNum extends Num[Int] {
    def zero = 0
    def one = 1

    def add(n1: Int, n2: Int) = n1 + n2
    def sub(n1: Int, n2: Int) = n1 - n2
    def mul(n1: Int, n2: Int) = n1 * n2

    def equals(n1: Int, n2: Int) = n1 == n2
    def compare(n1: Int, n2: Int) = n1.compare(n2)
  }

  implicit val intNum: Num[Int] = IntNum

  case class DefaultIWBTreeContext[X](implicit n: Num[Int]) extends IWBTreeContextImpl[Int,X,Any] {
    def sizing = n
  }
}