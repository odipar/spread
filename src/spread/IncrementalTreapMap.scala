package spread

object IncrementalTreapMap {
  import Hashing._
  import scala.language.implicitConversions

  type T[K,V] = TreapMap[K,V]
  type PO[K,V] = PrioFactory[K,V]

  // Generic Set and Map implementation with super fast specialized Treaps
  // Todo: add optional Reducer (Monoid) functionality

  trait Entry[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] {
    def key: K
    def value: V
  }

  trait TreeMap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V, T <: TreeMap[K,V,T]] extends Entry[K,V] {
    def left: T
    def right: T
    def first: K
    def last: K
    def isEmpty: Boolean
  }

  trait TreapMap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends TreeMap[K,V,TreapMap[K,V]] {
    def prio: Int

    def join(x: T[K,V])(implicit p: PO[K,V]): T[K,V]
    def split(k: K)(implicit p: PO[K,V]): (T[K,V],T[K,V],T[K,V])

    def get(k: K)(implicit p: PO[K,V]): Option[V]
    def put(e: Entry[K,V])(implicit p: PO[K,V]): T[K,V]
  }

  type TreapSet[K] = TreapMap[K,K]

  trait PrioFactory[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] {
    def prio(k: K): Int
    def compare(k1: K, k2: K): Int

    def create(e: Entry[K,V]): T[K,V]
    def create(k: T[K,V]): T[K,V]
    def create(l: T[K,V], k: T[K,V], r: T[K,V]): T[K,V]
  }

  trait TreapImpl[K,V] extends T[K,V] {
    def join(x: T[K,V])(implicit p: PO[K,V]): T[K,V] = {
      if (isEmpty) x
      else if (x.isEmpty) this
      else if (prio > x.prio) p.create(left,this,right join x)
      else p.create(this join x.left,x,x.right)
    }
    def split(k: K)(implicit p: PO[K,V]): (T[K,V],T[K,V],T[K,V]) = {
      if (isEmpty) (this,this,this)
      else {
        def cc = p.compare(key,k)
        def nt = p.create(this)

        if (cc == 0) (left,nt,right)
        else if (cc > 0) { val (l,m,r) = left.split(k) ; (l,m,r join nt join right) }
        else { val (l,m,r) = right.split(k) ; (left join nt join l,m,r) }
      }
    }
    def get(k: K)(implicit p: PO[K,V]) = {
      if (isEmpty) None
      else {
        val cc = p.compare(key,k)

        if (cc == 0) Some(value)
        else if (cc < 0) left.get(k)
        else right.get(k)
      }
    }
    def put(e: Entry[K,V])(implicit p: PO[K,V]) = {
      val te = p.create(e)
      if (isEmpty) te
      else {
        val k = e.key
        val l = last

        if (p.compare(k,l) > 0) this join te
        else {
          val f = first
          if (p.compare(k,f) < 0) te join this
          else { val (l,m,r) = split(e.key) ; l join te join r }
        }
      }
    }
    def entryString: String = key + ":" + value
  }

  case object NTreap extends TreapImpl[Any,Any] {
    def key = sys.error("empty treap has no key")
    def value = sys.error("empty treap has no value")
    def prio = 0
    def left = this
    def right = this
    def first = sys.error("empty treap has no first")
    def last = sys.error("empty treap has no last")
    def isEmpty = true
    override def toString = "'"
  }

  def empty[K,V] = NTreap.asInstanceOf[T[K,V]]

  trait LeafTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends TreapImpl[K,V] {
    def isEmpty = false
    def left = empty
    def right = empty
    def first = key
    def last = key
    override def toString = entryString
  }

  trait SetTreap[K] extends TreapImpl[K,K] {
    def value = key

    override def entryString = value.toString
  }

  case class LTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V](key: K, value: V,prio: Int) extends LeafTreap[K,V]
  case class LSetTreap[@specialized(Int,Long,Double) K](key: K,prio: Int) extends LeafTreap[K,K] with SetTreap[K]

  trait BinaryTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends TreapImpl[K,V] {
    def isEmpty = false
    def first = left.first
    def last = right.last
    override def toString = left + " " + entryString + " " + right
    override val hashCode = jh(left.hashCode ^ jh(value.hashCode - prio.hashCode) ^ jh(right.hashCode))
  }

  case class BTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V](left: T[K,V],key: K, value: V,prio: Int,right: T[K,V]) extends BinaryTreap[K,V]
  case class BSetTreap[@specialized(Int,Long,Double) K](left: T[K,K],key: K,prio: Int,right: T[K,K]) extends BinaryTreap[K,K] with SetTreap[K]

  trait BinLeftTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends TreapImpl[K,V] {
    def right = empty
    def isEmpty = false
    def first = left.first
    def last = key
    override def toString = left + " " + entryString
    override val hashCode = jh(left.hashCode ^ jh(value.hashCode - prio.hashCode))
  }

  case class BLeftTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V](left: T[K,V],key: K, value: V,prio: Int) extends BinLeftTreap[K,V]
  case class BSetLeftTreap[@specialized(Int,Long,Double) K](left: T[K,K],key: K,prio: Int) extends BinLeftTreap[K,K] with SetTreap[K]

  trait BinRightTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends TreapImpl[K,V] {
    def left = empty
    def isEmpty = false
    def first = key
    def last = right.last
    override def toString = entryString + " " + right
    override val hashCode = jh(value.hashCode ^ jh(right.hashCode - prio.hashCode))
  }

  case class BRightTreap[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V](key: K, value: V,prio: Int,right: T[K,V]) extends BinRightTreap[K,V]
  case class BSetRightTreap[@specialized(Int,Long,Double) K](key: K, prio: Int,right: T[K,K]) extends BinRightTreap[K,K] with SetTreap[K]

  trait SetPrioFactoryImpl[@specialized(Int,Long,Double) K] extends PrioFactory[K,K] {
    def create(e: Entry[K,K]): T[K,K] =  LSetTreap(e.key,prio(e.key))
    def create(k: T[K,K]): T[K,K] = LSetTreap(k.key,k.prio)
    def create(l: T[K,K],kk: T[K,K],r: T[K,K]): T[K,K] = {
      val k = kk.key
      val p = kk.prio
      if (l.isEmpty) {
        if (r.isEmpty) LSetTreap(k,p)
        else BSetRightTreap(k,p,r)
      }
      else if (r.isEmpty) BSetLeftTreap(l,k,p)
      else BSetTreap(l,k,p,r)
    }
    def create(k: K): T[K,K] = LSetTreap(k,prio(k))
  }

  trait MapPrioFactoryImpl[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V] extends PrioFactory[K,V] {
    def create(e: Entry[K,V]): T[K,V] = LTreap(e.key,e.value,prio(e.key))
    def create(k: T[K,V]): T[K,V] = LTreap(k.key,k.value,k.prio)
    def create(l: T[K,V], kk: T[K,V],r: T[K,V]): T[K,V] = {
      val k = kk.key
      val v = kk.value
      val p = kk.prio
      if (l.isEmpty) {
        if (r.isEmpty) LTreap(k,v,p)
        else BRightTreap(k,v,p,r)
      }
      else if (r.isEmpty) BLeftTreap(l,k,v,p)
      else BTreap(l,k,v,p,r)
    }
    def create(k: K, v: V): T[K,V] = LTreap(k,v,prio(k))
  }

  case class IPrioFactory[@specialized(Int,Long,Double) V]() extends MapPrioFactoryImpl[Int,V] {
    def prio(k: Int): Int = jenkinsHash(k)
    def compare(k1: Int, k2: Int) = k1.compare(k2)
  }

  case class ISetPrioFactory[@specialized(Int,Long,Double) V]() extends SetPrioFactoryImpl[Int] {
    def prio(k: Int): Int = jenkinsHash(k)
    def compare(k1: Int, k2: Int) = k1.compare(k2)
  }

  case class LPrioFactory[@specialized(Int,Long,Double) V]() extends MapPrioFactoryImpl[Long,V] {
    def prio(k: Long): Int = jenkinsHash(k.toInt) + jenkinsHash((k >>> 32).toInt)
    def compare(k1: Long, k2: Long) = k1.compare(k2)
  }

  case class MapEntry[@specialized(Int,Long,Double) K, @specialized(Int,Long,Double) V](key: K, value: V) extends Entry[K,V]
  case class SetEntry[@specialized(Int,Long,Double) K](key: K) extends Entry[K,K] { def value = key }

  import IncrementalMemoization.{Expr,ei}

  implicit def treap[K,V](x: TreapMap[K,V]): Expr[TreapMap[K,V]] = ei(x)

}
