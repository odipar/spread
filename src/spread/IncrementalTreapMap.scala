package spread

object IncrementalTreapMap {
  import Hashing._
  import scala.language.implicitConversions
  import scala.reflect.ClassTag

  type T[K,V] = TreapMap[K,V]
  type PO[K,V] = PrioFactory[K,V]

  trait TreapMap[K,V] {
    def prio: Int
    def key: K
    def value: V
    def left: T[K,V]
    def right: T[K,V]
    def first: K
    def last: K
    def isEmpty: Boolean
    def join(x: T[K,V])(implicit p: PO[K,V]): T[K,V]
    def split(k: K)(implicit p: PO[K,V]): (T[K,V],T[K,V],T[K,V])
    def get(k: K)(implicit p: PO[K,V]): Option[V]
    def put(k: K, v: V)(implicit p: PO[K,V]): TreapMap[K,V]
  }

  trait PrioFactory[K,V] {
    def prio(k: K): Int
    def compare(k1: K, k2: K): Int
    def create(k: K, v: V, p: Int): T[K,V]
    def create(l: T[K,V],k: K, v: V,p: Int,r: T[K,V]): T[K,V]
    def create(k: K, v: V): T[K,V] = create(k,v,prio(k))
  }

  trait TreapImpl[K,V] extends T[K,V] {
    def join(x: T[K,V])(implicit p: PO[K,V]): T[K,V] = {
      if (isEmpty) x
      else if (x.isEmpty) this
      else if (prio > x.prio) p.create(left,key,value,prio,right join x)
      else p.create(this join x.left,x.key, x.value,x.prio,x.right)
    }
    def split(k: K)(implicit p: PO[K,V]): (T[K,V],T[K,V],T[K,V]) = {
      if (isEmpty) (this,this,this)
      else {
        def cc = p.compare(key,k)
        if (cc == 0) (left,p.create(key,value,prio),right)
        else if (cc > 0) {
          val (l,m,r) = left.split(k)
          (l,m,r join p.create(key,value,prio) join right)
        }
        else {
          val (l,m,r) = right.split(k)
          (left join p.create(key,value,prio) join l,m,r)
        }
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
    def put(k: K, v: V)(implicit p: PO[K,V]): T[K,V] = {
      val entry = p.create(k,v)

      if (isEmpty) entry
      else {
        val cc = p.compare(key,k)

        if (cc > 0) this join entry
        else if (cc < 0) entry join this
        else {
          val (l,_,r) = split(k)
          l join entry join r
        }
      }
    }
  }

  case object NTreap extends TreapImpl[Any,Any] {
    def key = sys.error("empty treap has no key")
    def value = sys.error("empty treap has no value")
    def prio = sys.error("empty treap has no priority")
    def left = this
    def right = this
    def first = sys.error("empty treap has no first")
    def last = sys.error("empty treap has no last")
    def isEmpty = true
    override def toString = "'"
  }

  def empty[K,V] = NTreap.asInstanceOf[T[K,V]]

  trait LeafTreap[K,V] extends TreapImpl[K,V] {
    def isEmpty = false
    def left = empty
    def right = empty
    def first = key
    def last = key
    override def toString = key.toString + ":" + value.toString
  }

  case class LTreap[K,V](key: K, value: V,prio: Int) extends LeafTreap[K,V]

  trait BinaryTreap[K,V] extends TreapImpl[K,V] {
    def isEmpty = false
    def first = left.first
    def last = right.last
    override def toString = left + " " + key + ":" + value.toString + " " + right
    override val hashCode = jh(left.hashCode ^ jh(value.hashCode - prio.hashCode) + jh(right.hashCode))
  }

  case class BTreap[K,V](left: T[K,V],key: K, value: V,prio: Int,right: T[K,V]) extends BinaryTreap[K,V]

  trait BinLeftTreap[K,V] extends TreapImpl[K,V] {
    def right = empty
    def isEmpty = false
    def first = left.first
    def last = key
    override def toString = left + " " + key + ":" + value.toString
    override val hashCode = jh(left.hashCode ^ jh(value.hashCode - prio.hashCode))
  }

  case class BLeftTreap[K,V](left: T[K,V],key: K, value: V,prio: Int) extends BinLeftTreap[K,V]

  trait BinRightTreap[K,V] extends TreapImpl[K,V] {
    def left = empty
    def isEmpty = false
    def first = key
    def last = right.last
    override def toString = key.toString + ":" + value.toString + " " + right
    override val hashCode = jh(value.hashCode ^ jh(right.hashCode - prio.hashCode))
  }

  case class BRightTreap[K,V](key: K, value: V,prio: Int,right: T[K,V]) extends BinRightTreap[K,V]

  trait PrioFactoryImpl[K,V] extends PrioFactory[K,V] {
    def create(k: K, v: V, p: Int): T[K,V] = LTreap(k,v,p)
    def create(l: T[K,V],k: K, v: V,p: Int,r: T[K,V]): T[K,V] = {
      if (l.isEmpty) {
        if (r.isEmpty) LTreap(k,v,p); else BRightTreap(k,v,p,r)
      }
      else if (r.isEmpty) BLeftTreap(l,k,v,p)
      else BTreap(l,k,v,p,r)
    }
  }

  case class LPrioFactory[V]() extends PrioFactoryImpl[Long,V] {
    def prio(k: Long): Int = jenkinsHash(k.toInt) + jenkinsHash((k >>> 32).toInt)
    def compare(k1: Long, k2: Long) = k1.compare(k2)
  }
}
