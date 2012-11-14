package spread

object OrderedTreapSet {
  import AbstractImmutableOrderedSet._

  // See: http://www.cs.cmu.edu/~scandal/papers/treaps-spaa98.pdf
  trait JoinTreapImpl[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends Treap[X,M,P,SS,CC] {
    def join(o: SS)(implicit c: CC): (SS,CC) = { // returns the join of this with c
      if (isEmpty) (o,c)
      else if (o.isEmpty) (this.asInstanceOf[SS],c)
      else if (c.orderPriority(priority,o.priority) > 0) {
        val (r1,c1) = c.join(right,o) ; c1.create(left,some,r1)
      }
      else {
        val (l1,c1) = c.join(this.asInstanceOf[SS],o.left) ; c1.create(l1,o.some,o.right)
      }
    }
  }

  trait Treap[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends SISet[X,M,SS,CC] {
    def priority(implicit c: CC): P // returns the Treap priority
  }

  trait TreapContext[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends SISetContext[X,M,SS,CC] {
    def priorityHash(x: Option[X]): P // must return the deterministic priority hash of x
    def orderPriority(p1: P, p2: P): Int // returns the relative order of two priorities hashes
  }

  trait TreapImpl[X,M,P,SS <: TreapImpl[X,M,P,SS,CC], CC <: TreapContextImpl[X,M,P,SS,CC]]
    extends Treap[X,M,P,SS,CC] with SISetImpl[X,M,SS,CC]

  trait TreapContextImpl[X,M,P,SS <: TreapImpl[X,M,P,SS,CC], CC <: TreapContextImpl[X,M,P,SS,CC]]
    extends TreapContext[X,M,P,SS,CC] with SISetContextImpl[X,M,SS,CC]

  trait STreap[X,M,P] extends TreapImpl[X,M,P,STreap[X,M,P],STreapContext[X,M,P]] {
    def self = this
    type MM = STreap[X,M,P]
    type CC = STreapContext[X,M,P]
  }

  trait STreapContext[X,M,P] extends TreapContextImpl[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]{
    def self = this
    type MM = STreap[X,M,P]
    type CC = STreapContext[X,M,P]
  }

  trait STreapImpl[X,M,P] extends STreap[X,M,P] with JoinTreapImpl[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]

  trait STreapContextImpl[X,M,P] extends STreapContext[X,M,P] {  // Default Set Treap constructors
    val emptyC: MM = EmptySTreap()
    def empty = (emptyC,this)
    def create(x: X) = measure(None,Some(x),None) match {
      case None => (VLeafSTreap(x),this)
      case Some(m) => (MLeafSTreap(x,m),this)
    }
    def create(l: MM, x: Option[X], r: MM) = x match {
      case None => empty
      case Some(xx) => measure(l.measure,x,r.measure) match {
        case None => (VBinSTreap(l,xx,r),this)
        case Some(m) => (MBinSTreap(l,xx,m,r),this)
      }
    }
  }

  trait EmptyMeasure[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends Treap[X,M,P,SS,CC] {
    def measure(implicit c: CC) = None
  }

  trait LeafMeasure[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends Treap[X,M,P,SS,CC] {
    def m: M
    def measure(implicit c: CC) = Some(m)
  }

  trait EmptyTreap[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] {
    override def hashCode = 0
    def priority(implicit c: CC) = c.priorityHash(None)
    def isEmpty = true
    def some = None
    def left(implicit c: CC) = c.empty._1
    def right(implicit c: CC) = c.empty._1
    def measure(implicit c: CC) =  c.measure(None,None,None)
  }

  trait NonEmptyTreap[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends Treap[X,M,P,SS,CC]  {
    def priority(implicit c: CC) = c.priorityHash(some)
    def x: X
    def isEmpty = false
    def some = Some(x)
  }

  trait LeafTreap[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends NonEmptyTreap[X,M,P,SS,CC]  {
    override def hashCode = Hashing.jenkinsHash(x.hashCode)
    def left(implicit c: CC): SS = c.empty._1
    def right(implicit c: CC): SS = c.empty._1
  }

  trait BinTreap[X,M,P,SS <: Treap[X,M,P,SS,CC], CC <: TreapContext[X,M,P,SS,CC]] extends NonEmptyTreap[X,M,P,SS,CC]  {
    import Hashing.jenkinsHash // hashCode must be cached for efficient memoization
    override val hashCode = jenkinsHash(~l.hashCode + (jenkinsHash(x.hashCode) ^ jenkinsHash(r.hashCode) >>> 16) * 2057)

    def l: SS
    def r: SS
    def left(implicit c: CC) = l
    def right(implicit c: CC) = r
  }

  type EMS[X,M,P]= EmptyMeasure[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]
  type LMS[X,M,P]= LeafMeasure[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]

  trait NonEmptySTreap[X,M,P] extends STreapImpl[X,M,P] with NonEmptyTreap[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]
  trait LeafSTreap[X,M,P] extends NonEmptySTreap[X,M,P] with LeafTreap[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]
  trait BinSTreap[X,M,P] extends NonEmptySTreap[X,M,P] with BinTreap[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]
  case class EmptySTreap[X,M,P]() extends STreapImpl[X,M,P] with EmptyTreap[X,M,P,STreap[X,M,P],STreapContext[X,M,P]]
  case class VLeafSTreap[X,M,P](x: X) extends LeafSTreap[X,M,P] with EMS[X,M,P]
  case class MLeafSTreap[X,M,P](x: X, m: M) extends LeafSTreap[X,M,P] with LMS[X,M,P]
  case class VBinSTreap[X,M,P](l: STreap[X,M,P],x: X, r: STreap[X,M,P]) extends BinSTreap[X,M,P] with EMS[X,M,P]
  case class MBinSTreap[X,M,P](l: STreap[X,M,P],x: X,m: M, r: STreap[X,M,P]) extends BinSTreap[X,M,P] with LMS[X,M,P]

  import Hashing._

  case class DefaultSTreapContext[X](implicit o: Ordering[X], h: PriorityHasher[X]) extends STreapContextImpl[X,Any,Int] {
    def compareOrder(x1: X, x2: X): Int = o.compare(x1,x2)
    def priorityHash(x: Option[X]): Int = h.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)
  }
}