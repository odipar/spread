package spread

object OrderedTreapSet {
  import AbstractImmutableOrderedSet._

  // See: http://www.cs.cmu.edu/~scandal/papers/treaps-spaa98.pdf
  trait JoinTreapImpl[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends Treap[X,M,SS,CC] {
    def join(o: SS)(implicit c: CC): SS = { // returns the join of this with c
      if (isEmpty) o
      else if (o.isEmpty) this.asInstanceOf[SS]
      else if (priority > o.priority) c.create(left,some,c.join(right,o))
      else c.create(c.join(this.asInstanceOf[SS],o.left),o.some,o.right)
    }
  }

  trait Treap[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends SISet[X,M,SS,CC] {
    def priority(implicit c: CC): Int // returns the Treap priority
  }

  trait TreapContext[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends SISetContext[X,M,SS,CC] {
    def priorityHash(x: X): Int // must return the deterministic priority hash of x
  }

  trait TreapImpl[X,M, SS <: TreapImpl[X,M,SS,CC], CC <: TreapContextImpl[X,M,SS,CC]]
    extends Treap[X,M,SS,CC] with SISetImpl[X,M,SS,CC]

  trait TreapContextImpl[X,M, SS <: TreapImpl[X,M,SS,CC], CC <: TreapContextImpl[X,M,SS,CC]]
    extends TreapContext[X,M,SS,CC] with SISetContextImpl[X,M,SS,CC]

  trait STreap[X,M] extends TreapImpl[X,M,STreap[X,M],STreapContext[X,M]] {
    def self = this
    type MM = STreap[X,M]
    type CC = STreapContext[X,M]
  }

  trait STreapContext[X,M] extends TreapContextImpl[X,M,STreap[X,M],STreapContext[X,M]]{
    def self = this
    type MM = STreap[X,M]
    type CC = STreapContext[X,M]
  }

  trait STreapImpl[X,M] extends STreap[X,M] with JoinTreapImpl[X,M,STreap[X,M],STreapContext[X,M]]

  trait STreapContextImpl[X,M] extends STreapContext[X,M] {  // Default Set Treap constructors
    val emptyC: MM = EmptySTreap()
    def empty = emptyC
    def create(x: X) = measure(None,Some(x),None) match {
      case None => VLeafSTreap(x)
      case Some(m) => MLeafSTreap(x,m)
    }
    def create(l: MM, x: Option[X], r: MM) = x match {
      case None => empty
      case Some(xx) => measure(l.measure,x,r.measure) match {
        case None => VBinSTreap(l,xx,r)
        case Some(m) => MBinSTreap(l,xx,m,r)
      }
    }
  }

  trait EmptyMeasure[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends Treap[X,M,SS,CC] {
    def measure(implicit c: CC) = None
  }

  trait LeafMeasure[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends Treap[X,M,SS,CC] {
    def m: M
    def measure(implicit c: CC) = Some(m)
  }

  trait EmptyTreap[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] {
    override def hashCode = 0
    def priority(implicit c: CC) = 0
    def isEmpty = true
    def some = None
    def left(implicit c: CC) = c.empty
    def right(implicit c: CC) = c.empty
    def measure(implicit c: CC) =  c.measure(None,None,None)
  }

  trait NonEmptyTreap[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends Treap[X,M,SS,CC]  {
    def priority(implicit c: CC): Int = c.priorityHash(x)
    def x: X
    def isEmpty = false
    def some = Some(x)
  }

  trait LeafTreap[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends NonEmptyTreap[X,M,SS,CC]  {
    override def hashCode = Hashing.jenkinsHash(x.hashCode)
    def left(implicit c: CC) = c.empty
    def right(implicit c: CC) = c.empty
  }

  trait BinTreap[X,M, SS <: Treap[X,M,SS,CC], CC <: TreapContext[X,M,SS,CC]] extends NonEmptyTreap[X,M,SS,CC]  {
    import Hashing.jenkinsHash // hashCode must be cached for efficient memoization
    override val hashCode = jenkinsHash(~l.hashCode + (jenkinsHash(x.hashCode) ^ jenkinsHash(r.hashCode) >>> 16) * 2057)

    def l: SS
    def r: SS
    def left(implicit c: CC) = l
    def right(implicit c: CC) = r
  }

  type EMS[X,M]= EmptyMeasure[X,M,STreap[X,M],STreapContext[X,M]]
  type LMS[X,M]= LeafMeasure[X,M,STreap[X,M],STreapContext[X,M]]

  trait NonEmptySTreap[X,M] extends STreapImpl[X,M] with NonEmptyTreap[X,M,STreap[X,M],STreapContext[X,M]]
  trait LeafSTreap[X,M] extends NonEmptySTreap[X,M] with LeafTreap[X,M,STreap[X,M],STreapContext[X,M]]
  trait BinSTreap[X,M] extends NonEmptySTreap[X,M] with BinTreap[X,M,STreap[X,M],STreapContext[X,M]]
  case class EmptySTreap[X,M]() extends STreapImpl[X,M] with EmptyTreap[X,M,STreap[X,M],STreapContext[X,M]]
  case class VLeafSTreap[X,M](x: X) extends LeafSTreap[X,M] with EMS[X,M]
  case class MLeafSTreap[X,M](x: X, m: M) extends LeafSTreap[X,M] with LMS[X,M]
  case class VBinSTreap[X,M](l: STreap[X,M],x: X, r: STreap[X,M]) extends BinSTreap[X,M] with EMS[X,M]
  case class MBinSTreap[X,M](l: STreap[X,M],x: X,m: M, r: STreap[X,M]) extends BinSTreap[X,M] with LMS[X,M]

  import Hashing._

  case class DefaultSTreapContext[X](implicit o: Ordering[X], h: PriorityHasher[X]) extends STreapContextImpl[X,Any] {
    def compareOrder(x1: X, x2: X): Int = o.compare(x1,x2)
    def priorityHash(x: X): Int = h.hash(x)
  }
}