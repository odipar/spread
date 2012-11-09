package spread


object AbstractImmutableOrderedSet {

  // Abstract Immutable Ordered Set with a measurement.
  // Example measurements are size, depth, min, max, etc.
  // Treaps, AVL trees, Radix trees (and other concrete implementations), need at least implement SISet and SISetContext

  trait SISet[X,M, SS <: SISet[X,M,SS,CC], CC <: SISetContext[X,M,SS,CC]] {

    def self: SS
    def isEmpty: Boolean // returns true if empty - must be O(1)
    def measure(implicit c: CC): Option[M] // returns the measurement - must be O(1) - can be None always (no measure)

    def left(implicit c: CC): SS // returns all elements smaller than some - must be O(1)
    def some: Option[X] // returns the element between left and right  - must be O(1)
    def right(implicit c: CC): SS // returns all elements greater than some - must be O(1)

    def join(s: SS)(implicit c: CC): SS // returns the union of this with another s, given last(this) < first(s)
  }

  // SISetContext drives the construction of new SISets with empty, construct and join (building).
  // It must provide a (partial) order on SISet elements with compareOrder (ordering).
  // The accompanying SISet may be optionally measured via the measure method. (measuring).

  trait SISetContext[X,M, SS <: SISet[X,M,SS,CC], CC <: SISetContext[X,M,SS,CC]] {
    def self: CC
    implicit def c: CC = self

    def empty: SS // returns the empty Set - must be O(1)
    def create(x: X): SS // returns the Set with single x - must be O(1)
    def create(l: SS, x: Option[X], r: SS): SS // returns the Set containing l,x and r - must be O(1)
    def join(s1: SS, s2: SS): SS // returns the join of two disjoint Sets, must be O(max(log(size(this)),log(size(c))))
    def compareOrder(c: X, o: X): Int // compares two elements, to order them in the Set - must be O(1)
    def measure(l: Option[M], x: Option[X], r: Option[M]): Option[M] // returns measure on l,x and r - must be O(1)
  }

  trait SISetImpl[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]] extends SISet[X,M,SS,CC] {
    // abstract minimal implementations, based on the minimal set of abstract methods given by SISet

    def first(implicit c: CC): Option[X] = if (left.isEmpty) some ; else c.first(left) // returns the first element
    def last(implicit c: CC): Option[X] = if (right.isEmpty) some ; else c.last(right) // returns the last element

    def split(x: X)(implicit c: CC): (SS,Option[X],SS) = some match { // returns the split, given x -  O(log(size(s))
      case Some(e) => {
        val cc = c.compareOrder(e,x)
        if (cc > 0) {
          val (l,xx,r) = c.split(left,x)
          (l,xx,c.join(c.join(r,c.create(e)),right))
        }
        else if (cc < 0) {
          val (l,xx,r) = c.split(right,x)
          (c.join(c.join(left,(c.create(e))),l),xx,r)
        }
        else (left,some,right)
      }
      case None => (left,some,right)
    }

    def combine(s: SS, op: SetOperation[X,M,SS,CC])(implicit c: CC): SS = {
      // combines two Sets with bias - given the SetOperation and the (partial) order on the elements
      if (this eq s) op.combineEqual(self,s) // fast equality
      else if (isEmpty) op.combineJoin(self,s) // join if this Set is empty
      else if (s.isEmpty) op.swap.combineJoin(s,self) // join if other Set is empty
      else if (c.compareOrder(last.get,s.first.get) < 0) op.combineJoin(self,s) // join if Sets are disjoint
      else if (c.compareOrder(first.get,s.last.get) > 0) op.swap.combineJoin(s,self)
      else {
        val (l,xx,r) = s.split(some.get)
        val ms = op.combineElements(some,xx) match { // combine elements
          case None => c.empty
          case Some(x) => c.create(x)
        }
        left.combine(l,op) join ms join right.combine(r,op) // recurse on left and right Sets
      }
    }

    def union(s: SS)(implicit c: CC): SS = combine(s,c.union)
    def intersect(s: SS)(implicit c: CC): SS = combine(s,c.intersect)
    def difference(s: SS)(implicit c: CC): SS = combine(s,c.difference)

    def get(x: X)(implicit c: CC): Option[X] = split(x)._2 // override for a more efficient implementation
    def put(x: X)(implicit c: CC): (SS) = c.create(x) union self // override for a more efficient implementation
  }

  trait SISetContextImpl[X,M, SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SISetContext[X,M,SS,CC] {
    // default abstract implementations:
    // can be overwritten to implement, for example, memoization

    def first(s: SS): Option[X] = s.first // returns the first element - O(log(size(s))
    def last(s: SS): Option[X] = s.last // returns the last element - O(log(size(s))
    def split(s: SS, x: X): (SS,Option[X],SS) = s.split(x) // returns the split, given x - O(log(size(s))
    def measure(l: Option[M], x: Option[X], r: Option[M]): Option[M] = None // returns measure on l,x and r - must be O(1)
    def join(s1: SS, s2: SS): SS = s1.join(s2) // returns the join, must be O(max(log(size(this)),log(size(c))))

    val unionV: SetOperation[X,M,SS,CC] = LeftSetUnion() // construct once, to avoid excessive allocations
    val intersectV: SetOperation[X,M,SS,CC] = LeftSetIntersect()
    val differenceV: SetOperation[X,M,SS,CC] = SetDifference()

    def union = unionV
    def intersect = intersectV
    def difference = differenceV
  }

  // Abstract Set operation
  trait SetOperation[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]] {
    def swap: SetOperation[X,M,SS,CC] // must return the same SetOperation, but than with arguments swapped.
    def combineEqual(s1: SS, s2: SS)(implicit c: CC): SS // must return a (possibly empty) Set when Sets are equal
    def combineJoin(s1: SS, s2: SS)(implicit c: CC): SS  // must return a (possibly empty) Set when Sets are disjoint
    def combineElements(x1: Option[X], x2: Option[X]): Option[X] // may return either elements, or None

  }

  trait SetOperationImpl[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]] extends SetOperation[X,M,SS,CC] {
    // implementation
    def combineElements(x1: Option[X], x2: Option[X]): Option[X] = (x1,x2) match {
      case (Some(xx1),Some(xx2)) => combineEqualElements(xx1,xx2)
      case (Some(xx1),None) => combineLeftElement(xx1)
      case (None,Some(xx2)) => combineRightElement(xx2)
      case _ => None
    }
    def combineEqualElements(x1: X, x2: X): Option[X] // may return one of the elements when two elements are equal
    def combineLeftElement(left: X): Option[X] // may return the left element if only the left element is available
    def combineRightElement(right: X): Option[X] // may return the right element if only the right element is available
  }


  // common implementation for Difference and Union operation
  trait DifferenceUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SetOperationImpl[X,M,SS,CC] {
    def combineLeftElement(left: X): Option[X] = Some(left)
    def combineRightElement(right: X): Option[X] = Some(right)
  }

  trait SetUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SetOperationImpl[X,M,SS,CC] with DifferenceUnion[X,M,SS,CC] {
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
  }

  // common implementation for (left) Intersection and Union operation
  trait LeftIntersectUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SetOperationImpl[X,M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC): SS = s1
    def combineEqualElements(x1: X, x2: X): Option[X] = Some(x1)
  }

  // common implementation for (right) Intersection and Union operation
  trait RightIntersectUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SetOperationImpl[X,M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC): SS = s2
    def combineEqualElements(x1: X, x2: X): Option[X] = Some(x2)
  }

  case class LeftSetUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]()
    extends SetUnion[X,M,SS,CC] with LeftIntersectUnion[X,M,SS,CC] {
    lazy val swap: SetOperation[X,M,SS,CC] = RightSetUnion()
  }

  case class RightSetUnion[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]()
    extends SetUnion[X,M,SS,CC] with RightIntersectUnion[X,M,SS,CC] {
    lazy val swap: SetOperation[X,M,SS,CC] = LeftSetUnion()
  }

  trait SetIntersect[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends SetOperationImpl[X,M,SS,CC] {
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
    def combineLeftElement(left: X): Option[X] = None
    def combineRightElement(right: X): Option[X] = None
  }

  case class LeftSetIntersect[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]()
    extends SetIntersect[X,M,SS,CC] with LeftIntersectUnion[X,M,SS,CC] {
    lazy val swap: SetOperation[X,M,SS,CC] = RightSetIntersect()
  }

  case class RightSetIntersect[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]()
    extends SetIntersect[X,M,SS,CC] with LeftIntersectUnion[X,M,SS,CC] {
    lazy val swap: SetOperation[X,M,SS,CC] = LeftSetIntersect()
  }

  case class SetDifference[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]()
    extends SetOperationImpl[X,M,SS,CC] with DifferenceUnion[X,M,SS,CC] {
    def swap: SetOperation[X,M,SS,CC] = this
    def combineEqual(s1: SS,s2: SS)(implicit c: CC) = c.empty
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: X, x2: X): Option[X] = None
  }

    // Memoization SISetContext delegate (hash-consing) with smart constructors
  trait MemoizationContext[X,M,SS <: SISet[X,M,SS,CC], CC <: SISetContext[X,M,SS,CC]] extends SISetContext[X,M,SS,CC] {
    import java.lang.ref.WeakReference
    import collection.mutable.WeakHashMap

    def cc: CC // the delegate
    var mm = WeakHashMap.empty[SS,WeakReference[SS]]

    def m(x: SS): SS = mm.get(x) match {
      case None => { mm.put(x,new WeakReference(x)) ; x }
      case Some(xx) => val r = xx.get ; if (r == null) m(x) ; else r
    }

    override def empty: SS = cc.empty
    override def create(x: X) = m(cc.create(x)) // memoize
    override def create(l: SS, x: Option[X], r: SS) = m(cc.create(l,x,r)) // memoize
    override def compareOrder(x1: X, x2: X) = cc.compareOrder(x1,x2)
    override def measure(l: Option[M], x: Option[X], r: Option[M]): Option[M] = cc.measure(l,x,r)
  }
}