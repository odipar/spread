package spread

object OrderedSetImplementation {
  import AbstractImmutableOrderedSet._

  // OrderedISet combines a compatible SISetImpl and SISSetContextImpl into one coherent unit
  // and recasts the combination into a new Immutable Ordered Set

  trait OrderedISet[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC], SIS <: OrderedISet[X,M,SS,CC,SIS]] {

    def s: SS // returns the concrete SISet
    def c: CC // returns the concrete SISetContext

    def self: SIS

    def create(x: X):SIS = put(x)
    def create(l: SIS, e: Option[X], r: SIS) = construct(c.create(l.s,e,r.s))
    def construct(ss: (SS,CC)): SIS
    def first = s.first(c)
    def last = s.last(c)
    def first2 = s.first2(c)
    def last2 = s.last2(c)
    def isEmpty = s.isEmpty
    def some = s.some
    def measure = s.measure(c)
    def order(x1: X, x2: X) = c.compareOrder(x1,x2)
    def put(x: X): SIS = construct(s.put(x)(c))
    def put(x: Option[X]): SIS = x match {
      case None => self
      case Some(x) => put(x)
    }
    def get(x: X) = s.get(x)(c)
    def left = construct((s.left(c),c))
    def right = construct((s.right(c),c))
    def split(x: X): (SIS,Option[X],SIS) = {
      val (l,xx,r,cc) = s.split(x)(c)
      (construct((l,cc)),xx,construct((r,cc)))
    }
    def maximum(o: SIS) = construct(s.maximum(o.s)(c))
    def minimum(o: SIS) = construct(s.minimum(o.s)(c))
    def add(o: SIS) = construct(s.add(o.s)(c))
    def subtract(o: SIS) = construct(s.subtract(o.s)(c))
    def multiply(o: SIS) = construct(s.multiply(o.s)(c))

    //def difference(o: SIS) = construct(s.difference(o.s)(c))
    def sameInstanceAs(o: SIS): Boolean = s eq o.s // may return false, while == may return true
    def equalsTo(o: SIS): Boolean = sameInstanceAs(o)

    def prettyString2: String = (left.some,some,right.some) match {
      case (_,None,_) => ""
      case (None,Some(x),None) => x.toString
      case (Some(_),Some(x),None) => left.prettyString2 + " " + x.toString
      case (None,Some(x),Some(_)) => x.toString + " " + right.prettyString2
      case (_,Some(x),_) => left.prettyString2 + " " + x.toString + " " + right.prettyString2
    }
    def prettyString = "(" + prettyString2 + ")"
  }

  trait OrderedISetImpl[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]]
    extends OrderedISet[X,M,SS,CC,OrderedISetImpl[X,M,SS,CC]] {
    def self = this
    def construct(s: (SS,CC)) = if (s._1.isEmpty) EmptyOrderedISet(s._2) else LOrderedISet(s._1,s._2)
  }

  case class EmptyOrderedISet[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]](c: CC)
    extends OrderedISetImpl[X,M,SS,CC] {
    def s = c.empty._1
  }

  case class LOrderedISet[X,M,SS <: SISetImpl[X,M,SS,CC], CC <: SISetContextImpl[X,M,SS,CC]](s: SS, c: CC)
    extends OrderedISetImpl[X,M,SS,CC]
}
