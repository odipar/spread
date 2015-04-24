package spread

object IncrementalTreapRelation {

  import spread.IncrementalMemoization._

  import Hashing._
  import scala.language.implicitConversions
  import IncrementalMemoization._

  type T[A,B] = TreapRelation[A,B]
  type PO[A,B] = PrioFactory[A,B]

  // Generic Binary TreapRelation implementation with super fast specialized Treaps
  // TODO: add optional Reducer (Monoid) functionality

  trait RelOrdering[A,B] {
    def orderingA: Ordering[A]
    def orderingB: Ordering[B]

    def compare(k1: Tuple[A,B], k2: Tuple[A,B]): Int = {
      compare(k1.asInstanceOf[FullTuple[A,B]],k2.asInstanceOf[FullTuple[A,B]])
    }
    def compare(k1: FullTuple[A,B], k2: FullTuple[A,B]): Int = {
      val c = orderingA.compare(k1.first,k2.first)
      if (c != 0) c
      else orderingB.compare(k1.second,k2.second)
    }
  }

  trait Tuple[@specialized(Int,Long,Double) +A, @specialized(Int,Long,Double) +B] {
    def firstOption: Option[A]
    def secondOption: Option[B]

    def asFull[A,B]: FullTuple[A,B] = this.asInstanceOf[FullTuple[A,B]]

    def entryString: String
    override def toString = entryString
  }

  trait OptionTuple[@specialized(Int,Long,Double) +A, @specialized(Int,Long,Double) +B] extends Tuple[A,B]

  trait SomeTuple[@specialized(Int,Long,Double) +A, @specialized(Int,Long,Double) +B] extends Tuple[A,B]

  trait NoneTuple extends OptionTuple[Nothing,Nothing] {
    def firstOption = None
    def secondOption = None
    def entryString = ""
  }

  trait LeftTuple[@specialized(Int,Long,Double) +A] extends SomeTuple[A,Nothing] {
    def first: A
    def firstOption = Some(first)
    def secondOption = None
    def entryString = first.toString + ":null"
  }

  trait RightTuple[@specialized(Int,Long,Double) +B] extends SomeTuple[Nothing,B] {
    def second: B
    def firstOption = None
    def secondOption = Some(second)
    def entryString = "null:" + second.toString
  }

  trait FullTuple[@specialized(Int,Long,Double)+A,@specialized(Int,Long,Double)+B] extends SomeTuple[A,B]
  {
    def first: A
    def second: B

    def firstOption = Some(first)
    def secondOption = Some(second)

    def entryString: String = first + ":" + second
    override def toString = entryString
  }

  case object NoneT extends NoneTuple
  case class LeftT[@specialized(Int,Long,Double) A](first: A) extends LeftTuple[A]
  case class RightT[@specialized(Int,Long,Double) B](second: B) extends RightTuple[B]
  case class FullT[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](first: A, second: B) extends FullTuple[A,B]

  trait TreeRel[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B, T <: TreeRel[A,B,T]] extends Tuple[A,B] {
    def left: T
    def middle: T
    def right: T

    def min: Tuple[A,B]
    def max: Tuple[A,B]

    def isEmpty: Boolean
    def isLeaf : Boolean
  }

  trait TreapRelation[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends TreeRel[A,B,TreapRelation[A,B]] with F0[TreapRelation[A,B]] {
    def prio: Int

    def join(x: T[A,B])(implicit p: PO[A,B]): T[A,B]
    def split(a: FullTuple[A,B])(implicit p: PO[A,B]): (T[A,B],T[A,B],T[A,B])
    def splitA(a: A)(implicit p: PO[A,B]): (T[A,B],T[A,B],T[A,B])

    def put(e: FullTuple[A,B])(implicit p: PO[A,B]): T[A,B]
    def getMin(a: A)(implicit p: PO[A,B]): Tuple[A,B]
    def getMax(a: A)(implicit p: PO[A,B]): Tuple[A,B]

    def evalValue = this
    def containsBinding = false
    def containsQuote = false
  }

  trait PrioFactory[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends RelOrdering[A,B] {
    def prio(a: A, b: B): Int

    def create(e: FullTuple[A,B]): T[A,B]
    def create(k: T[A,B]): T[A,B]
    def create(l: T[A,B], k: T[A,B], r: T[A,B]): T[A,B]

    val union: ICombinator[A,B] = ICombinator(Union())
    val difference: ICombinator[A,B] = ICombinator(Difference())
    val intersect: ICombinator[A,B] = ICombinator(Intersect())
  }

  trait TreapImpl[A,B] extends T[A,B] {
    def join(x: T[A,B])(implicit p: PO[A,B]): T[A,B] = {
      if (isEmpty) x
      else if (x.isEmpty) this
      else if (prio > x.prio) p.create(left,this,right join x)
      else p.create(this join x.left,x,x.right)
    }
    def split(t: FullTuple[A,B])(implicit p: PO[A,B]): (T[A,B],T[A,B],T[A,B]) = {
      if (isEmpty) (this,this,this)
      else {
        def cc = p.compare(this,t)
        def nt = p.create(this)

        if (cc == 0) (left,nt,right)
        else if (cc > 0) { val (l,m,r) = left.split(t) ; (l,m,r join nt join right) }
        else { val (l,m,r) = right.split(t) ; (left join nt join l,m,r) }
      }
    }
    def splitA(t: A)(implicit p: PO[A,B]): (T[A,B],T[A,B],T[A,B]) = {
      if (isEmpty) (this,this,this)
      else {
        val a: A = asFull.first
        val cc = p.orderingA.compare(a,t)
        val nt = p.create(this)

        if (cc == 0) {
          val min = getMin(a).asFull
          val max = getMax(a).asFull

          val (l,m,r) = split(min)
          val (ml,mm,mr) = r.split(max)
          val smiddle = m join ml join mm
          (l,smiddle,mr)
        }
        else if (cc > 0) { val (l,m,r) = left.splitA(t) ; (l,m,r join nt join right) }
        else { val (l,m,r) = right.splitA(t) ; (left join nt join l,m,r) }
      }
    }
    def getMin(a: A)(implicit p: PO[A,B]): Tuple[A,B] = {
      if (isEmpty) empty
      else {
        val c = p.orderingA.compare(a,asFull.first)
        if (c == 0) {
          left.getMin(a) match {
            case n: NoneTuple => FullT(asFull.first,asFull.second)
            case k: FullTuple[A,B] => k
          }
        }
        else if (c > 0) right.getMin(a)
        else left.getMin(a)
      }
    }
    def getMax(a: A)(implicit p: PO[A,B]): Tuple[A,B] = {
      if (isEmpty) empty
      else {
        val c = p.orderingA.compare(a,asFull.first)
        if (c == 0) {
          right.getMax(a) match {
            case n: NoneTuple => FullT(asFull.first,asFull.second)
            case k: FullTuple[A,B] => k
          }
        }
        else if (c > 0) right.getMax(a)
        else left.getMax(a)
      }
    }
    def put(e: FullTuple[A,B])(implicit p: PO[A,B]) = {
      val te = p.create(e)
      if (isEmpty) te
      else {
        if (p.compare(e,max) > 0) {
          this join te
        }
        else {
          if (p.compare(e,min) < 0) te join this
          else { val (l,m,r) = split(e) ; l join te join r }
        }
      }
    }
  }

  case object NTreap extends TreapImpl[Any,Any] with NoneTuple {
    def isEmpty = true
    def isLeaf = false
    def first = this
    def second = this
    def prio = 0
    def left = this
    def middle = this
    def right = this
    def min = this
    def max = this
    def size = 0
    override def toString = "'"
  }

  def empty[A,B] = NTreap.asInstanceOf[T[A,B]]

  trait FullTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends TreapImpl[A,B] with FullTuple[A,B]

  trait LeafTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends FullTreap[A,B] {
    def isEmpty = false
    def isLeaf = true

    def left = empty
    def middle = this
    def right = empty
    def min = this
    def max = this
    override def toString = entryString
  }

  trait SetTreap[A] extends TreapImpl[A,A] with FullTreap[A,A] {
    def second = first
  }

  case class LTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](first: A, second: B,prio: Int) extends LeafTreap[A,B]
  case class LSetTreap[@specialized(Int,Long,Double) A](first: A,prio: Int) extends LeafTreap[A,A] with SetTreap[A]

  trait BinaryTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends FullTreap[A,B] {
    def isEmpty = false
    def isLeaf = false

    def min = left.min
    def max = right.max
    def middle = LTreap(first,second,prio)
    override def toString = left + " " + entryString + " " + right
    override val hashCode = jh(left.hashCode ^ jh(second.hashCode - prio.hashCode) ^ jh(right.hashCode))
  }

  case class BTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](left: T[A,B],first: A, second: B,prio: Int,right: T[A,B]) extends BinaryTreap[A,B]
  case class BSetTreap[@specialized(Int,Long,Double) A](left: T[A,A],first: A,prio: Int,right: T[A,A]) extends BinaryTreap[A,A] with SetTreap[A]

  trait BinLeftTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends FullTreap[A,B] {
    def isEmpty = false
    def isLeaf = false
    def right = empty
    def min = left.min
    def max = this
    def middle = LTreap(first,second,prio)
    override def toString = left + " " + entryString
    override val hashCode = jh(left.hashCode ^ jh(second.hashCode - prio.hashCode))
  }

  case class BLeftTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](left: T[A,B],first: A, second: B,prio: Int) extends BinLeftTreap[A,B]
  case class BSetLeftTreap[@specialized(Int,Long,Double) A](left: T[A,A],first: A,prio: Int) extends BinLeftTreap[A,A] with SetTreap[A]

  trait BinRightTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends FullTreap[A,B] {
    def isEmpty = false
    def isLeaf = false

    def left = empty
    def min = this
    def max = right.max
    def middle = LTreap(first,second,prio)
    override def toString = entryString + " " + right
    override val hashCode = jh(second.hashCode ^ jh(right.hashCode - prio.hashCode))
  }

  case class BRightTreap[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](first: A, second: B,prio: Int,right: T[A,B]) extends BinRightTreap[A,B]
  case class BSetRightTreap[@specialized(Int,Long,Double) A](first: A, prio: Int,right: T[A,A]) extends BinRightTreap[A,A] with SetTreap[A]


  trait MapPrioFactory[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends PrioFactory[A,B]{
    def create(a: A, b: B): T[A,B]
  }

  trait PrioFactoryImpl[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B] extends MapPrioFactory[A,B] {
    def create(e: FullTuple[A,B]): T[A,B] = LTreap(e.first,e.second,prio(e.first,e.second))
    def create(k: T[A,B]): T[A,B] = k match {
      case s: FullTreap[A,B] => LTreap(s.first,s.second,s.prio)
    }
    def create(l: T[A,B], s: T[A,B],r: T[A,B]): T[A,B] = s match {
      case kk: FullTreap[A,B] => {
        val k = kk.first
        val v = kk.second
        val p = kk.prio
        if (l.isEmpty) {
          if (r.isEmpty) LTreap(k,v,p)
          else BRightTreap(k,v,p,r)
        }
        else if (r.isEmpty) BLeftTreap(l,k,v,p)
        else BTreap(l,k,v,p,r)
      }
    }
    def create(a: A, b: B): T[A,B] = LTreap(a,b,prio(a,b))
  }

  case class DefaultPrioFact[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](implicit a: Ordering[A], b: Ordering[B]) extends PrioFactoryImpl[A,B] {
    def prio(a: A, b: B): Int = jh(jh(a.hashCode)) ^ jh(b.hashCode)
    def orderingA = a
    def orderingB = b
  }

  case class FullTupleOrdering[A,B](implicit oo1: Ordering[A], oo2: Ordering[B]) extends Ordering[FullTuple[A,B]] {
    def o1 = oo1
    def o2 = oo2

    def compare(t1: FullTuple[A,B], t2: FullTuple[A,B]): Int = {
      val c = o1.compare(t1.first,t2.first)
      if (c != 0) c
      else o2.compare(t1.second,t2.second)
    }
  }

  def compareOption[X](t1: Option[X], t2: Option[X])(implicit o: Ordering[X]): Int = (t1,t2) match {
    case (None,None) => 0
    case (None,_) => -1
    case (_,None) => 1
    case (Some(x),Some(y)) => o.compare(x,y)
  }

  case class SomeTupleOrdering[A,B](implicit oo1: Ordering[A], oo2: Ordering[B]) extends Ordering[SomeTuple[A,B]] {
    def o1 = oo1
    def o2 = oo2

    def compare(t1: SomeTuple[A,B], t2: SomeTuple[A,B]): Int = {
      val c = compareOption(t1.firstOption,t2.firstOption)
      if (c != 0) c
      else compareOption(t1.secondOption,t2.secondOption)
    }
  }

  implicit def fullTupleOrdering[A,B](implicit o1: Ordering[A], o2: Ordering[B]): FullTupleOrdering[A,B] = FullTupleOrdering()
  implicit def someTupleOrdering[A,B](implicit o1: Ordering[A], o2: Ordering[B]): SomeTupleOrdering[A,B] = SomeTupleOrdering()

  //implicit def fromFullToSomeOrdering[A,B](implicit o: FullTupleOrdering[A,B]): SomeTupleOrdering[A,B] = SomeTupleOrdering()(o.o1,o.o2)
  //implicit def fromSomeToFullOrdering[A,B](implicit o: SomeTupleOrdering[A,B]): FullTupleOrdering[A,B] = FullTupleOrdering()(o.o1,o.o2)

  implicit def prioFact[@specialized(Int,Long,Double) A, @specialized(Int,Long,Double) B](implicit a: Ordering[A], b: Ordering[B]): PrioFactory[A,B] = {
  // TODO: optimize with a memoization cache
    DefaultPrioFact()
  }

  trait SetOperation[A,B] {
    def combineEqual(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]): T[A,B]
    def combineDisjoint(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]): T[A,B]
  }

  case class Intersect[A,B]() extends SetOperation[A,B] {
    def combineEqual(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = t1
    def combineDisjoint(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = empty
    override def toString = "intersect"
  }

  case class Union[A,B]() extends SetOperation[A,B] {
    def combineEqual(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = t1
    def combineDisjoint(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = t1 join t2
    override def toString = "union"
  }

  case class Difference[A,B]() extends SetOperation[A,B] {
    def combineEqual(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = empty
    def combineDisjoint(t1: T[A,B], t2: T[A,B])(implicit p: PrioFactory[A,B]) = t1 join t2
    override def toString = "diff"
  }

  case class ICombinator[A,B](op: SetOperation[A,B])  {
    def apply(t1: T[A,B], t2: T[A,B])(implicit pr: PrioFactory[A,B]): T[A,B] = {
      if (t1 == t2) op.combineEqual(t1,t2) // fast reference equality
      else if ((t1.isLeaf && t2.isLeaf) && (pr.compare(t1,t2) == 0)) op.combineEqual(t1,t2) // leaf equality
      else if (t1.isEmpty || t2.isEmpty) op.combineDisjoint(t1,t2)
      else if (pr.compare(t1.max,t2.min) < 0) op.combineDisjoint(t1,t2)
      else if (pr.compare(t1.min,t2.max) > 0) op.combineDisjoint(t2,t1)
      else {
        val (l,m,r) = t1.split(t2.asFull)

        val left = apply(l,t2.left)
        val middle = apply(m,t2.middle)
        val right = apply(r,t2.right)

        left join middle join right
      }
    }
    override def toString = op.toString
  }

  /*case class ICombinator[A,B](op: SetOperation[A,B])(implicit pr: PrioFactory[A,B]) extends FA2[T[A,B],T[A,B],T[A,B]] with Infix {
    val ijoin: FA2[T[A,B],T[A,B],T[A,B]] = join()

    def apply(tt1: F0[T[A,B]], tt2: F0[T[A,B]]): Expr[T[A,B]] = {
      val t1 = ~tt1
      val t2 = ~tt2

      if (t1 == t2) op.combineEqual(t1,t2) // fast reference equality
      else if ((t1.isLeaf && t2.isLeaf) && (pr.compare(t1.first,t2.first) == 0)) op.combineEqual(t1,t2) // leaf equality
      else if (t1.isEmpty || t2.isEmpty) op.combineDisjoint(t1,t2)
      else if (pr.compare(t1.last,t2.first) < 0) op.combineDisjoint(t1,t2)
      else if (pr.compare(t1.first,t2.last) > 0) op.combineDisjoint(t2,t1)
      else {
        val e = t2.entry
        val (l,m,r) = t1.split(e.first)

        val left = %(this,l,t2.left)
        val middle = %(this,m,t2.middle)
        val right = %(this,r,t2.right)

        %(ijoin,%(ijoin,left,middle),right)
      }
    }
    override def toString = op.toString
  }

  case class join[A,B]()(implicit pr: PrioFactory[A,B]) extends FA2[T[A,B],T[A,B],T[A,B]] with Infix {
    def apply(tt1: F0[T[A,B]],tt2: F0[T[A,B]]): Expr[T[A,B]] = ~tt1 join ~tt2
  } */


}
