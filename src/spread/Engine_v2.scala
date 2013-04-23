package spread

import spread.AbstractImmutableOrderedSet.SetOperation

object Engine_v2 {
  import OrderedSetImplementation._
  import OrderedTreapSet._
  import Hashing._

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  type SST[X] = ST[X,Any,Int]
  type MS = MultiSetExpr
  type EP = EPair
  type MST = SST[MS]
  type MMT = SST[EP]

  object MSTreapContext extends STreapContextImpl[MS,Any,Int] {
    def compareOrder(x1: MS, x2: MS): Int = MSOrdering.compare(x1,x2)
    def priorityHash(x: Option[MS]): Int = MSHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val unionV2: SetOperation[MultiSetExpr,Any,STreap[MultiSetExpr,Any,Int],STreapContext[MultiSetExpr,Any,Int]] = LeftMultiSetSumUnion() // construct once, to avoid excessive allocations

    override def union = unionV2
    override def intersect = sys.error("not yet")
    override def difference = sys.error("not yet")
  }

  val noAlternatives: MST = EmptyOrderedISet(MSTreapContext) // no measure
  val emptyMap: MMT = EmptyOrderedISet(MMTreapContext)

  object MSHashing extends PriorityHasher[MS] {
    import Hashing.jenkinsHash
    def hash(s1: MS) = s1 match {
      case EExpr => jenkinsHash(0)
      case (Alternatives(s)) => jenkinsHash(s.hashCode)
      case (EInt(i1,_)) => jenkinsHash(i1)
      case (ESymbol(s,_)) => jenkinsHash(s.hashCode*123)
      case (Reduction(v1,s1)) => jenkinsHash(hash(v1)+13)
      case (EPair(label,value,_)) => jenkinsHash(jenkinsHash(hash(label)) + 13*hash(value))
    }
  }

  object MMTreapContext extends STreapContextImpl[EP,Any,Int] {
    def compareOrder(x1: EP, x2: EP): Int = MMOrdering.compare(x1,x2)
    def priorityHash(x: Option[EP]): Int = MMHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    //val unionV2: SetOperation[EPair,Any,STreap[EPair,Any,Int],STreapContext[EPair,Any,Int]] = LeftMultiMapSumUnion() // construct once, to avoid excessive allocations

    //override def union = union
    override def intersect = sys.error("not yet")
    override def difference = sys.error("not yet")
  }

  object MMOrdering extends Ordering[EP] {
    def compare(s1: EP, s2: EP):Int = MSOrdering.compare(s1.label,s2.label)
  }

  object MMHashing extends PriorityHasher[EP] {
    import Hashing.jenkinsHash
    def hash(s1: EP) = jenkinsHash(s1.label.hashCode*5 + 213)
  }
                                                          
  object MSOrdering extends Ordering[MS] {
    def compare(s1: MS, s2: MS):Int = {
      (s1,s2) match {
        case (EExpr,EExpr) => 0
        case (EExpr,_) => -1
        case (Reduction(v1,s1),Reduction(v2,s2)) => {
          val c = compare(v1,v2)
          if (c == 0) compare(s1,s2)
          else c
        }
        case (Reduction(v1,s1),EExpr) => 1
        case (Reduction(v1,s1),_) => -1
        case (EInt(i1,_),EInt(i2,_)) => i1 - i2
        case (EInt(i1,_),EExpr) => 1
        case (EInt(i1,_),Reduction(_,_)) => 1
        case (EInt(i1,_),_) => -1
        case (EPair(l1,v1,_),EPair(l2,v2,_)) => {
          val c = compare(v1,v2)
          if (c == 0) compare(l1,l2)
          else c
        }
        case (EPair(l1,v1,_),EExpr) => 1
        case (EPair(l1,v1,_),Reduction(_,_)) => 1
        case (EPair(l1,v1,_),EInt(_,_)) => 1
        case (EPair(l1,v1,_),_) => -1
        case (ESymbol(s1,_),ESymbol(s2,_)) => s1.compareTo(s2)
        case (ESymbol(s1,_),EExpr) => 1
        case (ESymbol(s1,_),Reduction(_,_)) => 1
        case (ESymbol(s1,_),EInt(_,_)) => 1
        case (ESymbol(s1,_),EPair(_,_,_)) => 1
        case (ESymbol(s1,_),_) => -1
        case (EAdd(a11,a12,_),EAdd(a21,a22,_)) => {
          val c = compare(a11,a21)
          if (c == 0) compare(a21,a21)
          else c
        }
        case (EAdd(_,_,_),EExpr) => 1
        case (EAdd(_,_,_),Reduction(_,_)) => 1
        case (EAdd(_,_,_),EInt(_,_)) => 1
        case (EAdd(_,_,_),EPair(_,_,_)) => 1
        case (EAdd(_,_,_),_) => -1
        case (s1: MultiSetExpr, s2: MultiSetExpr) => {
          val s = s1.some.get
          val (l1,ss1,r1) = s1.split(s)
          val (l2,ss2,r2) = s2.split(s)
          val c = compare(l1,l2)
          if (c == 0) ss2 match {
            case Some(x) => compare(r2,r2)
            case _ => -1
          }
          else c
        }
      }
    }
  }

  trait Expr[E, EXPR <: Expr[E,EXPR]] {
    //def one: Int  // (the base case: later stage, MultiMapExpr)
    def occurrence: Int // (a number, later stage MultiMapExpr)
    def setOccurrence(o: Int): EXPR

    def labels: MultiSetExpr // union of all labels found in all Pairs in all subexpressions
    def bind(b: MultiMapExpr): EXPR
    def normalize: EXPR
    def reduce: EXPR
    def source: EXPR

    def asString: String
  }

  trait MultiExpr[E, EXPR <: MultiExpr[E,EXPR]] extends Expr[E,EXPR] {
    // Multi- alternatives
    def first: Option[E]
    def some: Option[E]
    def last: Option[E]
    def split(e: E): (EXPR,Option[E],EXPR)

    // Multi- operations
    def combine(o: EXPR): EXPR
  }
  trait MultiMapExpr extends MultiExpr[EPair,MultiMapExpr]
  trait MultiSetExpr extends MultiExpr[MultiSetExpr,MultiSetExpr]

  trait Atom extends MultiSetExpr {
    def normalize = this
    def source = EExpr

    def first = some
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)
  }

  case object EExpr extends Atom {
    def setOccurrence(o: Int) = this
    def occurrence = 0
    def bind(b: MultiMapExpr): MultiSetExpr = this
    def labels = this
    def reduce = this
    def combine(o: MultiSetExpr) = o
    def some = None

    def asString = ""
  }


  def createAlt(e: MultiSetExpr): MultiSetExpr = {
    if (e.some == None) e
    else Alternatives(noAlternatives.put(e))
  }
  def createAlt(e: MST): MultiSetExpr = {
    if (e.isEmpty) EExpr
    else {
      val (l,ee,r) = e.split(e.some.get)
      if (l.isEmpty && r.isEmpty) ee.get
      else Alternatives(e)
    }
  }

  case class Alternatives(s: MST) extends MultiSetExpr {
    def setOccurrence(o: Int) = sys.error("not yet")
    def occurrence = 1
    def bind(b: MultiMapExpr) = {
      var e1 = s
      var r: MultiSetExpr = EExpr
      while (e1.first != None) {
        r = r combine e1.first.get.bind(b)
        e1 = e1.split(e1.first.get)._3
      }
      r
    }
    def reduce = {
      var e1 = s
      var s2: MultiSetExpr = EExpr
      while (e1.first != None) {
        var e2 = e1.first.get.reduce
        while (e2.first != None) {
          s2 = s2 combine e2.first.get
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      Reduction(s2,this)
    }
    def normalize = this
    def source = EExpr

    def first = s.first
    def some = s.some
    def last = s.last
    def split(e: MultiSetExpr) = {
      val (h,ee,t) = s.split(e) ; (createAlt(h),ee,createAlt(t))
    }

    lazy val labels = { // TODO: optimize with Measure
      var e1 = s
      var r: MultiSetExpr = EExpr
      while (e1.first != None) {
        r = r combine e1.first.get.labels
        e1 = e1.split(e1.first.get)._3
      }
      r
    }
    def combine(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt(s union s2)
      case ss => createAlt(ss) combine this
    }

    def asString = {
     var e1 = s
      var r: String = "{"
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + ", " }
        r = r + e1.first.get.asString
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + "}"
    }
  }

  def occurrenceAsString(i: Int): String = {
    if (i == 1) ""
    else i.toString + ":"
  }

  case class EInt(e: Int, occurrence: Int) extends Atom {
    def bind(b: MultiMapExpr) = this
    def setOccurrence(o: Int) = EInt(e,o)
    def labels = EExpr
    def reduce = this
    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def some = Some(this)

    def asString = occurrenceAsString(occurrence) + e.toString
  }

  case class ESymbol(s: String, occurrence: Int) extends Atom {
    def setOccurrence(o: Int) = ESymbol(s,o)
    def bind(b: MultiMapExpr) = this

    def labels = EExpr
    def reduce = this
    def some = Some(this)
    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString = occurrenceAsString(occurrence)  + s
  }

  case class EAdd(arg1: MultiSetExpr, arg2: MultiSetExpr, occurrence: Int) extends MultiSetExpr {
    def setOccurrence(o: Int) = EAdd(arg1,arg2,o)
    def bind(b: MultiMapExpr) = EAdd(arg1.bind(b),arg2.bind(b),occurrence)

    def normalize = EAdd(arg1.normalize,arg2.normalize, occurrence)
    def reduce = {
      val ra1 = arg1.reduce
      val ra2 = arg2.reduce

      //println("ra1: " + ra1)
      //println("ra2: " + ra2)

      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        var e2 = ra2
        while (e2.first != None) {
          (e1.first.get,e2.first.get) match {
            case (EInt(i1,o1),EInt(i2,o2)) => e = e combine EInt(i1+i2,o1*o2*occurrence)
            case _ => this
          }
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      Reduction(e,Reduction(EAdd(ra1,ra2,occurrence),this))
    }
    def source = EExpr

    def first = None
    def some = Some(this)
    def last = None
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = arg1.labels combine arg2.labels

    def asString = arg1.asString + " " + occurrenceAsString(occurrence) + "+ " + arg2.asString
  }

  case class Reduction(value: MultiSetExpr, source: MultiSetExpr) extends MultiSetExpr {
    lazy val labels = value.labels combine source.labels

    def bind(b: MultiMapExpr) = source.bind(b).reduce

    def setOccurrence(o: Int) = sys.error("not yet")
    def occurrence = value.occurrence
    def reduce = value.reduce

    def normalize = this

    def first = value.first
    def some = value.some
    def last = value.last
    def split(e: MultiSetExpr) =  value.split(e)

    def combine(o: MultiSetExpr) = createAlt(this) combine o

    def asString = {
      if (MSOrdering.compare(value,source) == 0) source.asString
      else value.asString + "@(" + source.asString +")"
    }
  }
  case class EMap(m: MMT, occurrence: Int) extends MultiMapExpr {
    def source = this
    def normalize = this

    def setOccurrence(o: Int) = EMap(m, occurrence)

    def bind(b: MultiMapExpr) = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put EPair(p.label.bind(b),p.value.bind(b),p.occurrence)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r,occurrence)
    }

    def reduce = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put EPair(p.label,p.value.reduce,p.occurrence)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r,occurrence)
    }
    lazy val labels = {
      var r: MultiSetExpr = EExpr
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r combine p.labels
        mm = mm.split(mm.first.get)._3
      }
      r
    }
    def first = m.first
    def some = m .some
    def last = m.last

    def split(e: EPair) = { val(l,ee,r) = m.split(e) ; (EMap(l,occurrence),ee,EMap(r,occurrence)) }

    def combine(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm,oo) =>  EMap(m union m,occurrence*oo)
      case _ => sys.error("no")
    }

    def asString = {
      var e1 = m
      var r: String = "["
      var i = 0
      while (m.first != None) {
        if (i > 0) { r = r + ", " }
        val p: EPair = e1.first.get
        r = r + (p.label.asString+"="+p.value.asString)
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + "]"
    }
  }

  case class EPair(label: MultiSetExpr, value: MultiSetExpr, occurrence: Int) extends MultiSetExpr {
    def setOccurrence(o: Int) = EPair(label,value,o)

    def bind(b: MultiMapExpr) = EPair(label.bind(b), value.bind(b), occurrence)

    lazy val labels = label combine label.labels combine value.labels

    def reduce = Reduction(value.reduce,this)

    def normalize = this
    def source = EExpr

    def first = some
    def some = Some(this)
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o

    def asString = occurrenceAsString(occurrence) + label.asString + "'" + value.asString
  }

  import AbstractImmutableOrderedSet._
  
  // Abstract MultiSet operation
  trait MultiSetOperation[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends SetOperation[MultiSetExpr,M,SS,CC]
  trait MultiSetOperationImpl[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends MultiSetOperation[M,SS,CC] with SetOperationImpl[MultiSetExpr,M,SS,CC] {
    def mul(s: SS, m: Int)(implicit c: CC): (SS,CC) = s.some match {
      case None => (s,c)
      case Some(x) => sys.error("not yet")//c.create(mul(s.left,m),Some(x.setOccurrence(x.occurrence * m)),mul(s.right,m))
    }
  }

  // common implementation for Difference and Union operation
  trait DifferenceUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] {
    def combineLeftElement(left: MultiSetExpr): Option[MultiSetExpr] = Some(left)
    def combineRightElement(right: MultiSetExpr): Option[MultiSetExpr] = Some(right)
  }

  trait MultiSetUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: MultiSetExpr, x2: MultiSetExpr): Option[MultiSetExpr] = {
      Some(x1.setOccurrence(x1.occurrence max x2.occurrence))
    } //Some(ME(x1.value,x1.count max x2.count))
  }


  trait MultiSetSumUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC)= mul(s1,2)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: MultiSetExpr, x2: MultiSetExpr): Option[MultiSetExpr] = {
      Some(x1.setOccurrence(x1.occurrence + x2.occurrence))
    } //Some(ME(x1.value,x1.count + x2.count))
  }

  case class LeftMultiSetSumUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetSumUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetUnion()
  }

  case class RightMultiSetSumUnion[V,M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetSumUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetUnion()
  }

  case class LeftMultiSetUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetUnion()
  }

  case class RightMultiSetUnion[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetUnion()
  }

  trait MultiSetIntersect[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
    def combineLeftElement(left: MultiSetExpr): Option[MultiSetExpr] = None
    def combineRightElement(right: MultiSetExpr): Option[MultiSetExpr] = None
    def combineEqualElements(x1: MultiSetExpr, x2: MultiSetExpr): Option[MultiSetExpr] = {
      Some(x1.setOccurrence(x1.occurrence min x2.occurrence))
    } // Some(ME(x1.value,x1.count min x2.count))
  }

  case class LeftMultiSetIntersect[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC]  {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetIntersect()
  }

  case class RightMultiSetIntersect[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetIntersect()
  }

  case class MultiSetDifference[M,SS <: SISetImpl[MultiSetExpr,M,SS,CC], CC <: SISetContextImpl[MultiSetExpr,M,SS,CC]]()
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def swap: MultiSetOperation[M,SS,CC] = this
    def combineEqual(s1: SS,s2: SS)(implicit c: CC) = c.empty
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: MultiSetExpr, x2: MultiSetExpr): Option[MultiSetExpr] = {
      Some(x1.setOccurrence(x1.occurrence - x2.occurrence))
    } //Some(ME(x1.value,(x1.count - x2.count) max 0))
  }

  // Abstract MultiMap operation
  /*trait MultiMapOperation[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends SetOperation[EPair,M,SS,CC]
  trait MultiMapOperationImpl[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends MultiMapOperation[M,SS,CC] with SetOperationImpl[EPair,M,SS,CC] {
    def mul(s: SS, m: Int)(implicit c: CC): (SS,CC) = s.some match {
      case None => (s,c)
      case Some(x) => sys.error("not yet")//c.create(mul(s.left,m),Some(x.setOccurrence(x.occurrence * m)),mul(s.right,m))
    }
  }

  // common implementation for Difference and Union operation
  trait DifferenceMapUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends MultiMapOperationImpl[M,SS,CC] {
    def combineLeftElement(left: EPair): Option[EPair] = Some(left)
    def combineRightElement(right: EPair): Option[EPair] = Some(right)
  }

  trait MultiMapUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends MultiMapOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: EPair, x2: EPair): Option[EPair] = {
      Some(EPair(x1.label,x1.value combine x2.value,x1.occurrence*x2.occurrence))
    } //Some(ME(x1.value,x1.count max x2.count))
  }


  trait MultiMapSumUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends MultiMapOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC)= mul(s1,2)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: EPair, x2: EPair): Option[EPair] = {
      Some(EPair(x1.label,x1.value combine x2.value,x1.occurrence*x2.occurrence))
    } //Some(ME(x1.value,x1.count + x2.count))
  }

  case class LeftMultiMapSumUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapSumUnion[M,SS,CC] {
    lazy val swap: MultiMapOperation[M,SS,CC] = RightMultiMapUnion()
  }

  case class RightMultiMapSumUnion[V,M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapSumUnion[M,SS,CC] {
    lazy val swap: MultiMapOperation[M,SS,CC] = LeftMultiMapUnion()
  }

  case class LeftMultiMapUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapUnion[M,SS,CC] {
    lazy val swap: MultiMapOperation[M,SS,CC] = RightMultiMapUnion()
  }

  case class RightMultiMapUnion[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapUnion[M,SS,CC] {
    lazy val swap: MultiMapOperation[M,SS,CC] = LeftMultiMapUnion()
  }

  trait MultiMapIntersect[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]
    extends MultiMapOperationImpl[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
    def combineLeftElement(left: EPair): Option[EPair] = None
    def combineRightElement(right: EPair): Option[EPair] = None
    def combineEqualElements(x1: EPair, x2: EPair): Option[EPair] = {
      Some(EPair(x1.label,x1.value combine x2.value,x1.occurrence*x2.occurrence))
    } // Some(ME(x1.value,x1.count min x2.count))
  }

  case class LeftMultiMapIntersect[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapIntersect[M,SS,CC]  {
    lazy val swap: MultiMapOperation[M,SS,CC] = RightMultiMapIntersect()
  }

  case class RightMultiMapIntersect[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapIntersect[M,SS,CC] {
    lazy val swap: MultiMapOperation[M,SS,CC] = LeftMultiMapIntersect()
  }

  case class MultiMapDifference[M,SS <: SISetImpl[EPair,M,SS,CC], CC <: SISetContextImpl[EPair,M,SS,CC]]()
    extends MultiMapOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def swap: MultiMapOperation[M,SS,CC] = this
    def combineEqual(s1: SS,s2: SS)(implicit c: CC) = c.empty
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: EPair, x2: EPair): Option[EPair] = {
      Some(EPair(x1.label,x1.value combine x2.value,x1.occurrence*x2.occurrence))
    } //Some(ME(x1.value,(x1.count - x2.count) max 0))
  } */
}
