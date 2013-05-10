package spread

import spread.AbstractImmutableOrderedSet.SetOperation

/* VERY rough prototype - just to experiment and define set of operators */

object Engine_v2 {
  import OrderedSetImplementation._
  import OrderedTreapSet._
  import Hashing._

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  type SST[X] = ST[X,Any,Int]
  type MS = MultiSetExpr
  type SP = PP
  type EP = PP
  type MST = SST[SP]
  type MMT = SST[EP]

  def order(v: MultiSetExpr): Int = v match {
    case EExpr => 0
    case (vv: Alternatives) => 1
    case (vv: ESymbol) => 2
    case (vv: Trace) => 3
    case (vv: LabeledExpr) => 4
    case (vv: EEMap) => 5
    case (vv: EInt) => 6
    case (vv: EAdd) => 7
    case (vv: EMul) => 8
    case (vv: EMax) => 9
    case (vv: EMin) => 10
    case (vv: ESub) => 11
    case (vv: EBind) => 12
    case (vv: EConcat) => 13
    case (vv: EForeach) => 14
    case (vv: PartialBinOp) => 15
    case (vv: ERed) => 16
    case (vv: EUnPack) => 17
    case (vv: EPack) => 18
    case (vv: EWipe) => 19
    case (vv: ETrace) => 20
    case (vv: EBindings) => 21
    case (vv: ETurn) => 22
    case (vv: EIota) => 23
    case (vv: ELift) => 24
    case (vv: EOrder) => 25
    case (vv: EChar) => 26
    case EFold => 27
  }

  object MSTreapContext extends STreapContextImpl[SP,Any,Int] {
    def compareOrder(x1: SP, x2: SP): Int = MSOrdering.compare(x1,x2)
    def priorityHash(x: Option[SP]): Int = MSHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val addV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetSumUnion() // construct once, to avoid excessive allocations
    val mulV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetDifference() // construct once, to avoid excessive allocations

    override def add = addV
    override def multiply = mulV
    override def maximum = maxV
    override def minimum = minV
    override def subtract = subV
  }

  val noAlternatives: MST = EmptyOrderedISet(MSTreapContext) // no measure
  val emptyMap: MMT = EmptyOrderedISet(MMTreapContext)

  object MSHashing extends PriorityHasher[SP] {
    import Hashing.jenkinsHash
    def hash(s1: SP): Int = 1

    def hash(s1: MS): Int = s1 match {
      case EExpr => jenkinsHash(0)
      case (Alternatives(s)) => jenkinsHash(s.hashCode)
      case (EInt(i1)) => jenkinsHash(jenkinsHash(i1)|i1)
      case (EAdd(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (ESub(a1,a2)) => jenkinsHash(hash(a1) - jenkinsHash(hash(a2)))
      case (EMul(a1,a2)) => jenkinsHash(hash(a1) * jenkinsHash(hash(a2)))
      case (EBind(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (EConcat(a1,a2)) => jenkinsHash(hash(a1) + hash(a2))

      //case (EForeach(a1)) => jenkinsHash(hash(a1) + 23231)
      case (ERed(a1)) => jenkinsHash(hash(a1)*201-1239)
      case (ETurn(a1)) => jenkinsHash(hash(a1)*348162-12131239)
      case (ETrace(a1)) => jenkinsHash(hash(a1)*223123-1239)
      case (EUnPack(a1)) => jenkinsHash(hash(a1)*123+123991)
      case (EOrder(a1)) => jenkinsHash(hash(a1)*122213+14)
      case (ELift(a1)) => jenkinsHash(hash(a1)*3151+141231)
      case (EBindings(a1)) => jenkinsHash(hash(a1)*312412+1232991)
      case (ESymbol(s)) => jenkinsHash(s.hashCode*123)
      case (EIota(s)) => jenkinsHash(hash(s)*34512)
      case (Trace(EMap(m))) => jenkinsHash(m.hashCode-4321)
      case (LabeledExpr(label,value)) => jenkinsHash(jenkinsHash(hash(label)) + 13*hash(value))
      case (EEMap(EMap(m))) => jenkinsHash(m.hashCode-1234)
      case (EChar(c)) => jenkinsHash(c.hashCode*12312234)
      case EFold => jenkinsHash(1234)
    }
  }

  object MMTreapContext extends STreapContextImpl[EP,Any,Int] {
    def compareOrder(x1: EP, x2: EP): Int = MMOrdering.compare(x1,x2)
    def priorityHash(x: Option[EP]): Int = MMHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val addV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetSumUnion() // construct once, to avoid excessive allocations
    val mulV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetDifference() // construct once, to avoid excessive allocations

    override def add = addV
    override def multiply = mulV
    override def maximum = maxV
    override def minimum = minV
    override def subtract = subV


    //override def difference = sys.error("not yet")
  }

  object MMOrdering extends Ordering[EP] {
    def compare(s1: EP, s2: EP):Int = MSOrdering.compare(s1.label,s2.label)
  }

  object MMHashing extends PriorityHasher[EP] {
    import Hashing.jenkinsHash
    def hash(s1: EP) = jenkinsHash(s1.label.hashCode*5 + 213)
  }

  object MSOrdering extends Ordering[MS] {
    def compareEqual(v1: MultiSetExpr, v2: MultiSetExpr): Int = (v1,v2) match {
      case (EExpr,EExpr) => 0
      case (Trace(EMap(t1)), Trace(EMap(t2))) => compareMap(t1,t2)
      case (LabeledExpr(l1,v1),LabeledExpr(l2,v2)) => {
        val c = compare(l1,l2)
        if (c == 0) compare(v1,v2)
        else c
      }
      case (EInt(i1), EInt(i2)) => i1 - i2
      case (ESymbol(s1),ESymbol(s2)) =>  s1.compareTo(s2)
      case (EChar(s1),EChar(s2)) =>  s1.compareTo(s2)
      case (EAdd(a11,a12),EAdd(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (ESub(a11,a12),ESub(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EMax(a11,a12),EMax(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EMin(a11,a12),EMin(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EBind(a11,a12),EBind(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EFold,EFold) => 0
      //case (EForeach(a11),EForeach(a21)) => compare(a11,a21)
      case (EConcat(a11,a12),EConcat(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EMul(a11,a12),EMul(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EEMap(EMap(m1)),EEMap(EMap(m2))) => {
        compareMap(m1,m2)
      }
      case (EOrder(e11),EOrder(e21)) => compare(e11,e21)
      case (PartialBinOp(e11),PartialBinOp(e21)) => compare(e11,e21)
      case (ERed(e11),ERed(e21)) => compare(e11,e21)
      case (ETrace(e11),ETrace(e21)) => compare(e11,e21)
      case (ELift(e11),ELift(e21)) => compare(e11,e21)
      case (EUnPack(e11),EUnPack(e21)) => compare(e11,e21)
      case (EForeach(e11),EForeach(e21)) => compare(e11,e21)
      case (EPack(e11),EPack(e21)) => compare(e11,e21)
      case (EWipe(e11),EWipe(e21)) => compare(e11,e21)
      case (EIota(e11),EIota(e21)) => compare(e11,e21)
      case (ETurn(e11),ETurn(e21)) => compare(e11,e21)
      case (EBindings(e11),EBindings(e21)) => compare(e11,e21)
      case (Alternatives(a1), Alternatives(a2)) => {
        compareAlt(a1,a2)
      }
    }
    def compare(s1: MS, s2: MS):Int = {
      val o1 = order(s1)
      val o2 = order(s2)
      if (o1 == o2) compareEqual(s1,s2)
      else o1 - o2
    }

    def compareMap(m1: MMT, m2: MMT): Int = {
      (m1.some, m2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = m1.first.get
          val f2 = m2.first.get
          val c = compare(f1,f2)
          if (c != 0)  c
          else compareMap(m1.split(f1)._3,m2.split(f2)._3)
        }
      }
    }
    def compare(p1: PP, p2: PP): Int = (p1,p2) match {
      case(t1: Trace,t2: Trace) => compare(t1,t2)
      case(t1: Trace, _) => compare(t1,p2.target)
      case(_,t2: Trace) => compare(p1.target,t2)
      case _ => compare(p1.target,p2.target)
    }
    def compareAlt(a1: MST, a2: MST): Int = {
      (a1.some, a2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = a1.first.get
          val f2 = a2.first.get
          val c = compare(f1,f2)
          if (c != 0)  c
          else compareAlt(a1.split(f1)._3,a2.split(f2)._3)
        }
      }
    }
  }

  trait Expr[E, EXPR <: Expr[E,EXPR]] {
    //def one: Int  // (the base case: later stage, MultiMapExpr)

    def bindings: MultiMapExpr // maximum of all bindings found in all Pairs in all subexpressions
    def labels: MultiSetExpr // maximum of all (unboud) labels found in all Pairs in all subexpressions
    def bind(b: MapPair): EXPR
    def wipe: EXPR
    def value: EXPR
    def reduce: EXPR

    def asString: String
    def topString = asString

  }

  trait PP {

    def labels: MultiSetExpr

    def label: MultiSetExpr
    def target: MultiSetExpr

    def bindings: MultiMapExpr // maximum of all bindings found in all Pairs in all subexpressions
    def bindPair(b: MapPair): PP
    def wipePair: PP
    def reducePair: PP

    def add(o: PP): Option[PP]
    def mul(o: PP): Option[PP]
    def max(o: PP): Option[PP]
    def min(o: PP): Option[PP]
    def subtract(o: PP): Option[PP]

    def asString: String
  }

  trait MultiExpr[E, EXPR <: MultiExpr[E,EXPR]] extends Expr[E,EXPR] {
    // Multi- alternatives
    def first: Option[E]
    def some: Option[E]
    def last: Option[E]
    def split(e: E): (EXPR,Option[E],EXPR)

    // Multi- operations
    def multiAdd(o: EXPR): EXPR
    def multiMul(o: EXPR): EXPR
    def multiMax(o: EXPR): EXPR
    def multiMin(o: EXPR): EXPR
    def multiSubtract(o: EXPR): EXPR
  }

  trait MultiMapExpr extends MultiExpr[PP,MultiMapExpr] {
    def isEmpty: Boolean
    def contains(p: PP): Boolean
    def get(p: PP): Option[PP]
    def put(p: PP): MultiMapExpr
  }

  trait MultiSetExpr extends MultiExpr[PP,MultiSetExpr] with SetPair {
    def isEmpty: Boolean
  }

  trait MultiSetExprI extends MultiSetExpr {
    def value = target
    def label = EInt(1)
    def reducePair = reduce
    def wipePair = wipe
    def bindPair(b: MapPair) = bind(b)

    def createP(p: SetPair): Option[PP] = {
      if (MSOrdering.compare(p.label,EInt(0))==0) None
      else if (p.target.isEmpty) None
      else Some(p)
    }
    def add(o: PP): Option[PP] = createP(createSetP(EAdd(label,o.label).reduce.wipe,this))
    def mul(o: PP): Option[PP] = createP(createSetP(EMul(label,o.label).reduce.wipe,this))
    def max(o: PP): Option[PP] = createP(createSetP(EMax(label,o.label).reduce.wipe,this))
    def min(o: PP): Option[PP] = createP(createSetP(EMin(label,o.label).reduce.wipe,this))
    def subtract(o: PP): Option[PP] = createP(createSetP(ESub(label,o.label).reduce.wipe,this))
  }

  trait Atom extends  MultiSetExprI {
    def isEmpty = false
    def target: MultiSetExpr = this
    def split(e: PP): (MultiSetExpr,Option[PP],MultiSetExpr) = (EExpr,some,EExpr)
    def first: Option[PP] = some
    def some: Option[PP] = Some(this)
    def last: Option[PP] = some

    def multiAdd(o: MultiSetExpr) =  createAlt(this) multiAdd o
    def multiMul(o: MultiSetExpr) = createAlt(this) multiMul o
    def multiMax(o: MultiSetExpr) = createAlt(this) multiMax o
    def multiMin(o: MultiSetExpr) =  createAlt(this) multiMin o
    def multiSubtract(o: MultiSetExpr) = createAlt(this) multiSubtract o

  }

  trait Op extends Atom {
  }

  trait UnaOp extends Op {
    lazy val labels = arg1.labels
    def wipe: UnaOp
    def bind(b: MapPair): UnaOp
    def arg1: MultiSetExpr
    def rebuild(arg1: MultiSetExpr): UnaOp

    def combineInt(i: Int): MultiSetExpr
    def combineMap(m: MultiMapExpr): MultiSetExpr
    def combineOther(m: MultiSetExpr): MultiSetExpr = rebuild(m)

    def reduce= {
      val ra1 = arg1.reduce
      ra1.value match {
        case (EInt(i1)) => red(red(this,rebuild(ra1)),combineInt(i1))
        case (EEMap(m1)) =>  red(red(this,rebuild(ra1)),combineMap(m1))
        case (Alternatives(a1)) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          while(a.first != None) {
            val p = a.first.get.reducePair
            e = e multiAdd createAlt(createSetP(p.label,rebuild(p.target).reduce))
            a = a.split(a.first.get)._3
          }
          red(red(this,rebuild(ra1)),createAlt3(e))
        }

        case a => {
          red(red(this,rebuild(ra1)),combineOther(ra1))
        }
      }
    }
  }

  def createEM(m: MultiMapExpr): MultiSetExpr = {
    if (m.isEmpty) EInt(0)
    else EEMap(m)
  }

  trait BinOp extends Op {
    lazy val labels = arg1.labels multiMax arg2.labels
    def combineInt(a1: Int, a2: Int): MultiSetExpr
    def combineMap(a1: MultiMapExpr, a2: MultiMapExpr): MultiSetExpr
    def combineOther(a1: MultiSetExpr, a2: MultiSetExpr): MultiSetExpr = rebuild(a1.value,a2.value)
    def fold(m: MultiMapExpr, b: BinOp): MultiSetExpr = {
      if (m.some != None) {
        val (l,e,r) = m.split(m.some.get)

        if (l.isEmpty) {
          if (r.isEmpty) e.get.target
          else rebuild(e.get.target,fold(r,b))
        }
        else {
          if (r.isEmpty) {
            if (MSOrdering.compare(l.first.get,l.last.get) == 0) {
              b.rebuild(l.first.get.target,e.get.target)
            }
            else rebuild(fold(l,b),e.get.target)
          }
          else {
            b.rebuild(rebuild(fold(l,b),e.get.target),fold(r,b))
          }
        }
      }
      else EExpr
    }
    def reduce = reduce2(arg1.reduce,arg2.reduce)

    def reduce2(ra1: MultiSetExpr, ra2: MultiSetExpr): MultiSetExpr = {

      (ra1.value,ra2.value) match {
        case (EInt(i1),EInt(i2)) => red(red(this,rebuild(ra1,ra2)),combineInt(i1,i2))
        case (EEMap(m1), EEMap(m2)) =>  red(red(this,rebuild(ra1,ra2)),combineMap(m1,m2))
        case (EInt(i1), EEMap(m2)) => red(red(this,rebuild(ra1,ra2)),combineMap(makeMap(i1),m2))
        case (EEMap(m1), EInt(i2)) => red(red(this,rebuild(ra1,ra2)),combineMap(m1,makeMap(i2)))
        case (EEMap(m1), EFold) => red(red(this,rebuild(ra1,ra2)),fold(m1,this).reduce.wipe)
        case (Alternatives(a1),Alternatives(a2)) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          while( a.first != None) {
            val p1 = a.first.get.reducePair
            var aa = a2
            while (aa.first != None) {
              val p2 = aa.first.get.reducePair
              val m = EMul(p1.label,p2.label).reduce.wipe
              e = e multiAdd createAlt(createSetP(m,rebuild(p1.target,p2.target).reduce.target))
              aa = aa.split(aa.first.get)._3
            }
            a = a.split(a.first.get)._3
          }
          red(red(this,rebuild(ra1,ra2)),createAlt3(e))
        }
        case (Alternatives(a1),e2) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          while(a.first != None) {
            val p = a.first.get.reducePair
            e = e multiAdd createAlt(createSetP(p.label,rebuild(p.target,e2).reduce.target))
            a = a.split(a.first.get)._3
          }
          red(red(this,rebuild(ra1,ra2)),createAlt3(e))
        }
        case (e2,Alternatives(a1)) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          while(a.first != None) {
            val p = a.first.get.reducePair
            e = e multiAdd createAlt(createSetP(p.label,rebuild(e2,p.target).reduce.target))
            a = a.split(a.first.get)._3
          }
          red(red(this,rebuild(ra1,ra2)),createAlt3(e))
        }
        case (f1,f2) => {
          red(red(this,rebuild(ra1,ra2)), combineOther(ra1,ra2))
        }
      }
    }
    def wipe: BinOp
    def bind(b: MapPair): BinOp
    def arg1: MultiSetExpr
    def arg2: MultiSetExpr
    def rebuild(arg1: MultiSetExpr, arg2: MultiSetExpr): BinOp
  }

  case object EExpr extends MultiSetExprI {
    override def labels = EExpr
    def isEmpty = true
    def target = this
    def split(e: PP) = (this,Some(this),this)
    def wipe = this
    def bind(b: MapPair): MultiSetExpr = this
    def bindings = EMap(emptyMap)
    def reduce = this
    def multiAdd(o: MultiSetExpr) = o
    def multiMul(o: MultiSetExpr) = this
    def multiMax(o: MultiSetExpr) = o
    def multiMin(o: MultiSetExpr) = this
    def multiSubtract(o: MultiSetExpr) = sys.error("no!")

    override def first: Option[SetPair] = None
    override def some: Option[SetPair] = None
    override def last: Option[SetPair] = None

    def asString = "EMPTY"
  }

  def createMap(e: MapPair): MultiMapExpr = EMap(emptyMap.put(e))
  def createAlt(e: SetPair): MultiSetExpr = Alternatives(noAlternatives.put(e))
  def createAlt3(e: MultiSetExpr): MultiSetExpr = {
    if (e.isEmpty) e
    val (l,ee,r) = e.split(e.some.get)
    if (l.isEmpty && r.isEmpty) {
      if (MSOrdering.compare(ee.get.label,EInt(1)) == 0) {  ee.get.target }
      else e
    }
    else e
  }

  /*def createAlt2(e: MST): MultiSetExpr = {
    if (e.isEmpty) EExpr
    else Alternatives(e)
  } */

  def createAlt2(e: MST): MultiSetExpr = {
    if (e.isEmpty) EExpr
    else {
      val (l,ee,r) = e.split(e.some.get)
      if (l.isEmpty && r.isEmpty) {
        if (MSOrdering.compare(ee.get.label,EInt(1)) == 0) ee.get.target
        else Alternatives(e)
      }
      else Alternatives(e)
    }
    Alternatives(e)
  }

  def domul(a1: MultiSetExpr, a2: MultiSetExpr): MultiSetExpr =
  {
    a1 match {

      case (EInt(i1)) => {
        if (i1 == 1) a2
        else {
          a2 match {
            case (EInt(i2)) => {
              if (i2 == 1) a1
              else EMul(a1,a2)
            }
          }
        }
      }
      case _ => {
        a2 match {
          case (EInt(i2)) => {
            if (i2 == 1) a1
            else EMul(a1,a2)
          }
        }
      }
    }
  }

  case class Alternatives(s: MST) extends MultiSetExprI {
    def isEmpty = s.isEmpty
    def target = this
    def bind(b: MapPair) = {
      var e1 = s
      var r = noAlternatives
      while (e1.first != None) {
        val p: PP = e1.first.get.bindPair(b)
        r = r put createSetP(p.label,p.target)
        e1 = e1.split(e1.first.get)._3
      }
      createAlt2(r)
    }

    def reduce = {
      var e1 = s
      var r = noAlternatives
      while (e1.first != None) {
        val p: PP = e1.first.get.reducePair
        r = r put p
        e1 = e1.split(e1.first.get)._3
      }
      red(this,createAlt2(r))
    }
    def wipe = {
      var e1 = s
      var r = noAlternatives
      while (e1.first != None) {
        val p: PP = e1.first.get.wipePair
        r = r put createSetP(p.label,p.target)
        e1 = e1.split(e1.first.get)._3
      }
      createAlt2(r)
    }

    override def first = s.first.asInstanceOf[Option[SetPair]]
    override def some = s.some.asInstanceOf[Option[SetPair]]
    override def last = s.last.asInstanceOf[Option[SetPair]]

    override def split(e: PP): (MultiSetExpr,Option[PP],MultiSetExpr) = { val (l,ee,r) = s.split(e) ; (createAlt2(l),ee,createAlt2(r))}

    lazy val bindings = { // TODO: optimize with Measure
    var e1 = s
      var r: MultiMapExpr = EMap(emptyMap)
      while (e1.first != None) {
        r = r multiAdd e1.first.get.bindings
        e1 = e1.split(e1.first.get)._3
      }
      r
    }
    lazy val labels = { // TODO: optimize with Measure
    var e1 = s
      var r: MultiSetExpr = EExpr
      while (e1.first != None) {
        r = r multiAdd e1.first.get.labels
        e1 = e1.split(e1.first.get)._3
      }
      r
    }
    def multiAdd(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt2(s add s2)
      case ss => {this multiAdd createAlt(ss.asInstanceOf[SetPair])}
    }
    def multiMul(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt2(s multiply s2)
      case ss => {this multiMul createAlt(ss.asInstanceOf[SetPair])}
    }
    def multiMax(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt2(s maximum s2)
      case ss => {this multiMax createAlt(ss.asInstanceOf[SetPair])}
    }
    def multiMin(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt2(s minimum s2)
      case ss => {this multiMin createAlt(ss.asInstanceOf[SetPair])}
    }
    def multiSubtract(s2: MultiSetExpr) =  s2 match {
      case Alternatives(s2: MST) => createAlt2(s subtract s2)
      case ss => {this multiSubtract createAlt(ss.asInstanceOf[SetPair])}
    }
    def asString = {
      var e1 = s
      var r: String = "{"
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + "," }
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

  case class EInt(e: Int) extends Atom {
    override def labels = EExpr
    def wipe = this
    def bind(b: MapPair) = this
    def bindings = EMap(emptyMap)
    def reduce = this

    def asString = { if (e >= 0) e.toString ; else "_" + (-e) }
  }

  case class EChar(s: Char) extends Atom {
    override def labels = EExpr
    def wipe = this
    def bind(b: MapPair) = this

    def bindings = EMap(emptyMap)
    def reduce = this

    def asString = "\"" + s + "\""
  }


  case class ESymbol(s: String) extends Atom {
    override def labels = EExpr
    def wipe = this
    def bind(b: MapPair) = this

    def bindings = EMap(emptyMap)
    def reduce = this

    def asString = s
  }

  case class EOrder(binop: BinOp) extends BinOp {
    // [1,2] 2 +
    def arg1 = binop.arg1
    def arg2 = binop.arg2

    def rebuild(arg1: MultiSetExpr, arg2: MultiSetExpr) = EOrder(binop.rebuild(arg1,arg2))
    def wipe = EOrder(binop.wipe)
    def bind(b: MapPair) = {
      EOrder(binop.bind(b))
    }

    def combineInt(i1: Int, i2: Int) = EInt(i1)
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1)


    lazy val bindings = binop.bindings
    override def reduce = red(this,binop.rebuild(arg2,arg1).reduce)

    def asString =  binop.asString +"\\"
  }

  case class PartialBinOp(binop: BinOp) extends UnaOp {
     // [1,2] 2 +
    def arg1 = binop.arg1
    def rebuild(arg1: MultiSetExpr) = PartialBinOp(binop.rebuild(arg1,binop.arg2))
    def wipe = PartialBinOp(binop.wipe)
    def bind(b: MapPair): UnaOp = {
      PartialBinOp(binop.bind(b))
    }

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = createEM(m)

    lazy val bindings = binop.bindings
    override def reduce = binop.reduce

    def asString =  binop.asString
  }

  case class EForeach(u: UnaOp) extends UnaOp {
    def arg1 = u.arg1
    def rebuild(arg1: MultiSetExpr) = EForeach(u.rebuild(arg1))
    def wipe = EForeach(u.wipe)
    def bind(b: MapPair) = {
      EForeach(u.bind(b))
    }

    def bindings = arg1.bindings

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = {
      var mm = emptyMap
      var im = m
      while (im.first != None) {
        val p = im.first.get
        mm = mm put MMapPair(p.label,u.rebuild(p.target).reduce.value)
        im = im.split(im.first.get)._3
      }
      createEM(EMap(mm))
    }

    def asString =  u.asString + "@"
  }

  case class EBindings(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = EBindings(arg1)
    def wipe = EBindings(arg1.wipe)
    def bind(b: MapPair) = EBindings(arg1.bind(b))

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = sys.error("no")

    override def reduce = {
      val a = arg1
      red(this,a.labels)
    }

    def bindings = arg1.bindings

    def asString =  arg1.asString + " /b"
  }

  case class ELift(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = ELift(arg1)
    def wipe = ELift(arg1.wipe)
    def bind(b: MapPair) = ELift(arg1.bind(b))

    def combineInt(i: Int) = createEM(createMap(MMapPair(EInt(0),EInt(i))))
    def combineMap(m: MultiMapExpr) = {
      var mm = m
      var nm = emptyMap
      var i = 0
      while (mm.first != None) {
        val e = EEMap(createMap(mm.first.get.asInstanceOf[MapPair]))
        nm = nm put MMapPair(EInt(i),e)
        mm = mm.split(mm.first.get)._3
        i = i +1
      }
      createEM(EMap(nm))
    }
    /*override def combineOther(m: MultiSetExpr) = {
      EEMap(createMap(MMapPair(EInt(0),m)))
    } */

    def bindings = arg1.bindings

    def asString =  arg1.asString + " ^"
  }

  case class EIota(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = EIota(arg1)
    def wipe = EIota(arg1.wipe)
    def bind(b: MapPair) = EIota(arg1.bind(b))

    def combineInt(i: Int) = {
      var m = emptyMap
      var ii = 0
      while (ii < i) {
        val in = EInt(ii)
        m = m put MMapPair(in,in)
        ii = ii + 1
      }
      createEM(EMap(m))
    }
    def combineMap(m: MultiMapExpr) = sys.error("not yet")
    //override def combineOther(m: MultiSetExpr) = m.value.reduce

    def bindings = arg1.bindings

    def asString =  arg1.asString + " ~"
  }

  case class ERed(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = ERed(arg1)
    def wipe = ERed(arg1.wipe)
    def bind(b: MapPair) = ERed(arg1.bind(b))

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = createEM(m.reduce)
    override def combineOther(m: MultiSetExpr): MultiSetExpr = m match {
      case t: Trace => t.value.reduce
      case _ => rebuild(m)
    }
    def bindings = arg1.bindings

    def asString =  arg1.asString + " $"
  }

  case object EFold extends Atom {
    def labels = EExpr
    def rebuild(arg1: MultiSetExpr) = this
    def bind(b: MapPair) = this

    def wipe = this
    def MapPair(b: MultiMapExpr) = this
    def reduce = this
    def bindings = EMap(emptyMap)

    def asString = "."
  }

  case class EUnPack(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = EUnPack(arg1)

    def wipe = EUnPack(arg1.wipe)
    def bind(b: MapPair) = EUnPack(arg1.bind(b))

    def bindings = arg1.bindings
    def combineInt(i: Int) = combineMap(makeMap(i))
    def combineMap(m: MultiMapExpr) = {
      var mm = noAlternatives
      var im = m
      while (im.first != None) {
        val p = im.first.get
        val np = LabeledExpr(p.label,p.target)
        mm = mm put np
        im = im.split(im.first.get)._3
      }
      createAlt2(mm)
    }

    def asString = arg1.asString + " >"
  }

  case class EPack(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = EPack(arg1)

    def wipe = EPack(arg1.wipe)
    def bind(b: MapPair) = EPack(arg1.bind(b))

    def bindings = arg1.bindings
    def combineInt(i: Int) = createEM(createMap(MMapPair(EInt(0),EInt(i))))
    def combineMap(m: MultiMapExpr) = createEM(createMap(MMapPair(EInt(0),createEM(m))))
    override def reduce= {
      val ra1 = arg1.reduce

      ra1.value match {
        case (EInt(i1)) => red(red(this,rebuild(ra1)),combineInt(i1))
        case (EEMap(m1)) =>  red(red(this,rebuild(ra1)),combineMap(m1))
        case (Alternatives(a1)) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          var m = emptyMap
          var i = 0
          while(a.first != None) {
            val p = a.first.get.asInstanceOf[SetPair]
            val alt = p match {
              case s: SSetPair => createAlt(s)
              case _ => p.target
            }
            val np = MMapPair(EInt(i),alt)
            m = m put np
            a = a.split(a.first.get)._3
            i = i + 1
          }
          red(red(this,rebuild(ra1)),createEM(EMap(m)))
        }

        case a => {
          red(red(this,rebuild(ra1)),combineOther(ra1))
        }
      }
    }
    def asString = arg1.asString + " <"
  }

  case class ETrace(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = ETrace(arg1)

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = sys.error("no use")

    override def reduce = {
      val r = arg1.reduce
      (r) match {
        case Trace(m) => {
          val k = createEM(m)
          red(this,red(rebuild(r),k))
        }
        case _ => red(this, rebuild(r))
      }
    }

    def wipe = ETrace(arg1.wipe)
    def bind(b: MapPair) = ETrace(arg1.bind(b))

    def bindings = arg1.bindings

    def asString = arg1.asString + " /t"
  }

  case class EWipe(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = EWipe(arg1)

    def wipe = EWipe(arg1.wipe)
    def bind(b: MapPair) = EWipe(arg1.bind(b))

    def bindings = arg1.bindings

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = createEM(m.wipe)
    override def combineOther(m: MultiSetExpr): MultiSetExpr = m match {
      case t: Trace => t.wipe
      case _ => rebuild(m)
    }

    override def reduce= {
      val ra1 = arg1.reduce

      ra1.value match {
        case (EInt(i1)) => red(red(this,rebuild(ra1)),combineInt(i1))
        case (EEMap(m1)) =>  red(red(this,rebuild(ra1)),combineMap(m1))
        case (Alternatives(a1)) => {
          var a = a1
          var e: MultiSetExpr = EExpr
          while(a.first != None) {
            val p = a.first.get.wipePair
            e = e multiAdd createAlt(createSetP(p.label,p.target))
            a = a.split(a.first.get)._3
          }
          red(red(this,rebuild(ra1)),createAlt3(e))
        }

        case a => {
          red(red(this,rebuild(ra1)),combineOther(ra1))
        }
      }
    }

    def asString = arg1.asString + " #"
  }

  case class ETurn(arg1: MultiSetExpr) extends UnaOp {
    def rebuild(arg1: MultiSetExpr) = ETurn(arg1)

    def wipe = ETurn(arg1.wipe)
    def bind(b: MapPair) = ETurn(arg1.bind(b))

    def bindings = arg1.bindings

    def combineInt(i: Int) = EInt(i)
    def combineMap(m: MultiMapExpr) = {
      var nm = emptyMap
      var mm = m
      while (mm.first != None) {
        val mpair = mm.first.get
        var ns = mpair.target
        ns match {
          case s: Alternatives => {
            while (ns.first != None) {
              val spair = ns.first.get
              val alt = createAlt(createSetP(spair.label,mpair.label))
              val nspair = createSetP(spair.label,mpair.label)
              val npair = MMapPair(spair.target,alt)
              nm = nm put npair
              ns = ns.split(ns.first.get)._3
            }
          }
          case _ => {
            val npair = MMapPair(ns,mpair.label)
            nm = nm put npair
          }
        }

        mm = mm.split(mm.first.get)._3
      }
      createEM(EMap(nm))
    }
    def asString = arg1.asString + " %"
  }

  case class EEMap(e: MultiMapExpr) extends Atom {
    def wipe = createEM(e.wipe)
    def bind(b: MapPair) = createEM(e.bind(b))
    override def labels = e.labels
    def bindings = e.bindings
    def reduceMap = createEM(e.reduce)
    def reduce = this

    def asString = e.asString

    override def wipePair = wipe
    override def reducePair = reduce
    override def bindPair(b: MapPair) = this
  }

  def createTrace(mm: MultiMapExpr): MultiSetExpr = {
    val (l,ee,r) = mm.split(mm.some.get)
    if (l.isEmpty && r.isEmpty) ee.get.target
    else Trace(mm)
  }

  def red(source: MultiSetExpr, reduction: MultiSetExpr): MultiSetExpr = {
    if (MSOrdering.compare(source,reduction) == 0) source
    else source match {
      case Trace(trace) => {
        val p = trace.last.get
        val l = EAdd(p.label,EInt(1)).reduce.wipe
        val m: MultiMapExpr = trace.put(MMapPair(l,reduction))
        Trace(m)
      }
      case _ =>
      {
        var m = emptyMap put MMapPair(EInt(0),source) put MMapPair(EInt(1),reduction)
        createTrace(EMap(m))
      }
    }
  }

  case class EAdd(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EAdd(arg1,arg2)

    def bind(b: MapPair) = {
      println("arg1: " + arg1.asString)
      println("arg2: " + arg2.asString)
      EAdd(arg1.bind(b),arg2.bind(b))
    }

    def wipe = EAdd(arg1.wipe,arg2.wipe)

    def combineInt(i1: Int, i2: Int) = EInt(i1 + i2)
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1 multiAdd m2)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " +"
  }

  case class ESub(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = ESub(arg1,arg2)

    def bind(b: MapPair) = ESub(arg1.bind(b),arg2.bind(b))

    def wipe = ESub(arg1.wipe,arg2.wipe)

    def combineInt(i1: Int, i2: Int) = EInt(i1 - i2)
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1 multiSubtract m2)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " -"
  }


  case class EMin(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EMin(arg1,arg2)

    def bind(b: MapPair) = EMin(arg1.bind(b),arg2.bind(b))

    def wipe = EMin(arg1.wipe,arg2.wipe)

    def combineInt(i1: Int, i2: Int) = EInt(i1.min(i2))
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1 multiMin m2)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " &"
  }

  case class EMax(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EMax(arg1,arg2)

    def bind(b: MapPair) = EMax(arg1.bind(b),arg2.bind(b))

    def wipe = EMax(arg1.wipe,arg2.wipe)

    def combineInt(i1: Int, i2: Int) = EInt(i1.max(i2))
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1 multiMax m2)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " |"
  }


  def makeSeq(m: MultiMapExpr, i: Int): (Int, MultiMapExpr) = {
    var mm = emptyMap
    var lm = m
    var ii = i
    while (lm.first != None) {
      val p = lm.first.get
      val np = MMapPair(EInt(ii),p.target)
      mm = mm put np
      lm = lm.split(lm.first.get)._3
      ii = ii + 1
    }
    (ii,EMap(mm))
  }

  def makeMap(i: Int): MultiMapExpr = {
    if (i == 0) EMap(emptyMap)
    else {
      val sp = createSetP(EInt(i),EInt(0))
      val mp = MMapPair(EInt(0),createAlt(sp))
      val m = emptyMap put mp
      EMap(m)
    }
  }

  case class EConcat(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EConcat(arg1,arg2)

    def bind(b: MapPair) = EConcat(arg1.bind(b),arg2.bind(b))

    def combineInt(i1: Int, i2: Int) = combineMap(makeMap(i1),makeMap(i2))
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = {
      val (i,s1) = makeSeq(m1,0)
      val (i2,s2) = makeSeq(m2,i)
      createEM(s1 multiAdd s2)
    }

    def wipe = EConcat(arg1.wipe,arg2.wipe)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = subexpr3(arg1) + ";" + subexpr3(arg2)
  }

  case class LabeledExpr(l: MultiSetExpr, v: MultiSetExpr) extends Atom {
    override def labels = l.labels multiMax v.labels multiMax l
    def bind(b: MapPair) = {
      val mp = MMapPair(l,v)
      if (MMOrdering.compare(b,mp) == 0) {
        LabeledExpr(l,b.target)
      }
      else LabeledExpr(l.bind(b), v.bind(b))
    }

    def wipe = LabeledExpr(l.wipe,v.wipe)
    def reduce = {
      if (v.some == None) {
        red(this,LabeledExpr(l.reduce,v))
      }
      else {
        val vr = v.reduce
        red(red(this,LabeledExpr(l.reduce,vr)),vr)
      }
    }

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = {
      val rest = l.bindings multiMax v.bindings
      if (v.first != None) createMap(MMapPair(l,v)) multiAdd rest
     else rest
    }

    def asString = {
      if (v.some == None) subexpr2(l) + "'"
      else subexpr2(l) + "'" + subexpr2(v)
    }
  }

  case class EMul(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EMul(arg1,arg2)
    def bind(b: MapPair) = EMul(arg1.bind(b),arg2.bind(b))

    def wipe = EMul(arg1.wipe,arg2.wipe)
    def combineInt(i1: Int, i2: Int) = EInt(i1 * i2)
    def combineMap(m1: MultiMapExpr, m2: MultiMapExpr) = createEM(m1 multiMul m2)

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " *"
  }


  def iterate(m1: MultiMapExpr, m2: MultiMapExpr): MultiMapExpr = {
    var m4 = m1
    var e2 = m2
    while (e2.first != None) {
      var m3: MultiMapExpr = EMap(emptyMap)
      val p = e2.first.get
      val l = p.label
      val a = p.target
      var e3 = a
      while (e3.first != None) {
        val v = e3.first.get
        val np = MMapPair(l,createAlt3(createAlt(v.asInstanceOf[SetPair])))
        m3 = m3 multiAdd m4.bind(np)
        e3 = e3.split(e3.first.get)._3
      }
      m4 = m3
      e2 = e2.split(e2.first.get)._3
    }
    m4
  }
  case class EBind(arg1: MultiSetExpr, arg2: MultiSetExpr) extends BinOp {
    def rebuild(arg1: MultiSetExpr,arg2: MultiSetExpr) = EBind(arg1,arg2)

    def bind(b: MapPair) = EBind(arg1.bind(b),arg2.bind(b))

    def combineInt(i1: Int, i2: Int) = EInt(i1)
    def combineMap(a1: MultiMapExpr, a2: MultiMapExpr) = createEM(iterate(a1,a2))
    override def combineOther(a1: MultiSetExpr, a2: MultiSetExpr) = {
      a2.value match {
        case EEMap(m) => combineBind(a1,m)
        case _ => rebuild(a1,a2)
      }
    }
    override def reduce: MultiSetExpr = {
      val r1 = arg1.reduce
      val r2 = arg2.reduce
      arg1.reduce match {
        case t: Trace =>  {
          red(red(this,rebuild(r1,r2)),combineOther(r1,r2))
        }
        case _ => reduce2(r1,r2)
      }
    }
    def combineBind(a1: MultiSetExpr, a2: MultiMapExpr): MultiSetExpr = {
      var b = a2
      var r = a1
      while (b.first != None) {
        val p = b.first.get
        r = r.bind(b.first.get.asInstanceOf[MapPair])
        b = b.split(b.first.get)._3
      }
      r
    }
    def wipe = EBind(arg1.wipe,arg2.wipe)
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    lazy val bindings = arg1.bindings multiMax arg2.bindings

    def asString = arg1.asString + " " + arg2.asString + " !"
  }

  case class Trace(trace: MultiMapExpr) extends Atom {
    def labels = trace.labels
    def bindings = trace.bindings
    def bind(b: MapPair) = {
      // TODO: optimize with measure
      var t = trace
      var found = false
      while (t.last != None && !found) {
        val e = t.last.get
        val l = e.labels
        if (!(l multiMin b.label).isEmpty) found = true
        else t = t.split(e)._1
      }
      if (found) {
        val e = t.last.get
        e.target.bind(b)
      }
      else this
    }
    override def value =  trace.last.get.target.value
    def reduce = this
    def wipe = trace.last.get.target.wipe

    def asString = {
      var e1 = trace
      var r: String = "("
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + " => " }
        r = r + e1.first.get.target.asString
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + ")"
    }
    override def topString = {
      var e1 = trace
      var r: String = ""
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + " => " }
        r = r + e1.first.get.target.asString
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + ""
    }
  }

  case class EMap(m: MMT) extends MultiMapExpr {

    def isEmpty = m.isEmpty
    def value = this
    def wipe= {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p: PP = mm.first.get.wipePair
        r = r put MMapPair(p.label,p.target)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }

    def bind(b: MapPair) = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put MMapPair(p.label.bind(b),p.target.bind(b))
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }

    def reduce = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put MMapPair(p.label.reduce,p.target.reduce)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }
    lazy val bindings = {
      var r: MultiMapExpr = EMap(emptyMap)
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r multiMax p.bindings
        mm = mm.split(mm.first.get)._3
      }
      r
    }
    lazy val labels = {
      var r: MultiSetExpr = EExpr
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r multiMax p.labels
        mm = mm.split(mm.first.get)._3
      }
      r
    }
    def first = m.first
    def some = m .some
    def last = m.last

    def get(e: PP) = m.get(e)
    def contains(e: PP) = get(e) != None
    def put(e: PP) = EMap(m.put(e))

    def split(e: PP) = { val(l,ee,r) = m.split(e) ; (EMap(l),ee,EMap(r)) }

    def multiAdd(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  { EMap(m add mm) }
      case _ => sys.error("no")
    }
    def multiMul(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  EMap(m multiply mm)
      case _ => sys.error("no")
    }
    def multiMax(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  EMap(m maximum mm)
      case _ => sys.error("no")
    }
    def multiMin(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  EMap(m minimum mm)
      case _ => sys.error("no")
    }
    def multiSubtract(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  { EMap(m subtract mm) }
      case _ => sys.error("no")
    }
    def isString: Boolean  = {
      var e1 = m
      var i: Int = 0
      var seq = true
      var str = true
      while ((e1.first != None) && seq && str) {
        seq = MSOrdering.compare(e1.first.get.label,EInt(i)) == 0
        str = e1.first.get.target match {
          case e: EChar => true
          case _ => false
        }
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      if (i > 1) (seq && str)
      else false
    }
    def isSequence: Boolean  = {
      var e1 = m
      var i: Int = 0
      var seq = true
      while ((e1.first != None) && seq) {
        seq = MSOrdering.compare(e1.first.get.label,EInt(i)) == 0
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      if (i > 1) seq
      else false
    }

    def doChar(e: MultiSetExpr): String = e match {
      case EChar(c) => c.toString
      case _ => e.asString
    }

    def asString = {
      val (str,seq,el2,st,el,et) = {
        if (isString) (true,true,"","\"","","\"")
        else if (isSequence) (false,true,"","",".","")
        else (false,false,"=","[",",","]")
      }
      var e1 = m
      var r: String = st
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + el }
        val p: PP = e1.first.get
        val vv = {
          if (str) doChar(p.target)
          else if (seq) subexpr(p.target)
          else p.target.asString
        }
        if (MSOrdering.compare(p.label,p.target) == 0) r = r + vv
        else {
          if (seq || str) r = r + vv
          else r = r + (p.label.asString+el2+vv)
        }
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + et
    }
  }

  trait SetPair extends PP


  def createSetP(l: MultiSetExpr, v: MultiSetExpr): SetPair = {
    if (MSOrdering.compare(l,EInt(1)) == 0) v.asInstanceOf[SetPair]
    else SSetPair(l,v)
    //SSetPair(l,v)
  }
  case class SSetPair(label: MultiSetExpr, target: MultiSetExpr) extends SetPair {

    def bindPair(b: MapPair) = {
      createSetP(label.bind(b), target.bind(b))
    }

    lazy val bindings = label.bindings multiMax target.bindings
    lazy val labels = label.labels multiMax target.labels

    def reducePair = {
      createSetP(label.reduce,target.reduce)
    }

    def wipePair = createSetP(label.wipe,target.wipe)


    def first = some
    def some = Some(this)
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def asString = {
      //if (MSOrdering.compare(label,EInt(1)) == 0) target.asString
      label.asString + ":" +target.asString
    }

    def createP(p: SetPair): Option[PP] = {
      if (MSOrdering.compare(p.label,EInt(0))==0) None
      else if (p.target.isEmpty) None
      else Some(p)
    }
    def add(o: PP): Option[PP] = createP(createSetP(EAdd(label,o.label).reduce.wipe,target))
    def mul(o: PP): Option[PP] = createP(createSetP(EMul(label,o.label).reduce.wipe,target))
    def max(o: PP): Option[PP] = createP(createSetP(EMax(label,o.label).reduce.wipe,target))
    def min(o: PP): Option[PP] = createP(createSetP(EMin(label,o.label).reduce.wipe,target))
    def subtract(o: PP): Option[PP] = createP(createSetP(ESub(label,o.label).reduce.wipe,target))
  }

  trait MapPair extends PP

  case class MMapPair(label: MultiSetExpr, target: MultiSetExpr) extends MapPair {

    def bindPair(b: MapPair) = {
      if (MMOrdering.compare(this,b) == 0) MMapPair(label,b.target)
      else MMapPair(label.bind(b), target.bind(b))
    }

    lazy val bindings = label.bindings multiMax  target.bindings
    lazy val labels = label.labels multiMax target.labels

    def reducePair = {
      val l = label.reduce
      val v = target.reduce
      MMapPair(l,v)
    }

    def wipePair = {
      MMapPair(label.wipe,target.wipe)
    }

    def first = some
    def some = Some(this)
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def asString = {
      if (target == EExpr) subexpr2(label) + "`"
      else subexpr2(label) + "'" + subexpr2(target)
    }

    def createP(p: MultiSetExpr): Option[PP] = {
      if (MSOrdering.compare(p.label,EInt(0))==0) None
      else if (p.isEmpty) None
      else Some(MMapPair(label,p))
    }

    def add(o: PP): Option[PP] = createP(target multiAdd o.target)
    def mul(o: PP): Option[PP] =  {
      createP(target multiMul o.target)
    }
    def max(o: PP): Option[PP] = createP(target multiMax o.target)
    def min(o: PP): Option[PP] = createP(target multiMin o.target)
    def subtract(o: PP): Option[PP] = createP(target multiSubtract o.target)
  }

  def subexpr2(s: MultiSetExpr): String = s match {
    case e: EInt => e.asString
    case s: ESymbol => s.asString
    case a: Alternatives => s.asString
    case t: Trace => t.asString
    case EFold => EFold.asString
    case c: EChar => c.asString
    case m: EEMap => m.asString
    case EExpr => EExpr.asString
    case _ => "(" + s.asString + ")"
  }

  def subexpr3(s: MultiSetExpr): String = s match {
    case e: EInt => e.asString
    case s: ESymbol => s.asString
    case a: Alternatives => s.asString
    case t: Trace => t.asString
    case EFold => EFold.asString
    case c: EConcat => c.asString
    case c: EChar => c.asString
    case m: EEMap => m.asString
    case EExpr => EExpr.asString
    case _ => "(" + s.asString + ")"
  }

  def subexpr(s: MultiSetExpr): String = s match {
    case e: EInt => e.asString
    case s: ESymbol => s.asString
    case a: Alternatives => s.asString
    case t: Trace => t.asString
    case c: EChar => c.asString
    case EFold => EFold.asString
    case EEMap(x: EMap) => {
      if (x.isString) EEMap(x).asString
      else if (x.isSequence) "(" + x.asString + ")"
      else EEMap(x).asString
    }
    case EExpr => EExpr.asString
    case _ => "(" + s.asString + ")"
  }
  import AbstractImmutableOrderedSet._

  // Abstract MultiSet operation
  trait MultiSetOperation[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends SetOperation[PP,M,SS,CC]

  trait MultiSetOperationImpl[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperation[M,SS,CC] with SetOperationImpl[PP,M,SS,CC] {
    def square(s: SS)(implicit c: CC): (SS,CC) = s.some match {
      case None => (s,c)
      case Some(x) => {
        val (l,c1) = square(s.left)(c)
        val (r,c2) = square(s.right)(c1)
        val e = x mul x
        c2.create(l,e,r)
      }
    }
    def twice(s: SS)(implicit c: CC): (SS,CC) = s.some match {
      case None => (s,c)
      case Some(x) => {
        val (l,c1) = twice(s.left)(c)
        val (r,c2) = twice(s.right)(c1)
        val e = x add x
        c2.create(l,e,r)
      }
    }
  }

  // common implementation for Difference and Union operation
  trait DifferenceUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] {
    def combineLeftElement(left: PP): Option[PP] = Some(left)
    def combineRightElement(right: PP): Option[PP] = Some(right)
  }

  trait MultiSetUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      x1 max x2
    } //Some(ME(x1.target,x1.count max x2.count))
  }


  trait MultiSetSumUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = twice(s1)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      x1 add x2
    } //Some(ME(x1.target,x1.count + x2.count))
  }

  case class LeftMultiSetSumUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetSumUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetSumUnion()
  }


  case class RightMultiSetSumUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetSumUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetSumUnion()
  }

  case class LeftMultiSetUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetUnion()
  }

  case class RightMultiSetUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetUnion[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetUnion()
  }

  trait MultiSetIntersect[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = (s1,c)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
    def combineLeftElement(left: PP): Option[PP] = None
    def combineRightElement(right: PP): Option[PP] = None
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      x1 min x2
    } // Some(ME(x1.target,x1.count min x2.count))
  }

  trait MultiSetMultiply[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = square(s1)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
    def combineLeftElement(left: PP): Option[PP] = None
    def combineRightElement(right: PP): Option[PP] = None
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      x1 mul x2
    } // Some(ME(x1.target,x1.count min x2.count))
  }

  case class LeftMultiSetMultiply[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetMultiply[M,SS,CC]  {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetMultiply()
  }

  case class RightMultiSetMultiply[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetMultiply[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetMultiply()
  }


  case class LeftMultiSetIntersect[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC]  {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetIntersect()
  }

  case class RightMultiSetIntersect[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetIntersect()
  }

  trait MultiSetDifference[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS,s2: SS)(implicit c: CC) = c.empty
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      x1 subtract x2
    } //Some(ME(x1.target,(x1.count - x2.count) max 0))
  }

  case class LeftMultiSetDifference[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetDifference[M,SS,CC]  {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetDifference()
  }

  case class RightMultiSetDifference[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetDifference[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetDifference()
  }
}

