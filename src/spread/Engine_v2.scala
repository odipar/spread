package spread

import spread.AbstractImmutableOrderedSet.SetOperation

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
    case EExpr => 1
    case (vv: Alternatives) => 2
    case (vv: ESymbol) => 3
    case (vv: Reduction) => 4
    case (vv: LabeledExpr) => 5
    case (vv: EEMap) => 6
    case (vv: EInt) => 7
    case (vv: EAdd) => 8
    case (vv: EMul) => 9
    case (vv: EBind) => 10
    case (vv: EIter) => 12
    case (vv: ERed) => 13
    case (vv: EDesolve) => 14
    case (vv: EWipe) => 15
  }

  object MSTreapContext extends STreapContextImpl[SP,Any,Int] {
    def compareOrder(x1: SP, x2: SP): Int = MSOrdering.compare(x1,x2)
    def priorityHash(x: Option[SP]): Int = MSHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val unionV2: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetSumUnion() // construct once, to avoid excessive allocations

    override def union = unionV2
    override def intersect = sys.error("not yet")
    override def difference = sys.error("not yet")
  }

  val noAlternatives: MST = EmptyOrderedISet(MSTreapContext) // no measure
  val emptyMap: MMT = EmptyOrderedISet(MMTreapContext)

  object MSHashing extends PriorityHasher[SP] {
    import Hashing.jenkinsHash
    def hash(s1: SP): Int = 1
    
    def hash(s1: MS): Int = s1 match {
      case EExpr => jenkinsHash(0)
      case (Alternatives(s)) => jenkinsHash(s.hashCode)
      case (EInt(i1)) => jenkinsHash(i1)
      case (EAdd(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (EMul(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (EBind(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (EIter(a1,a2)) => jenkinsHash(hash(a1) + jenkinsHash(hash(a2)))
      case (ERed(a1)) => jenkinsHash(hash(a1)*201-1239)
      case (EDesolve(a1)) => jenkinsHash(hash(a1)*123+123991)
      case (ESymbol(s)) => jenkinsHash(s.hashCode*123)
      case (Reduction(v1,e1)) => jenkinsHash(hash(v1)+13)
      case (LabeledExpr(label,value)) => jenkinsHash(jenkinsHash(hash(label)) + 13*hash(value))
      case (EEMap(EMap(m))) => jenkinsHash(m.hashCode-1234)
    }
  }

  object MMTreapContext extends STreapContextImpl[EP,Any,Int] {
    def compareOrder(x1: EP, x2: EP): Int = MMOrdering.compare(x1,x2)
    def priorityHash(x: Option[EP]): Int = MMHashing.hash(x.get)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val unionV2: SetOperation[PP,Any,STreap[PP,Any,Int],STreapContext[PP,Any,Int]] = LeftMultiSetSumUnion() // construct once, to avoid excessive allocations

    override def union = unionV2
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
    def compareEqual(v1: MultiSetExpr, v2: MultiSetExpr): Int = (v1,v2) match {
      case (EExpr,EExpr) => 0
      case (Reduction(v1,s1),Reduction(v2,s2)) => {
        val c = compare(v1,v2)
        if (c == 0) compare(s1,s2)
        else c
      }
      case (LabeledExpr(l1,v1),LabeledExpr(l2,v2)) => {
        val c = compare(v1,v2)
        if (c == 0) compare(l1,l2)
        else c
      }
      case (EInt(i1), EInt(i2)) => i1 - i2
      case (ESymbol(s1),ESymbol(s2)) => s1.compareTo(s2)
      case (EAdd(a11,a12),EAdd(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EBind(a11,a12),EBind(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (EIter(a11,a12),EIter(a21,a22)) => {
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
      case (ERed(e11),ERed(e21)) => compare(e11,e21)
      case (EDesolve(e11),EDesolve(e21)) => compare(e11,e21)
      case (EWipe(e11),EWipe(e21)) => compare(e11,e21)
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
    def compare(p1: PP, p2: PP): Int = {
      compare(p1.value,p2.value)
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

    def labels: MultiSetExpr // union of all labels found in all Pairs in all subexpressions
    def bind(b: MultiMapExpr): EXPR
    def wipe: EXPR
    def value: EXPR
    def source: EXPR
    def reduce: EXPR

    def asString: String
  }

  trait PP {

    def label: MultiSetExpr
    def value: MultiSetExpr

    def labels: MultiSetExpr // union of all labels found in all Pairs in all subexpressions
    def bindPair(b: MultiMapExpr): PP
    def wipePair: PP
    def reducePair: PP

    def add(o: PP): PP
    def mul(o: PP): PP
    def max(o: PP): PP
    def min(o: PP): PP
    def neg: PP

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
  trait MultiMapExpr extends MultiExpr[PP,MultiMapExpr] {
    def contains(p: PP): Boolean
    def get(p: PP): Option[PP]
  }

  trait MultiSetExpr extends MultiExpr[PP,MultiSetExpr] with SetPair

  trait MultiSetExprI extends MultiSetExpr {
    def label = EInt(1)
    def reducePair = reduce
    def wipePair = wipe
    def bindPair(b: MultiMapExpr) = bind(b)

    def add(o: PP): PP = {
      val a = EAdd(label,o.label)
      SSetPair(a,this)
    }
    def mul(o: PP): PP = {
      SSetPair(EMul(label,o.label).reduce.wipe,this)
    }
    def max(o: PP): PP = sys.error("no yet")
    def min(o: PP): PP = sys.error("no yet")
    def neg: PP = sys.error("no yet")
  }

  trait Atom extends  MultiSetExprI {
    def value: MultiSetExpr = this
    def source = EExpr
    def split(e: PP): (MultiSetExpr,Option[PP],MultiSetExpr) = (EExpr,some,EExpr)
    def first: Option[PP] = some
    def some: Option[PP] = Some(this)
    def last: Option[PP] = some
  }

  trait Operator extends Atom

  case object EExpr extends Atom {
    def wipe = this
    def bind(b: MultiMapExpr): MultiSetExpr = this
    def labels = this
    def reduce = this
    def combine(o: MultiSetExpr) = o
    override def first: Option[SetPair] = None
    override def some: Option[SetPair] = None
    override def last: Option[SetPair] = None

    def asString = "EMPTY"
  }

  def createMap(e: MapPair): MultiMapExpr = EMap(emptyMap.put(e))
  def createAlt(e: SetPair): MultiSetExpr = Alternatives(noAlternatives.put(e))

  def createAlt2(e: MST): MultiSetExpr = {
    if (e.isEmpty) EExpr
    else {
      val (l,ee,r) = e.split(e.some.get)
      if (l.isEmpty && r.isEmpty && (1==0)) {
        val p: PP = ee.get
        if (MSOrdering.compare(p.label,EInt(1)) == 0) p.value
        else Alternatives(e)
      }
      else Alternatives(e)
    }
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

  case class Alternatives(s: MST) extends Atom {
    def bind(b: MultiMapExpr) = {
      var e1 = s
      var r = noAlternatives
      while (e1.first != None) {
        val p: PP = e1.first.get.bindPair(b)
        r = r put SSetPair(p.label,p.value)
        e1 = e1.split(e1.first.get)._3
      }
      createAlt2(r)
    }

    def reduce = {
      var e1 = s
      var s2: MultiSetExpr = EExpr
      while (e1.first != None) {
        val p: PP = e1.first.get.reducePair
        var e2: MultiSetExpr = p.value
        while (e2.first != None ) {
          val p2: PP = e2.first.get.reducePair
          s2 = s2 combine createAlt(SSetPair(domul(p.label,p2.label),p2.value.reduce))
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      red(s2,this)
    }
    def wipe = {
      var e1 = s
      var r = noAlternatives
      while (e1.first != None) {
        val p: PP = e1.first.get.wipePair
        r = r put SSetPair(p.label,p.value)
        e1 = e1.split(e1.first.get)._3
      }
      createAlt2(r)
    }

    override def first = s.first.asInstanceOf[Option[SetPair]]
    override def some = s.some.asInstanceOf[Option[SetPair]]
    override def last = s.last.asInstanceOf[Option[SetPair]]

    override def split(e: PP): (MultiSetExpr,Option[PP],MultiSetExpr) = { val (l,ee,r) = s.split(e) ; (createAlt2(l),ee,createAlt2(r))}

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
      case Alternatives(s2: MST) => createAlt2(s union s2)
      case ss => {this combine createAlt(ss.asInstanceOf[SetPair])}
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
    def wipe = this
    def bind(b: MultiMapExpr) = this
    def labels = EExpr
    def reduce = this
    def combine(o: MultiSetExpr) = createAlt(this) combine o

    def asString = e.toString
  }

  case class ESymbol(s: String) extends Atom {
    def wipe = this
    def bind(b: MultiMapExpr) = this

    def labels = EExpr
    def reduce = this
    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString = s
  }

  case class ERed(m: MultiSetExpr) extends Operator {
    def wipe = ERed(m.wipe)
    def bind(b: MultiMapExpr) = ERed(m.bind(b))

    def labels = m.labels
    def reduce = {
      var a1 = m.reduce
      var e1 = a1
      var e: MultiSetExpr = EExpr
      while (e1.first != None) {
        val p = e1.first.get
        val ee: PP = p.reducePair
        e = e combine createAlt(SSetPair(ee.label,ee.value))
        e1 = e1.split(e1.first.get)._3
      }
      if (e.some == None) red(a1,this)
      else red(e,red(ERed(a1),this))
    }

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString =  m.asString + " $"
  }

  case class EDesolve(m: MultiSetExpr) extends Atom {
    def wipe = EDesolve(m.wipe)
    def bind(b: MultiMapExpr) = EDesolve(m.bind(b))

    def labels = m.labels
    def reduce = {
      val ra1 = m.reduce
      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        val r = e1.first.get.reducePair
        r match {
            case EEMap(m) => {
              var e2 = m
              while (e2.first != None) {
                val p: PP = e2.first.get.reducePair
                val l: LabeledExpr = LabeledExpr(p.label,p.value)
                e = e combine createAlt(l)
                e2 = e2.split(e2.first.get)._3
              }
            }
            case _ =>
        }
        e1 = e1.split(e1.first.get)._3
      }
      red(e,red(EDesolve(ra1),this))
    }

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString = m.asString + " >"
  }

  case class EWipe(m: MultiSetExpr) extends Operator {
    def wipe = EWipe(m.wipe)
    def bind(b: MultiMapExpr) = EWipe(m.bind(b))

    def labels = m.labels
    def reduce = {
      var a1 = m.reduce
      var e1 = a1
      var e: MultiSetExpr = EExpr
      while (e1.first != None) {
        val ee = e1.first.get
        val p: PP = ee.wipePair
        e = e combine createAlt(SSetPair(p.label,p.value))
        e1 = e1.split(e1.first.get)._3
      }
      red(e,red(EWipe(a1),this))
    }
    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString = m.asString + " #"
  }

  case class EEMap(e: MultiMapExpr) extends Atom {
    def wipe = EEMap(e.wipe)
    def bind(b: MultiMapExpr) = EEMap(e.bind(b))

    def labels = e.labels
    def reduce = EEMap(e.reduce)
    def combine(o: MultiSetExpr) = createAlt(this) combine o
    def asString = e.asString

    override def wipePair = wipe
    override def reducePair = reduce
    override def bindPair(b: MultiMapExpr) = this
  }

  def red(a1: MultiSetExpr, a2: MultiSetExpr): MultiSetExpr = {
    if (MSOrdering.compare(a1,a2) == 0) a1
    else a2 match {
      /*case Reduction(a21,a22) => {
        if (MSOrdering.compare(a1,a21) == 0) red(a1,a22)
        else Reduction(a1,a2)
      } */
      case _ => Reduction(a1,a2)
    }
  }

  case class EAdd(arg1: MultiSetExpr, arg2: MultiSetExpr) extends Operator {
    def bind(b: MultiMapExpr) = EAdd(arg1.bind(b),arg2.bind(b))

    def wipe = EAdd(arg1.wipe,arg2.wipe)
    def reduce = {
      val ra1 = arg1.reduce
      val ra2 = arg2.reduce

      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        var e2 = ra2
        while (e2.first != None) {
          val a1 = e1.first.get
          val a2 = e2.first.get

          (a1.value, a2.value) match {
            case (EInt(i1),EInt(i2)) => e = e combine EInt(i1+i2)
            case (EEMap(m1),EEMap(m2)) => e = e combine EEMap(m1 combine m2)
            case _ => e = e combine EAdd(a1.value,a2.value)
          }
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      if (e.some == None) EAdd(ra1,ra2)
      else red(e,red(EAdd(ra1,ra2),this))
    }
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = arg1.labels combine arg2.labels

    def asString = arg1.asString + " " + arg2.asString + " +"
  }

  case class LabeledExpr(l: MultiSetExpr, v: MultiSetExpr) extends Atom {
    def bind(b: MultiMapExpr) = {
        val mp = MMapPair(l,v)
        if (b.contains(mp)) {
          val vv: PP = b.get(mp).get
          LabeledExpr(l,vv.value)
        }
        else LabeledExpr(l.bind(b), v.bind(b))
      }

    def wipe = LabeledExpr(l.wipe,v.wipe)
    def reduce = {
      if (v.some == None) {
        LabeledExpr(l.reduce,v)
      }
      else {
        val vr = v.reduce
        red(v,red(LabeledExpr(l.reduce,vr),this))
      }
    }

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = l combine l.labels combine v.labels

    def asString = {
      if (v.some == None) l.asString + "`"
      else l.asString + "'" + v.asString
    }
  }

  def iterate(m1: MultiMapExpr, m2: MultiMapExpr): MultiMapExpr = {
    var m4 = m1
    var e2 = m2
    while (e2.first != None) {
      var m3: MultiMapExpr = EMap(emptyMap)
      val p = e2.first.get
      val l = p.label
      val a = p.value
      var e3 = a
      while (e3.first != None) {
        val v = e3.first.get
        val np = MMapPair(l,v.value)
        var nm = createMap(np)
        m3 = m3 combine m4.bind(nm)
        e3 = e3.split(e3.first.get)._3
      }
      m4 = m3
      e2 = e2.split(e2.first.get)._3
    }
    m4
  }

  case class EIter(arg1: MultiSetExpr, arg2: MultiSetExpr) extends Operator {
    def bind(b: MultiMapExpr) = EIter(arg1.bind(b),arg2.bind(b))

    def wipe = EIter(arg1.wipe,arg2.wipe)
    def reduce = {
      val ra1 = arg1.reduce
      val ra2 = arg2.reduce
      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        var e2 = ra2
        while (e2.first != None) {
          (e1.first.get,e2.first.get) match {
            case (EInt(i1),_) => e = e combine EInt(i1)
            case (EEMap(m1),EEMap(m2)) => {
              e = e combine EEMap(iterate(m1,m2))
            }
            case (EEMap(m1),EInt(_)) => e = e combine EEMap(m1)
            case _ =>
          }
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      if (e.some == None) EIter(ra1,ra2)
      else red(e,red(EIter(ra1,ra2),this))
    }
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = arg1.labels combine arg2.labels

    def asString = arg1.asString + " " + arg2.asString + " ~"
  }

  case class EMul(arg1: MultiSetExpr, arg2: MultiSetExpr) extends Operator {
    def bind(b: MultiMapExpr) = EMul(arg1.bind(b),arg2.bind(b))

    def wipe = EMul(arg1.wipe,arg2.wipe)
    def reduce = {
      val ra1 = arg1.reduce
      val ra2 = arg2.reduce

      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        var e2 = ra2
        while (e2.first != None) {

          val a1 = e1.first.get
          val a2 = e2.first.get

          (a1.value, a2.value) match {
            case (EInt(i1),EInt(i2)) =>  e = e combine EInt(i1*i2)
            case _ =>
          }
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      if (e.some == None) EMul(ra1,ra2)
      else red(e,red(EMul(ra1,ra2),this))
    }

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = arg1.labels combine arg2.labels

    def asString = arg1.asString + " " + arg2.asString + " *"
  }

  case class EBind(arg1: MultiSetExpr, arg2: MultiSetExpr) extends Operator {
    def bind(b: MultiMapExpr) = EBind(arg1.bind(b),arg2.bind(b))

    def wipe = EBind(arg1.wipe,arg2.wipe)
    def reduce = {
      val ra1 = arg1.reduce
      val ra2 = arg2.reduce

      var e: MultiSetExpr = EExpr

      var e1 = ra1
      while (e1.first != None) {
        var e2 = ra2
        while (e2.first != None) {
          val a1 = e1.first.get
          val a2 = e2.first.get

          (a1.value, a2.value) match {
            case (EEMap(m1), EEMap(m2)) => e = e combine EEMap(m1.bind(m2))
            case _ =>
          }
          e2 = e2.split(e2.first.get)._3
        }
        e1 = e1.split(e1.first.get)._3
      }
      red(e,red(EBind(ra1,ra2),this))
    }

    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def combine(o: MultiSetExpr) = createAlt(this) combine o
    lazy val labels = arg1.labels combine arg2.labels

    def asString = arg1.asString + " " + arg2.asString + " !"
  }


  case class Reduction(value: MultiSetExpr, source: MultiSetExpr) extends MultiSetExprI {
    lazy val labels = value.labels combine source.labels

    def bind(b: MultiMapExpr) = source.bind(b).reduce
    def wipe = value
    def setOccurrence(o: Int) = sys.error("not yet")
    def reduce = this
    def first = value.first
    def some = value.some
    def last = value.last
    def split(e: PP) =  value.split(e)

    def combine(o: MultiSetExpr): MultiSetExpr = this combine o

    def asString = {
      if (MSOrdering.compare(value,source) == 0) "("+source.asString + ")@=" + value.asString
      else {
        "("+source.asString + ")@" + value.asString
      }
    }
  }
  case class EMap(m: MMT) extends MultiMapExpr {
    def source = this

    def value = this
    def wipe= {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p: PP = mm.first.get.wipePair
        r = r put MMapPair(p.label,p.value)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }

    def bind(b: MultiMapExpr) = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put MMapPair(p.label.bind(b),p.value.bind(b))
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }

    def reduce = {
      var r = emptyMap
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r put MMapPair(p.label.reduce,p.value.reduce)
        mm = mm.split(mm.first.get)._3
      }
      EMap(r)
    }
    lazy val labels = {
      var r: MultiSetExpr = EExpr
      var mm = m
      while (mm.first != None) {
        val p = mm.first.get
        r = r combine p.value.labels
        mm = mm.split(mm.first.get)._3
      }
      r
    }
    def first = m.first
    def some = m .some
    def last = m.last

    def get(e: PP) = m.get(e)
    def contains(e: PP) = get(e) != None

    def split(e: PP) = { val(l,ee,r) = m.split(e) ; (EMap(l),ee,EMap(r)) }

    def combine(o: MultiMapExpr): MultiMapExpr = o match {
      case EMap(mm) =>  EMap(m union mm)
      case _ => sys.error("no")
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
      seq
    }
    def asString = {
      val (seq,el2,st,el,et) = {
        if (isSequence) (true,"","",".","")
        else (false,"=","[",",","]")
      }
      var e1 = m
      var r: String = st
      var i = 0
      while (e1.first != None) {
        if (i > 0) { r = r + el }
        val p: PP = e1.first.get
        val vv = {
          if (seq) subexpr(p.value)
          else p.value.asString
        }
        if (MSOrdering.compare(p.label,p.value) == 0) r = r + vv
        else {
          if (seq) r = r + vv
          else r = r + (p.label.asString+el2+vv)
        }
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + et
    }
  }

  trait SetPair extends PP

  case class SSetPair(label: MultiSetExpr, v: MultiSetExpr) extends SetPair {

    def bindPair(b: MultiMapExpr) = SSetPair(label.bind(b), value.bind(b))

    lazy val labels = label.labels combine value.labels

    def value = v.value

    def reducePair = {
      val l = label.reduce
      val v = value.reduce
      SSetPair(l,v)
    }

    def wipePair = SSetPair(label.wipe,value.wipe)
    def source = this

    def first = some
    def some = Some(this)
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def asString = {
      if (MSOrdering.compare(label,EInt(1)) == 0) value.asString
      else label.asString + ":" +value.asString
    }

    def add(o: PP): PP = SSetPair(EAdd(label,o.label),value)
    def mul(o: PP): PP = sys.error("not yet")
    def max(o: PP): PP = sys.error("not yet")
    def min(o: PP): PP = sys.error("not yet")
    def neg: PP = sys.error("not yet")
  }

  trait MapPair extends PP

  case class MMapPair(label: MultiSetExpr, value: MultiSetExpr) extends MapPair {

    def bindPair(b: MultiMapExpr) = {
      if (b.contains(this)) MMapPair(label,b.get(this).get.value)
      else MMapPair(label.bind(b), value.bind(b))
    }

    lazy val labels = label combine label.labels combine value.labels

    def reducePair = {
      val l = label.reduce
      val v = value.reduce
      MMapPair(l,v)
    }

    def wipePair = MMapPair(label.wipe,value.wipe)
    def source = this

    def first = some
    def some = Some(this)
    def last = some
    def split(e: MultiSetExpr) = (EExpr,some,EExpr)

    def asString = {
      if (value == EExpr) subexpr2(label) + "`"
      else subexpr2(label) + "'" + subexpr2(value)
    }

    def add(o: PP): PP = {
      MMapPair(label,value combine o.value)
    }
    def mul(o: PP): PP = sys.error("not yet")
    def max(o: PP): PP = sys.error("not yet")
    def min(o: PP): PP = sys.error("not yet")
    def neg: PP = sys.error("not yet")
  }
  
  def subexpr2(s: MultiSetExpr): String = s match {
    case e: EInt => e.asString
    case s: ESymbol => s.asString
    case a: Alternatives => s.asString
    case m: EEMap => m.asString
    case EExpr => EExpr.asString
    case _ => "(" + s.asString + ")"
  }

  def subexpr(s: MultiSetExpr): String = s match {
    case e: EInt => e.asString
    case s: ESymbol => s.asString
    case a: Alternatives => s.asString
    case m: EEMap => m.asString
    case EExpr => EExpr.asString
    case _ => "(" + s.asString + ")"
  }
  import AbstractImmutableOrderedSet._

  // Abstract MultiSet operation
  trait MultiSetOperation[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends SetOperation[PP,M,SS,CC]

  trait MultiSetOperationImpl[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperation[M,SS,CC] with SetOperationImpl[PP,M,SS,CC] {
    def twice(s: SS)(implicit c: CC): (SS,CC) = s.some match {
      case None => (s,c)
      case Some(x) => {
        val (l,c1) = twice(s.left)(c)
        val (r,c2) = twice(s.right)(c1)
        val e = x add x
        c2.create(l,Some(e),r)
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
      Some(x1 max x2)
    } //Some(ME(x1.value,x1.count max x2.count))
  }


  trait MultiSetSumUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def combineEqual(s1: SS, s2: SS)(implicit c: CC) = twice(s1)
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      Some(x1 add x2)
    } //Some(ME(x1.value,x1.count + x2.count))
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
      Some(x1)
    } // Some(ME(x1.value,x1.count min x2.count))
  }

  case class LeftMultiSetIntersect[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC]  {
    lazy val swap: MultiSetOperation[M,SS,CC] = RightMultiSetIntersect()
  }

  case class RightMultiSetIntersect[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetIntersect[M,SS,CC] {
    lazy val swap: MultiSetOperation[M,SS,CC] = LeftMultiSetIntersect()
  }

  case class MultiSetDifference[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]()
    extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
    def swap: MultiSetOperation[M,SS,CC] = this
    def combineEqual(s1: SS,s2: SS)(implicit c: CC) = c.empty
    def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
    def combineEqualElements(x1: PP, x2: PP): Option[PP] = {
      Some(x1)
    } //Some(ME(x1.value,(x1.count - x2.count) max 0))
  }
}

