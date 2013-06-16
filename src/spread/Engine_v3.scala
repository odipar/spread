package spread


object Engine_v3 {
  import AbstractImmutableOrderedSet._
  import OrderedSetImplementation._
  import SequenceImplementation._
  import OrderedTreapSet._
  import WeightBalancedSequence._
  import Hashing._
  import java.math.BigInteger

  lazy val emptySet = emptyMSet

  trait Expr {
    def bind(label: Expr, value: Expr): Expr
    def bindings: MultiMap
    def addDependencies(d: MST): Expr
    def dependencies: MST
    def reduce: Expr
    def wipe: Expr
    def isRedex: Boolean
  }

  trait NoDepedencies extends Expr {
    def dependencies = emptySet
  }

  def ece(d: MST, c: CEX): Expr = {
    if (c.isEmpty) Empty
    else {
      if (d.isEmpty) ECompoundExpr(c)
      else DECompoundExpr(d,c)
    }
  }
  def ece2(d: MST, c: CEX): ECompoundExprImpl = {
    if (d.isEmpty) ECompoundExpr(c)
    else DECompoundExpr(d,c)
  }

  trait MultiContainer[M <: MultiContainer[M]] extends Expr {
    def multiAdd(o: M): Expr
    def multiSub(o: M): Expr
    def multiMul(o: M): Expr
    def multiMax(o: M): Expr
    def multiMin(o: M): Expr
  }
  trait MultiSet extends MultiContainer[MultiSet]
  trait MultiMap extends MultiContainer[MultiMap]
  trait Trace extends Expr
  trait IndexedExpr extends Expr
  trait CompoundExpr extends IndexedExpr with Expr

  case class ETrace(t: TTT) extends ETraceImpl with NoDepedencies
  case class DETrace(dependencies: MST, t: TTT) extends ETraceImpl

  def makeTrace(d: MST, t: TTT): ETraceImpl = {
    if (d.isEmpty) ETrace(t)
    else DETrace(d,t)
  }

  trait ETraceImpl extends Trace {
    def t: TTT
    def addDependencies(dd: MST) = makeTrace(dependencies maximum dd,t)
    def bindings = not_yet
    def bind(label: Expr, value: Expr) = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    /*def maxPos(c: TTT): Number = {
      if (c.isEmpty) zero
      else {
        val tp = c.some.get
        val m: Number = tp.second match {
          case e: ECompoundExprImpl => e.cex.last.get.first
          case _ => zero
        }
        maxPos(c.left) max m max maxPos(c.right)
      }
    } */
  }

  def mmp(d: MST, m: MMT): Expr = {
    if (m.isEmpty) zero.addDependencies(d)
    else mmp2(d,m)
  }

  def mmp2(d: MST, m: MMT): MMapImpl = {
    if (d.isEmpty) MMap(m)
    else DMMap(d,m)
  }

  case class DMMap(dependencies: MST, m: MMT) extends MMapImpl
  case class MMap(m: MMT) extends MMapImpl with NoDepedencies

  trait MMapImpl extends MultiMap with Arg {
    def m: MMT
    def addDependencies(dd: MST) = mmp(dependencies maximum dd,m)
    def bind(label: Expr, value: Expr) = {
      var nm = emptyMMap
      var mm = m
      while (mm.first != None) {
        var p = mm.first.get
        value match {
          case ms: MSetImpl => {
            var mms = ms.m
            while (mms.first != None) {
              val ps = mms.first.get
              nm = nm put MapP(p.first.bind(label,ps.second),p.second.bind(label,ps.second))
              mms = mms.split(ps)._3
            }
          }
          case _ => {
            nm = nm put MapP(p.first.bind(label,value),p.second.bind(label,value))
          }
        }
        mm = mm.split(mm.first.get)._3
      }
      mmp(dependencies,nm)
    }

    def bindings = not_yet
    def reduce = not_yet
    def wipe = not_yet
    def isRedex = false

    def multiAdd(o: MultiMap) = o match { case mm: MMapImpl => mmp(dependencies maximum mm.dependencies,m add mm.m) }
    def multiSub(o: MultiMap) = o match { case mm: MMapImpl => mmp(dependencies maximum mm.dependencies,m subtract mm.m) }
    def multiMul(o: MultiMap) = o match { case mm: MMapImpl => mmp(dependencies maximum mm.dependencies,m multiply mm.m) }
    def multiMax(o: MultiMap) = o match { case mm: MMapImpl => mmp(dependencies maximum mm.dependencies,m maximum mm.m) }
    def multiMin(o: MultiMap) = o match { case mm: MMapImpl => mmp(dependencies maximum mm.dependencies,m minimum mm.m) }

    def isString: Boolean = {
      var e1 = m
      var i = zero
      var seq = true
      var str = true
      while ((e1.first != None) && seq && str) {
        seq = ExprOrdering.compare(e1.first.get.first,i) == 0
        str = e1.first.get.second match {
          case e: ECharImpl => true
          case _ => false
        }
        e1 = e1.split(e1.first.get)._3
        i = i.incr
      }
      if (i.compare(one) > 0) (seq && str)
      else false
    }
    def isSequence: Boolean  = {
      var e1 = m
      var i = zero
      var seq = true
      while ((e1.first != None) && seq) {
        seq = ExprOrdering.compare(e1.first.get.first,i) == 0
        e1 = e1.split(e1.first.get)._3
        i = i.incr
      }
      if (i.compare(one) > 0) seq
      else false
    }

  }

  case class MSet(m: MST) extends MSetImpl with NoDepedencies
  case class DMSet(dependencies: MST, m: MST) extends MSetImpl

  def makeMSet(d: MST, m: MST): MSetImpl = {
    if (d.isEmpty) MSet(m)
    else DMSet(d,m)
  }

  trait MSetImpl extends MultiSet {
    def m: MST
    def isEmpty = m.isEmpty
    def addDependencies(dd: MST): Expr = makeMSet(dependencies maximum dd,m)
    def bind(label: Expr, value: Expr) = {
      var nm = emptyMSet
      var mm = m
      while (nm.first == None) {
        var p = mm.first.get
        nm = nm put SetP(fullReduce(p.first.bind(label,value)),p.second.bind(label,value))
        mm = mm.split(nm.first.get)._3
      }
      makeMSet(dependencies,nm)
    }
    def bindings = not_yet
    def reduce = not_yet
    def wipe = not_yet
    def isRedex = false
    def multiAdd(o: MultiSet) = o match { case mm: MSetImpl => makeMSet(dependencies maximum mm.dependencies,m add mm.m) }
    def multiSub(o: MultiSet) = o match { case mm: MSetImpl => makeMSet(dependencies maximum mm.dependencies,m subtract mm.m) }
    def multiMul(o: MultiSet) = o match { case mm: MSetImpl => makeMSet(dependencies maximum mm.dependencies,m multiply mm.m) }
    def multiMax(o: MultiSet) = o match { case mm: MSetImpl => makeMSet(dependencies maximum mm.dependencies,m maximum mm.m) }
    def multiMin(o: MultiSet) = o match { case mm: MSetImpl => makeMSet(dependencies maximum mm.dependencies,m minimum mm.m) }
  }

  trait Pair[A <: Expr, B <: Expr] {
    def first: A
    def second: B
  }

  trait Triple[A <: Expr, B <: Expr, C <: Expr] extends Pair[A,B] {
    def third: C
  }

  case class EPair[A <: Expr,B <: Expr](first: A, second: B) extends Pair[A,B]
  case class ETriple[A <: Expr,B <: Expr, C <: Expr](first: A, second: B, third: C) extends Triple[A,B,C]

  trait MPair[A <: Expr, B <: Expr] extends Pair[A,B] {
    def neg: Option[MPair[A,B]]
    def add(o: MPair[A,B]): Option[MPair[A,B]]
    def mul(o: MPair[A,B]): Option[MPair[A,B]]
    def max(o: MPair[A,B]): Option[MPair[A,B]]
    def min(o: MPair[A,B]): Option[MPair[A,B]]
    def subtract(o: MPair[A,B]): Option[MPair[A,B]]
  }

  trait MultiSetPair extends MPair[Expr,Expr]
  trait MultiMapPair extends MPair[Expr,Expr]
  trait LabeledExpr extends Pair[Expr,Expr] with Expr
  trait CompoundPair extends EP {
    def bind(label: Expr, value: Expr): CompoundPair
  }
  trait TracePair extends MPair[Number,Expr]
  trait Operator extends Expr {
    def bind(label: Expr, value: Expr) = this
    def asString: String
  }

  //def createNumber: Number
  lazy val _five: Number = EInt(BigInteger.valueOf(-5))
  lazy val _four: Number = EInt(BigInteger.valueOf(-4))
  lazy val _three: Number = EInt(BigInteger.valueOf(-3))
  lazy val _two: Number = EInt(BigInteger.valueOf(-2))
  lazy val _one: Number = EInt(BigInteger.valueOf(-1))
  lazy val zero: Number = EInt(BigInteger.valueOf(0))
  lazy val one: Number = EInt(BigInteger.valueOf(1))
  lazy val two: Number = EInt(BigInteger.valueOf(2))
  lazy val three: Number = EInt(BigInteger.valueOf(3))

  case class SetP(first: Expr, second: Expr) extends MultiSetPair {
    def op(o: MPair[Expr,Expr], bo: BinaryOperator) = {
      val i = emptyExpr put CP(zero,first) put CP(one,o.first) put CP(two,bo)
      val e = fullReduce(ece(emptySet,i))
      if (ExprOrdering.compare(e,zero) == 0) None
      else Some(SetP(e,second))
    }
    def neg = op(SetP(zero,second),makeSub(emptySet))
    def add(o: MPair[Expr,Expr]) = op(o,makeAdd(emptySet))
    def mul(o: MPair[Expr,Expr]) = op(o,makeMul(emptySet))
    def max(o: MPair[Expr,Expr]) = op(o,makeMax(emptySet))
    def min(o: MPair[Expr,Expr]) = op(o,makeMin(emptySet))
    def subtract(o: MPair[Expr,Expr]) = op(o,makeSub(emptySet))
  }

  def asMSet(o: Expr): MSetImpl = o match {
    case m: MSetImpl => m
    case _ => makeMSet(o.dependencies,emptyMSet put SetP(one,o))
  }

  def mp(m: MapP): Option[MapP] = m.second match {
    case ms: MSetImpl => {
      if (ms.isEmpty) None
      else Some(m)
    }
    case _ => Some(m)
  }

  case class MapP(first: Expr, second: Expr) extends MultiMapPair {
    def neg = not_yet // MapP(first, asMSet(second).neg)
    def add(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiAdd asMSet(o.second)))
    def mul(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMul asMSet(o.second)))
    def max(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMax asMSet(o.second)))
    def min(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMin asMSet(o.second)))
    def subtract(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiSub asMSet(o.second)))

  }
  trait UnaryOperator extends Operator {
    def reduce(arg1: Expr): Pair[Expr,Expr]
  }

  trait BinaryOperator extends Operator {
    def reduce(arg1: Expr, arg2: Expr): Triple[Expr,Expr,Expr]
  }

  def makeSeq(m: MMT, i: Number): (Number,MMT) = {
    var mm = emptyMMap
    var lm = m
    var ii = i
    while (lm.first != None) {
      val p = lm.first.get
      val np = MapP(ii,p.second)
      mm = mm put np
      lm = lm.split(lm.first.get)._3
      ii = ii.incr
    }
    (ii,mm)
  }

  case class EConcat(dependencies: MST) extends BinOpImp with NormalExpr {
    def recreate(d: MST) = EConcat(d)
    def reduceNumber(arg1: Number, arg2: Number) = reduceMap(makeMap(arg1),makeMap(arg2))
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = (arg1,arg2) match {
      case (m1: MMapImpl, m2: MMapImpl) => {
        val (i,s1) = makeSeq(m1.m,zero)
        val (i2,s2) = makeSeq(m2.m,i)
        mmp(m1.dependencies maximum m2.dependencies,s1 maximum s2)
      }
    }
    def asString = "`+"
  }


  case class ELabeledExpr(first: Expr, second: Expr) extends ELabeledExprImpl with NoDepedencies
  case class DELabeledExpr(dependencies: MST, first: Expr, second: Expr) extends ELabeledExprImpl

  def makeLabeledExpr(d: MST, first: Expr, second: Expr): ELabeledExprImpl = {
    if (d.isEmpty) ELabeledExpr(first,second)
    else DELabeledExpr(d,first,second)
  }

  trait ELabeledExprImpl extends LabeledExpr with NormalExpr {
    override def isRedex = second.isRedex
    override def reduce: Expr = ece(emptySet,emptyExpr put CP(zero,second.addDependencies(first.dependencies).addDependencies(emptySet put SetP(one,first))))
    def addDependencies(dd: MST) = makeLabeledExpr(dependencies maximum dd,first,second)
    def bind(label: Expr, value: Expr) = {
      if (ExprOrdering.compare(first,label) == 0) makeLabeledExpr(dependencies,first,value)
      else makeLabeledExpr(dependencies,fullReduce(first.bind(label,value)),fullReduce(second.bind(label,value)))
    }
  }

  trait Number extends Expr {
    def zero: Number
    def one: Number
    def incr: Number
    def decr: Number
    def split2: Number
    def add(n: Number): Number
    def mul(n: Number): Number
    def min(n: Number): Number
    def max(n: Number): Number
    def sub(n: Number): Number
    def compare(n: Number): Int
  }

  trait NormalExpr extends Expr {
    def bindings = not_yet
    def reduce: Expr = this
    def wipe = this
    def isRedex = false
  }

  trait NoBind extends Expr {
    def bind(label: Expr, value: Expr) = this
  }

  case object Empty extends NormalExpr with NoBind with NoDepedencies {
    def addDependencies(d: MST) = this
  }

  trait CompoundPairImpl extends CompoundPair {
    def add(o: EP): Option[EP] = {
      (this,o) match {
        case (CP(f1,s1: ETraceImpl),CP(f2,s2: ETraceImpl)) => Some(this)
        case _ => {
          val ts = second
          val os = o.second
          (ts,os) match {
            case (e1: ECompoundExprImpl, e2: ECompoundExprImpl) => {
              val c = e1 concat e2
              if (c.isEmpty) None
              else Some(CP(first,c))
            }
            case (Empty,_) => None
            case (_,Empty) => None
            case (e1: ECompoundExprImpl, _) => if (e1.isEmpty) None ; else Some(o)
            case (_,e2: ECompoundExprImpl) =>  Some(o)
            case _ => Some(o)
          }
        }
      }
    }
    def neg = not_yet
    def subtract(o: EP): Option[EP] = not_yet
    def mul(o: EP): Option[EP] = not_yet
    def max(o: EP): Option[EP] = not_yet
    def min(o: EP): Option[EP] = not_yet
  }

  def not_yet = sys.error("not yet")

  var lll = 0

  case class CP(f: Number, s: Expr) extends CompoundPairImpl {
    def bind(label: Expr, value: Expr) = CP(f,s.bind(label,value))
    def first = f
    def labeledValue = s match {
      case e: ELabeledExprImpl => {
        if (ExprOrdering.compare(e.second, Empty) == 0) s
        else e.second.addDependencies(emptySet put SetP(one,e.first)).addDependencies(e.dependencies).addDependencies(e.first.dependencies)
      }
      case l: ETraceImpl => {
        lll = lll + 1
        //if (lll == 2) sys.error("no")
        l.t.last.get.second
      }
      case _ => s
    }
    def second = labeledValue
  }


  case class Symbol(s: String) extends SymbolImpl with NoDepedencies
  case class DSymbol(dependencies: MST, s: String) extends SymbolImpl

  def msymbol(d: MST, s: String): SymbolImpl = {
    if (d.isEmpty) Symbol(s)
    else DSymbol(d,s)
  }

  trait SymbolImpl extends NoBind {
    def s: String
    def addDependencies(dd: MST) = msymbol(dependencies maximum dd,s)
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
  }

  trait Arg extends Expr

  def makeChar(d: MST, c: Char): ECharImpl = {
    if (d.isEmpty) EChar(c)
    else DEChar(d,c)
  }

  case class EChar(c: Char) extends ECharImpl with NoDepedencies
  case class DEChar(dependencies: MST, c: Char) extends ECharImpl

  trait ECharImpl extends NoBind {
    def c: Char
    def addDependencies(dd: MST) = makeChar(dependencies maximum dd,c)
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
  }

  def simplify(ee: Expr): Expr = ee match {
    case e: ECompoundExprImpl => {
      if (e.left.isEmpty && e.right.isEmpty) {
        e.some match {
          case None => Empty
          case Some(CP(_,e: ELabeledExprImpl)) => e
          case Some(p) => simplify(p.second)
        }
      }
      else ee
    }
    case _ => ee
  }

  def fullReduce(e: Expr): Expr = {
    var ee = e
    while (ee.isRedex) ee = ee.reduce
    simplify(ee)
  }

  def fullReduce2(e: Expr): Expr = {
    if (e.isRedex) {
      var ee = e
      var t = emptyTrace
      var i = zero
      while (ee.isRedex) {
        t = t put TraceP(i,ee)
        i = i.incr
        ee = ee.reduce
      }
      val s = simplify(ee)
      if (ExprOrdering.compare(s,Empty) == 0) Empty
      else {
        t = t put TraceP(i,ee)
        makeTrace(emptySet,t)
      }
    }
    else e
  }

  case object ERed extends ERedImpl with NoDepedencies
  case class DERed(dependencies: MST) extends ERedImpl

  def makeRed(d: MST): ERedImpl = {
    if (d.isEmpty) ERed
    else DERed(d)
  }

  trait ERedImpl extends UnaryOperator with NormalExpr with UnaOpImp {
    def recreate(d: MST) = makeRed(d)
    def reduceNumber(i: Number) = i
    def reduceMap(arg1: MMapImpl) = arg1 match {
      case m: MMapImpl =>  {
        var mm = m.m
        var nm = emptyMMap
        while (mm.first != None) {
          val p = mm.first.get
          if (p.second.isRedex) {
            val r = fullReduce2(p.second)
            if (ExprOrdering.compare(r,Empty) != 0) {
              val np = MapP(p.first,r)
              nm = nm put np
            }
          }
          else nm = nm put p
          mm = mm.split(mm.first.get)._3
        }
        mmp(m.dependencies,nm)
      }
    }
    def asString = "$"
  }

  case class EForeach(a: Expr) extends EForeachImpl with NoDepedencies
  case class DEForeach(dependencies: MST, a: Expr) extends EForeachImpl

  def makeForeach(d: MST, a: Expr): EForeachImpl = {
    if (d.isEmpty) EForeach(a)
    else DEForeach(d,a)
  }

  trait EForeachImpl extends UnaOpImp with UnaryOperator {
    def a: Expr
    override def bind(label: Expr, value: Expr) = makeForeach(dependencies,a.bind(label,value))
    def recreate(d: MST) = makeForeach(d,a)
    def reduceNumber(i: Number) = reduceMap(makeMap(i))
    def reduceMap(arg1: MMapImpl) = arg1 match {
      case m: MMapImpl =>  {
        a match {
          case Empty => mmp(m.dependencies,foreach2(m.m))
          case _ => mmp(m.dependencies,foreach(m.m))
        }
      }
    }
    def foreach2(m: MMT): MMT = {
      if (m.isEmpty) m
      else {
        val e = m.some.get
        val c1 = fullReduce(e.second)
        if (ExprOrdering.compare(c1,Empty) != 0) foreach2(m.left) put MapP(e.first,c1) add foreach2(m.right)
        else foreach2(m.left) add foreach2(m.right)
      }
    }
    def foreach(m: MMT): MMT = {
      if (m.isEmpty) m
      else {
        val e = m.some.get
        val c = ece(emptySet,emptyExpr put CP(zero,e.second) put CP(one,a))
        val c1 = fullReduce(c)
        if (ExprOrdering.compare(c1,Empty) != 0) foreach(m.left) put MapP(e.first,c1) add foreach(m.right)
        else foreach(m.left) add foreach(m.right)
      }
    }
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = sys.error("not allowed")
  }

  def log2(i: Int): Int = {
    var l = 1; var ii = i
    while (ii > 1) { l = l * 2 ; ii = ii / 2 }
    l / 2
  }

  case class EFold(a: Expr) extends EFoldImpl with NoDepedencies
  case class DEFold(dependencies: MST, a: Expr) extends EFoldImpl

  def makeFold(d: MST, a: Expr): EFoldImpl = {
    if (d.isEmpty) EFold(a)
    else DEFold(d,a)
  }
  
  trait EFoldImpl extends UnaOpImp with UnaryOperator {
    def a: Expr
    override def bind(label: Expr, value: Expr) = makeFold(dependencies,a.bind(label,value))
    def recreate(d: MST) = makeFold(d,a)
    def reduceNumber(i: Number) = reduceMap(makeMap(i))
    def reduceMap(arg1: MMapImpl) = arg1 match {
      case m: MMapImpl=> {
        if (ExprOrdering.compare(a,Empty) == 0) ece(m.dependencies,fold2(m.m))
        else {
          val (f,_) = fold(m.m,zero)
          fullReduce(ece(m.dependencies,f))
        }
      }
    }
    def fold2(m: MMT): CEX = {
      var e = emptyExpr
      var mm = m
      var i = zero
      while (mm.first != None) {
        val p = mm.first.get
        e = e put CP(i,p.second)
        mm = mm.split(mm.first.get)._3
        i = i.incr
      }
      e
    }
    def fold(m: MMT,i: Number): (CEX, Number) = {
      val s = sizem(m)
      if (s == 0) (emptyExpr,i)
      else if (s == 1) (emptyExpr put CP(i,m.some.get.second),i.incr)
      else {
        val l1 = log2(s)
        val (l,e,rr) = splitm(l1,m)
        val r = rr put e.get
        val (fr,i1) = fold(l,i)
        val (fl,i2) = fold(r,i1)
        (fr add fl put CP(i2,a),i2.incr)
      }
    }
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = sys.error("not allowed")
  }

  trait EIntImpl extends Number with NormalExpr with Arg with NoBind {
    def integer: BigInteger
    def addDependencies(dd: MST): Expr = create(dd,integer)
    def create(i: BigInteger): Number = create(dependencies,i)
    def create(dd: MST, i: BigInteger): Number = {
      // TODO: Optimize
      val nd = dependencies maximum dd
      if (nd.isEmpty) EInt(i)
      else DEInt(nd,i)
    }
    def zero: Number = create(BigInteger.valueOf(0))
    def one: Number = create(BigInteger.valueOf(1))
    def incr: Number = this add one
    def decr: Number = this sub one
    def split2: Number = create(integer divide BigInteger.valueOf(2))
    def add(n: Number): Number = n match { case n2: EIntImpl => create(n2.dependencies,integer add n2.integer) }
    def mul(n: Number): Number = n match { case n2: EIntImpl => create(n2.dependencies,integer multiply  n2.integer) }
    def min(n: Number): Number = n match { case n2: EIntImpl => create(n2.dependencies,integer min n2.integer) }
    def max(n: Number): Number = n match { case n2: EIntImpl => create(n2.dependencies,integer max n2.integer) }
    def sub(n: Number): Number = n match{ case n2: EIntImpl => create(n2.dependencies,integer subtract n2.integer) }
    def compare(n: Number): Int = n match { case n2: EIntImpl => integer.compareTo(n2.integer) }
    def asString: String = {
      if (integer.signum < 0) "_" + integer.negate.toString
      else integer.toString
    }
  }

  case class EInt(integer: BigInteger) extends EIntImpl with NoDepedencies
  case class DEInt(dependencies: MST, integer: BigInteger) extends EIntImpl

  case object EPack extends EPackImpl with NoDepedencies
  case class DEPack(dependencies: MST) extends EPackImpl

  def makePack(d: MST): EPackImpl = {
    if (d.isEmpty) EPack
    else DEPack(d)
  }
  trait EPackImpl extends UnaryOperator with NormalExpr {
    def addDependencies(d: MST) = makePack(d maximum dependencies)
    def reduce(arg1: Expr) = EPair(Empty,mmp(emptySet,emptyMMap put MapP(zero,arg1)))
    def asString = "^"
  }

  case object ECut extends ECutImpl with NoDepedencies
  case class DECut(dependencies: MST) extends ECutImpl

  def makeCut(d: MST): ECutImpl = {
    if (d.isEmpty) ECut
    else DECut(d)
  }
  
  trait ECutImpl extends UnaryOperator with NormalExpr {
    def addDependencies(d: MST) = makeCut(d maximum dependencies)
    def reduce(arg1: Expr) = arg1 match {
      case m: MMapImpl => {
        val s = sizem(m.m)
        val l1 = s / 2
        val (l,e,rr) = splitm(l1,m.m)
        val r = rr put e.get
        EPair(mmp(m.dependencies,l),mmp(m.dependencies,r))
      }
      case n: Number => {
        var i1 = n.split2
        var i2 = n.sub(i1)
        EPair(i2,i1)
      }
    }

    def asString = "/cut"
  }

  case object ETurn extends ETurnImpl with NoDepedencies
  case class DETurn(dependencies: MST) extends ETurnImpl

  def makeTurn(d: MST): ETurnImpl = {
    if (d.isEmpty) ETurn
    else DETurn(d)
  }
  
  trait ETurnImpl extends UnaryOperator with NormalExpr with UnaOpImp {
    def recreate(d: MST) = makeTurn(d)
    def reduceNumber(i: Number) = i
    def reduceMap(arg1: MMapImpl) = arg1 match {
      case m: MMapImpl => {
        var nm = emptyMMap
        var mm = m.m
        while (mm.first != None) {
          val p = mm.first.get
          p.second match {
            case s: MSetImpl=> {
              var ss = s.m
              while (ss.first != None) {
                val sp = ss.first.get
                if (ExprOrdering.compare(sp.first,one) == 0) { nm = nm put MapP(ss.first.get.second,p.first) }
                else { nm = nm put MapP(sp.second,makeMSet(s.dependencies,emptySet put SetP(sp.first,p.first))) }
                ss = ss.split(ss.first.get)._3
              }
            }
            case _ => {
              nm = nm put MapP(p.second,p.first)
            }
          }
          mm = mm.split(mm.first.get)._3
        }
        mmp(m.dependencies,nm)
      }
    }
    def asString = "%"
  }

  case object EIota extends EIotaImpl with NoDepedencies
  case class DEIota(dependencies: MST) extends EIotaImpl

  def makeIota(d: MST): EIotaImpl = {
    if (d.isEmpty) EIota
    else DEIota(d)
  }

  trait EIotaImpl extends UnaryOperator with NormalExpr with UnaOpImp {
    def recreate(d: MST) = makeIota(d)
    def reduceNumber(i: Number) = {
      var m = emptyMMap
      var ii = i.zero
      // TODO: sign
      while (ii.compare(i) < 0) {
        m = m put MapP(ii,ii)
        ii = ii.incr
      }
      mmp(i.dependencies,m)
    }
    def reduceMap(arg1: MMapImpl) = not_yet
    def asString = "~"
  }

  def makeMap(i: Number): MMapImpl = {
    if (i.compare(i.zero) == 0) mmp2(i.dependencies,emptyMMap)
    else {
      val sp = SetP(i,i.zero)
      val mp = MapP(i.zero,makeMSet(emptySet,emptyMSet put sp))
      val m = emptyMMap put mp
      mmp2(i.dependencies,m)
    }
  }

  case object ESwap extends ESwapImpl with NoDepedencies
  case class DESwap(dependencies: MST) extends ESwapImpl

  def makeSwap(d: MST): ESwapImpl = {
    if (d.isEmpty) ESwap
    else DESwap(d)
  }
  
  trait ESwapImpl extends BinaryOperator with NormalExpr {
    def addDependencies(dd: MST) = makeSwap(dependencies maximum dd)
    def reduce(arg1: Expr, arg2: Expr) = ETriple(arg2,arg1,Empty)
    def asString = "\\"
  }

  trait UnaOpImp extends UnaryOperator {
    def recreate(d: MST): UnaOpImp
    def addDependencies(dd: MST) = recreate(dependencies maximum dd)
    def reduceNumber(arg1: Number): Expr
    def reduceMap(arg1: MMapImpl): Expr
    def reduce(arg1: Expr) = (arg1) match {
      //case (a1: ELabeledExpr) => reduce(a1.second)
      case (a1: Number) => EPair(Empty,reduceNumber(a1).addDependencies(dependencies))
      case (a1: MMapImpl) => EPair(Empty,reduceMap(a1).addDependencies(dependencies))
    }
  }
  trait BinOpImp extends BinaryOperator {
    def recreate(d: MST): BinOpImp
    def addDependencies(dd: MST) = recreate(dependencies maximum dd)
    def reduceNumber(arg1: Number, arg2: Number): Expr
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl): Expr
    def reduce(arg1: Expr, arg2: Expr) = {
      ETriple(Empty,Empty,reduce2(arg1,arg2).addDependencies(dependencies))
    }
    def reduce2(arg1: Expr, arg2: Expr): Expr = (arg1,arg2) match {
      case (a1: Number,a2: Number) => reduceNumber(a1,a2)
      case (a1: Number,a2: MMapImpl) => reduce2(makeMap(a1),a2)
      case (a1: MMapImpl,a2: Number) => reduce2(a1,makeMap(a2))
      case (a1: MMapImpl,a2: MMapImpl) => reduceMap(a1,a2)
    }
  }

  object EBind extends EBindImpl with NoDepedencies
  case class DEBind(dependencies: MST) extends EBindImpl
  
  def makeBind(d: MST): EBindImpl = {
    if (d.isEmpty) EBind
    else DEBind(d)
  }
  
  trait EBindImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeBind(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg2 match {
      case m: MMapImpl => {
        var mm = m.m
        var a: Expr = arg1
        while (mm.first != None) {
          val p = mm.first.get
          a = a.bind(p.first,p.second)
          mm = mm.split(mm.first.get)._3
        }
        a.addDependencies(m.dependencies)
      }
    }
    def asString = "!"
  }


  case object EAdd extends EAddImpl with NoDepedencies
  case class DEAdd(dependencies: MST) extends EAddImpl

  def makeAdd(d: MST): EAddImpl = {
    if (d.isEmpty) EAdd
    else DEAdd(d)
  }

  trait EAddImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeAdd(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1 add arg2
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg1 multiAdd arg2
    def asString = "+"
  }

  case object EMul extends EMulImpl with NoDepedencies
  case class DEMul(dependencies: MST) extends EMulImpl

  def makeMul(d: MST): EMulImpl = {
    if (d.isEmpty) EMul
    else DEMul(d)
  }

  trait EMulImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeMul(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1 mul arg2
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg1 multiMul arg2
    def asString = "*"
  }

  case object EMax extends EMaxImpl with NoDepedencies
  case class DEMax(dependencies: MST) extends EMaxImpl

  def makeMax(d: MST): EMaxImpl = {
    if (d.isEmpty) EMax
    else DEMax(d)
  }


  trait EMaxImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeMax(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1 max arg2
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg1 multiMax arg2
    def asString = "|"
  }

  case object EMin extends EMinImpl with NoDepedencies
  case class DEMin(dependencies: MST) extends EMinImpl

  def makeMin(d: MST): EMinImpl = {
    if (d.isEmpty) EMin
    else DEMin(d)
  }


  trait EMinImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeMin(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1 min arg2
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg1 multiMin arg2
    def asString = "&"
  }


  case object ESub extends ESubImpl with NoDepedencies
  case class DESub(dependencies: MST) extends ESubImpl

  def makeSub(d: MST): ESubImpl = {
    if (d.isEmpty) ESub
    else DESub(d)
  }

  trait ESubImpl extends BinOpImp with NormalExpr {
    def recreate(d: MST) = makeSub(d)
    def reduceNumber(arg1: Number, arg2: Number) = arg1 sub arg2
    def reduceMap(arg1: MMapImpl, arg2: MMapImpl) = arg1 multiSub arg2
    def asString = "-"
  }

  lazy val emptyCompound = ece2(emptySet,emptyExpr)

  def sizem(mmt: MMT): Int = mmt.measure match {
    case None => 0
    case Some(x) => x.size
  }
  def splitm(n: Int, cex: MMT): (MMT,Option[MP],MMT) = {
    if (n < 0) (emptyMMap,None,cex)
    else if (n == 0) (emptyMMap,cex.first,cex.split(cex.first.get)._3)
    else if (n >= sizem(cex)) (cex,None,emptyMMap)
    else {
      val ls = sizem(cex.left)
      if (n == ls) (cex.left,cex.some,cex.right)
      else if (n < ls) { val (l,e,r) = splitm(n,cex.left) ; (l,e,r put cex.some.get add cex.right) }
      else { val (l,e,r) = splitm(n-ls-1,cex.right) ; (cex.left add l put cex.some.get,e,r) }
    }
  }

  def size(cex: CEX): Int = cex.measure match {
    case None => 0
    case Some(x) => x.size
  }
  def splite(n: Int, cex: CEX): (CEX,Option[EP],CEX) = {
    if (n < 0) (emptyExpr,None,cex)
    else if (n == 0) (emptyExpr,cex.first,cex.split(cex.first.get)._3)
    else if (n >= size(cex)) (cex,None,emptyExpr)
    else {
      val ls = size(cex.left)
      if (n == ls) (cex.left,cex.some,cex.right)
      else if (n < ls) { val (l,e,r) = splite(n,cex.left) ; (l,e,r put cex.some.get add cex.right) }
      else { val (l,e,r) = splite(n-ls-1,cex.right) ; (cex.left add l put cex.some.get,e,r) }
    }
  }


  case class DECompoundExpr(dependencies: MST, cex: CEX) extends ECompoundExprImpl
  case class ECompoundExpr(cex: CEX) extends ECompoundExprImpl with NoDepedencies

  trait ECompoundExprImpl extends CompoundExpr {
    def cex: CEX
    def addDependencies(dd: MST) = ece(dependencies maximum dd, cex)
    def bind(label: Expr, value: Expr) = {
      var nm = emptyExpr
      var mm = cex
      while (mm.first != None) {
        var p = mm.first.get.asInstanceOf[CompoundPair]
        nm = nm put p.bind(label,value)
        mm = mm.split(mm.first.get)._3
      }
      ece(dependencies,nm)
    }
    def isEmpty = cex.isEmpty
    def first: Option[EP] = if (left.isEmpty) some ; else left.first
    def last: Option[EP] = if (right.isEmpty) some ; else right.last
    def first2: Option[EP] = {
      if (left.isEmpty) { if (right.isEmpty) None ; else right.first }
      else left.first2 match { case None => some ; case x => x }
    }
    def last2: Option[EP] = {
      if (right.isEmpty) {  if (left.isEmpty) None ; else left.last}
      else right.last2 match {  case None => some ; case x => x }
    }
    def concat(e: ECompoundExprImpl): ECompoundExprImpl = {
      if (e.cex.isEmpty) this
      else if (isEmpty) e
      else ece2(dependencies maximum e.dependencies,cex add e.cex)
    }
    def put(e: Option[EP]): ECompoundExprImpl = e match {
      case None => this
      case Some(x) => put(x)
    }

    def split(n: Int): (ECompoundExprImpl,Option[EP],ECompoundExprImpl) = {
      val (l,e,r) = splite(n,cex)
      (ece2(dependencies,l),e,ece2(dependencies,r))
    }
    def put(x: EP): ECompoundExprImpl =  ece2(dependencies,cex add (emptyExpr put x))
    lazy val left: ECompoundExprImpl = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExprImpl) => {
          val l = e.left
          if (l.isEmpty) ece2(dependencies,cex.left)
          else ece2(dependencies,cex.left put CP(cex.some.get.first,l))
        }
        case _ => ece2(dependencies,cex.left)
      }
    }
    lazy val right: ECompoundExprImpl = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExprImpl) => {
          val r = e.right
          if (r.isEmpty) ece2(dependencies,cex.right)
          else ece2(dependencies,cex.right put CP(cex.some.get.first,r))
        }
        case _ => ece2(dependencies,cex.right)
      }
    }
    lazy val some: Option[EP] = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExprImpl) => {
          val nn = e.some
          nn match {
            case None => None
            case Some(x) => {
              val ee = ece(e.dependencies,emptyExpr put x)
              Some(CP(cex.some.get.first,ee))
            }
          }
        }
        case _ => {
          cex.some match {
            case None => None
            case Some(x) => {
              val ee = CP(x.first,x.second.addDependencies(dependencies))
              Some(ee)
            }
          }
        }
      }
    }

    def bindings = not_yet
    def wipe = this
    def isRedex = cex.measure match {
      case None => false
      case Some(x) => x.isRedex
    }

    def reduce = {
      val r = reduce3
      //if (r.isEmpty) Empty
      //else r
      r
    }

    def cp(i: Number, p: Expr): CompoundPair = CP(i,p)

    def flatten(a: Option[Expr]): Option[Expr] = a match {
      case Some(ee: ECompoundExprImpl) => {
        flatten(ee.first.flatMap(e => Some(e.second.addDependencies(ee.dependencies))))
      }
      case Some(e: ELabeledExprImpl) => {
        flatten(Some(e.second.addDependencies(emptySet put SetP(one,e.first))))
      }
      case Some(e: ETraceImpl) => flatten(e.t.last.flatMap(e => Some(e.second)))
      case _ => a
    }

    def re(e: Option[EP], n: Expr): EP = e match {
      case None => sys.error("illegal state")
      case Some(ep) => ep.second match {
        case cc: ECompoundExprImpl => {
          val k = ece(cc.dependencies,emptyExpr put re(cc.first,n))
          CP(ep.first,k)
        }
        case _ => CP(ep.first,n)
      }
    }

    def reduce2(first: Option[EP], second: Option[EP]): (ECompoundExprImpl,ECompoundExprImpl) = {
      val f = first.flatMap(l => Some(l.second))
      val s = second.flatMap(l => Some(l.second))

      (flatten(f),flatten(s)) match {
        case (Some(a1), Some(a2: MultiSet)) => {
          val p = mconcat(Some(a1),Some(a2))
          val c = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,p))
          val r = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,Empty))
          (r,c concat c)
        }
        case (Some(a1: MultiSet),Some(a2)) => {
          val p = mconcat(Some(a1),Some(a2))
          val c = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,p))
          val r = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,Empty))
          (r,c concat c)
        }
        case _ => sys.error("illegal state")
      }
    }

    def reduce(first: Option[EP], second: Option[EP], third: Option[EP]): (ECompoundExprImpl,ECompoundExprImpl) = {
      val f = first.flatMap(l => Some(l.second))
      val s = second.flatMap(l => Some(l.second))
      val t = third.flatMap(l => Some(l.second))

      (flatten(f),flatten(s),flatten(t)) match {
        case (Some(a1: Arg),Some(u: UnaryOperator),_) => {
          val p = u.reduce(a1)
          val c = ece2(emptySet,emptyExpr put re(first,p.first) put re(second,p.second))
          val r = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,Empty))
          (r,c concat c)
        }
        case (_,Some(a1: Arg),Some(u: UnaryOperator)) => {
          val p = u.reduce(a1)
          val c = ece2(emptySet,emptyExpr put re(second,p.first) put re(third,p.second))
          val r = ece2(emptySet,emptyExpr put re(second,Empty) put re(third,Empty))
          (r,c concat c)
        }

        case (Some(a1: Arg),Some(a2: Arg),Some(u: BinaryOperator)) => {
          val p = u.reduce(a1,a2)
          val c = ece2(emptySet,emptyExpr put re(first,p.first) put re(second,p.second) put re(third,p.third))
          val r = ece2(emptySet,emptyExpr put re(first,Empty) put re(second,Empty) put re(third,Empty))
          (r,c concat c)
        }
        case _ => sys.error("illegal state")
      }
    }

    def econcat(a: Expr, b: Expr): Expr = (a,b) match {
      case (e1: ECompoundExprImpl, e2: ECompoundExprImpl) => {
        val p1 = CP(zero,e1)
        val p2 = CP(one,e2)
        val e = emptyExpr put p1 put p2
        ece(e1.dependencies maximum e2.dependencies,e)
      }

      case (e1: ECompoundExprImpl, a2: Expr) => {
        val p = e1.last.get
        val i = p.first.incr
        e1 put CP(i,a2)
      }
      case (a1: Expr, e2: ECompoundExprImpl) => {
        val p = e2.first.get
        val i = p.first.decr
        e2 put CP(i,a1)
      }
      case (e1,e2) => {
        val p1 = CP(zero,e1)
        val p2 = CP(one,e2)
        val e = emptyExpr put p1 put p2
        ece(emptySet,e)
      }
    }

    def mconcat(f: Option[Expr], s: Option[Expr]): Expr = {

      (flatten(f),flatten(s)) match {
        case (Some(as: MSetImpl), Some(bs: MSetImpl)) => {
          var a = as.m
          var e: MST = emptyMSet
          while (a.first != None) {
            var b = bs.m
            while (b.first != None) {
              val p1: SP = a.first.get
              val p2: SP = b.first.get
              val ec = econcat(p1.second,p2.second)
              val m = ece(emptySet,emptyExpr put CP(zero,p1.first) put CP(one,p2.first) put CP(two,makeMul(emptySet)))
              e = e put SetP(fullReduce(m),fullReduce(ec))
              b = b.split(b.first.get)._3
            }
            a = a.split(a.first.get)._3
          }
          makeMSet(as.dependencies maximum bs.dependencies,e)
        }
        case (Some(as: MSetImpl), Some(b)) => {
          var a = as.m
          var e: MST = emptyMSet
          while (a.first != None) {
            val p: SP = a.first.get
            val ec = econcat(p.second,b)
            e = e put SetP(p.first,fullReduce(ec))
            a = a.split(a.first.get)._3
          }
          makeMSet(as.dependencies,e)
        }
        case (Some(b), Some(as: MSetImpl)) => {
          var a = as.m
          var e: MST = emptyMSet
          while (a.first != None) {
            val p: SP = a.first.get
            val ec = econcat(b,fullReduce(p.second))
            e = e put SetP(p.first,ec)
            a = a.split(a.first.get)._3
          }
          makeMSet(as.dependencies,e)
        }
        case _ => sys.error("illegal state")
      }
    }

    def typ2(e: Option[EP]) = e.flatMap(e => Some(e.second))

    def reduce3: ECompoundExprImpl = cex.measure match {
      case None => this
      case Some(x) => {
        if (1 == 1) { //x.isRedex) {
        // TODO: optimize to cut and paste, given specific positions
        val l = left
          val x = some
          val r = right

          val ll = l.last
          var rf = r.first
          val ll2 = l.last2
          val rf2 = r.first2

          val (re1,re2) = {
            if (qredexp(ll2,ll,x))reduce(ll2,ll,x)
            else if (qredexp(ll,x,rf)) reduce(ll,x,rf)
            else if (qredexp(x,rf,rf2)) reduce(x,rf,rf2)
            else if (mredexp(ll,x)) reduce2(ll,x)
            else if (mredexp(x,rf)) reduce2(x,rf)
            else (emptyCompound,emptyCompound)
          }
          val lll: ECompoundExprImpl = (l concat re1).reduce3
          val xxx: ECompoundExprImpl = emptyCompound.put(x) concat re1
          val rrr: ECompoundExprImpl = (r concat re1).reduce3
          val p = lll concat xxx concat rrr

          p concat re2
        }
        else this
      }
    }
  }

  type EP = MPair[Number,Expr]
  type SP = MPair[Expr,Expr]
  type MP = MPair[Expr,Expr]
  type TP = MPair[Number,Expr]

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  type SS[N,X,M] =  SeqImpl[N,X,M, IWBTree[N,X,M], IWBTreeContext[N,X,M]]

  type CEX = ST[EP,ExprMeasure,Int]
  type MST = ST[SP,SetMeasure,Int]
  type MMT = ST[MP,MapMeasure,Int]
  type TTT = ST[TP,TraceMeasure,Int]
  type SEQ = SS[Int,Expr,ExprSeqMeasure]
  type PAT = SS[Int,Path,AnyRef]

  val emptyExprSeq: SEQ = EmptySeqImpl(ExprSeqContext)
  val emptyPath: PAT = EmptySeqImpl(PathContext)

  val emptyExpr: CEX = EmptyOrderedISet(ExprTreapContext)
  val emptyMSet: MST = EmptyOrderedISet(MSetTreapContext)
  val emptyMMap: MMT = EmptyOrderedISet(MMapTreapContext)
  val emptyTrace: TTT = EmptyOrderedISet(TraceTreapContext)

  case class TraceP(first: Number, second: Expr) extends TP {
    def neg = not_yet
    def add(o: TP): Option[TP] = not_yet
    def subtract(o: TP): Option[TP] = not_yet
    def mul(o: TP): Option[TP] = not_yet
    def max(o: TP): Option[TP] = not_yet
    def min(o: TP): Option[TP] = not_yet
  }

  case class ExprMeasure(size: Int, flattened: SEQ) {
    def isRedex: Boolean = flattened.measure match {
      case Some(x) => x.isRedex
      case None => false
    }
  }

  def sredex(o: Option[EP]): Option[Expr] = o.flatMap(l => Some(l.second))
  def tredex(o: Option[EP]): ExprType = typ(sredex(o))
  def mredexa(first: Option[Expr], second: Option[Expr]): Boolean = (typ(first),typ(second)) match {
    case (s: SomeExpr,MultiExpr) => true
    case (MultiExpr,s: SomeExpr) => true
    case _ => false
  }

  def mredexp(first: Option[EP], second: Option[EP]): Boolean = mredexa(sredex(first),sredex(second))
  def qredexp(first: Option[EP], second: Option[EP], third: Option[EP]): Boolean = {
    qredexa(sredex(first),sredex(second),sredex(third))
  }

  def qredexa(first: Option[Expr], second: Option[Expr], third: Option[Expr]): Boolean = {
    (typ(first),typ(second),typ(third)) match {
      case (ArgExpr,UnaExpr,_) => true
      case (_,ArgExpr,UnaExpr) => true
      case (ArgExpr,ArgExpr,BinExpr) => true
      case _ => false
    }
  }

  trait ExprType

  trait SomeExpr extends ExprType

  case object NoExpr extends ExprType
  case object UnaExpr extends ExprType with SomeExpr
  case object BinExpr extends ExprType with SomeExpr
  case object ArgExpr extends ExprType with SomeExpr
  case object MultiExpr extends ExprType with SomeExpr
  case object SymbolExpr extends ExprType with SomeExpr
  case object NoArgExpr extends ExprType

  def typ(v: Expr): ExprType = v match {
    case o: UnaryOperator => UnaExpr
    case b: BinaryOperator => BinExpr
    case m: MMapImpl => ArgExpr
    case c: ECompoundExprImpl => typ(c.first.flatMap(f => Some(f.second)))
    case l: ELabeledExprImpl => typ(l.second)
    case t: ETraceImpl => typ(t.t.last.get.second)
    case m: MSetImpl => MultiExpr
    case a: Arg => ArgExpr
    case s: SymbolImpl => SymbolExpr
    case _ => NoArgExpr
  }
  def typ(v: Option[Expr]): ExprType = v match {
    case None => NoExpr
    case Some(x) => typ(x)
  }

  object MSetOrdering extends Ordering[SP] {
    def compare(x1: SP, x2: SP) = ExprOrdering.compare(x1.second,x2.second)
  }

  case class SetMeasure(size: Int)

  object MSetTreapContext extends STreapContextImpl[SP,SetMeasure,Int] {
    type SS = STreap[SP,SetMeasure,Int]

    def compareOrder(x1: SP, x2: SP): Int = MSetOrdering.compare(x1,x2)
    def priorityHash(x: Option[SP]): Int = {
      val xx = x.get
      jenkinsHash(ExprHashing.hash(xx.second))
    }
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)
    def size(e: Option[SetMeasure]) = e match {
      case None => 0
      case Some(x) => x.size
    }
    override def measure(l: SS, x: Option[SP], r: SS): Option[SetMeasure] = {
      val s = size(l.measure) + size(r.measure) + 1
      Some(SetMeasure(s))
    }
    val ms = MSetOperations[Expr,Expr]()

    type SOT = SetOperation[SP,SetMeasure,STreap[SP,SetMeasure,Int],STreapContext[SP,SetMeasure,Int]]

    val addV: SOT = ms.LeftMultiSetSumUnion()
    val mulV: SOT = ms.LeftMultiSetMultiply()
    val maxV: SOT = ms.LeftMultiSetUnion()
    val minV: SOT = ms.LeftMultiSetIntersect()
    val subV: SOT = ms.LeftMultiSetDifference()

    override def add = addV
    override def multiply = mulV
    override def maximum = maxV
    override def minimum = minV
    override def subtract = subV
  }

  case class MapMeasure(size: Int)

  object MMapTreapContext extends STreapContextImpl[MP,MapMeasure,Int] {
    type SS = STreap[MP,MapMeasure,Int]

    def compareOrder(x1: MP, x2: MP): Int = ExprOrdering.compare(x1.first,x2.first)

    def priorityHash(x: Option[MP]): Int = {
      val xx = x.get
      jenkinsHash(ExprHashing.hash(xx.first))
    }
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    def size(e: Option[MapMeasure]) = e match {
      case None => 0
      case Some(x) => x.size
    }
    override def measure(l: SS, x: Option[MP], r: SS): Option[MapMeasure] = {
      val s = size(l.measure) + size(r.measure) + 1
      Some(MapMeasure(s))
    }

    val ms = MSetOperations[Expr,Expr]()

    type SOT = SetOperation[MP,MapMeasure,STreap[MP,MapMeasure,Int],STreapContext[MP,MapMeasure,Int]]

    val addV: SOT = ms.LeftMultiSetSumUnion()
    val mulV: SOT = ms.LeftMultiSetMultiply()
    val maxV: SOT = ms.LeftMultiSetUnion()
    val minV: SOT = ms.LeftMultiSetIntersect()
    val subV: SOT = ms.LeftMultiSetDifference()

    override def add = addV
    override def multiply = mulV
    override def maximum = maxV
    override def minimum = minV
    override def subtract = subV
  }

  trait ExprSeqMeasure {
    def isRedex: Boolean
  }

  object NormalMeasure extends ExprSeqMeasure { val isRedex = false }
  object RedexMeasure extends ExprSeqMeasure { val isRedex = true }

  def isRedex2(m: Option[ExprSeqMeasure]): Boolean = m match {
    case Some(e: ExprSeqMeasure) => e.isRedex
    case None => false
  }
  object PathContext extends IWBTreeContextImpl[Int,Path,AnyRef] {
    type SS = IWBTree[Int,Path,AnyRef]

    def sizing = IntNum
    def compareOrder(x1: Path, x2: Path): Int = not_yet
  }

  object ExprSeqContext extends IWBTreeContextImpl[Int,Expr,ExprSeqMeasure] {
    type SS = IWBTree[Int,Expr,ExprSeqMeasure]

    def sizing = IntNum
    def compareOrder(x1: Expr, x2: Expr): Int = not_yet

    override def measure(l: SS, r: SS): Option[ExprSeqMeasure] = {
      val lr = isRedex2(l.measure)
      val rr = isRedex2(r.measure)
      if (lr || rr) Some(RedexMeasure)
      else {
        if (qredexa(l.last2,l.last,r.first) || qredexa(l.last,r.first,r.first2) | mredexa(l.last,r.first)) {
          Some(RedexMeasure)
        }
        else Some(NormalMeasure)
      }
    }
  }

  trait TraceMeasure

  object TraceTreapContext extends STreapContextImpl[TP,TraceMeasure,Int] {
    type SS = STreap[TP,TraceMeasure,Int]

    def compareOrder(x1: TP, x2: TP): Int = ExprOrdering.compare(x1.first,x2.first)
    def priorityHash(x: Option[TP]): Int = ExprHashing.hash(x.get.first)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val ms = MSetOperations[Number,Expr]()

    val addV: SetOperation[TP,TraceMeasure,STreap[TP,TraceMeasure,Int],STreapContext[TP,TraceMeasure,Int]] = ms.LeftMultiSetSumUnion()
    override def add = addV
  }

  object ExprTreapContext extends STreapContextImpl[EP,ExprMeasure,Int] {
    type SS = STreap[EP,ExprMeasure,Int]

    def compareOrder(x1: EP, x2: EP): Int = ExprOrdering.compare(x1,x2)
    def priorityHash(x: Option[EP]): Int = ExprHashing.hash(x.get.first)
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val normal = Some(false)
    val redex = Some(true)

    def size(o: Option[ExprMeasure]): Int = o match {
      case None => 0 ; case Some(x) => x.size
    }

    def flattened(o: Option[ExprMeasure]): SEQ = o match {
      case None => emptyExprSeq ; case Some(x) => x.flattened
    }

    def flattened(p: EP): SEQ = flattened(p.second)


    def flattened(p: Expr): SEQ = p match {
      case e: ECompoundExprImpl =>  e.cex.measure match {
        case None => emptyExprSeq
        case Some(x) => x.flattened
      }
      case e: ELabeledExprImpl => flattened(e.second)
      case e: ETraceImpl => {
        flattened(e.t.last.get.second)
      }
      case s => emptyExprSeq add s
    }

    def rd(o: Option[Boolean]): Boolean = o match {
      case None => false ; case Some(x) => x
    }

    override def measure(l: SS, x: Option[EP], r: SS): Option[ExprMeasure] = {
      val lm = l.measure
      val rm = r.measure
      val s = size(lm) + size(rm) + 1
      val f = flattened(lm) append flattened(x.get) append flattened(rm)
      Some(ExprMeasure(s,f))
    }

    val ms = MSetOperations[Number,Expr]()

    val addV: SetOperation[EP,ExprMeasure,STreap[EP,ExprMeasure,Int],STreapContext[EP,ExprMeasure,Int]] = ms.LeftMultiSetSumUnion()
    override def add = addV
  }

  object ExprHashing extends PriorityHasher[Expr] {
    import Hashing.jenkinsHash
    def hash(s1: Expr): Int = s1 match {
      case Empty => jenkinsHash(0)
      case i: EIntImpl => jenkinsHash(jenkinsHash(i.integer.hashCode))
      case m: MSetImpl => jenkinsHash(m.m.hashCode + -398127)
      case m: MMapImpl=> jenkinsHash(m.m.hashCode + 1283173)
      case s: SymbolImpl => jenkinsHash(s.s.hashCode)
      case f: EForeachImpl => jenkinsHash(hash(f.a) ^ 5817264)
      case f: EFoldImpl => jenkinsHash(jenkinsHash(hash(f.a) ^ 49517382))
      case u: Operator => jenkinsHash(u.toString.hashCode)
      case c: ECharImpl => jenkinsHash(c.c.toString.hashCode ^ 234091802)
      case c: ECompoundExprImpl => jenkinsHash(c.cex.hashCode + 7124568)
      case l: LabeledExpr =>  jenkinsHash(hash(l.first) ^ hash(l.second))
      case t: ETraceImpl => jenkinsHash(t.t.hashCode ^ 94122681)
    }
  }

  object ExprOrdering extends Ordering[EP] {
    def compare(v1: EP, v2: EP): Int = compare(v1.first,v2.first)
    def compare(v1: Expr, v2: Expr): Int = {
      val o1 = order(v1)
      val o2 = order(v2)
      if (o1 == o2) compareEqual(v1,v2)
      else o1 - o2
    }
    def order(v1: Expr): Int = v1 match {
      case Empty => -1
      case i: Number => 0
      case c: ECharImpl => 1
      case f: EFoldImpl => 2
      case f: EForeachImpl => 3
      case c: CompoundExpr => 4
      case l: SymbolImpl => 5
      case ms: MSetImpl => 6
      case mm: MMapImpl => 7
      case lb: ELabeledExprImpl => 8
      case t: ETraceImpl => 9
      case u: UnaryOperator => 10
      case b: BinaryOperator => 11
    }
    def compareEqual(v1: Expr, v2: Expr) = (v1,v2) match {
      case (Empty,Empty) => 0
      case (f1: EFoldImpl, f2: EFoldImpl) => compare(f1.a,f2.a)
      case (f1: EForeachImpl, f2: EForeach) => compare(f1.a,f2.a)
      case (n1: Number, n2: Number) => n1.compare(n2)
      case (c1: ECharImpl, c2: ECharImpl) => c1.c.compare(c2.c)
      case (u1: Operator, u2: Operator) => u1.asString.compareTo(u2.asString)
      case (s1: SymbolImpl, s2: SymbolImpl) => s1.s.compareTo(s2.s)
      case (e1: ECompoundExprImpl, e2: ECompoundExprImpl) => compareCompoundExpr(e1.cex,e2.cex)
      case (s1: MSetImpl, s2: MSetImpl) => compareMSet(s1.m,s2.m)
      case (m1: MMapImpl, m2: MMapImpl) => compareMMap(m1.m,m2.m)
      case (t1: ETraceImpl, t2: ETraceImpl) => compareTrace(t1.t,t2.t)
      case (l1: ELabeledExprImpl, l2: ELabeledExprImpl) => {
        val c = compare(l1.first,l2.first)
        if (c == 0) compare(l1.second,l2.second) ; else c
      }
    }
    def compareCompoundExpr(c1: CEX, c2: CEX): Int = {
      // TODO: more efficient comparison
      // TODO: make abstract and factor out common functionality to be used for compareMSet
      (c1.some, c2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = c1.first.get
          val f2 = c2.first.get
          val c = compare(f1.second,f2.second)
          if (c != 0)  c
          else compareCompoundExpr(c1.split(f1)._3,c2.split(f2)._3)
        }
      }
    }
    def compareTrace(t1: TTT, t2: TTT): Int = {
      // TODO: more efficient comparison
      (t1.some, t2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = t1.first.get
          val f2 = t2.first.get
          val c = ExprOrdering.compare(f1.first,f2.first)
          if (c != 0)  c
          else {
            val c = ExprOrdering.compare(f1.second,f2.second)
            if (c != 0) c
            else compareTrace(t1.split(f1)._3,t2.split(f2)._3)
          }
        }
      }
    }
    def compareMSet(c1: MST, c2: MST): Int = {
      // TODO: more efficient comparison
      (c1.some, c2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = c1.first.get
          val f2 = c2.first.get
          val c = MSetOrdering.compare(f1,f2)
          if (c != 0)  c
          else compareMSet(c1.split(f1)._3,c2.split(f2)._3)
        }
      }
    }
    def compareMMap(m1: MMT, m2: MMT): Int = {
      // TODO: more efficient comparison
      (m1.some, m2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = m1.first.get
          val f2 = m2.first.get
          val c = ExprOrdering.compare(f1.first,f2.first)
          if (c != 0)  c
          else {
            val c = ExprOrdering.compare(f1.second,f2.second)
            if (c != 0) c
            else compareMMap(m1.split(f1)._3,m2.split(f2)._3)
          }
        }
      }
    }
  }

  case class MSetOperations[A <: Expr,B <: Expr]() {
    type PP = MPair[A,B]

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
          if (e == None) l join r
          else c2.create(l,e,r)
        }
      }
      def neg(s: SS)(implicit c: CC): (SS,CC) = s.some match {
        case None => (s,c)
        case Some(x) => {
          val (l,c1) = neg(s.left)(c)
          val (r,c2) = neg(s.right)(c1)
          val e = x.neg
          if (e == None) l join r
          else c2.create(l,e,r)
        }
      }
      def twice(s: SS)(implicit c: CC): (SS,CC) = s.some match {
        case None => (s,c)
        case Some(x) => {
          val (l,c1) = twice(s.left)(c)
          val (r,c2) = twice(s.right)(c1)
          val e = x add x
          if (e == None) l join r
          else c2.create(l,e,r)
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
      def combineEqualElements(x1: PP, x2: PP): Option[PP] = x1 max x2
    }


    trait MultiSetSumUnion[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
      extends MultiSetOperationImpl[M,SS,CC] with DifferenceUnion[M,SS,CC] {
      def combineEqual(s1: SS, s2: SS)(implicit c: CC) = twice(s1)
      def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
      def combineEqualElements(x1: PP, x2: PP): Option[PP] = x1 add x2
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
      def combineEqualElements(x1: PP, x2: PP): Option[PP] = x1 min x2
    }

    trait MultiSetMultiply[M,SS <: SISetImpl[PP,M,SS,CC], CC <: SISetContextImpl[PP,M,SS,CC]]
      extends MultiSetOperationImpl[M,SS,CC] {
      def combineEqual(s1: SS, s2: SS)(implicit c: CC) = square(s1)
      def combineJoin(s1: SS, s2: SS)(implicit c: CC) = c.empty
      def combineLeftElement(left: PP): Option[PP] = None
      def combineRightElement(right: PP): Option[PP] = None
      def combineEqualElements(x1: PP, x2: PP): Option[PP] = x1 mul x2
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
      def combineJoin(s1: SS, s2: SS)(implicit c: CC) = {
        val (r,c1) = neg(s2)
        s1.join(r)(c1)
      }
      def combineEqualElements(x1: PP, x2: PP): Option[PP] =  x1 subtract x2
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

  def asInt(n: Number): Int = {
    n match {
      case i: EIntImpl => i.integer.toString.toInt
      case _ => sys.error("illegal state")
    }
  }
  trait Path {
    def asString: String
    def size: Int
  }

  case class LeafPath(index: Number) extends Path {
    def asString = index.toString
    def size = 1
  }
  case class CompoundPath(c: PAT) extends Path {
    def size = c.size
    def asString: String = {
      "<" + asString(c) + ">"
    }
    def asString(e: PAT): String = {
      if (e.size == 0) ""
      else if (e.size == 1) { e.first.get.asString }
      else {
        val l = e.left
        var r = e.right
        if (l.size == 0) asString(r)
        else {
          if (r.size == 0) asString(l)
          else asString(l) + "," + asString(r)
        }
      }
    }
  }

}