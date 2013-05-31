package spread

object Engine_v3 {
  import AbstractImmutableOrderedSet._
  import OrderedSetImplementation._
  import SequenceImplementation._
  import OrderedTreapSet._
  import WeightBalancedSequence._
  import Hashing._

  trait Expr {
    def labels: MultiSet
    def bind(label: Expr, value: Expr): Expr
    def bindings: MultiMap
    def reduce: Expr
    def wipe: Expr
    def isRedex: Boolean
    def asString: String
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

  def mmp(m: MMT): Expr = {
    if (m.isEmpty) zero
    else MMap(m)
  }

  case class MMap(m: MMT) extends MultiMap {
    def labels = not_yet
    def bind(label: Expr, value: Expr) = {
      var nm = emptyMMap
      var mm = m
      while (nm.first == None) {
        var p = mm.first.get
        nm = nm put MapP(p.first.bind(label,value),p.second.bind(label,value))
        mm = mm.split(nm.first.get)._3
      }
      MMap(nm)
    }

    def bindings = not_yet
    def reduce = not_yet
    def wipe = not_yet
    def isRedex = false

    def multiAdd(o: MultiMap) = o match { case MMap(mm) => mmp(m add mm) }
    def multiSub(o: MultiMap) = o match { case MMap(mm) => mmp(m subtract mm) }
    def multiMul(o: MultiMap) = o match { case MMap(mm) => mmp(m multiply mm) }
    def multiMax(o: MultiMap) = o match { case MMap(mm) => mmp(m maximum mm) }
    def multiMin(o: MultiMap) = o match { case MMap(mm) => mmp(m minimum mm) }

    def isString: Boolean = {
      var e1 = m
      var i = zero
      var seq = true
      var str = true
      while ((e1.first != None) && seq && str) {
        seq = ExprOrdering.compare(e1.first.get.first,i) == 0
        str = e1.first.get.second match {
          case e: EChar => true
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

    def doChar(e: Expr): String = e match {
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
        val p: MP = e1.first.get
        val vv = {
          if (str) doChar(p.second)
          else if (seq) ssubexpr(p.second)
          else p.second.asString
        }
        if (ExprOrdering.compare(p.first,p.second) == 0) r = r + vv
        else {
          if (seq || str) r = r + vv
          else r = r + (p.first.asString+el2+vv)
        }
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      r + et
    }
  }
  case class MSet(m: MST) extends MultiSet {
    def isEmpty = m.isEmpty
    def labels = not_yet
    def bind(label: Expr, value: Expr) = {
      var nm = emptyMSet
      var mm = m
      while (nm.first == None) {
        var p = mm.first.get
        nm = nm put SetP(p.first.bind(label,value),p.second.bind(label,value))
        mm = mm.split(nm.first.get)._3
      }
      MSet(nm)
    }
    def bindings = not_yet
    def reduce = not_yet
    def wipe = not_yet
    def isRedex = false
    def asString = {
      var ex = m
      var ii = 0
      var s = "{"
      while (ex.first != None) {
        if (ii > 0) s = s + ","
        val ix  = ex.first.get
        s = s + ix.asString
        ii = ii + 1
        ex = ex.split(ex.first.get)._3
      }
      s +"}"
    }
    def multiAdd(o: MultiSet) = o match { case MSet(mm) => MSet(m add mm) }
    def multiSub(o: MultiSet) = o match { case MSet(mm) => MSet(m subtract mm) }
    def multiMul(o: MultiSet) = o match { case MSet(mm) => MSet(m multiply mm) }
    def multiMax(o: MultiSet) = o match { case MSet(mm) => MSet(m maximum mm) }
    def multiMin(o: MultiSet) = o match { case MSet(mm) => MSet(m minimum mm) }
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
    def add(o: MPair[A,B]): Option[MPair[A,B]]
    def mul(o: MPair[A,B]): Option[MPair[A,B]]
    def max(o: MPair[A,B]): Option[MPair[A,B]]
    def min(o: MPair[A,B]): Option[MPair[A,B]]
    def subtract(o: MPair[A,B]): Option[MPair[A,B]]

    def asString: String
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
   }

  val zero: Number = EInt(0)
  val one: Number = EInt(1)
  val two: Number = EInt(2)
  val three: Number = EInt(3)

  case class SetP(first: Expr, second: Expr) extends MultiSetPair {
    def op(o: MPair[Expr,Expr], bo: BinaryOperator) = {
      val i = emptyExpr put CP(zero,first) put CP(one,o.first) put CP(three,bo)
      val e = fullReduce(ECompoundExpr(i))
      if (ExprOrdering.compare(e,zero) == 0) None
      else Some(SetP(e,second))
    }
    def add(o: MPair[Expr,Expr]) = op(o,EAdd)
    def mul(o: MPair[Expr,Expr]) = op(o,EMul)
    def max(o: MPair[Expr,Expr]) = op(o,EMax)
    def min(o: MPair[Expr,Expr]) = op(o,EMin)
    def subtract(o: MPair[Expr,Expr]) = op(o,ESub)
    def asString = {
      if (ExprOrdering.compare(first,one) == 0) second.asString
      else first.asString + ":" + second.asString
    }
  }

  def asMSet(o: Expr): MSet = o match {
    case m: MSet => m
    case _ => MSet(emptyMSet put SetP(one,o))
  }

  def mp(m: MapP): Option[MapP] = m.second match {
    case ms: MSet => {
      if (ms.isEmpty) None
      else Some(m)
    }
    case _ => Some(m)
  }

  case class MapP(first: Expr, second: Expr) extends MultiMapPair {
    def add(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiAdd asMSet(o.second)))
    def mul(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMul asMSet(o.second)))
    def max(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMax asMSet(o.second)))
    def min(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiMin asMSet(o.second)))
    def subtract(o: MPair[Expr,Expr]) = mp(MapP(first,asMSet(second) multiSub asMSet(o.second)))

    def asString = {
      if (ExprOrdering.compare(first,second) == 0) second.asString
      else first.asString + "=" + second.asString
    }
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

  case object EConcat extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = reduceMap(makeMap(arg1),makeMap(arg2))
    def reduceMap(arg1: MMap, arg2: MMap) = (arg1,arg2) match {
      case (MMap(m1),MMap(m2)) => {
        val (i,s1) = makeSeq(m1,zero)
        val (i2,s2) = makeSeq(m2,i)
        mmp(s1 maximum s2)
      }
    }
    def asString = ";+"
  }

  case class ELabeledExpr(first: Expr, second: Expr) extends LabeledExpr with NormalExpr {
    def bind(label: Expr, value: Expr) = {
      if (ExprOrdering.compare(first,label) == 0) ELabeledExpr(first,value)
      else ELabeledExpr(first.bind(label,value),second.bind(label,value))
    }
    def asString = {
      if (second == Empty) first.asString + "'"
      else first.asString + "'" + second.asString
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
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
  }

  trait NoBind extends Expr {
    def bind(label: Expr, value: Expr) = this
  }

  case object Empty extends NormalExpr with NoBind {
    def asString = "empty"
  }

  trait CompoundPairImpl extends CompoundPair {
    def add(o: EP): Option[EP] = {
      (second,o.second) match {
        case (e1: ECompoundExpr, e2: ECompoundExpr) => {
          val c = e1 concat e2
          if (c.isEmpty) None ; else Some(CP(first,c))
        }
        case (Empty,_) => None
        case (_,Empty) => None
        case (e1: ECompoundExpr, _) => if (e1.isEmpty) None ;  else Some(o)
        case (_,e2: ECompoundExpr) =>  if (e2.isEmpty) None ; Some(o)
        case _ => Some(o)
      }
    }
    def subtract(o: EP): Option[EP] = not_yet
    def mul(o: EP): Option[EP] = not_yet
    def max(o: EP): Option[EP] = not_yet
    def min(o: EP): Option[EP] = not_yet
  }

  def not_yet = sys.error("not yet")

  case class CP(f: Number, s: Expr) extends CompoundPairImpl {
    def bind(label: Expr, value: Expr) = CP(f,s.bind(label,value))
    def first = f
    def label = s match {
      case e: ELabeledExpr => e.first
      case _ => Empty
    }
    def labeledValue = s match {
      case e: ELabeledExpr => e.second
      case _ => s
    }
    def second = labeledValue
    def asString = subexpr(s)
  }

  case class Symbol(s: String) extends NoBind {
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = s
  }

  trait Arg extends NormalExpr

  case class EChar(c: Char) extends NoBind {
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = "\"" + c + "\""
  }

  def simplify(ee: Expr): Expr = ee match {
    case e: ECompoundExpr => {
      if (e.left.isEmpty && e.right.isEmpty) {
        e.some match {
          case None => ee
          case Some(CP(_,e: ELabeledExpr)) => e
          case Some(p) => simplify(p.second)
        }
      }
      else ee
    }
    case _ => ee
  }

  def fullReduce(e: Expr): Expr = {
    var ee: Expr = e
    while (ee.isRedex) ee = ee.reduce
    simplify(ee)
  }

  case class EForeach(a: Expr) extends UnaOpImp with UnaryOperator {
    def reduceNumber(i: Number) = makeMap(i)
    def reduceMap(arg1: MMap) = arg1 match {
      case MMap(m) =>  MMap(foreach(m))
    }
    def foreach(m: MMT): MMT = {
      if (m.isEmpty) m
      else {
        val e = m.some.get
        val c = emptyCompound put CP(zero,e.second) put CP(one,a)
        val c1 = fullReduce(c)
        foreach(m.left) put MapP(e.first,c1) add foreach(m.right)
      }
    }
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = "@" + subexpr(a)
  }

  def log2(i: Int): Int = {
    var l = 1; var ii = i
    while (ii > 1) { l = l * 2 ; ii = ii / 2 }
    l / 2
  }

  case class EFold(a: Expr) extends UnaOpImp with UnaryOperator {
    def reduceNumber(i: Number) = makeMap(i)
    def reduceMap(arg1: MMap) = arg1 match {
      case MMap(m) => val (f,_) = fold(m,zero) ; fullReduce(ECompoundExpr(f))
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
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = a match {
      case Empty => "."
      case _ => "." + subexpr(a)
    }
  }

  case class EInt(i: Int) extends Number with Arg with NoBind {
    def create(i: Int) = EInt(i)
    def zero: Number = create(0)
    def one: Number = create(1)
    def incr: Number = create(i+1)
    def decr: Number = create(i-1)
    def split2: Number = create(i/2)
    def add(n: Number): Number = n match { case EInt(n2) => create(i + n2) }
    def mul(n: Number): Number = n match { case EInt(n2) => create(i * n2) }
    def min(n: Number): Number = n match { case EInt(n2) => create(i min n2) }
    def max(n: Number): Number = n match { case EInt(n2) => create(i max n2) }
    def sub(n: Number): Number = n match { case EInt(n2) => create(i - n2) }
    def compare(n: Number): Int = n match { case EInt(n2) => i - n2 }
    def asString = {
      if (i<0) "_" + (-i).toString
      else i.toString
    }
  }

  case object EPack extends UnaryOperator with NormalExpr {
    def reduce(arg1: Expr) = EPair(Empty,MMap(emptyMMap put MapP(arg1,arg1)))
    def asString = "^"
  }

  case object EDup extends UnaryOperator with NormalExpr {
    def reduce(arg1: Expr) = EPair(arg1,arg1)
    def asString = ">"
  }

  case object EDrop extends UnaryOperator with NormalExpr {
    def reduce(arg1: Expr) = EPair(Empty,Empty)
    def asString = "<"
  }

  case object ESplit extends UnaryOperator with NormalExpr {
    def reduce(arg1: Expr) = arg1 match {
      case MMap(m) => {
        val s = sizem(m)
        val l1 = s / 2
        val (l,e,rr) = splitm(l1,m)
        val r = rr put e.get
        EPair(mmp(l),mmp(r))
      }
      case n: Number => {
        var i1 = n.split2
        var i2 = n.sub(i1)
        EPair(i2,i1)
      }
    }

    def asString = "/"
  }

  case object EIota extends UnaryOperator with NormalExpr with UnaOpImp {
    def reduceNumber(i: Number) = {
      var m = emptyMMap
      var ii = i.zero
      // TODO: sign
      while (ii.compare(i) < 0) {
        m = m put MapP(ii,ii)
        ii = ii.incr
      }
      mmp(m)
    }
    def reduceMap(arg1: MMap) = not_yet
    def asString = "~"
  }

  def makeMap(i: Number): MMap = {
    if (i.compare(i.zero) == 0) MMap(emptyMMap)
    else {
      val sp = SetP(i,i.zero)
      val mp = MapP(i.zero,MSet(emptyMSet put sp))
      val m = emptyMMap put mp
      MMap(m)
    }
  }
  case object ESwap extends BinaryOperator with NormalExpr {
    def reduce(arg1: Expr, arg2: Expr) = ETriple(arg2,arg1,Empty)
    def asString = "\\"
  }

  trait UnaOpImp extends UnaryOperator {
    def reduceNumber(arg1: Number): Expr
    def reduceMap(arg1: MMap): Expr
    def reduce(arg1: Expr) = (arg1) match {
      case (a1: ELabeledExpr) => reduce(a1.second)
      case (a1: Number) => EPair(Empty,reduceNumber(a1))
      case (a1: MMap) => EPair(Empty,reduceMap(a1))
    }
  }
  trait BinOpImp extends BinaryOperator {
    def reduceNumber(arg1: Number, arg2: Number): Expr
    def reduceMap(arg1: MMap, arg2: MMap): Expr
    def reduce(arg1: Expr, arg2: Expr) = (arg1,arg2) match {
      case (a1: ELabeledExpr,a2) => reduce(a1.second,a2)
      case (a1, a2: ELabeledExpr) => reduce(a1,a2.second)
      case (a1: Number,a2: Number) => ETriple(Empty,Empty,reduceNumber(a1,a2))
      case (a1: Number,a2: MMap) => reduce(makeMap(a1),a2)
      case (a1: MMap,a2: Number) => reduce(a1,makeMap(a2))
      case (a1: MMap,a2: MMap) => ETriple(Empty,Empty,reduceMap(a1,a2))
    }
  }

  case object EBind extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1
    def reduceMap(arg1: MMap, arg2: MMap) = arg2 match {
      case MMap(m) => {
        var mm = m
        var a = arg1
        while (mm.first != None) {
          val p = mm.first.get
          a = a.bind(p.first,p.second)
          mm = mm.split(mm.first.get)._3
        }
        a
      }
    }
    def asString = "!"
  }

  case object EAdd extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 add arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiAdd arg2
    def asString = "+"
  }

  case object EMul extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 mul arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMul arg2
    def asString = "*"
  }
  case object EMax extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 max arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMax arg2
    def asString = "|"
  }
  case object EMin extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 min arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMin arg2
    def asString = "&"
  }
  case object ESub extends BinOpImp with NormalExpr {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 sub arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiSub arg2
    def asString = "-"
  }

  lazy val emptyCompound = ECompoundExpr(emptyExpr)

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

  case class ECompoundExpr(cex: CEX) extends CompoundExpr {
    def bind(label: Expr, value: Expr) = {
      var nm = emptyExpr
      var mm = cex
      while (mm.first != None) {
        var p = mm.first.get.asInstanceOf[CompoundPair]
        nm = nm put p.bind(label,value)
        mm = mm.split(mm.first.get)._3
      }
      ECompoundExpr(nm)
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
    def concat(e: ECompoundExpr): ECompoundExpr = {
      if (e.cex.isEmpty) this
      else if (isEmpty) e
      else ECompoundExpr(cex add e.cex)
    }
    def put(e: Option[EP]): ECompoundExpr = e match {
      case None => this
      case Some(x) => put(x)
    }

    def split(n: Int): (ECompoundExpr,Option[EP],ECompoundExpr) = {
      val (l,e,r) = splite(n,cex)
      (ECompoundExpr(l),e,ECompoundExpr(r))
    }
    def put(x: EP): ECompoundExpr =  ECompoundExpr(cex add (emptyExpr put x))
    lazy val left: ECompoundExpr = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExpr) => {
          val l = e.left
          if (l.isEmpty) ECompoundExpr(cex.left)
          else ECompoundExpr(cex.left put CP(cex.some.get.first,e.left))
        }
        case _ =>  ECompoundExpr(cex.left)
      }
    }
    lazy val right: ECompoundExpr = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExpr) => {
          val r = e.right
          if (r.isEmpty) ECompoundExpr(cex.right)
          else ECompoundExpr(cex.right put CP(cex.some.get.first,e.right))
        }
        case _ => ECompoundExpr(cex.right)
      }
    }
    lazy val some: Option[EP] = {
      val s = cex.some.flatMap(s => Some(s.second))
      s match {
        case Some(e: ECompoundExpr) => {
          val nn = e.some
          nn match {
            case None => None
            case Some(x) => {
              val ee = ECompoundExpr(emptyExpr put x)
              Some(CP(cex.some.get.first,ee))
            }
          }
        }
        case _ => cex.some
      }
    }

    def labels = not_yet
    def bindings = not_yet
    def wipe = this
    def isRedex = cex.measure match {
      case None => false
      case Some(x) => x.isRedex
    }
    def reduce = reduce3

    def cp(i: Number, p: Expr): CompoundPair = CP(i,p)

    def flatten(a: Option[Expr]): Option[Expr] = a match {
      case Some(e: ECompoundExpr) => flatten(e.first.flatMap(e => Some(e.second)))
      case _ => a
    }

    def re(e: Option[EP], n: Expr): EP = e match {
      case None => sys.error("illegal state")
      case Some(ep) => ep.second match {
        case cc: ECompoundExpr => {
          val k = ECompoundExpr(emptyExpr put re(cc.first,n))
          CP(ep.first,k)
        }
        case _ => CP(ep.first,n)
      }
    }

    def reduce(first: Option[EP], second: Option[EP], third: Option[EP]): (ECompoundExpr,ECompoundExpr) = {
      val f = first.flatMap(l => Some(l.second))
      val s = second.flatMap(l => Some(l.second))
      val t = third.flatMap(l => Some(l.second))

      (flatten(f),flatten(s),flatten(t)) match {
        case (a1,Some(u: UnaryOperator),_) => {
          val p = u.reduce(a1.get)
          val c = ECompoundExpr(emptyExpr put re(first,p.first) put re(second,p.second))
          val r = ECompoundExpr(emptyExpr put re(first,Empty) put re(second,Empty))
          (r,c concat c)
        }
        case (_,a1,Some(u: UnaryOperator)) => {
          val p = u.reduce(a1.get)
          val c = ECompoundExpr(emptyExpr put re(second,p.first) put re(third,p.second))
          val r = ECompoundExpr(emptyExpr put re(second,Empty) put re(third,Empty))
          (r,c concat c)
        }

        case (a1,a2,Some(u: BinaryOperator)) => {
          val p = u.reduce(a1.get,a2.get)
          val c = ECompoundExpr(emptyExpr put re(first,p.first) put re(second,p.second) put re(third,p.third))
          val r = ECompoundExpr(emptyExpr put re(first,Empty) put re(second,Empty) put re(third,Empty))
          (r,c concat c)
        }
        case _ => sys.error("illegal state")
      }
    }

    def econcat(a: Expr, b: Expr): Expr = (a,b) match {
      case (e1: ECompoundExpr, e2: ECompoundExpr) => {
        val p1 = CP(zero,e1)
        val p2 = CP(one,e2)
        val e = emptyExpr put p1 put p2
        ECompoundExpr(e)
      }

      case (e1: ECompoundExpr, a2: Expr) => {
        val p = e1.last.get
        val i = p.first.incr
        e1 put CP(i,a2)
      }
      case (a1: Expr, e2: ECompoundExpr) => {
        val p = e2.first.get
        val i = p.first.decr
        e2 put CP(i,a1)
      }
      case (e1,e2) => {
        val p1 = CP(zero,e1)
        val p2 = CP(one,e2)
        val e = emptyExpr put p1 put p2
        ECompoundExpr(e)
      }
    }

    def mconcat(a: Expr, b: Expr): Expr = (a,b) match {
      case (MSet(as), MSet(bs)) => {
        var a = as
        var e: MST = emptyMSet
        while (a.first != None) {
          var b = bs
          while (b.first != None) {
            val p1: SP = a.first.get
            val p2: SP = b.first.get
            val ec = econcat(p1.second,p2.second)
            val m = ECompoundExpr(emptyExpr put CP(zero,p1.first) put CP(one,p2.first) put CP(two,EMul))
            e = e put SetP(fullReduce(m),fullReduce(ec))
            b = b.split(b.first.get)._3
          }
          a = a.split(a.first.get)._3
        }
        MSet(e)
      }
      case (MSet(as), b) => {
        var a = as
        var e: MST = emptyMSet
        while (a.first != None) {
          val p: SP = a.first.get
          val ec = econcat(p.second,b)
          e = e put SetP(p.first,fullReduce(ec))
          a = a.split(a.first.get)._3
        }
        MSet(e)
      }
      case (b, MSet(as)) => {
        var a = as
        var e: MST = emptyMSet
        while (a.first != None) {
          val p: SP = a.first.get
          val ec = econcat(b,fullReduce(p.second))
          e = e put SetP(p.first,ec)
          a = a.split(a.first.get)._3
        }
        MSet(e)
      }
    }

    def typ2(e: Option[EP]) = e.flatMap(e => Some(e.second))

    def reduce3: ECompoundExpr = cex.measure match {
      case None => this
      case Some(x) => {
        if (x.isRedex) {
          val l = left
          val x = some
          val r = right

          if (mredexp(l.last,x)) {
            val ll = l.last
            val l_n = (l put CP(ll.get.first,Empty)).reduce
            val x_n = CP(x.get.first,mconcat(ll.get.second,x.get.second))
            val r_n = r.reduce
            l_n put x_n concat r_n
          }
          else if (mredexp(x,r.first)) {
            val rf = r.first
            val l_n = l.reduce
            val x_n = CP(x.get.first,mconcat(x.get.second,rf.get.second))
            val r_n = (r put CP(rf.get.first,Empty)).reduce
            l_n put x_n concat r_n
          }
          else {
            val (re1,re2) = {
              if (qredexp(l.last2,l.last,x)) reduce(l.last2,l.last,x)
              else if (qredexp(l.last,x,r.first)) reduce(l.last,x,r.first)
              else if (qredexp(x,r.first,r.first2)) reduce(x,r.first,r.first2)
              else (emptyCompound,emptyCompound)
            }


            val ll: ECompoundExpr = (l concat re1).reduce
            val xx: ECompoundExpr = emptyCompound.put(x) concat re1
            val rr: ECompoundExpr = (r concat re1).reduce
            val p = ll concat xx concat rr

            p concat re2
          }
        }
        else this
      }
    }

    def asString = asString(cex)

    def asString(c: CEX): String = {
      if (c.isEmpty) "empty"
      else {
        val s = " "
        val l = {
          if (c.left.isEmpty) ""
          else asString(c.left) + s
        }
        val r = {
          if (c.right.isEmpty) ""
          else s + asString(c.right)
        }
        l + c.some.get.asString + r
      }
    }
  }

  def ssubexpr(o: Expr): String = o match {
    case m: MMap => {
      if (m.isSequence) "(" + m.asString +")"
      else m.asString
    }
    case e: ECompoundExpr => "(" + o.asString + ")"
    case _ =>  o.asString
  }

  def subexpr(o: Expr): String = o match {
    case e: ECompoundExpr => "(" + o.asString + ")"
    case _ =>  o.asString
  }

  type EP = MPair[Number,Expr]
  type SP = MPair[Expr,Expr]
  type MP = MPair[Expr,Expr]

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  type SS[N,X,M] =  SeqImpl[N,X,M, IWBTree[N,X,M], IWBTreeContext[N,X,M]]

  type CEX = ST[EP,ExprMeasure,Int]
  type MST = ST[SP,SetMeasure,Int]
  type MMT = ST[MP,MapMeasure,Int]
  type SEQ = SS[Int,Expr,ExprSeqMeasure]

  val emptyExprSeq: SEQ = EmptySeqImpl(ExprSeqContext)
  val emptyExpr: CEX = EmptyOrderedISet(ExprTreapContext)
  val emptyMSet: MST = EmptyOrderedISet(MSetTreapContext)
  val emptyMMap: MMT = EmptyOrderedISet(MMapTreapContext)

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
  case object NoArgExpr extends ExprType

  def typ(v: Expr): ExprType = v match {
    case o: UnaryOperator => UnaExpr
    case b: BinaryOperator => BinExpr
    case m: MMap => ArgExpr
    case c: ECompoundExpr => typ(c.first.flatMap(f => Some(f.second)))
    case m: MSet => MultiExpr
    case a: Arg => ArgExpr
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

    val addV: SOT = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    val mulV: SOT = ms.LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SOT = ms.LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SOT = ms.LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SOT = ms.LeftMultiSetDifference() // construct once, to avoid excessive allocations

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

    val addV: SOT = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    val mulV: SOT = ms.LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SOT = ms.LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SOT = ms.LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SOT = ms.LeftMultiSetDifference() // construct once, to avoid excessive allocations

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
  object ExprSeqContext extends IWBTreeContextImpl[Int,Expr,ExprSeqMeasure] {
    type SS = IWBTree[Int,Expr,ExprSeqMeasure]

    def sizing = IntNum
    def compareOrder(x1: Expr, x2: Expr): Int = not_yet

    override def measure(l: SS, r: SS): Option[ExprSeqMeasure] = {
      val lr = isRedex2(l.measure)
      val rr = isRedex2(r.measure)
      if (lr || rr) Some(RedexMeasure)
      else {
        if (mredexa(l.last,r.first) | qredexa(l.last2,l.last,r.first) || qredexa(l.last,r.first,r.first2)) {
          Some(RedexMeasure)
        }
        else Some(NormalMeasure)
      }
    }
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

    def flattened(p: EP): SEQ = p.second match {
      case e: ECompoundExpr =>  e.cex.measure.get.flattened
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

    val addV: SetOperation[EP,ExprMeasure,STreap[EP,ExprMeasure,Int],STreapContext[EP,ExprMeasure,Int]] = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    override def add = addV
  }

  object ExprHashing extends PriorityHasher[Expr] {
    import Hashing.jenkinsHash
    def hash(s1: Expr): Int = s1 match {
      case EInt(i) => jenkinsHash(jenkinsHash(i)|i)
      case MSet(m) => jenkinsHash(m.hashCode + -398127)
      case MMap(m) => jenkinsHash(m.hashCode + 1283173)
      case Symbol(s) => jenkinsHash(s.hashCode)
      case u: Operator => jenkinsHash(u.toString.hashCode)
      case EChar(c) => jenkinsHash(c.toString.hashCode ^ 234091802)
      case ECompoundExpr(c) => jenkinsHash(c.hashCode + 7124568)
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
      case c: EChar => 1
      case u: UnaryOperator => 2
      case b: BinaryOperator => 3
      case c: CompoundExpr => 4
      case l: Symbol => 5
      case ms: MSet => 6
      case mm: MMap => 7
      case lb: ELabeledExpr => 8
    }
    def compareEqual(v1: Expr, v2: Expr) = (v1,v2) match {
      case (Empty,Empty) => 0
      case (n1: Number, n2: Number) => n1.compare(n2)
      case (EChar(c1), EChar(c2)) => c1.compare(c2)
      case (u1: Operator, u2: Operator) => u1.asString.compareTo(u2.asString)
      case (Symbol(s1), Symbol(s2)) => s1.compareTo(s2)
      case (ECompoundExpr(c1), ECompoundExpr(c2)) => compareCompoundExpr(c1,c2)
      case (MSet(s1), MSet(s2)) => compareMSet(s1,s2)
      case (MMap(m1), MMap(m2)) => compareMMap(m1,m2)
      case (ELabeledExpr(l1,e1), ELabeledExpr(l2,e2)) => {
        val c = compare(l1,l2)
        if (c == 0) compare(e1,e2) ; else c
      }
    }
    def compareCompoundExpr(c1: CEX, c2: CEX): Int = {
      // TODO: more efficient comparison
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
      def combineJoin(s1: SS, s2: SS)(implicit c: CC) = not_yet
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
}