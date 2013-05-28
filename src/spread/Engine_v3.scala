package spread

import spread.AbstractImmutableOrderedSet.{SetOperationImpl, SISetContextImpl, SISetImpl, SetOperation}
import spread.AbstractImmutableSequence.Num

object Engine_v3 {
  import OrderedSetImplementation._
  import SequenceImplementation._
  import OrderedTreapSet._
  import WeightBalancedSequence._
  import Hashing._

  var p = 0
  trait Expr {
    def labels: MultiSet
    def bindings: MultiMap
    def reduce: Expr
    def wipe: Expr
    def isRedex: Boolean
    def asString: String
  }

  trait Atom extends Expr
  trait MultiSet extends Atom {
    def multiAdd(o: MultiSet): Atom
    def multiSub(o: MultiSet): Atom
    def multiMul(o: MultiSet): Atom
    def multiMax(o: MultiSet): Atom
    def multiMin(o: MultiSet): Atom
  }
  trait MultiMap extends Atom {
    def multiAdd(o: MultiMap): Atom
    def multiSub(o: MultiMap): Atom
    def multiMul(o: MultiMap): Atom
    def multiMax(o: MultiMap): Atom
    def multiMin(o: MultiMap): Atom
  }
  trait Trace extends Atom
  trait IndexedExpr extends Expr
  trait CompoundExpr extends IndexedExpr with Atom

  def mmp(m: MMT): Atom = {
    if (m.isEmpty) EInt(0)
    else MMap(m)
  }

  case class MMap(m: MMT) extends MultiMap {
    def labels = not_yet
    def bindings = not_yet
    def reduce = not_yet
    def wipe = not_yet
    def isRedex = false

    def multiAdd(o: MultiMap) = o match {
      case MMap(mm) => mmp(m add mm)
      case _ => not_yet
    }
    def multiSub(o: MultiMap) = o match {
      case MMap(mm) => mmp(m subtract mm)
      case _ => not_yet
    }
    def multiMul(o: MultiMap) = o match {
      case MMap(mm) => mmp(m multiply mm)
      case _ => not_yet
    }
    def multiMax(o: MultiMap) = o match {
      case MMap(mm) => mmp(m maximum mm)
      case _ => not_yet
    }
    def multiMin(o: MultiMap) = o match {
      case MMap(mm) => mmp(m minimum mm)
      case _ => not_yet
    }

    def isString: Boolean = {
      var e1 = m
      var i: Int = 0
      var seq = true
      var str = true
      while ((e1.first != None) && seq && str) {
        seq = ExprOrdering.compare(e1.first.get.first,EInt(i)) == 0
        str = e1.first.get.second match {
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
        seq = ExprOrdering.compare(e1.first.get.first,EInt(i)) == 0
        e1 = e1.split(e1.first.get)._3
        i = i + 1
      }
      if (i > 1) seq
      else false
    }

    def doChar(e: Atom): String = e match {
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
          else if (seq) subexpr(p.second)
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
    def multiAdd(o: MultiSet) = o match {
      case MSet(mm) => MSet(m add mm)
      case _ => not_yet
    }
    def multiSub(o: MultiSet) = o match {
      case MSet(mm) => MSet(m subtract mm)
      case _ => not_yet
    }
    def multiMul(o: MultiSet) = o match {
      case MSet(mm) => MSet(m multiply mm)
      case _ => not_yet
    }
    def multiMax(o: MultiSet) = o match {
      case MSet(mm) => MSet(m maximum mm)
      case _ => not_yet
    }
    def multiMin(o: MultiSet) = o match {
      case MSet(mm) => MSet(m minimum mm)
      case _ => not_yet
    }

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

  trait MultiSetPair extends MPair[Atom,Atom]
  trait MultiMapPair extends MPair[Atom,Atom]
  trait LabeledExpr extends Pair[Atom,Atom] with Atom
  trait CompoundPair extends EP
  trait TracePair extends MPair[Number,Expr]
  trait Operator extends Atom

  case class SetP(first: Atom, second: Atom) extends MultiSetPair {
    def add(o: MPair[Atom,Atom]) = {
      val i = emptyExpr put CP(EInt(0),first) put CP(EInt(1),o.first) put CP(EInt(3),EAdd)
      val e = ECompoundExpr(i).reduce.reduce.asInstanceOf[Atom]
      Some(SetP(e,second))
    }
    def mul(o: MPair[Atom,Atom]) = {
      val i = emptyExpr put CP(EInt(0),first) put CP(EInt(1),o.first) put CP(EInt(3),EMul)
      val e = ECompoundExpr(i).reduce.reduce.asInstanceOf[Atom]
      Some(SetP(e,second))
    }
    def max(o: MPair[Atom,Atom]) = {
      val i = emptyExpr put CP(EInt(0),first) put CP(EInt(1),o.first) put CP(EInt(3),EMax)
      val e = ECompoundExpr(i).reduce.reduce.asInstanceOf[Atom]
      Some(SetP(e,second))
    }
    def min(o: MPair[Atom,Atom]) = {
      val i = emptyExpr put CP(EInt(0),first) put CP(EInt(1),o.first) put CP(EInt(3),EMin)
      val e = ECompoundExpr(i).reduce.reduce.asInstanceOf[Atom]
      Some(SetP(e,second))
    }
    def subtract(o: MPair[Atom,Atom]) = {
      val i = emptyExpr put CP(EInt(0),first) put CP(EInt(1),o.first) put CP(EInt(3),ESub)
      val e = ECompoundExpr(i).reduce.reduce.asInstanceOf[Atom]
      Some(SetP(e,second))
    }

    def asString = {
      if (ExprOrdering.compare(first,EInt(1)) == 0) second.asString
      else first.asString + ":" + second.asString
    }
  }

  def asMSet(o: Atom): MSet = o match {
    case m: MSet => m
    case _ => MSet(emptyMSet put SetP(EInt(1),o))
  }

  def mp(m: MapP): Option[MapP] = m.second match {
    case ms: MSet => {
      if (ms.isEmpty) None
      else Some(m)
    }
    case _ => Some(m)
  }

  case class MapP(first: Atom, second: Atom) extends MultiMapPair {
    def add(o: MPair[Atom,Atom]) = mp(MapP(first,asMSet(second) multiAdd asMSet(o.second)))
    def mul(o: MPair[Atom,Atom]) = mp(MapP(first,asMSet(second) multiMul asMSet(o.second)))
    def max(o: MPair[Atom,Atom]) = mp(MapP(first,asMSet(second) multiMax asMSet(o.second)))
    def min(o: MPair[Atom,Atom]) = mp(MapP(first,asMSet(second) multiMin asMSet(o.second)))
    def subtract(o: MPair[Atom,Atom]) = mp(MapP(first,asMSet(second) multiSub asMSet(o.second)))

    def asString = {
      if (ExprOrdering.compare(first,second) == 0) second.asString
      else first.asString + "=" + second.asString
    }
  }
  trait UnaryOperator extends Operator {
    def reduce(arg1: Atom): Pair[Atom,Atom]
  }

  trait BinaryOperator extends Operator {
    def reduce(arg1: Atom, arg2: Atom): Triple[Atom,Atom,Atom]
  }

  def makeSeq(m: MMT, i: Int): (Int,MMT) = {
    var mm = emptyMMap
    var lm = m
    var ii = i
    while (lm.first != None) {
      val p = lm.first.get
      val np = MapP(EInt(ii),p.second)
      mm = mm put np
      lm = lm.split(lm.first.get)._3
      ii = ii + 1
    }
    (ii,mm)
  }

  case object EConcat extends NormalAtom {
    def reduce(arg1: Atom, arg2: Atom): Triple[Atom,Atom,Atom] = (arg1,arg2) match {
      case (n: Number,_) => reduce(makeMap(n),arg2)
      case (_,n: Number) => reduce(arg1,makeMap(n))
      case (MMap(m1),MMap(m2)) => {
        val (i,s1) = makeSeq(m1,0)
        val (i2,s2) = makeSeq(m2,i)
        ETriple(Empty,Empty,mmp(s1 maximum s2))
      }
      case _ => not_yet
    }
    def asString = ";"
  }

  case class ELabeledExpr(first: Atom, second: Atom) extends LabeledExpr with NormalAtom {
    def asString = {
      if (second == Empty) first.asString + "'"
      else first.asString + "'" + second.asString
    }
  }

  trait Number extends Atom {
    def zero: Number
    def one: Number
    def incr: Number
    def decr: Number
    def add(n: Number): Number
    def mul(n: Number): Number
    def min(n: Number): Number
    def max(n: Number): Number
    def sub(n: Number): Number
    def compare(n: Number): Int
  }

  trait NormalAtom extends Atom {
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
  }

  case object Empty extends NormalAtom {
    def asString = "."
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

  case class CP(f: Number, s: Atom) extends CompoundPairImpl {
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

  case class Symbol(s: String) extends Atom {
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = s
  }

  trait Arg extends NormalAtom

  case class EChar(c: Char) extends Atom {
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = "\"" + c + "\""
  }

  case class EForeach(a: Atom) extends UnaOpImp with UnaryOperator {
    def reduceNumber(i: Number) = makeMap(i)
    def reduceMap(arg1: MMap) = arg1 match {
      case MMap(m) =>  MMap(foreach(m))
    }
    def foreach(m: MMT): MMT = m
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = "@" + subexpr(a)
  }

  case class EFold(a: Atom) extends UnaOpImp with UnaryOperator {
    def reduceNumber(i: Number) = makeMap(i)
    def reduceMap(arg1: MMap) = arg1 match {
      case MMap(m) => {
        val mm = fold(m)
        mm
      }
    }
    def fold(m: MMT): Atom = {
      if (m.some == None) Empty
      else {
        val l = m.left
        val x = m.some.get
        val r = m.right
        if (l.isEmpty) {
          if (r.isEmpty) x.second
          else ECompoundExpr(emptyExpr put CP(EInt(0),x.second) put CP(EInt(1),fold(r)) put CP(EInt(2),a))
        }
        else {
          if (r.isEmpty) {
            if (l.left.isEmpty && l.right.isEmpty) {
              ECompoundExpr(emptyExpr put CP(EInt(0),l.some.get.second) put CP(EInt(1),x.second) put CP(EInt(2),a))
            }
            else ECompoundExpr(emptyExpr put CP(EInt(0),fold(l)) put CP(EInt(1),x.second) put CP(EInt(2),a))
          }
          else ECompoundExpr(emptyExpr put CP(EInt(0),fold(l)) put CP(EInt(1),x.second) put CP(EInt(2),a) put CP(EInt(3),fold(r)) put CP(EInt(4),a))
        }
      }
    }
    def labels = not_yet
    def bindings = not_yet
    def reduce = this
    def wipe = this
    def isRedex = false
    def asString = "%" + subexpr(a)
  }

  case class EInt(i: Int) extends Number with Arg {
    def create(i: Int) = EInt(i)
    def zero: Number = create(0)
    def one: Number = create(1)
    def incr: Number = create(i+1)
    def decr: Number = create(i-1)
    def add(n: Number): Number = n match {
      case EInt(n2) => create(i + n2)
    }
    def mul(n: Number): Number = n match {
      case EInt(n2) => create(i * n2)
    }
    def min(n: Number): Number = n match {
      case EInt(n2) => create(i min n2)
    }
    def max(n: Number): Number = n match {
      case EInt(n2) => create(i max n2)
    }
    def sub(n: Number): Number = n match {
      case EInt(n2) => create(i - n2)
    }
    def compare(n: Number): Int = n match {
      case EInt(n2) => i - n2
    }
    def asString = {
      if (i<0) "_" + (-i).toString
      else i.toString
    }
  }

  case object EDup extends UnaryOperator with NormalAtom {
    def reduce(arg1: Atom) = EPair(arg1,arg1)
    def asString = "`"
  }

  case object EIota extends UnaryOperator with NormalAtom with UnaOpImp {
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
    def reduceMap(arg1: MMap) = {
      not_yet
    }
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
  case object ESwap extends BinaryOperator with NormalAtom {
    def reduce(arg1: Atom, arg2: Atom) = ETriple(arg2,arg1,Empty)
    def asString = "\\"
  }

  trait UnaOpImp extends UnaryOperator {
    def reduceNumber(arg1: Number): Atom
    def reduceMap(arg1: MMap): Atom
    def reduce(arg1: Atom) = (arg1) match {
      case (a1: ELabeledExpr) => reduce(a1.second)
      case (a1: Number) => EPair(Empty,reduceNumber(a1))
      case (a1: MMap) => EPair(Empty,reduceMap(a1))
    }
  }
  trait BinOpImp extends BinaryOperator {
    def reduceNumber(arg1: Number, arg2: Number): Atom
    def reduceMap(arg1: MMap, arg2: MMap): Atom
    def reduce(arg1: Atom, arg2: Atom) = (arg1,arg2) match {
      case (a1: ELabeledExpr,a2) => reduce(a1.second,a2)
      case (a1, a2: ELabeledExpr) => reduce(a1,a2.second)
      case (a1: Number,a2: Number) => ETriple(Empty,Empty,reduceNumber(a1,a2))
      case (a1: Number,a2: MMap) => reduce(makeMap(a1),a2)
      case (a1: MMap,a2: Number) => reduce(a1,makeMap(a2))
      case (a1: MMap,a2: MMap) => ETriple(Empty,Empty,reduceMap(a1,a2))
    }
  }
  case object EAdd extends BinOpImp with NormalAtom {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 add arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiAdd arg2
    def asString = "+"
  }
  case object EMul extends BinOpImp with NormalAtom {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 mul arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMul arg2
    def asString = "*"
  }
  case object EMax extends BinOpImp with NormalAtom {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 max arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMax arg2
    def asString = "|"
  }
  case object EMin extends BinOpImp with NormalAtom {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 min arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiMin arg2
    def asString = "&"
  }
  case object ESub extends BinOpImp with NormalAtom {
    def reduceNumber(arg1: Number, arg2: Number) = arg1 sub arg2
    def reduceMap(arg1: MMap, arg2: MMap) = arg1 multiSub arg2
    def asString = "-"
  }

  lazy val emptyCompound = ECompoundExpr(emptyExpr)

  case class ECompoundExpr(cex: CEX) extends CompoundExpr {
    {
      p = p +1
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
    def put(x: EP): ECompoundExpr = {
      ECompoundExpr(cex add (emptyExpr put x))
    }
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
    def reduce = {
      reduce3
    }

    def cp(i: Number, p: Atom): CompoundPair = CP(i,p)

    def flatten(a: Option[Atom]): Option[Atom] = a match {
      case Some(e: ECompoundExpr) => flatten(e.first.flatMap(e => Some(e.second)))
      case _ => a
    }

    def re(e: Option[EP], n: Atom): EP = e match {
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
        case (a1,Some(EConcat),a2) => {
          val p = EConcat.reduce(a1.get,a2.get)
          val c = ECompoundExpr(emptyExpr put re(first,p.first) put re(second,p.second) put re(third,p.third))
          val r = ECompoundExpr(emptyExpr put re(first,Empty) put re(second,Empty) put re(third,Empty))
          (r,c concat c)
        }
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

    def econcat(a: Atom, b: Atom): Atom = (a,b) match {
      case (e1: ECompoundExpr, e2: ECompoundExpr) => {
        val p1 = CP(EInt(0),e1)
        val p2 = CP(EInt(1),e2)
        val e = emptyExpr put p1 put p2
        ECompoundExpr(e)
      }

     case (e1: ECompoundExpr, a2: Atom) => {
        val p = e1.last.get
        val i = p.first.incr
        e1 put CP(i,a2)
      }
      case (a1: Atom, e2: ECompoundExpr) => {
        val p = e2.first.get
        val i = p.first.decr
        e2 put CP(i,a1)
      }
      case (e1,e2) => {
        val p1 = CP(EInt(0),e1)
        val p2 = CP(EInt(1),e2)
        val e = emptyExpr put p1 put p2
        ECompoundExpr(e)
      }
    }

    def mconcat(a: Atom, b: Atom): Atom = (a,b) match {
      case (MSet(as), MSet(bs)) => {
        var a = as
        var e: MST = emptyMSet
        while (a.first != None) {
          var b = bs
          while (b.first != None) {
            val p1: SP = a.first.get
            val p2: SP = b.first.get
            val ec = econcat(p1.second,p2.second).reduce.asInstanceOf[Atom]
            //TODO: Multiply
            e = e put SetP(p1.first,ec)
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
          val ec = econcat(p.second,b).reduce.asInstanceOf[Atom]
          e = e put SetP(p.first,ec)
          a = a.split(a.first.get)._3
        }
        MSet(e)
      }
      case (b, MSet(as)) => {
        var a = as
        var e: MST = emptyMSet
        while (a.first != None) {
          val p: SP = a.first.get
          val ec = econcat(b,p.second).reduce.asInstanceOf[Atom]
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
      if (c.isEmpty) "."
      else {
      val s = {
        if (c.some.get.second == EConcat) ""
        else " "
      }
      val l = {
        if (c.left.isEmpty) ""
        else {
          if (c.left.last.get.second == EConcat) {
            asString(c.left)
          }
          else asString(c.left) + s
        }
      }
      val r = {
        if (c.right.isEmpty) ""
        else {
          if (c.right.first.get.second == EConcat) asString(c.right)
          else s + asString(c.right)
        }
      }
      l + c.some.get.asString + r
    }
    }
  }

  def subexpr(o: Atom): String = o match {
    case e: ECompoundExpr => "(" + o.asString + ")"
    case _ =>  o.asString
  }

  type EP = MPair[Number,Atom]
  type SP = MPair[Atom,Atom]
  type MP = MPair[Atom,Atom]

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  type SS[N,X,M] =  SeqImpl[N,X,M, IWBTree[N,X,M], IWBTreeContext[N,X,M]]

  type CEX = ST[EP,ExprMeasure,Int]
  type MST = ST[SP,Any,Int]
  type MMT = ST[MP,Any,Int]
  type SEQ = SS[Int,Atom,ExprSeqMeasure]

  val emptyExprSeq: SEQ = EmptySeqImpl(ExprSeqContext)
  val emptyExpr: CEX = EmptyOrderedISet(ExprTreapContext)
  val emptyMSet: MST = EmptyOrderedISet(MSetTreapContext)
  val emptyMMap: MST = EmptyOrderedISet(MMapTreapContext)

  case class ExprMeasure(size: Int, flattened: SEQ) {
    def isRedex: Boolean = flattened.measure match {
      case Some(x) => x.isRedex
      case None => false
    }
  }

  def sredex(o: Option[EP]): Option[Atom] = o.flatMap(l => Some(l.second))

  def tredex(o: Option[EP]): AtomType = typ(sredex(o))

  def mredexa(first: Option[Atom], second: Option[Atom]): Boolean = (typ(first),typ(second)) match {
    case (s: SomeAtom,MultiAtom) => true
    case (MultiAtom,s: SomeAtom) => true
    case _ => false
  }

  def mredexp(first: Option[EP], second: Option[EP]): Boolean = mredexa(sredex(first),sredex(second))

  def qredexp(first: Option[EP], second: Option[EP], third: Option[EP]): Boolean = {
    qredexa(sredex(first),sredex(second),sredex(third))
  }

  def qredexa(first: Option[Atom], second: Option[Atom], third: Option[Atom]): Boolean = {
    (typ(first),typ(second),typ(third)) match {
      case (ArgAtom,UnaAtom,_) => true
      case (_,ArgAtom,UnaAtom) => true
      case (ArgAtom,ArgAtom,BinAtom) => true
      case (ArgAtom,ConcatAtom,ArgAtom) => true
      case _ => false
    }
  }

  trait AtomType

  trait SomeAtom extends AtomType

  case object NoAtom extends AtomType
  case object UnaAtom extends AtomType with SomeAtom
  case object BinAtom extends AtomType with SomeAtom
  case object ArgAtom extends AtomType with SomeAtom
  case object MultiAtom extends AtomType with SomeAtom
  case object ConcatAtom extends AtomType with SomeAtom
  case object NoArgAtom extends AtomType

  def typ(v: Atom): AtomType = v match {
    case o: UnaryOperator => UnaAtom
    case b: BinaryOperator => BinAtom
    case EConcat => ConcatAtom
    case m: MMap => ArgAtom
    case c: ECompoundExpr => typ(c.first.flatMap(f => Some(f.second)))
    case m: MSet => MultiAtom
    case a: Arg => ArgAtom
    case _ => NoArgAtom
  }
  def typ(v: Option[Atom]): AtomType = v match {
    case None => NoAtom
    case Some(x) => typ(x)
  }

  object MSetOrdering extends Ordering[SP] {
    def compare(x1: SP, x2: SP) = ExprOrdering.compare(x1.second,x2.second)
  }
  object MSetTreapContext extends STreapContextImpl[SP,Any,Int] {
    type SS = STreap[SP,Any,Int]

    def compareOrder(x1: SP, x2: SP): Int = MSetOrdering.compare(x1,x2)
    def priorityHash(x: Option[SP]): Int = {
      val xx = x.get
      jenkinsHash(ExprHashing.hash(xx.first)+134517) ^ ExprHashing.hash(xx.second)
    }
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val ms = MSetOperations[Atom,Atom]()

    val addV: SetOperation[SP,Any,STreap[SP,Any,Int],STreapContext[SP,Any,Int]] = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    val mulV: SetOperation[SP,Any,STreap[SP,Any,Int],STreapContext[SP,Any,Int]] = ms.LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SetOperation[SP,Any,STreap[SP,Any,Int],STreapContext[SP,Any,Int]] = ms.LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SetOperation[SP,Any,STreap[SP,Any,Int],STreapContext[SP,Any,Int]] = ms.LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SetOperation[SP,Any,STreap[SP,Any,Int],STreapContext[SP,Any,Int]] = ms.LeftMultiSetDifference() // construct once, to avoid excessive allocations

    override def add = addV
    override def multiply = mulV
    override def maximum = maxV
    override def minimum = minV
    override def subtract = subV
  }

  object MMapTreapContext extends STreapContextImpl[MP,Any,Int] {
    type SS = STreap[MP,Any,Int]

    def compareOrder(x1: MP, x2: MP): Int = ExprOrdering.compare(x1.first,x2.first)

    def priorityHash(x: Option[MP]): Int = {
      val xx = x.get
      jenkinsHash(ExprHashing.hash(xx.second) ^ 234912813) ^ ExprHashing.hash(xx.first)
    }
    def orderPriority(p1: Int, p2: Int) = p1.compareTo(p2)

    val ms = MSetOperations[Atom,Atom]()

    val addV: SetOperation[MP,Any,STreap[MP,Any,Int],STreapContext[MP,Any,Int]] = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    val mulV: SetOperation[MP,Any,STreap[MP,Any,Int],STreapContext[MP,Any,Int]] = ms.LeftMultiSetMultiply() // construct once, to avoid excessive allocations
    val maxV: SetOperation[MP,Any,STreap[MP,Any,Int],STreapContext[MP,Any,Int]] = ms.LeftMultiSetUnion() // construct once, to avoid excessive allocations
    val minV: SetOperation[MP,Any,STreap[MP,Any,Int],STreapContext[MP,Any,Int]] = ms.LeftMultiSetIntersect() // construct once, to avoid excessive allocations
    val subV: SetOperation[MP,Any,STreap[MP,Any,Int],STreapContext[MP,Any,Int]] = ms.LeftMultiSetDifference() // construct once, to avoid excessive allocations

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
  object ExprSeqContext extends IWBTreeContextImpl[Int,Atom,ExprSeqMeasure] {
    type SS = IWBTree[Int,Atom,ExprSeqMeasure]

    def sizing = IntNum
    def compareOrder(x1: Atom, x2: Atom): Int = not_yet

    override def measure(l: SS, r: SS): Option[ExprSeqMeasure] = {
      val lr = isRedex2(l.measure)
      val rr = isRedex2(r.measure)
      if (lr || rr) Some(RedexMeasure)
      else {
        /*
        (mredex(l.last,x)) {
            val ll = l.last
            val l_n = (l put CP(ll.get.first,Empty)).reduce
            val x_n = CP(x.get.first,mconcat(ll.get.second,x.get.second))
            val r_n = r.reduce
            l_n put x_n concat r_n
          }
          else if (mredex(x,r.first)) {
            val rf = r.first
            val l_n = l.reduce
            val x_n = CP(x.get.first,mconcat(x.get.second,rf.get.second))
            val r_n = (r put CP(rf.get.first,Empty)).reduce
            l_n put x_n concat r_n
          }
          else {
            val (re1,re2) = {
              if (qredex(l.last2,l.last,x)) reduce(l.last2,l.last,x)
              else if (qredex(l.last,x,r.first)) reduce(l.last,x,r.first)
              else if (qredex(x,r.first,r.first2)) reduce(x,r.first,r.first2)
         */
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

    val ms = MSetOperations[Number,Atom]()

    val addV: SetOperation[EP,ExprMeasure,STreap[EP,ExprMeasure,Int],STreapContext[EP,ExprMeasure,Int]] = ms.LeftMultiSetSumUnion()  // construct once, to avoid excessive allocations
    override def add = addV
  }

  object ExprHashing extends PriorityHasher[Atom] {
    import Hashing.jenkinsHash
    def hash(s1: Atom): Int = s1 match {
      case EInt(i) => jenkinsHash(i.hashCode)
      case MSet(m) => jenkinsHash(m.hashCode + -398127)
      case MMap(m) => jenkinsHash(m.hashCode + 1283173)
      case Symbol(s) => jenkinsHash(s.hashCode)
      case u: Operator => jenkinsHash(u.toString.hashCode)
      case EChar(c) => jenkinsHash(c.toString.hashCode ^ 234091802)
      case ECompoundExpr(c) => jenkinsHash(c.hashCode + 7124568)
      case EConcat => 3412313
    }
  }

  object ExprOrdering extends Ordering[EP] {
    def compare(v1: EP, v2: EP): Int = compare(v1.first,v2.first)
    def compare(v1: Atom, v2: Atom): Int = {
      val o1 = order(v1)
      val o2 = order(v2)
      if (o1 == o2) compareEqual(v1,v2)
      else o1 - o2
    }
    def order(v1: Atom): Int = v1 match {
      case Empty => -1
      case i: Number => 0
      case c: EChar => 1
      case u: UnaryOperator => 2
      case b: BinaryOperator => 3
      case c: CompoundExpr => 4
      case l: Symbol => 5
      case ms: MSet => 6
      case mm: MMap => 7
      case EConcat => 8
    }
    def compareEqual(v1: Atom, v2: Atom) = (v1,v2) match {
      case (Empty,Empty) => 0
      case (n1: Number, n2: Number) => n1.compare(n2)
      case (EChar(c1), EChar(c2)) => c1.compare(c2)
      case (u1: Operator, u2: Operator) => u1.asString.compareTo(u2.asString)
      case (Symbol(s1), Symbol(s2)) => s1.compareTo(s2)
      case (ECompoundExpr(c1), ECompoundExpr(c2)) => compareCompoundExpr(c1,c2)
      case (MSet(s1), MSet(s2)) => compareMSet(s1,s2)
      case (MMap(m1), MMap(m2)) => compareMMap(m1,m2)
      case (EConcat,EConcat) => 0
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
      def combineJoin(s1: SS, s2: SS)(implicit c: CC) = s1.join(s2)
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