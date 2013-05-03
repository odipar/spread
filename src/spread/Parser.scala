package spread

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.collection.immutable.Stack
import scala.util.parsing.input.Reader
import scala.util.parsing.input._

object Parser {

  import Engine_v2._

  object SpreadParser extends RegexParsers
  {
    def doParse(s: String) = parse(program,s)
    def reg(s: String) = regex(new Regex(s))

    trait Term

    object Empty extends Term

    trait Atom extends Term {
      def toSpread: MultiSetExpr
    }

    case class Number(i: Int) extends Atom {
      def toSpread = EInt(i)
    }
    case class MSymbol(s: String) extends Atom {
      def toSpread = ESymbol(s)
    }
    case class MPair(label: E, target: E) extends Term {
      def toSpread: MapPair = MMapPair(label.toSpread,target.toSpread)
    }
    case class SPair(label: E, target: E) extends Term {
      def toSpread: SetPair = {
        if (MSOrdering.compare(label.toSpread,EInt(1)) == 0) target.toSpread.asInstanceOf[SetPair]
        else SSetPair(label.toSpread,target.toSpread)
      }
    }
    case class Labeled(label: E, target: E) extends Term {
      def toSpread: MultiSetExpr = LabeledExpr(label.toSpread,target.toSpread)
    }

    trait Operator extends Term
    trait UnaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr): MultiSetExpr
    }
    trait BinaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr): MultiSetExpr
    }
    case object Add extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EAdd(arg1,arg2)
    }
    case object Subtract extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = ESub(arg1,arg2)
    }
    case object Max extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EMax(arg1,arg2)
    }
    case object Min extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EMin(arg1,arg2)
    }
    case object Mul extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EMul(arg1,arg2)
    }
    case class Sequence(l: List[Atom]) extends Term {
      def toSpread = {
        var i = l.iterator
        var ii = 0
        var em = emptyMap
        while (i.hasNext) {
          val e = i.next.toSpread
          em = em put MMapPair(EInt(ii),e)
          ii = ii + 1
        }
        EEMap(EMap(em))
      }
    }
    case object Foreach extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) =  arg1 match {
        case u: UnaOp => {
          EForeach(u)
        }
        case b: BinOp => EForeach(PartialBinOp(b))
      }
    }
    case object Order extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) =  arg1 match {
        case u: UnaOp => {
          sys.error("unary op can't be ordered")
        }
        case b: BinOp => {
          EOrder(b)
        }
      }
    }
    case object Bind extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EBind(arg1,arg2)
    }
    case object Bindings extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EBindings(arg1)
    }
    case object Reduce extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = ERed(arg1)
    }
    case object Turn extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = ETurn(arg1)
    }
    case object Wipe extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EWipe(arg1)
    }
    case object UnPack extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EUnPack(arg1)
    }
    case object Pack extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EPack(arg1)
    }
    case object OTrace extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = ETrace(arg1)
    }

    case class E(l: List[Term]) extends Atom {
      def toSpread: MultiSetExpr = {
        var s = Stack[MultiSetExpr]()
        var i = l.iterator
        while (i.hasNext) {
          i.next match {
            case d: Sequence => s = s push d.toSpread
            case p: Labeled => s = s push p.toSpread
            case a: Atom => s = s push a.toSpread
            case u: UnaryOp => {
              val arg1 = s.top
              s = s.pop
              s = s push u.toSpread(arg1)
            }
            case b: BinaryOp => {
              val arg2 = s.top
              s = s.pop
              val arg1 = s.top
              s = s.pop
              s = s push b.toSpread(arg1,arg2)
            }
          }
        }
        if (s.size == 0) EExpr
        else if (s.size == 1) s.top
        else sys.error("parse error: expressions is unbalanced: " + s)
      }
    }
    case class T(l: List[E]) extends Atom {
      def toSpread = {
        val ll = l
        var i = ll.iterator
        var m = emptyMap
        var ii = 0
        while (i.hasNext) {
          val e = i.next.toSpread
          val p = MMapPair(EInt(ii),e)
          m = m put p
          ii = ii + 1
        }

        createTrace(EMap(m))
      }
    }

    case class A(l: List[SPair]) extends Atom {
      def toSpread = {
        val ll = l.reverse
        var i = ll.iterator
        var aa = noAlternatives
        while (i.hasNext) {
          val e = i.next.toSpread
          aa = aa put e
        }
        val p = createAlt2(aa)
        p
      }
    }
    case class M(l: List[MPair]) extends Atom {
      def toSpread = {
        var i = l.iterator
        var em = emptyMap
        while (i.hasNext) {
          val e = i.next.toSpread
          em = em put e
        }
        EEMap(EMap(em))
      }
    }

    final def getReadLine(input: Reader[Char]) : ParseResult[E] =
    {
      program(input)
    }

    final def parse(input: Reader[Char]) : ParseResult[E] =
    {
      program(input)
    }

    final val eolc = (13:Int).toChar
    final val lfdc = (10:Int).toChar
    lazy val lfd = elem("linefeed", ch => ch == lfdc)
    lazy val eol = elem("end-of-line", ch => ch == eolc)
    lazy val ret = lfd | (eol ~ lfd)

    lazy val program = expr ~ ret ^^ { case e ~ r => e }
    lazy val expr: Parser[E] = rep(elem) ^^ { case l => E(l) }
    lazy val elem: Parser[Term] = labeled | sequence | atom | operator
    lazy val trace: Parser[T] = "(" ~ repsep(expr,",") ~ ")" ^^ { case "(" ~ l ~ ")" => T(l) }
    lazy val atom: Parser[Atom] = spreadsheet | trace | alternatives | number | symbol
    lazy val alternatives: Parser[A] = "{" ~ repsep(setpair,",") ~ "}" ^^ { case "{" ~ l ~ "}" =>  A(l) }
    lazy val spreadsheet: Parser[M] = "[" ~ repsep(mappair,",") ~ "]" ^^ { case "[" ~ l ~ "]" => M(l) }
    lazy val number = reg("[-]?[0-9]+") ^^ { i => Number(i.toInt) }
    lazy val symbol = reg("[a-zA-Z0-9]+") ^^ { t => MSymbol(t) }
    lazy val operator = special | unary | binary
    lazy val special = traceo
    lazy val unary = reduce | wipe | pack | unpack | foreach | bindings
    lazy val labeled = nlabeled | alabeled
    lazy val mappair = meqpair | spair
    lazy val setpair = msetpair | ssetpair
    lazy val ssetpair = expr ^^ { case l => SPair(E(List(Number(1))),l) }
    lazy val msetpair = expr ~ ":" ~ expr ^^ { case m ~ ":" ~ l => SPair(m,l) }
    lazy val path: Parser[List[Atom]] = repsep(atom,".") ^^ { case l => l }
    lazy val sequence: Parser[Term] = atom ~ "." ~ path ^^ { case e1 ~ "." ~ e2 => Sequence(List(e1) ++ e2)}
    lazy val spair = expr ^^ {case e => MPair(e,e) }
    lazy val meqpair = expr ~ "=" ~ expr ^^ {case e1 ~ "=" ~ e2 => MPair(e1,e2)}
    lazy val nlabeled = (sequence | atom) ~ "`" ^^ { case e1 ~ "`" => Labeled(E(List(e1)),E(List())) }
    lazy val alabeled = (sequence | atom) ~ "'" ~ (sequence | atom) ^^ { case e1 ~ "'" ~ e2 => Labeled(E(List(e1)),E(List(e2))) }
    lazy val binary = add | subtract | mul | max | min | bind | order
    lazy val wipe = "#" ^^ { case o => Wipe }
    lazy val turn = "~" ^^ { case o => Turn }
    lazy val unpack = ">" ^^ { case o => UnPack }
    lazy val pack = "<" ^^ { case o => Pack }
    lazy val reduce = "$" ^^ { case o => Reduce }
    lazy val add = "+" ^^ { case o => Add }
    lazy val subtract = "-" ^^ { case o => Subtract }
    lazy val max = "|" ^^ { case o => Max }
    lazy val min = "&" ^^ { case o => Min }
    lazy val mul = "*" ^^ { case o => Mul }
    lazy val foreach = "/" ^^ { case o => Foreach }
    lazy val order = "\\" ^^ { case o => Order }
    lazy val bind = "!" ^^ { case o => Bind }
    lazy val bindings = "^" ^^ { case o => Bindings }

    lazy val traceo = "@<" ^^ { case o => OTrace }
  }
}
