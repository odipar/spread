package spread

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.collection.immutable.Stack

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
      def toSpread = EInt(i,1)
    }
    case class MSymbol(s: String) extends Atom {
      def toSpread = ESymbol(s,1)
    }
    case class Pair(label: E, target: E) extends Term {
      def toSpread: EPair = EPair(label.toSpread,target.toSpread,1)
    }

    trait Operator extends Term
    trait UnaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr): MultiSetExpr
    }
    trait BinaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr): MultiSetExpr
    }
    case object Add extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EAdd(arg1,arg2,1)
    }
    case object Mul extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EMul(arg1,arg2,1)
    }
    case class Sequence(l: List[Atom]) extends Term {
      def toSpread = {
        var i = l.iterator
        var ii = 0
        var em = emptyMap
        while (i.hasNext) {
          val e = i.next.toSpread
          em = em put EPair(EInt(ii,1),e,1)
          ii = ii + 1
        }
        EEMap(EMap(em,1),1)
      }
    }
    case object Bind extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EBind(arg1,arg2,1)
    }
    case object Iter extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EIter(arg1,arg2,1)
    }
    case object Reduce extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = ERed(arg1,1)
    }
    case object Wipe extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EWipe(arg1,1)
    }
    case object Desolve extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = EDesolve(arg1,1)
    }
    case class E(l: List[Term]) extends Atom {
      def toSpread: MultiSetExpr = {
        var s = Stack[MultiSetExpr]()
        var i = l.iterator
        while (i.hasNext) {
          i.next match {
            case d: Sequence => s = s push d.toSpread
            case p: Pair => s = s push p.toSpread
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
    case class A(l: List[E]) extends Atom {
      def toSpread = {
        var i = l.iterator
        var aa = noAlternatives
        while (i.hasNext) {
          val e = i.next.toSpread
          aa = aa put e
        }
        createAlt(aa)
      }
    }
    case class S(l: List[Pair]) extends Atom {
      def toSpread = {
        var i = l.iterator
        var em = emptyMap
        while (i.hasNext) {
          val e = i.next.toSpread
          em = em put e
        }
        EEMap(EMap(em,1),1)
      }
    }

    lazy val program = expr
    lazy val expr: Parser[E] = rep1(elem) ^^ { case l => E(l) }
    lazy val elem: Parser[Term] = pair | sequence | atom | operator
    lazy val sexpr: Parser[E] = "(" ~ expr ~ ")" ^^ { case "(" ~ l ~ ")" => l }
    lazy val atom: Parser[Atom] = spreadsheet | sexpr | alternatives | number | symbol
    lazy val alternatives: Parser[A] = "{" ~ repsep(expr,",") ~ "}" ^^ { case "{" ~ l ~ "}" =>  A(l) }
    lazy val spreadsheet: Parser[S] = "[" ~ repsep(mpair,",") ~ "]" ^^ { case "[" ~ l ~ "]" => S(l) }
    lazy val number = reg("[-]?[0-9]+") ^^ { i => Number(i.toInt) }
    lazy val symbol = reg("[a-zA-Z0-9]+") ^^ { t => MSymbol(t) }
    lazy val operator = unary | binary
    lazy val unary = reduce | wipe | desolve
    lazy val pair = npair | apair
    lazy val mpair = meqpair | spair
    lazy val path: Parser[List[Atom]] = repsep(atom,".") ^^ { case l => l }
    lazy val sequence: Parser[Term] = atom ~ "." ~ path ^^ { case e1 ~ "." ~ e2 => Sequence(List(e1) ++ e2)}
    lazy val spair = expr ^^ {case e => Pair(e,e) }
    lazy val meqpair = expr ~ "=" ~ expr ^^ {case e1 ~ "=" ~ e2 => Pair(e1,e2)}
    lazy val npair = (sequence | atom) ~ "`" ^^ { case e1 ~ "`" => Pair(E(List(e1)),E(List())) }
    lazy val apair = (sequence | atom) ~ "'" ~ (sequence | atom) ^^ { case e1 ~ "'" ~ e2 => Pair(E(List(e1)),E(List(e2))) }
    lazy val binary = add | mul | bind | iter
    lazy val wipe = "#" ^^ { case o => Wipe }
    lazy val desolve = ">" ^^ { case o => Desolve }
    lazy val reduce = "$" ^^ { case o => Reduce }
    lazy val add = "+" ^^ { case o => Add }
    lazy val mul = "*" ^^ { case o => Mul }
    lazy val iter = "~" ^^ { case o => Iter }
    lazy val bind = "!" ^^ { case o => Bind }

    def makeSpreadSheet(l: List[EPair]): MultiSetExpr = {
      val i = l.iterator
      var e = emptyMap
      while (i.hasNext) {
        e = e.put(i.next)
      }
      EEMap(EMap(e,1),1)
    }
  }

}
