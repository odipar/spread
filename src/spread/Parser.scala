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

    trait Atom extends Term

    case class Number(i: Int) extends Atom
    case class MSymbol(s: String) extends Atom
    case class Pair(label: E, target: E) extends Term {
      def toSpread: EPair = EPair(label.toSpread,target.toSpread,1)
    }

    trait Operator extends Atom
    trait UnaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr): MultiSetExpr
    }
    trait BinaryOp extends Operator {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr): MultiSetExpr
    }
    case object Add extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EAdd(arg1,arg2,1)
    }
    case object Bind extends BinaryOp {
      def toSpread(arg1: MultiSetExpr, arg2: MultiSetExpr) = EBind(arg1,arg2,1)
    }
    case object Reduce extends UnaryOp {
      def toSpread(arg1: MultiSetExpr) = ERed(arg1,1)
    }
    case class E(l: List[Term]) extends Term {
      def toSpread: MultiSetExpr = {
        var s = Stack[MultiSetExpr]()
        var i = l.iterator
        while (i.hasNext) {
          i.next match {
            case p: Pair => s = s.push(p.toSpread)
            case S(sl) => {
              var i = sl.iterator
              var em = emptyMap
              while (i.hasNext) {
                val e = i.next.toSpread
                em = em put e
              }
              s = s.push(EEMap(EMap(em,1),1))
            }
            case Empty => s = s push EExpr
            case ee: E => s = s push ee.toSpread
            case Number(i) => s = s.push(EInt(i,1))
            case A(a) => {
              var i = a.iterator
              var aa = noAlternatives
              while (i.hasNext) {
                val e = i.next.toSpread
                aa = aa put e
              }
              s = s.push(createAlt(aa))
            }
            case MSymbol(i) => s = s.push(ESymbol(i,1))
            case u: UnaryOp => {
              val arg1 = s.top
              s = s.pop
              s = s.push(u.toSpread(arg1))
            }
            case b: BinaryOp => {
              val arg2 = s.top
              s = s.pop
              val arg1 = s.top
              s = s.pop
              s = s.push(b.toSpread(arg1,arg2))
            }
          }
        }
        if (s.size == 0) EExpr
        else if (s.size == 1) s.top
        else sys.error("stack unbalanced: " + s)
      }
    }
    case class A(l: List[E]) extends Atom
    case class S(l: List[Pair]) extends Term

    lazy val program = expr
    lazy val expr: Parser[E] = rep1(elem) ^^ { case l => E(l) }
    lazy val empty: Parser[Term] = "." ^^ { case "." => Empty }
    lazy val elem: Parser[Term] = pair | atom
    lazy val sexpr: Parser[E] = "(" ~ expr ~ ")" ^^ { case "(" ~ l ~ ")" => l }
    lazy val atom: Parser[Term] = empty | spreadsheet | sexpr | alternatives | number | symbol | operator
    lazy val alternatives: Parser[A] = "{" ~ repsep(expr,",") ~ "}" ^^ { case "{" ~ l ~ "}" =>  A(l) }
    lazy val spreadsheet: Parser[S] = "[" ~ repsep(mpair,',') ~ "]" ^^ { case "[" ~ l ~ "]" => S(l) }
    lazy val number = reg("[-]?[0-9]+") ^^ { i => Number(i.toInt) }
    lazy val symbol = reg("[a-zA-Z0-9]+") ^^ { t => MSymbol(t) }
    lazy val operator = unary | binary
    lazy val unary = reduce
    lazy val pair = apair
    lazy val mpair = expr ~ "=" ~ expr ^^ {case e1 ~ "=" ~ e2 => Pair(e1,e2)}
    lazy val apair = atom ~ "'" ~ atom ^^ { case e1 ~ "'" ~ e2 => Pair(E(List(e1)),E(List(e2))) }
    lazy val binary = add | bind
    lazy val reduce = "$" ^^ { case o => Reduce }
    lazy val add = "+" ^^ { case o => Add }
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
