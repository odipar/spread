package spread

import scala.language.postfixOps
import scala.util.parsing.combinator.{PackratParsers, Parsers}
import java.lang.Character._

object Parser_v3 {

  import Engine_v3._

  object SpreadParser extends Parsers with PackratParsers {
    type Elem = Char

    import Engine_v3._

    def makeInt(s: String): Int = {
      if (s.startsWith("_")) (-s.split("_")(1).toInt)
      else s.toInt
    }

    final def buildString(l: scala.List[Char]) : String = {
      if (l.isEmpty) "" ; else l.head.toString + buildString(l.tail)
    }

    final def getReadLine(input: PackratReader[Char]) : ParseResult[Expr] = program(input)
    final def parse(input: PackratReader[Char]) : ParseResult[Expr] =  program(input)

    final val eolc = (13:Int).toChar
    final val lfdc = (10:Int).toChar
    lazy val lfd = elem("linefeed", ch => ch == lfdc)
    lazy val eol = elem("end-of-line", ch => ch == eolc)
    lazy val ret = lfd | (eol ~ lfd)

    def chrExcept(cs: Char*) = elem("", ch => (cs forall (ch !=)))

    def createMSet(ll: List[SetP]): Expr  = {
      val l = ll.reverse
      var e = emptyMSet
      var i = l.iterator
      var ii = 0
      while (i.hasNext) {
        val a = i.next
        e = e put a
        ii = ii + 1
      }
      MSet(emptySet,e)
    }
    def createMMap(l: List[MapP]): Expr  = {
      var e = emptyMMap
      var i = l.iterator
      var ii = 0
      while (i.hasNext) {
        val a = i.next
        e = e put a
        ii = ii + 1
      }
      MMap(emptySet,e)
    }
    def createExpr(l: List[Expr]): Expr  = {
      var e = emptyExpr
      var i = l.iterator
      var ii = zero
      while (i.hasNext) {
        val a = i.next
        e = e put CP(ii,a)
        ii = ii.incr
      }
      if (e.left.isEmpty && e.right.isEmpty) {
        e.some.get match {
          case CP(_,e: ELabeledExpr) => e
          case p => p.second
        }
      }
      else ECompoundExpr(emptySet,e)
    }
    def str(l: List[Char]): Expr = {
      if (l.size == 1) EChar(emptySet,l.head)
      else {
        val it = l.iterator
        var i = zero
        var m = emptyMMap
        while (it.hasNext) {
          val e = it.next
          m = m put MapP(i,EChar(emptySet,e))
          i = i.incr
        }
        MMap(emptySet,m)
      }
    }

    def seq(l: List[Expr]): Expr = {
      var i = l.iterator
      var ii = zero
      var em = emptyMMap
      while (i.hasNext) {
        val e = i.next
        em = em put MapP(ii,e)
        ii = ii.incr
      }
      MMap(emptySet,em)
    }

    lazy val digit = elem("digit", isDigit) ^^ {c => c}
    lazy val letter = elem("letter", isLetter) ^^ {c => c}
    lazy val character = chrExcept('\"')
    lazy val sp = ' ' ^^^ { }
    lazy val wse = sp | ret
    lazy val ws = rep(wse)
    lazy val ws2 = rep(sp)
    lazy val program: Parser[Expr] = expr2 <~ ret ^^ { case e => e }
    lazy val expr: Parser[Expr] = ws ~> rep1sep(elem,ws) <~ ws ^^ { case l  => createExpr(l) }
    lazy val expr2: Parser[Expr] = ws2 ~> rep1sep(elem,ws) <~ ws2 ^^ { case l  => createExpr(l) }
    lazy val elem: Parser[Expr] =  fold | foreach | labeled | sequence | atom
    lazy val atom: Parser[Expr] =  alternatives | string | number | subexpr | map | operator | symbol
    lazy val operator = unary | binary
    lazy val unary = dup | pack | iota | reduce | split
    lazy val fold = afold | efold
    lazy val afold = '.' ~> satom ^^ { case e => EFold(emptySet,e)}
    lazy val efold = '.' ^^ { case e => EFold(emptySet,Empty) }
    lazy val foreach = '@' ~> satom ^^ { case e => EForeach(emptySet,e) }
    lazy val dup = '>' ^^ { case a => EDup(emptySet) }
    lazy val pack = '^' ^^ { case a => EPack(emptySet) }
    lazy val undup = '<' ~> satom ^^ { case e => EUnDup(emptySet,e) }
    lazy val reduce = '$' ^^ { case a => ERed(emptySet) }
    lazy val iota = '~' ^^ { case a => EIota(emptySet) }
    lazy val binary = add | mul | max | min | sub | swap | concat | bind | undup
    lazy val add = '+' ^^ { case a => EAdd(emptySet) }
    lazy val mul = '*' ^^ { case a => EMul(emptySet) }
    lazy val max = '|' ^^ { case a => EMax(emptySet) }
    lazy val min = '&' ^^ { case a => EMin(emptySet) }
    lazy val sub = '-' ^^ { case a => ESub(emptySet) }
    lazy val bind = '!' ^^ { case a => EBind(emptySet) }
    lazy val split = '/' ^^ { case a => ESplit(emptySet) }
    lazy val concat = ';' ~> '+' ^^ { case a => EConcat(emptySet) }
    lazy val swap = '\\' ^^ { case a => ESwap(emptySet) }
    lazy val string = '\"' ~> rep1(character) <~ '\"' ^^ { case s => str(s) }
    lazy val setpair = msetpair | ssetpair
    lazy val ssetpair = expr ^^ { case l => SetP(one,l) }
    lazy val msetpair = expr ~ ':' ~! expr ^^ { case m ~ ':' ~ l => SetP(m,l) }
    lazy val alternatives: Parser[Expr] = '{' ~> (repsep(setpair,',') <~ '}') ^^ { case l => createMSet(l) }
    lazy val subexpr = '(' ~> expr2 <~ ')' ^^  { case e => e }
    lazy val number = posnumber | negnumber
    lazy val posnumber = rep1(digit) ^^ { i => EInt(emptySet,makeInt(buildString(i))) }
    lazy val negnumber = '_' ~> rep1(digit) ^^ { case i => EInt(emptySet,-makeInt(buildString(i))) }
    lazy val symbol: Parser[Expr] = rep1(letter) ^^ { t => Symbol(emptySet,buildString(t)) }
    lazy val satom: Parser[Expr] =  atom | sequence
    lazy val labeled = alabeled | nlabeled
    lazy val nlabeled = satom <~ '\'' ^^ { case e1 => ELabeledExpr(emptySet,e1,Empty) }
    lazy val alabeled = satom ~ '\'' ~ satom ^^ { case e1 ~ '\'' ~ e2 => ELabeledExpr(emptySet,e1,e2)}
    lazy val map: Parser[Expr] = '[' ~> repsep(mappair,',') <~ ']' ^^ { case l => createMMap(l) }
    lazy val mappair = meqpair | spair
    lazy val meqpair = expr ~ '=' ~! expr ^^ {case e1 ~ '=' ~ e2 => MapP(e1,e2)}
    lazy val spair = expr ^^ {case e => MapP(e,e) }
    lazy val path: Parser[List[Expr]] = rep1sep(atom,'.') ^^ { case l => l }
    lazy val sequence = atom ~ '.' ~ path ^^ { case e1 ~ '.' ~ e2 => seq(List(e1) ++ e2)}

  }
}