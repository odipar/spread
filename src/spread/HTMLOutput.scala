package spread

import scala.xml._
import Engine_v3._
import java.math.BigInteger
import language.implicitConversions

object HTMLOutput {

  def toString(o: Expr): String = {
    val n: Node = toHTML(o)
    n.text
  }
  def toHTML(o: Expr): Node = o match {
    case f: EFoldImpl => foldToHTML(f)
    case f: EForeachImpl => foreachToHTML(f)
    case i: EIntImpl => <pre>{i.asString}</pre>
    case s: MSetImpl => SetToHTML(s)
    case t: ETraceImpl => TraceToHTML(t)
    case o: Operator => OperatorToHTML(o)
    case c: EChar => CharToHTML(c)
    case s: SymbolImpl => SymbolToHTML(s)
    case c: ECompoundExprImpl => tableModelToHTML(ECTableModel(c))
    case m: MMapImpl => {
      if (m.isString) stringToHTML(m)
      else if (m.isSequence) sequenceToHTML(m)
      else mapToHTML(m)
    }
    case l: ELabeledExprImpl => LabeledExprToHTML(l)
    case _ => sys.error("illegal state: " + o)
  }

  def OperatorToHTML(o: Operator): Node = <pre>{o.asString}</pre>
  def SymbolToHTML(c: SymbolImpl): Node = <pre>{c.s}</pre>
  def CharToHTML(c: EChar): Node = <pre>{"\"" + c.c + "\""}</pre>

  def foldToHTML(f: EFoldImpl): Node = f.a match {
    case Empty => <pre>{"."}</pre>
    case _ => {
      <table><tr>{td4(<pre>{"."}</pre>)}{td4(subHTML(f.a))}</tr></table>
    }
  }

  def foreachToHTML(f: EForeachImpl): Node = f.a match {
    case Empty => <pre>{"@"}</pre>
    case _ => {
      <table><tr>{td4(<pre>{"@"}</pre>)}{td4(subHTML(f.a))}</tr></table>
    }
  }

  def LabeledExprToHTML(l: ELabeledExprImpl): Node = {
    val le = {
      if (l.second == Empty) td3(subHTML(l.first)) :+ td3(quote)
      else td3(subHTML(l.first)) :+ td3(quote) :+ td3(subHTML(l.second))
    }
    <table><tr>{le}</tr></table>
  }

  def TraceToHTML(t: ETraceImpl): Node = {
    <table><tr>{td4(openbracket)}{td2(tableModelToHTML(ETableModel(t)))}{td4(closebracket)}</tr></table>
  }

  def etd0(o: Expr): Node = td0(toHTML(o))
  def etd0(o: Node): Node = <td class="mytd">{" "}{o}</td>
  def td0(o: Expr): Node = td0(toHTML(o))
  def td0(o: Node): Node = <td class="mytd">{o}</td>
  def td2(o: Expr): Node = td2(toHTML(o))
  def td2(o: Node): Node = <td class="mytd2">{o}</td>
  def td3(o: Expr): Node = td3(toHTML(o))
  def td3(o: Node): Node = <td class="mytd3">{o}</td>
  def td4(o: Expr): Node = td4(toHTML(o))
  def td4(o: Node): Node = <td class="mytd4">{o}</td>

  def tracestep = TraceSymbol(<pre class="tracestep">{"=>"}</pre>)
  def semicolon = PosSymbol(<table><tr><td class="comma">{";"}</td></tr></table>)
  def colon = PosSymbol(<table><tr><td class="comma">{":"}</td></tr></table>)
  def mequals = PosSymbol(<table><tr><td class="equals">{"="}</td></tr></table>)
  def quote = QuoteSymbol(<table><tr><td class="quot">{"'"}</td></tr></table>)
  def comma = PosSymbol(<table><tr><td class="comma">{","}</td></tr></table>)
  def openbracket = OpenSymbol(<pre class="bracket">{"("}</pre>)
  def closebracket = CloseSymbol(<pre class="bracket">{")"}</pre>)
  def curlyopenbracket = OpenSymbol(<pre class="cbracket">{"{"}</pre>)
  def curlyclosebracket = CloseSymbol(<pre class="cbracket">{"}"}</pre>)
  def squareopenbracket = OpenSymbol(<pre class="sbracket">{"["}</pre>)
  def squareclosebracket = CloseSymbol(<pre class="sbracket">{"]"}</pre>)

  def stringToHTML(m: MMapImpl): Node = {
    var s = "\""

    var mm = m.m
    while (mm.first != None) {
      var mp = mm.first.get
      mp.second match {
        case e: ECharImpl => s = s + e.c
        case _ => sys.error("illegal state")
      }

      mm = mm.split(mp)._3
    }

    s = s +"\""

    <pre>{s}</pre>
  }

  def sequenceToHTML(m: MMapImpl): Node = {
    var ss: NodeSeq = <s></s>

    var mm = m.m
    var i = 0

    while (mm.first != None) {
      var mp = mm.first.get

      if (i > 0) ss = ss :+ td3(semicolon)

      if (i == 0) ss = td3(subHTML(mp.second))
      else ss = ss :+ td3(subHTML(mp.second))

      mm = mm.split(mp)._3
      i = i + 1
    }

    <table><tr>{ss}</tr></table>
  }

  def sHTML(s: Node): Node = {
    <table><tr>{td3(openbracket)}{td3(s)}{td3(closebracket)}</tr></table>
  }

  def subHTML(o: Expr): Node = o match {
    case m: MMapImpl => {
      if (m.isSequence) sHTML(toHTML(m))
      else toHTML(m)
    }
    case f: EForeachImpl => sHTML(toHTML(o))
    case f: EFoldImpl => sHTML(toHTML(o))
    case e: ELabeledExprImpl => sHTML(toHTML(o))
    case e: ECompoundExprImpl => sHTML(toHTML(o))
    case _ => toHTML(o)
  }

  def mapToHTML(m: MMapImpl): Node = {
    var ss: NodeSeq = td4(squareopenbracket)

    var mm = m.m
    var i = 0

    while (mm.first != None) {
      var mp = mm.first.get
      if (i > 0) ss = ss :+ td3(comma)

      if (ExprOrdering.compare(mp.first,mp.second) == 0) ss = ss :+ td3(mp.second)
      else ss = ss :+ td3(mp.first) :+ td3(mequals) :+ td3(mp.second)

      mm = mm.split(mp)._3
      i = i + 1
    }

    ss = ss :+ td4(squareclosebracket)

    <table><tr>{ss}</tr></table>
  }

  def SetToHTML(s: MSetImpl): Node = {
    var ss: NodeSeq = td4(curlyopenbracket)

    var mm = s.m
    var i = 0

    while (mm.first != None) {
      var mp = mm.first.get
      if (i > 0) ss = ss :+ td3(comma)

      if (ExprOrdering.compare(mp.first,one) == 0) ss = ss :+ td3(mp.second)
      else ss = ss :+ td3(mp.first) :+ td3(colon) :+ td3(mp.second)

      mm = mm.split(mp)._3
      i = i + 1
    }

    ss = ss :+ td4(curlyclosebracket)

    <table><tr>{ss}</tr></table>
  }

  def tableModelToHTML(tm: TableModel): Node = {
    var c = tm.columnCount
    var r = tm.rowCount

    var rr = zero
    var rows: NodeSeq = <r></r>


    while (rr.compare(r) < 0) {
      var cc = zero
      var cols: NodeSeq = <c></c>

      var i = 0

      while (cc.compare(c) < 0) {
        val td: Node = tm.getValue(rr,cc) match {
          case None => { i = i - 1 ; <td class="mytd2"><pre></pre></td>  }
          case Some(OpenSymbol(x)) => {
            if (i <= 0) {
              i = -2
              <td class="mytd4">{x}</td>
            }
            else {
              i = -2
              <td class="mytd4">{" "}{x}</td>
            }
          }
          case Some(QuoteSymbol(x)) => { i = -1 ; <td class="mytd4">{x}</td> }
          case Some(TraceSymbol(x)) => { i = -1 ; <td class="mytd4">{" "}{x}{" "}</td> }
          case Some(CloseSymbol(x)) => <td class="mytd4">{x}</td>
          case Some(PosSymbol(x)) => <td class="mytd4">{x}</td>
          case Some(x) => {
            if (i > 0) etd0(x)
            else {
              i = 0
              td0(x)
            }
          }
        }
        if (cc.compare(zero) == 0) cols = td
        else cols = cols :+ td
        cc = cc.incr
        i = i + 1
      }
      val tr = <tr>{cols}</tr>
      if (rr.compare(zero) == 0) rows = tr
      else rows = rows :+ tr
      rr = rr.incr
    }
    <table>{rows}</table>
  }

  trait TableModel {
    def columnCount: Number
    def rowCount: Number
    def getValue(row: Number, col: Number): Option[SpreadNode]
  }

  case class ECTableModel(cc: ECompoundExprImpl) extends TableModel {
    def path(p: PAT, c: ECompoundExprImpl, i: Number): PAT = {
      var pp = p
      val cp = c.cex.get(CP(i,Empty))
      cp match {
        case Some(CP(_,ec: ECompoundExprImpl)) => {
          val cp = (emptyPath add LeafPath(_three)) append path(ec) append (emptyPath add LeafPath(_four))
          var in = 0
          var s = cp.size
          while (in<s) {
            val (l,r) = cp.split(in)

            r.first.get match {
              case l: LeafPath => pp = pp add CompoundPath(emptyPath add LeafPath(i) add l)
              case c: CompoundPath =>  {
                pp = pp add CompoundPath((emptyPath add LeafPath(i)) append c.c)
              }
            }
            in = in + 1
          }
        }
        case Some(CP(_,t: ELabeledExprImpl)) => {
          t.second match {
            case ec: ECompoundExprImpl => {
              val cp = (emptyPath add LeafPath(_two) add LeafPath(_one) add LeafPath(_three)) append path(ec) append (emptyPath add LeafPath(_four))
              var in = 0
              var s = cp.size
              while (in<s) {
                val (l,r) = cp.split(in)

                r.first.get match {
                  case l: LeafPath => pp = pp add CompoundPath(emptyPath add LeafPath(i) add l)
                  case c: CompoundPath =>  {
                    pp = pp add CompoundPath((emptyPath add LeafPath(i)) append c.c)
                  }
                }
                in = in + 1
              }
            }
            case _ =>  pp = pp add LeafPath(i)
          }
        }
        case _ => pp = pp add LeafPath(i)
      }
      pp
    }

    def path(c: ECompoundExprImpl): PAT  = {
      if (c.isEmpty) emptyPath
      else {
        var pp = emptyPath
        var i = zero
        val ii = c.last.get.first
        while (i.compare(ii) < 0) {
          pp = path(pp,c,i)
          i = i.incr
        }
        pp = path(pp,c,i)
        pp
      }
    }
    lazy val path: PAT = path(cc)
    val columnCount = EInt(BigInteger.valueOf(path.size))
    val rowCount = one
    def pathToElement(c: ECompoundExprImpl, p: Path): Option[SpreadNode] = {
      p match {
        case l: LeafPath => {
          var pi: LeafPath = l
          if (pi.index.compare(_two) < 0) {
            if (pi.index.compare(_three) == 0) Some(openbracket)
            else if (pi.index.compare(_four) == 0) Some(closebracket)
            else Some(tracestep)
          }
          else {
            c.cex.get(CP(pi.index,Empty)) match {
              case Some(CP(_,t: ETraceImpl)) => {
                Some(toHTML(t))
              }
              case Some(CP(_,e)) => Some(toHTML(e))
              case _ =>  None
            }
          }
        }
        case cp: CompoundPath => {
          var pi: LeafPath = cp.c.first.get.asInstanceOf[LeafPath]
          if (pi.index.compare(_two) < 0) {
            if (pi.index.compare(_three) == 0) Some(openbracket)
            else if (pi.index.compare(_four) == 0) Some(closebracket)
            else Some(tracestep)
          }
          else {

            var e = c.cex.get(CP(pi.index,Empty))

            e match {
              case Some(CP(_,e: ECompoundExprImpl)) => {
                val nc = cp.c.split(1)._2
                if (nc.size == 1) {
                  pathToElement(e,nc.last.get)
                }
                else pathToElement(e,CompoundPath(nc))
              }
              case Some(CP(_,e: ELabeledExprImpl)) => {
                e.second match {
                  case ee: ECompoundExprImpl => {
                    val nc = cp.c.split(1)._2
                    if (nc.size == 1) {
                      val ii = nc.last.get
                      ii match {
                        case LeafPath(i) => {
                          if (i.compare(_two) == 0) Some(toHTML(e.first))
                          else if (i.compare(_one) == 0) Some(quote)
                          else pathToElement(ee,nc.last.get)
                        }
                      }
                    }
                    else pathToElement(ee,CompoundPath(nc))
                  }
                  case _ => {
                    val ll = cp.c.last.get.asInstanceOf[LeafPath]
                    val nc = cp.c.split(1)._2
                    if (iszeropath(nc)) Some(toHTML(e))
                    else None
                  }
                }
              }
              case Some(CP(_,t: ETraceImpl)) => {
                val ll = cp.c.last.get.asInstanceOf[LeafPath]
                val nc = cp.c.split(1)._2
                if (iszeropath(nc)) Some(toHTML(t))
                else None
              }
              case Some(CP(_,e: Expr)) => {
                val ll = cp.c.last.get.asInstanceOf[LeafPath]
                val nc = cp.c.split(1)._2
                if (iszeropath(nc)) Some(toHTML(e))
                else None
              }
              case _ => None
            }
          }
        }
      }
    }

    def iszeropath(p: PAT): Boolean = {
      if (p.size == 1) p.first.get match {
        case l: LeafPath => (l.index.compare(zero) == 0)
        case _ => sys.error("illegal state")
      }
      else iszeropath(p.left) && iszeropath(p.right)
    }
    def getValue(c: ECompoundExprImpl, ii: Number) = {
      val i: Int = asInt(ii)
      val (l: PAT,r: PAT) = path.split(i)
      pathToElement(c,r.first.get)
    }
    def getValue(row: Number, col: Number): Option[SpreadNode] = getValue(cc,col)
  }

  case class ETableModel(trace: ETraceImpl) extends TableModel {
    lazy val cModel = {
      var t = trace.t
      t.first.get.second match {
        case e: ECompoundExprImpl => {
          var tm = ECTableModel(e)
          var max = zero
          while (t.first != None) {
            val tp = t.first.get
            var ntm = ECTableModel(tp.second.asInstanceOf[ECompoundExprImpl])
            if (ntm.columnCount.compare(tm.columnCount) > 0) {
              tm = ntm
            }
            t = t.split(tp)._3
          }
          tm
        }
        case ee => {
          ECTableModel(ece2(emptySet,emptyExpr put CP(zero,ee)))
        }
      }

    }
    lazy val columnCount = cModel.columnCount.incr
    lazy val rowCount = trace.t.last.get.first.incr
    def getValue(row: Number, col: Number) = {
      if (col.compare(columnCount.decr) == 0) {
        if (row.compare(rowCount.decr) == 0) None
        else Some(tracestep)
      }
      else {

        trace.t.get(TraceP(row,Empty)) match {
          case None => None
          case Some(x) => x.second match {
            case l: ELabeledExprImpl => {
              if (col.compare(zero) == 0) Some(toHTML(l.first))
              else if (col.compare(one) == 0) Some(quote)
              else cModel.getValue(ece2(emptySet,emptyExpr put CP(zero,l.second)),col)
            }
            case c: ECompoundExprImpl => cModel.getValue(c,col)
            case xx => Some(toHTML(xx))
          }
        }
      }
    }
  }

  trait SpreadNode {
    def node: Node
  }

  case class Symbol(node: Node) extends SpreadNode
  case class PosSymbol(node: Node) extends SpreadNode
  case class OpenSymbol(node: Node) extends SpreadNode
  case class CloseSymbol(node: Node) extends SpreadNode
  case class TraceSymbol(node: Node) extends SpreadNode
  case class QuoteSymbol(node: Node) extends SpreadNode

  implicit def fromSpreadNode(n: SpreadNode): Node = n.node
  implicit def toSpreadNode(n: Node): SpreadNode = Symbol(n)
}