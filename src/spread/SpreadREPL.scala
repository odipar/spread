package spread

import javax.swing._
import javax.swing.text._
import java.awt._
import java.awt.event._
import Parser_v3.SpreadParser._
import scala.util.parsing.input.CharSequenceReader
import javax.swing.text.html._
import Engine_v3._

class SpreadREPL extends JPanel {

  def startInput = {
    input.setCaretPosition
  }

  var input = new SpreadInput
  var output = new SpreadOutput

  {
    setLayout(new BorderLayout())
    var scrollPane1 = new JScrollPane(input)
    var scrollPane2 = new JScrollPane(output)

    //textPane.setPreferredSize(new Dimension(640, 400))

    val s = new JSplitPane(JSplitPane.VERTICAL_SPLIT,scrollPane2,scrollPane1)
   s.setDividerLocation(400 + s.getInsets().left);

    add(s, BorderLayout.CENTER);
  }

  class SpreadOutput extends JTextPane {
    def doc: HTMLDocument = getDocument.asInstanceOf[HTMLDocument]

    {
      var kit = new HTMLEditorKit
      var styleSheet = kit.getStyleSheet
      styleSheet.addRule("body { color:#000; font-family:courier; margin: 0px; font-size: 11px; }")
      styleSheet.addRule("pre { color:#000; font-family:courier; margin: 0px; font-size: 11px; }")
      styleSheet.addRule("table { border: 0px; padding: 0px; border-spacing:0; border-collapse:collapse; }")
      styleSheet.addRule("th { border: 0px; }")

      styleSheet.addRule("pre.cbracket { padding: 0px; background: #f5f5f5; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");
      styleSheet.addRule("pre.sbracket { padding: 0px; background: #eeeeee; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");
      styleSheet.addRule("pre.bracket { padding: 0px; background: #eeeeee; margin: 0; border-width: 0px 0px 0 0;font-size: 11px; }");
      styleSheet.addRule("pre.equals { padding: 1px; background: #eeeeee; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 8px; }");
      styleSheet.addRule("pre.tracestep { padding: 0px; background: #ffffff; margin: 2 0 0 0; border-width: 0 0 0 0; border-style: solid; font-size: 8px; }");

      styleSheet.addRule("td.comma { padding: 0px; background: #f2f2f2; margin: 0; border-width: 0px 0px 0 0;font-size: 8px; }");
      styleSheet.addRule("td.bracket { padding: 0px; background: #bbbbbb; margin: 0; border-width: 0px 0px 0 0;font-size: 10px; }");
      styleSheet.addRule("td.quot { padding: 0px; background: #eeeeee; margin: 0; border-width: 0 0 0 0;font-size: 10px; }");
      styleSheet.addRule("td.labeltd { padding: 0px; background: #eeeeee; margin: 0; border-width: 0 0 0 0;font-size: 11px; }");
      styleSheet.addRule("td.equals { padding: 0px; background: #eeeeee; margin: 1px; border-width: 0 0 0 0; font-size: 8px; }");

      styleSheet.addRule("td.mytd4 { padding: 0px; background: #f5f5f5; margin: 0 0 0 0; border-width: 0 0 0 0; font-size: 11px; }");
      styleSheet.addRule("td.mytd3 { padding: 0px; background: #ffffff; margin: 0 0 0 0; border-width: 0 0 0 0; font-size: 11px; }");
      styleSheet.addRule("td.mytd2 { padding: 2px; background: #ffffff; margin: 0 0 0 0; border-width: 0 0 0 0; font-size: 11px; }");
      styleSheet.addRule("td.mytd { padding: 2px; background: #ffffff; margin: 0 0 0 0; border-width: 1px;  border-style: solid; font-size: 11px; }");
      styleSheet.addRule("td.mapp { padding: 2px; background: #efefef; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");
      styleSheet.addRule("td.tdkey { padding: 2px; background: #eeeeee; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");
      styleSheet.addRule("td.tdbracket { padding: 0px; background: #dddddd; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 13px; }");
      styleSheet.addRule("td.tdrbracket { padding: 0px; background: #dddddd; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");
      styleSheet.addRule("td.tdvalue { padding: 2px; background: #ffffff; margin: 0; border-width: 0 0 0 0; border-style: solid; font-size: 11px; }");

      styleSheet.addRule("h1 {color: blue;}")
      styleSheet.addRule("h2 {color: #ff0000;}")
      styleSheet.addRule("pre { margin: 0; }")
      setEditorKit(kit)

      var doc = kit.createDefaultDocument()
      setDocument(doc);
      setText("<html><body id='start'><pre> </pre></body></html>")
    }

    def appendExpr(e: Expr) = {

    }
    def appendHTML(s: String) = {
      val e = doc.getElement("start")
      doc.insertBeforeStart(e,s)
      doc.insertBeforeStart(e,"<pre> </pre>")
    }
  }

  class SpreadInput extends JTextPane {
    var position : Position = document.getEndPosition

    final def document = getDocument

    {
      var rkey = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, Event.SHIFT_MASK);
      getInputMap.put(rkey, RunAction)
      appendString("SPREAD REPL v0.3.\nUse Shift+Enter to rewrite expressions.\n")
      startInput
    }

    var outputAttr : SimpleAttributeSet =
    {
      var l = new SimpleAttributeSet
      StyleConstants.setFontFamily(l, "monospaced");
      StyleConstants.setFontSize(l, 14);
      StyleConstants.setForeground(l, new Color(0,0,255));
      l
    }

    var outputAttr2 : SimpleAttributeSet =
    {
      var l = new SimpleAttributeSet
      StyleConstants.setFontFamily(l, "monospaced");
      StyleConstants.setFontSize(l, 14);
      StyleConstants.setForeground(l, new Color(0,0,255));
      l.addAttribute("FIX","ME")
      l
    }

    var inputAttr : SimpleAttributeSet =
    {
      var l = new SimpleAttributeSet
      StyleConstants.setFontFamily(l, "monospaced");
      StyleConstants.setFontSize(l, 14);
      StyleConstants.setForeground(l, new Color(0,0,0));
      l
    }

    def setCaretPosition: Unit = {
      setCaretPosition(document.getLength)
    }
    def startInput()
    {
      document.insertString(document.getLength, "=>", inputAttr)
      position = document.createPosition(document.getLength - 1)
      document.insertString(document.getLength, " ", inputAttr)
      setCaretPosition
    }


    def appendString(s: String)
    {
      position = document.createPosition(document.getLength - 1)
      var i = 0
      while (i < s.length) {
        var a = outputAttr
        if ((i % 2) == 0) { a = outputAttr2 }
        document.insertString(document.getLength, "" + s.charAt(i),a)
        i = i + 1
      }
      setCaretPosition(document.getLength)
    }

    object RunAction extends AbstractAction("Run")
    {
      override def actionPerformed(evt: ActionEvent) : Unit =
      {
        val r = try
        {
          document.getText(position.getOffset+1, document.getLength - position.getOffset - 1)
        }
        catch { case t: Throwable => "" }
        var reader = new CharSequenceReader(r.trim + "\n")
        step(new PackratReader(reader))
        appendString("\n")
        startInput
      }
    }

    def step(r: PackratReader[Char]) {
      var br = r
      var one = false
      while((!br.atEnd) && (!one))
      {
        try
        {
          val li = getReadLine(br)
          li match
          {
            case Success(line, remainder) => line match
            {
              case _ =>
              {
                val pi = parse(br)

                pi match
                {
                  case Success(term, remainder) =>
                  {
                    var t = term
                    var r = fullReduce2(t)
                    //println(r.asHTML)
                    output.appendHTML(r.asHTML)
                    appendString("\n")
                    appendString(r.asString)
                    appendString("\n")
                    br = remainder.asInstanceOf[PackratReader[Char]]
                  }
                  case Failure(msg, remainder) => { appendString("\nFailure: " + msg.toString) ; one = true }
                  case Error(msg, remainder) => { appendString("\nError: " + msg.toString) ; one = true }
                }
              }
            }
            case Failure(msg, remainder) => { appendString("\nFailure: " + msg.toString) ; one = true }
            case Error(msg, remainder) => { appendString("\nError: " + msg.toString) ; one = true }
          }
        }
        catch
          {
            case e: Throwable =>
            {
              e.printStackTrace(System.out)
              appendString("\n " + e.toString)
              one = true
            }
          }
      }
    }
  }
}
