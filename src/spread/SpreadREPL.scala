package spread

import javax.swing._
import javax.swing.text._
import java.awt._
import java.awt.event._
import Parser.SpreadParser._
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object SpreadREPL extends JTextPane() {
  var position : Position = document.getEndPosition

  final def document = getDocument

  {
    var rkey = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, Event.SHIFT_MASK);
    getInputMap.put(rkey, RunAction)
    appendString("SPREAD REPL v0.1.\nUse Shift+Enter to rewrite expressions.\n")
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

  var inputAttr : SimpleAttributeSet =
  {
    var l = new SimpleAttributeSet
    StyleConstants.setFontFamily(l, "monospaced");
    StyleConstants.setFontSize(l, 14);
    StyleConstants.setForeground(l, new Color(0,0,0));
    l
  }

  def startInput()
  {
    document.insertString(document.getLength, "=>", inputAttr)
    position = document.createPosition(document.getLength - 1)
    document.insertString(document.getLength, " ", inputAttr)
    setCaretPosition(document.getLength)
  }


  def appendString(s: String)
  {
    position = document.createPosition(document.getLength - 1)
    document.insertString(document.getLength, s, outputAttr)
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
                  appendString("\n")
                  appendString(term.toSpread.reduce.topString)
                  br = remainder.asInstanceOf[PackratReader[Char]]
                  appendString("\n")
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
