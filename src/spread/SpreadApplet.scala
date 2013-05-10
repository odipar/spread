package spread

import javax.swing._
import javax.swing.text._
import java.awt._
import java.awt.event._
import java.io._
import enchilada.core._
import enchilada.expression._
import enchilada.storage._

class SpreadApplet extends JApplet
{
  var repl = SpreadREPL
  var scrollPane : JScrollPane = new JScrollPane(repl)

  override def init
  {
    setLayout(new BorderLayout)
    getContentPane().add(scrollPane,BorderLayout.CENTER)
  }

  /*def runCode(s: String) : Unit =
  {
    repl.appendString(s)
    repl.RunAction.actionPerformed(null)
  } */
}