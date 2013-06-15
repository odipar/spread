package spread

import javax.swing._
import javax.swing.text._
import java.awt._

class SpreadApplet extends JApplet
{

  override def init = {
    //Execute a job on the event-dispatching thread:
    //creating this applet's GUI.
    try {
      javax.swing.SwingUtilities.invokeAndWait(new Runnable() {
        def run = createGUI
      })
    }
  }

  def createGUI = {
    var textArea = new SpreadREPL
    var scrollPane = new JScrollPane(textArea)

    var content = new JPanel
    content.setLayout(new BorderLayout)
    content.add(scrollPane, BorderLayout.CENTER)

    setContentPane(content)

    textArea.input.requestFocusInWindow

  }
}