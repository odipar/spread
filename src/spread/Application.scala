package spread

import javax.swing.{JScrollPane, JFrame}
import java.awt.BorderLayout

object Application
{
  final def main(args: Array[String])
  {
    var f = new SpreadFrame
    f.setVisible(true)
  }

  class SpreadFrame extends JFrame
  {
    var textPane = SpreadREPL
    var scrollPane = new JScrollPane(textPane)

    {
      setLayout(new BorderLayout)
      setSize(640, 400);
      getContentPane().add(scrollPane,BorderLayout.CENTER)
    }
  }
}
