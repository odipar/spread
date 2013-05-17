package spread

import javax.swing.{JScrollPane, JFrame}
import java.awt.BorderLayout

import java.math.BigInteger
import java.io._
import javax.swing._
import javax.swing.text._
import java.awt._
import java.awt.event._
import javax.swing._
import javax.swing.event._
import javax.swing.text._

object Application
{
  final def main(args: Array[String]): Unit =
  {
    var f = new JFrame()
    f.setLayout(new BorderLayout);
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    var textPane = new SpreadREPL
    var scrollPane = new JScrollPane(textPane)
    //textPane.setPreferredSize(new Dimension(640, 400))
    scrollPane.setPreferredSize(new Dimension(640, 400))
    scrollPane.setMinimumSize(new Dimension(10, 10))
    f.add(scrollPane, BorderLayout.CENTER);
    f.pack
    f.setVisible(true)
  }
}
