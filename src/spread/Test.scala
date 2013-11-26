package spread

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference

/*
 Copyright 2013: Robbert van Dalen
 */

object Test {
  import language.implicitConversions
  import IncrementalMemoization._
  import IncrementalTreap._
  import IncrementalArithmetic._
  import IncrementalTreeView._

  import javax.swing.tree._
  import javax.swing._
  import java.awt._

  val fac: Function1[I,I] = new Function1[I,I] {
    def apply(i: I): I  = {
      if (i() <= 1) 1
      else i * $(fac,i() - 1)
    }
    override def toString = "fac"
  }

  val fib: Function1[I,I] = new Function1[I,I] {
    def apply(a: I): I  = {
      val i = a()

      if (i <= 1) a
      else $(fib,i -1) + $(fib,i - 2)
    }
    override def toString = "fib"
  }

  lazy val sum3 = force11(sum(intord))

  def sum(p: PrioOrdering[Int,Int]): Function1[VFT[Int,Int],I] = new Function1[VFT[Int,Int],I] {
    def apply(a: VFT[Int,Int]): I = {
      val t = a()

      if (t.isEmpty) 0
      else if (t.left.isEmpty && t.right.isEmpty) t.value
      else if (t.left.isEmpty) (t.value:I) + $(sum3,t.right)
      else if (t.right.isEmpty) $(sum3,t.left) + t.value
      else $(sum3,t.left) + t.value + $(sum3,t.right)
    }
    override def toString = "sum"
  }

  final def main(args: Array[String]): Unit =
  {
    val iord = intord

    var c = 10
    var i = 2

    var r0: VFT[Int,Int] = T(1)
    while (i < c) {
      r0 = iord.join(r0,T(i))
      i = i +1
    }

    val rr0 = r0
    val rr1 = $(sum3,r0())

    val tree1 = createTView(GNode(null,rr0))
    val tree2 = createTView(GNode(null,rr1))

    var f = new JFrame()
    f.setLayout(new BorderLayout)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    var sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,tree1,tree2)

    f.add(sp, BorderLayout.CENTER)
    f.pack()
    f.setVisible(true)
  }

  def createTView(t: MyTreeNode): JComponent = {
    val model1 = new DefaultTreeModel(t)

    var tree1 = new JTree(model1)
    tree1.setShowsRootHandles(true)
    tree1.setPreferredSize(new Dimension(600, 600))
    tree1.setMinimumSize(new Dimension(10, 10))
    tree1
  }
}
