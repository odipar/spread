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
  import IncrementalArithmetic.._

  import scala.language.existentials

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
      else if (t.left.isEmpty) toI(t.value) + $(sum3,t.right)
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


    val i1 = rr0
    val i2 = rr1

    val node1 = GenericTreeNode(null,i1)
    expand(node1)

    val model1 = new DefaultTreeModel(node1)

    var tree1 = new JTree(model1)
    tree1.setShowsRootHandles(true)
    tree1.setPreferredSize(new Dimension(600, 600))
    tree1.setMinimumSize(new Dimension(10, 10))

    val node2 = GenericTreeNode(null,i2)
    val model2 = new DefaultTreeModel(node2)
    var tree2 = new JTree(model2)
    tree2.setShowsRootHandles(true)
    tree2.setPreferredSize(new Dimension(600, 600))
    tree2.setMinimumSize(new Dimension(10, 10))

    var f = new JFrame()
    f.setLayout(new BorderLayout)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    var sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,tree1,tree2)

    f.add(sp, BorderLayout.CENTER)
    f.pack()
    f.setVisible(true)
  }

  trait MyTreeNode extends TreeNode {
    { println("totf: " + totf) }
    def parent: TreeNode
    def childs: Vector[TreeNode]

    def children = scala.collection.JavaConversions.asJavaEnumeration(childs.iterator)
    def	getAllowsChildren = true
    def getChildAt(childIndex: Int): TreeNode = childs(childIndex)
    def	getChildCount: Int = childs.size
    def getIndex(node: TreeNode): Int = sys.error("no")
    def getParent: TreeNode = parent
  }

  def expand(t: TreeNode): Unit = {
    var c = t.getChildCount
    var i = 0
    while (i < c) {
      expand(t.getChildAt(i))
      i = i + 1
    }
  }

  case class GenericTreeNode(parent: TreeNode, t: Any) extends MyTreeNode {

    lazy val childs: Vector[TreeNode] = {
      t match {
        //case x: FTreap[_,_] => Vector[TreeNode]()
        case v: LazyF1[_,_] => Vector[TreeNode](ForceTreeNode(this,v),GenericTreeNode(this,v.a))
        case v: LazyF2[_,_,_] => Vector[TreeNode](ForceTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b))
       // case v: LazyFI1[_,_] => Vector[TreeNode](ForceTreeNode(this,v),GenericTreeNode(this,v.a))
       // case v: LazyFI2[_,_,_] => Vector[TreeNode](ForceTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b))
        case v: LazyD1[_,_] => Vector[TreeNode](ForceTreeNode(this,v),EvalTreeNode(this,v),GenericTreeNode(this,v.a))
        case v: LazyD2[_,_,_] => Vector[TreeNode](ForceTreeNode(this,v),EvalTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b))
        //case v: LazyFD1[_,_] => Vector[TreeNode](ForceTreeNode(this,v),EvalTreeNode(this,v),GenericTreeNode(this,v.a))
        //case v: LazyFD2[_,_,_] => Vector[TreeNode](ForceTreeNode(this,v),EvalTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b))
        case ff: Trace[_,_] => Vector[TreeNode](GenericTreeNode(this, ff.f))
        case _ => Vector[TreeNode]()
      }
    }
    def	isLeaf: Boolean = childs.size == 0
    override def toString = t.toString
  }

  case class EvalTreeNode(parent: TreeNode, t: FValue[_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GenericTreeNode(this, t.eval))
    }
    def	isLeaf: Boolean = false
    override def toString = "EVAL {" + t + "}"
  }

  case class ForceTreeNode(parent: TreeNode, t: FValue[_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GenericTreeNode(this, t.force))
    }
    def	isLeaf: Boolean = false
    override def toString = "FORCE {" + t + "}"
  }
}
