package spread

import scala.sys

/*

 Copyright 2013: Robbert van Dalen

 */

object Test {
  import IncrementalMemoization._
  import language.implicitConversions

  import javax.swing.tree._
  import javax.swing._
  import java.awt._


  // Proof of concept works!
  final def main(args: Array[String]): Unit = {}
  {

    println("sum4.1: " + $_(sum4,Vector(1,2,3,4,5,6,7,8))()())
    println("sum4.2: " + $_(sum4,Vector(10,2,3,4,5,6,7,8))()())

    /*
    val node1 = MyTreeNode(null,f1)
    val model1 = new DefaultTreeModel(node1)
    var tree1 = new JTree(model1)
    tree1.setPreferredSize(new Dimension(600, 300))
    tree1.setMinimumSize(new Dimension(10, 10))

    val node2 = MyTreeNode(null,f2)
    val model2 = new DefaultTreeModel(node2)
    var tree2 = new JTree(model2)
    tree2.setPreferredSize(new Dimension(600, 300))
    tree2.setMinimumSize(new Dimension(10, 10))

    var f = new JFrame()
    f.setLayout(new BorderLayout)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    f.add(tree1, BorderLayout.EAST)
    f.add(tree2, BorderLayout.WEST)
    f.pack()
    f.setVisible(true)              */

  }

  def sum(v: Vector[Int]): Int = {
    val s = v.size
    if (s == 0) sys.error("empty sum")
    else if (s == 1) v(0)
    else {
      val si = s / 2
      val (l,r) = v.splitAt(si)
      sum(l) + sum(r)
    }
  }

  type Sum2Type = F1[Vector[Int],Int]
  type AddType = F2[Int,Int,Int]

  lazy val add: AddType = new AddType {
    def apply(a1: Int, a2: Int): Int = a1 + a2
    override def toString = "+"
  }

  lazy val sum2: Sum2Type = new Sum2Type {
    def apply(v: Vector[Int]): Int = {
      val s = v.size
      if (s == 0) sys.error("empty sum")
      else if (s == 1) v(0)
      else {
        val si = s / 2
        val (l,r) = v.splitAt(si)
        add(sum2(l),sum2(r))
      }
    }
  }

  lazy val sum3: Sum2Type = new Sum2Type {
    def apply(v: Vector[Int]): Int = {
      val s = v.size
      if (s == 0) sys.error("empty sum")
      else if (s == 1) v(0)
      else {
        val si = s / 2
        val (l,r) = v.splitAt(si)
        $(add,$(sum3,l),$(sum3,r))
      }
    }
  }

  type FInt = FCall[Int]
  case class CInt(i: Int) extends FInt {  def apply = i }
  implicit def intToFInt(i: Int): FInt = CInt(i)

  type Sum4Type = F1[Vector[Int],FInt]

  lazy val sum4: Sum4Type = new Sum4Type {
    def apply(v: Vector[Int]): FInt = {
      val s = v.size
      if (s == 0) sys.error("empty sum")
      else if (s == 1) v(0)
      else {
        val si = s / 2
        val (l,r) = v.splitAt(si)
        $_(add,$$(sum4,l),$$(sum4,r))
      }
    }
    override def toString = "sum4"
  }

 /* case class MyTreeNode(parent: MyTreeNode, t: Trace[Any]) extends TreeNode {
    import scala.collection.JavaConversions._

    lazy val childs: Vector[TreeNode] = t.step match {
      case UnaTrace(f,a1) => Vector[TreeNode](MyTreeNode(this, t.step))
      case BinTrace(f,a1,a2) => Vector[TreeNode](MyTreeNode(this, t.step))
      case TUnaTrace(f,a1) => Vector[TreeNode](MyTreeNode(this, a1.step))
      case TBinTrace(f,a1,a2) => Vector[TreeNode](MyTreeNode(this, a1.step),MyTreeNode(this,a2.step))
      case _ => Vector[TreeNode]()
    }

    def children = scala.collection.JavaConversions.asJavaEnumeration(childs.iterator)
    def	getAllowsChildren = true
    def getChildAt(childIndex: Int): TreeNode = childs(childIndex)
    def	getChildCount: Int = childs.size
    def getIndex(node: TreeNode): Int = sys.error("no")
    def getParent: TreeNode = parent
    def	isLeaf: Boolean = childs.size == 0

    override def toString = t.toString
  }                                    */
}
