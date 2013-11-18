package spread

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference

/*

 Copyright 2013: Robbert van Dalen

 */

object Test {
  import language.implicitConversions
  import IncrementalMemoization._
  import scala.language.existentials

  import javax.swing.tree._
  import javax.swing._
  import java.awt._


  def fac[X](i: Int): I = {
    if (i <= 1) 1
    else i * fac(i-1)
  }

  val fac2: Function1[Int,I] = new Function1[Int,I] {
    def apply(i: Int): I = {
      if (i <= 1) i
      else i * $(fac2,i-1)
    }
    override def toString = "fac2"
  }

  def fib(i: Int): I = {
    if (i <= 1) i
    else fib(i-1) + fib(i-2)
  }

  val fib2: Function1[Int,I] = new Function1[Int,I] {
    def apply(i: Int): I = {
      if (i <= 1) i
      else $(fib2,i-1) + $(fib2,i-2)
    }
    override def toString = "fib2"
  }

  final def main(args: Array[String]): Unit =
  {

    var join = fjoin(intord)

    val k1 = join(T(1),T(2))
    val k2 = join(T(3),T(4))

    val j = join1(intord)
    val r = %%(j,FI(k1),FI(k2))
    println("j: " + r)

    val i1 = r
    val i2 = r
    /*val i1 = fib2(10)
    val i2 = fib2(5)
      */
    val node1 = GenericTreeNode(null,i1)
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

  case class GenericTreeNode(parent: TreeNode, t: Any) extends MyTreeNode {

    lazy val childs: Vector[TreeNode] = {
      t match {
        case b: TTer[_,_,_,_] => {
          Vector[TreeNode](GForceTreeNode(this,b), GenericTreeNode(this, b.origin._1),GenericTreeNode(this, b.origin._2),GenericTreeNode(this, b.origin._3))
        }
        case b: TBin[_,_,_] => {
          Vector[TreeNode](GForceTreeNode(this,b), GenericTreeNode(this, b.origin._1),GenericTreeNode(this, b.origin._2))
        }
        case b: TUna[_,_] => {
          Vector[TreeNode](GForceTreeNode(this,b), GenericTreeNode(this, b.origin))
        }
        case TInt(i) => Vector[TreeNode](GenericTreeNode(this, i))
        case v: LazyFValue[_,_] => Vector[TreeNode](LazyTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this, v.eval))
        case v: LazyF2Value[_,_,_] => Vector[TreeNode](LazyTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b),GenericTreeNode(this, v.eval))
        case v: LazyF3Value[_,_,_,_] => Vector[TreeNode](LazyTreeNode(this,v),GenericTreeNode(this,v.a),GenericTreeNode(this,v.b),GenericTreeNode(this,v.c),GenericTreeNode(this, v.eval))
        case ff: FForce[_,_,_,_] => Vector[TreeNode](ForceTreeNode(this,ff),GenericTreeNode(this, ff.f))
        case f: FValue[_,_] => Vector[TreeNode](GenericTreeNode(this, f.origin))
        case _ => Vector[TreeNode]()
      }
    }
    def	isLeaf: Boolean = childs.size == 0
    override def toString = t.toString
  }

  case class GForceTreeNode(parent: TreeNode, t: FValue[_,_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GenericTreeNode(this, t()))
    }
    def	isLeaf: Boolean = false
    override def toString = "EVAL {" + t + "}"
  }

  case class ForceTreeNode(parent: TreeNode, t: FForce[_,_,_,_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GenericTreeNode(this, t()))
    }
    def	isLeaf: Boolean = false
    override def toString = "EVAL {" + t.u + "}"
  }

  case class LazyTreeNode(parent: TreeNode, t: FValue[_,_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GenericTreeNode(this, t.force))
    }
    def	isLeaf: Boolean = false
    override def toString = "FORCE {" + t.toString + "}"
  }
}
