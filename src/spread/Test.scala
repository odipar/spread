package spread

import scala.sys

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

  final def main(args: Array[String]): Unit =
  {
    var ff1 = sum5(Add)
    //var ff2 = fac2(Mul,Add)

    var f1 = %%(ff1,Vector(1,2,3,4,5,6,7,8))
    var f2 = %%(ff1,Vector(9,2,3,4,5,6,7,8))

    val node1 = MyTreeNode(null,f1)
    val model1 = new DefaultTreeModel(node1)
    var tree1 = new JTree(model1)
    tree1.setShowsRootHandles(true)
    tree1.setPreferredSize(new Dimension(600, 600))
    tree1.setMinimumSize(new Dimension(10, 10))

    val node2 = MyTreeNode(null,f2)
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

  type AddType = (Int,Int) => Int

  lazy val add: AddType = new AddType {
    def apply(a1: Int, a2: Int): Int = a1 + a2
    override def toString = "+"
  }

  type Sum2Type = Vector[Int] => Int

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
    override def toString = "sum2"
  }


  case class MyTreeNode(parent: MyTreeNode, t: Any) extends TreeNode {

    lazy val childs: Vector[TreeNode] = {
      println("fc: " + fc)
      t match {
        case u: LUna[_,_] => Vector[TreeNode](MyTreeNode(this, u.eval))
        case b: LBin[_,_,_] => Vector[TreeNode](MyTreeNode(this, b.eval))
        case u: OUna[_,_] => Vector[TreeNode](MyTreeNode(this, u.arg1))
        case b: OBin[_,_,_] => Vector[TreeNode](MyTreeNode(this, b.a1), MyTreeNode(this, b.a2))
        case _ => Vector[TreeNode]()
      }
    }

    def children = scala.collection.JavaConversions.asJavaEnumeration(childs.iterator)
    def	getAllowsChildren = true
    def getChildAt(childIndex: Int): TreeNode = childs(childIndex)
    def	getChildCount: Int = childs.size
    def getIndex(node: TreeNode): Int = sys.error("no")
    def getParent: TreeNode = parent
    def	isLeaf: Boolean = childs.size == 0

    override def toString = t.toString
  }

  lazy val i = (x: Int) => x + 1
  lazy val b = 2
  lazy val c = i(b)


  case class fib(add: FF2[F0[Int]]) extends FF1[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int]) = {
      val aa = arg1()
      if (aa <= 1) arg1
      else %(add,%(self,aa-1),%(self,aa-2))
    }
    override def toString = "fib"
  }

  case class fib2(add: FF2[F0[Int]]) extends FF1[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int]) = {
      val aa = arg1()
      if (aa <= 1) arg1
      else  %%(add,%%(self,aa-1),%%(self,aa-2))
    }
    override def toString = "fib2"
  }

  case class sum5[L](add: FF2[F0[Int]]) extends F1[F0[Vector[Int]],F0[Int]] {
    def self = this
    def apply(arg1: F0[Vector[Int]]) = {
      val vv = arg1()
      val s = vv.size
      if (s == 0) sys.error("empty sum")
      else if (s == 1) vv(0)
      else {
        val (l,r) = vv.splitAt(s / 2)
        %(add,%%(self,l),%%(self,r))
      }
    }
    override def toString = "sum5"
  }
  case class fac(mul: FF2[F0[Int]], add: FF2[F0[Int]]) extends FF1[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int]) = {
      val aa = arg1()
      if (aa <= 1) arg1
      else %(mul,arg1,%(self,aa-1))
    }
    override def toString = "fac"
  }

  case class fac2(mul: FF2[F0[Int]], add: FF2[F0[Int]]) extends FF1[F0[Int]] {
    def self = this
    def apply(arg1: F0[Int]) = {
      val aa = arg1()
      if (aa <= 1) arg1
      else %%(mul,arg1,%%(self,aa-1))
    }
    override def toString = "fac2"
  }
}
