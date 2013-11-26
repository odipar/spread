package spread

/*
  Copyright 2013: Robbert van Dalen
 */

object IncrementalTreeView {
  import javax.swing.tree.TreeNode
  import spread.IncrementalMemoization._

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
    while (i < c) { expand(t.getChildAt(i)) ; i = i + 1 }
  }

  case class GNode(parent: TreeNode, t: Any) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      t match {
        case v: LazyF1[_,_] => Vector[TreeNode](FNode(this,v),GNode(this,v.a))
        case v: LazyF2[_,_,_] => Vector[TreeNode](FNode(this,v),GNode(this,v.a),GNode(this,v.b))
        case v: LazyD1[_,_] => Vector[TreeNode](FNode(this,v),ENode(this,v),GNode(this,v.a))
        case v: LazyD2[_,_,_] => Vector[TreeNode](FNode(this,v),ENode(this,v),GNode(this,v.a),GNode(this,v.b))
        case ff: Trace[_,_] => Vector[TreeNode](GNode(this, ff.f))
        case _ => Vector[TreeNode]()
      }
    }
    def	isLeaf: Boolean = childs.size == 0
    override def toString = t.toString
  }

  case class ENode(parent: TreeNode, t: FValue[Any]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GNode(this, t.eval))
    }
    def	isLeaf: Boolean = false
    override def toString = "EVAL {" + t + "}"
  }

  case class FNode(parent: TreeNode, t: FValue[Any]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GNode(this, t.force))
    }
    def	isLeaf: Boolean = false
    override def toString = "FORCE {" + t + "}"
  }
}