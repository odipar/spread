package spread

/*
  Copyright 2013: Robbert van Dalen
 */

object IncrementalTreeView {
  import javax.swing.tree.TreeNode
  import spread.IncrementalMemoization._
  import scala.language.existentials

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
        case i: Iterate[_,_] => {
          Vector[TreeNode](INode(this,i.to))
        }
        case v: LazyF1[_,_] => Vector[TreeNode](FNode(this,v),INode(this,v),GNode(this,v.a))
        case v: LazyF2[_,_,_] => Vector[TreeNode](FNode(this,v),INode(this,v),GNode(this,v.a),GNode(this,v.b))
        case v: LazyD1[_,_] => Vector[TreeNode](FNode(this,v),INode(this,v),GNode(this,v.a))
        case v: LazyD2[_,_,_] => Vector[TreeNode](FNode(this,v),INode(this,v),GNode(this,v.a),GNode(this,v.b))
        case ff: Trace[_,_] => Vector[TreeNode](GNode(this, ff.to))
        case _ => { Vector[TreeNode]() }
      }
    }
    def	isLeaf: Boolean = childs.size == 0
    override def toString = {
      t.toString
      /*import Serializer._
      val s = toIds(t)
      toNodes(s).toString */
    }
  }

  case class INode(parent: TreeNode, t: FValue[_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GNode(this, t.iterate))
    }
    def	isLeaf: Boolean = false
    override def toString = "ITERATE {" + t + "}"
  }

  case class FNode(parent: TreeNode, t: FValue[_]) extends MyTreeNode {
    lazy val childs: Vector[TreeNode] = {
      Vector[TreeNode](GNode(this, t.finish))
    }
    def	isLeaf: Boolean = false
    override def toString = "FINISH {" + t + "}"
  }
}
