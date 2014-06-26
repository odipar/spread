package spread

/*
  Copyright 2014: Robbert van Dalen
 */

object IncrementalTreeView {

  import javax.swing.tree.TreeNode
  import spread.IncrementalMemoization._
  import scala.language.existentials

  trait MyTreeNode extends TreeNode {
    def parent: TreeNode
    def childs: Vector[TreeNode]
    def children = scala.collection.JavaConversions.asJavaEnumeration(childs.iterator)
    def getAllowsChildren = true
    def getChildAt(childIndex: Int): TreeNode = childs(childIndex)
    def getChildCount: Int = childs.size
    def getIndex(node: TreeNode): Int = sys.error("no")
    def getParent: TreeNode = parent
  }

  def expand(t: TreeNode): Unit = {
    var c = t.getChildCount
    var i = 0
    while (i < c) {
      expand(t.getChildAt(i)); i = i + 1
    }
  }
}