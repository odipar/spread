package org.spread.core.experiment.sequence

import org.spread.core.language.Annotation.sp

object TreeSequence {

  import org.spread.core.experiment.sequence.Sequence._
  import scala.reflect.ClassTag

  @inline final def minWidth: Int = 16
  @inline final def maxWidth: Int= minWidth * 4

  def emptyTree[@sp X](implicit t: ClassTag[X], c: DefaultTreeContext[X]): TreeSeq[X] = c.empty

  case class DefaultTreeContext[@sp X]()(implicit t: ClassTag[X]) extends SeqContext[X, TreeSeq[X]] {
    val emptyNode: TreeNode[X] = Empty()
    val empty: TreeSeq[X] = TreeSeq[X](emptyNode)(this)
    def tag: ClassTag[X] = t
    
    def createLeaf(a: Array[X]): TreeNode[X] = {
      if (a.length == 0) emptyNode
      else Leaf[X](a)
    }
    
    def createTree(a: Array[TreeNode[X]]): TreeNode[X] = {
      val sz = new Array[Long](a.length)
      var ts: Long = 0
      var i = 0
      var s = a.length

      while (i < s) {ts = ts + a(i).size; sz(i) = ts; i = i + 1}

      Branch[X](a, sz)
    }
  }

  implicit def defaultTreeContext[@sp X](implicit t: ClassTag[X]): DefaultTreeContext[X] = DefaultTreeContext[X]()

  case class TreeSeq[@sp X](n: TreeNode[X])(implicit c: DefaultTreeContext[X]) extends Seq[X, TreeSeq[X]] {
    type S = TreeSeq[X]
    type C = DefaultTreeContext[X]
    
    def self: S = this
    def emptySeq(implicit c: C): S = c.empty
    def parts: Array[S] = {
      n match {
        case Empty() => Array()
        case Leaf(_) => Array(this)
        case Branch(children, _) => children.map(x => TreeSeq(x))
      }
    }

    def append[S2 <: S](o: S2)(implicit c: C): S = TreeSeq(n append o.n)
    def split(i: Long)(implicit c: C): (S, S) = {
      val (l, r) = n.split(i)
      (TreeSeq(l), TreeSeq(r))
    }

    def equalTo[S2 <: S](o: S2): Boolean = ???

    def size: Long = n.size
    def height: Int = n.height

    def toArray: Array[X] = n.toArray(c)

    def createSeq(a: Array[X])(implicit c: C): TreeSeq[X] = createSeq(a, 0, a.length)
    def createSeq(a: Array[X], start: Int, size: Int)(implicit c: C): TreeSeq[X] = {
      implicit def t: ClassTag[X] = c.tag

      if (size <= ((maxWidth / 3) * 4)) {
        val b = new Array[X](size.toInt)
        val end = start + size
        var i = start
        var ii = 0
        while (i < end) {
          b(ii) = a(i)
          i = i + 1
          ii = ii + 1
        }
        TreeSeq(c.createLeaf(b))
      }
      else {
        val ms = size / 2
        createSeq(a, start, ms).append(createSeq(a, start + ms, size - ms))
      }
    }

    def tag: ClassTag[X] = c.tag

    def apply(i: Long): X = n(i)
    def first: X = n.first
    def last: X = n.last
  }

  trait TreeNode[@sp X] {
    type C = DefaultTreeContext[X]
    type N = TreeNode[X]

    def asLeaf(s: N): Leaf[X] = s.asInstanceOf[Leaf[X]]
    def asBranch(s: N): Branch[X] = s.asInstanceOf[Branch[X]]

    def empty: N = Empty[X]()
    def createPair(n1: N, n2: N)(implicit c: C): N = createTree(Array(n1, n2))
    def createLeaf(a: Array[X])(implicit c: C): N = c.createLeaf(a)
    def createTree(a: Array[TreeNode[X]])(implicit c: C): N = c.createTree(a)

    def size: Long
    def height: Int

    def split(i: Long)(implicit c: C): (TreeNode[X], TreeNode[X])
    def append(o: TreeNode[X])(implicit c: C): TreeNode[X]

    def toArray(implicit c: C): Array[X]

    def appendBranches(s1: Branch[X], s2: Branch[X])(implicit c: C): N = {
      assert(s1.height == s2.height) // only append trees with same height

      if (s1.childCount >= minWidth && s2.childCount >= minWidth) createPair(s1, s2)
      else {
        val merged = s1.children ++ s2.children
        if (merged.length <= maxWidth) createTree(merged)
        else {val (l, r) = merged.splitAt((merged.length + 1) >> 1); createPair(createTree(l), createTree(r))}
      }
    }

    def appendLeafs(s1: Leaf[X], s2: Leaf[X])(implicit c: C): N = {
      assert((s1.height == 0) && (s2.height == 0))

      implicit def t: ClassTag[X] = c.tag
      
      if (s1.size >= minWidth && s2.size >= minWidth) createPair(s1, s2)
      else {
        val tsize = s1.size + s2.size
        if (tsize <= maxWidth) createLeaf(s1.values ++ s2.values)
        else {
          if (s1.size > s2.size) {
            val (l, r) = s1.split(s1.size / 2)
            createPair(l, r.append(s2))
          }
          else {
            val (l, r) = s2.split(s2.size / 2)
            createPair(s1.append(l), r)
          }
        }
      }
    }

    def append2(ss1: N, ss2: N)(implicit c: C): N = {
      if (ss2.size == 0) ss1
      else if (ss1.size == 0) ss2
      else if ((ss1.height == 0) && (ss2.height == 0)) appendLeafs(asLeaf(ss1), asLeaf(ss2))
      else if (ss1.height == ss2.height) appendBranches(asBranch(ss1), asBranch(ss2))
      else if (ss1.height > ss2.height) {
        val s1 = asBranch(ss1)
        val newLast = s1.lastChild.append(ss2)
        if (newLast.height == s1.height) append2(s1.withoutLastChild, newLast)
        else s1.replaceLastChild(newLast)
      }
      else {
        val s2 = asBranch(ss2)
        val newFirst = ss1.append(s2.firstChild)
        if (newFirst.height == s2.height) append2(newFirst, s2.withoutFirstChild)
        else s2.replaceFirstChild(newFirst)
      }
    }

    def apply(i: Long): X
    def first: X
    def last: X
  }

  case class Empty[@sp X]() extends TreeNode[X] {
    def error: Nothing = sys.error("empty sequence")

    def size: Long = 0
    def height: Int = -1

    def split(i: Long)(implicit c: C): (TreeNode[X], TreeNode[X]) = (this, this)
    def append(o: TreeNode[X])(implicit c: C): TreeNode[X] = o
    def toArray(implicit c: C): Array[X] = {
      implicit def t: ClassTag[X] = c.tag
      Array[X]()
    }

    def apply(i: Long): X = error
    def first: X = error
    def last: X = error
  }

  case class Leaf[@sp X](values: Array[X]) extends TreeNode[X] {
    def size: Long = values.length
    def height = 0

    def append(o: TreeNode[X])(implicit c: C): TreeNode[X] = append2(this, o)
    def split(i: Long)(implicit c: C): (N, N) = {
      if (i >= size) (this, empty)
      else if (i < 0) (empty, this)
      else {
        val (left: Array[X], right: Array[X]) = values.splitAt(i.toInt)
        (createLeaf(left), createLeaf(right))
      }
    }
    def toArray(implicit c: C): Array[X] = values
    def apply(i: Long): X = values(i.toInt)
    def first: X = values(0)
    def last: X = values(values.length - 1)

    override lazy val hashCode: Int = values.hashCode
    override def toString: String = values.foldLeft("<")((x, y) => x + " " + y) + " >"
  }

  case class Branch[@sp X](children: Array[TreeNode[X]], sizes: Array[Long]) extends TreeNode[X] {
    lazy val height: Int = children(0).height + 1
    def size: Long = sizes(sizes.length - 1)

    def childCount: Int = children.length
    def firstChild = children(0)
    def lastChild = children(childCount - 1)
    def childAt(i: Int): N = children(i)
    def setChild(i: Int, s: N)(implicit c: C): N = {val nc = children.clone; nc(i) = s; createTree(nc)}
    def replaceFirstChild(first: N)(implicit c: C): N = setChild(0, first)
    def replaceLastChild(last: N)(implicit c: C): N = setChild(childCount - 1, last)
    def withoutFirstChild(implicit c: C): N = createTree(children.slice(1, childCount))
    def withoutLastChild(implicit c: C): N = createTree(children.slice(0, childCount - 1))
    def offsetForChild(index: Int): Long = {
      if (index == 0) 0.toLong
      else sizes(index - 1)
    }
    def childAtIndex(index: Long): Int = {
      // TODO: binary search
      if (index < size && index >= 0) {var i = 0; while (sizes(i) <= index) {i = i + 1}; i}
      else sys.error("index out of bounds")
    }

    def toArray(implicit c: C): Array[X] = ???

    def append(o: TreeNode[X])(implicit c: C): TreeNode[X] = append2(this, o)
    def split(i: Long)(implicit c: C): (N, N) = {
      if (i >= size) (this, empty)
      else if (i < 0) (empty, this)
      else {
        val sIndex = childAtIndex(i)
        val offset = offsetForChild(sIndex)
        var (left, right) = childAt(sIndex).split(i - offset)

        for (i <- 0 until sIndex) left = childAt(sIndex - i - 1).append(left)
        for (i <- (sIndex + 1) until childCount) right = right.append(childAt(i))

        (left, right)
      }
    }

    def apply(i: Long): X = {
      val cc = childAtIndex(i)
      val o = offsetForChild(cc)
      childAt(cc)(i - o)
    }
    def first: X = children(0).first
    def last: X = children(children.length - 1).last

    override lazy val hashCode: Int = children.hashCode
    override def toString: String = children.foldLeft("<")((x, y) => x + " " + y) + " >"

  }
}
