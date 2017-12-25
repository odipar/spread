package org.spread.core.experiment.sequence

import org.spread.core.language.Annotation.sp

object TreeSequence {

  import org.spread.core.experiment.sequence.Sequence.Seq
  import scala.reflect.ClassTag

  @inline final def minWidth = 16
  @inline final def maxWidth = minWidth * 4

  def emptyTree[@sp X](implicit t: ClassTag[X]): TreeSeq[X] = TreeSeq(Empty[X]())

  case class TreeSeq[@sp X](n: TreeNode[X])(implicit t: ClassTag[X]) extends Seq[X, TreeSeq[X]] {
    type S = TreeSeq[X]

    def self: S = this
    def emptySeq: S = TreeSeq(n.empty)
    def parts: Array[S] = {
      n match {
        case Empty() => Array()
        case Leaf(_) => Array(this)
        case Branch(children, _) => children.map(x => TreeSeq(x))
      }
    }

    def append[S2 <: S](o: S2): S = TreeSeq(n append o.n)
    def split(i: Long): (S, S) = {
      val (l, r) = n.split(i)
      (TreeSeq(l), TreeSeq(r))
    }

    def equalTo[S2 <: S](o: S2): Boolean = ???

    def size: Long = n.size
    def height: Int = n.height

    def tag: ClassTag[X] = t
    def toArray: Array[X] = n.toArray

    def createSeq(a: Array[X]): TreeSeq[X] = createSeq(a, 0, a.length)

    def createSeq(a: Array[X], start: Int, size: Int): TreeSeq[X] = {
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
        TreeSeq(Leaf(b))
      }
      else {
        val ms = size / 2
        createSeq(a, start, ms).append(createSeq(a, start + ms, size - ms))
      }
    }

    def apply(i: Long): X = n(i)
    def first: X = n.first
    def last: X = n.last
  }

  var nodes: Long = 0

  trait TreeNode[@sp X] {
    {
      nodes = nodes + 1
    }
    type N = TreeNode[X]

    def asLeaf(s: N): Leaf[X] = s.asInstanceOf[Leaf[X]]
    def asBranch(s: N): Branch[X] = s.asInstanceOf[Branch[X]]

    def empty: N = Empty[X]()
    def createPair(n1: N, n2: N): N = createTree(Array(n1, n2))
    def createLeaf(a: Array[X]): N = {
      if (a.size == 0) Empty[X]()
      else Leaf(a)
    }
    def createTree(a: Array[TreeNode[X]]): N = {
      val sz = new Array[Long](a.length)
      var ts: Long = 0
      var i = 0
      var s = a.length

      while (i < s) {ts = ts + a(i).size; sz(i) = ts; i = i + 1}

      Branch(a, sz)
    }

    def size: Long
    def height: Int

    def split(i: Long)(implicit t: ClassTag[X]): (TreeNode[X], TreeNode[X])
    def append(o: TreeNode[X])(implicit t: ClassTag[X]): TreeNode[X]

    def toArray(implicit t: ClassTag[X]): Array[X]

    def appendBranches(s1: Branch[X], s2: Branch[X]): N = {
      assert(s1.height == s2.height) // only append trees with same height

      if (s1.childCount >= minWidth && s2.childCount >= minWidth) createPair(s1, s2)
      else {
        val merged = s1.children ++ s2.children
        if (merged.length <= maxWidth) createTree(merged)
        else {val (l, r) = merged.splitAt((merged.length + 1) >> 1); createPair(createTree(l), createTree(r))}
      }
    }

    def appendLeafs(s1: Leaf[X], s2: Leaf[X])(implicit t: ClassTag[X]): N = {
      assert((s1.height == 0) && (s2.height == 0))

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

    def append2(ss1: N, ss2: N)(implicit t: ClassTag[X]): N = {
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
    def error = sys.error("empty sequence")

    def size = 0
    def height = -1

    def split(i: Long)(implicit t: ClassTag[X]) = (this, this)
    def append(o: TreeNode[X])(implicit t: ClassTag[X]): TreeNode[X] = o
    def toArray(implicit t: ClassTag[X]): Array[X] = Array()

    def apply(i: Long): X = error
    def first: X = error
    def last: X = error
  }

  case class Leaf[@sp X](values: Array[X]) extends TreeNode[X] {
    def size = values.length

    def height = 0

    def append(o: TreeNode[X])(implicit t: ClassTag[X]): TreeNode[X] = append2(this, o)
    def split(i: Long)(implicit t: ClassTag[X]): (N, N) = {
      if (i >= size) (this, empty)
      else if (i < 0) (empty, this)
      else {
        val (left: Array[X], right: Array[X]) = values.splitAt(i.toInt)
        (createLeaf(left), createLeaf(right))
      }
    }
    def toArray(implicit t: ClassTag[X]): Array[X] = values
    def apply(i: Long): X = values(i.toInt)
    def first: X = values(0)
    def last: X = values(values.length - 1)

    override lazy val hashCode = values.hashCode
    override def toString = values.foldLeft("<")((x, y) => x + " " + y) + " >"
  }

  case class Branch[@sp X](children: Array[TreeNode[X]], sizes: Array[Long]) extends TreeNode[X] {
    val heightVal: Int = children(0).height + 1
    def height: Int = heightVal
    def size: Long = sizes(sizes.length - 1)

    def childCount = children.length
    def firstChild = children(0)
    def lastChild = children(childCount - 1)
    def childAt(i: Int): N = children(i)
    def setChild(i: Int, s: N): N = {val nc = children.clone; nc(i) = s; createTree(nc)}
    def replaceFirstChild(first: N): N = setChild(0, first)
    def replaceLastChild(last: N): N = setChild(childCount - 1, last)
    def withoutFirstChild: N = createTree(children.slice(1, childCount))
    def withoutLastChild: N = createTree(children.slice(0, childCount - 1))
    def offsetForChild(index: Int) = {
      if (index == 0) 0.toLong
      else sizes(index - 1)
    }
    def childAtIndex(index: Long): Int = {
      // TODO: binary search
      if (index < size && index >= 0) {var i = 0; while (sizes(i) <= index) {i = i + 1}; i}
      else sys.error("index out of bounds")
    }

    def toArray(implicit t: ClassTag[X]): Array[X] = ???

    def append(o: TreeNode[X])(implicit t: ClassTag[X]): TreeNode[X] = append2(this, o)
    def split(i: Long)(implicit t: ClassTag[X]): (N, N) = {
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

    def apply(i: Long) = {
      val cc = childAtIndex(i)
      val o = offsetForChild(cc)
      childAt(cc)(i - o)
    }
    def first: X = children(0).first
    def last: X = children(children.length - 1).last

    override lazy val hashCode = children.hashCode
    override def toString = children.foldLeft("<")((x, y) => x + " " + y) + " >"

  }
}
