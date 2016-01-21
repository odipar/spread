package spread

/**
  * Copyright 2016: Robbert van Dalen
  */
object SplitHash {

  //
  // SplitHash is an immutable, uniquely represented Sequence ADT (Authenticated Data Structure),
  // It is based on SeqHash's innovative hashing scheme, that was invented by Jelle van den Hooff.
  //
  // Like SeqHashes, SplitHashes can be concatenated in O(log(n)).
  // But SplitHash extends SeqHash by allowing Sequences to also be split in O(log(n)).
  // It also solves SeqHash's issue with repeating nodes by applying RLE (Run Length Encoding) compression.
  //
  // SplitHash is the first known History-Independent(HI) Sequence ADT with these properties.
  //
  trait SplitHash[X] {
    def size: Int                                     // O(1)
    def concat(other: SplitHash[X]): SplitHash[X]     // O(log(size))
    def split(at: Int): (SplitHash[X], SplitHash[X])  // O(log(size))
    def first: X                                      // O(log(size))
    def last: X                                       // O(log(size))
  }

  case class SplitHashImpl[X](tree: SHNode[X]) extends SplitHash[X] {
    def size = { if (tree == null) 0 ; else tree.size }
    def concat(other: SplitHash[X]) = SplitHashImpl(SplitHash.concat(tree,other.asInstanceOf[SplitHashImpl[X]].tree))
    def split(at: Int) = (SplitHashImpl(leftSplit(tree,at)), SplitHashImpl(rightSplit(tree,at)))
    def first: X = tree.first
    def last: X = tree.last
  }

  val empty: SplitHash[Int] = SplitHashImpl(null)
  def intSplitHash(i: Int): SplitHash[Int] = SplitHashImpl(IntNode(i))



  // An 'infinitely' expandable Hash (up to Int.MAX_INT)
  // Bits returned by a Hash should ideally be drawn from a cryptographic hash (i.e. SHA256)
  // And the first 32 bits should ideally be cached and returned by head
  trait Hash {
    def head: Int
    def bit(i: Int): Byte
    override def hashCode = head
  }

  // A Hashable object
  trait Hashable {
    def hash: Hash
  }

  // A canonical SH(Split Hash) tree node
  trait SHNode[X] extends Hashable {
    def left: SHNode[X]
    def right: SHNode[X]
    def height: Int
    def size: Int
    def first: X
    def last: X

    // Utility methods to detect and deal with consecutive, equal nodes
    private def get[X](e1: SHNode[X]): SHNode[X] = e1 match { case RLENode(h,_) => h ; case _ => e1 }
    private def size[X](n: SHNode[X]): Int = n match { case RLENode(_,s) => s ; case _ => 1 }
    def isMultipleOf(n: SHNode[X]): Boolean = get(this) == get(n)
    def combine(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(get(this),size(this) + size(n))
      else BinNode(this,n)
    }
    def combine2(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(get(this),size(this) + size(n))
      else TempBinNode(this,n)
    }
  }

  // A Leaf node that holds a single element
  trait LeafNode[X] extends SHNode[X] {
    def left = null
    def right = null
    def height = 0
    def size = 1
    def value: X
    def first = value
    def last = value
  }

  case class IntNode(value: Int) extends LeafNode[Int] with Hash {
    def head = Hashing.jenkinsHash(value)
    def hash = this

    override def bit(index: Int) = {
      var p = index
      var h = head
      while (p > 31) {
        val h1 = Hashing.jenkinsHash(value) ^ h
        h = Hashing.jenkinsHash(h) ^ h1
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }

    override def toString = value.toString
  }

  // A full binary node holds a hash (Int), size (Int), height (Int) and its left and right sub-trees
  // TODO: create a compressed SHNode for sizes < 32 to reduce memory footprint
  case class BinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X] with Hash {
    def first = left.first
    def last = right.last
    val size = left.size + right.size
    val height = 1 + (left.height max right.height)
    def hash = this
    val head = Hashing.jenkinsHash(left.hash.head ^ Hashing.jenkinsHash(right.hash.head))

    override def equals(o: Any): Boolean = {
      // fast reference equality check (=true when references are equal)
      if (this.eq(o.asInstanceOf[AnyRef])) { true }
      // fast hashcode equality check (=false if hashcodes not equal)
      else if (o.hashCode != hashCode) { false }
      else {
        o match {
          case BinNode(l,r) => (l == left) && (r == right)
          case _ => false
        }
      }
    }

    override def bit(index: Int): Byte = {
      var p = index
      var h = head
      while (p > 31) {
        h = Hashing.jenkinsHash(h)
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }

    override def toString = "[" + left + "|" + right + "]"
  }

  // A RLE(Run Length Encoded) node denotes the repetition of another node
  case class RLENode[X](node: SHNode[X], multiplicity: Int) extends SHNode[X] with Hash {
    def left = {
      if (multiplicity < 4) node
      else RLENode(node, multiplicity/2)
    }
    def right = {
      if (multiplicity < 3) node
      else RLENode(node,multiplicity - (multiplicity/2))
    }
    def size = node.size * multiplicity
    def height = node.height
    def hash = this
    val head = node.hash.head ^ Hashing.jenkinsHash(multiplicity)
    override def bit(index: Int): Byte = {
      var p = index
      var h = head
      while (p > 31) {
        h = Hashing.jenkinsHash(h)
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }
    def first = node.first
    def last = node.last
    override def toString = multiplicity + ":" + node
  }

  // A temporary binary node that shouldn't  be part of a canonical tree
  case class TempBinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X]  {
    def error = sys.error("Internal inconsistency. Should not be called")
    val height = 1 + (left.height max right.height)
    def hash = error
    def size = error
    def first = error
    def last = error
  }

  // Iterates sub-nodes in post-order, given a certain target height
  class LeftNodeIterator[X](var tree: SHNode[X], target_height: Int) extends Iterator[SHNode[X]] {
    var nstack: List[SHNode[X]] = List()

    def hasNext = (tree != null) || (!nstack.isEmpty)
    def next: SHNode[X] = {
      if (tree != null) {
        val t = tree
        tree = null
        leftMost(t)
      }
      else if (!nstack.isEmpty) {
        val head = nstack.head
        nstack = nstack.tail
        leftMost(head.right)
      }
      else null
    }
    def leftMost(node: SHNode[X]): SHNode[X] = {
      var leftNode = node
      while(leftNode.height > target_height) {
        nstack = leftNode +: nstack
        leftNode = leftNode.left
      }
      leftNode
    }
  }

  // Iterates sub-nodes in pre-order, given a certain target height
  class RightNodeIterator[X](var tree: SHNode[X], target_height: Int) extends Iterator[SHNode[X]] {
    var nstack: List[SHNode[X]] = List()

    def hasNext = (tree != null) || (!nstack.isEmpty)
    def next: SHNode[X] = {
      if (tree != null) {
        val t = tree
        tree = null
        rightMost(t)
      }
      else if (!nstack.isEmpty) {
        val head = nstack.head
        nstack = nstack.tail
        rightMost(head.left)
      }
      else null
    }
    def rightMost(node: SHNode[X]): SHNode[X] ={
      var rightNode = node
      while (rightNode.height > target_height) {
        nstack = rightNode +: nstack
        rightNode = rightNode.right
      }
      rightNode
    }
  }

  // Lazily consume the Iterator, whilst caching its elements at a certain index.
  // TODO: Optimize with extensible Array (Vector is too heavyweight)
  class LazyIndexableIterator[X](it: Iterator[SHNode[X]]) {
    var v: Vector[SHNode[X]] = Vector()

    def apply(i: Int): SHNode[X] ={
      if (i >= v.size) {
        var ii = v.size
        while ((ii <= i) && (it.hasNext)) { v = v :+ it.next }
      }
      if (i >= v.size) null
      else v(i)
    }
  }

  // Build a temporary tree for left or right Fringe consumption only
  def to_tmp_tree[X](s: List[SHNode[X]]): SHNode[X] = {
    if (s.size == 0) null
    else if (s.size == 1) s.head
    else s.head.combine2(to_tmp_tree(s.tail))
  }

  final val Unknown: Byte = 0
  final val Merge: Byte = 1
  final val Fringe: Byte = 2

  // Determine the left and right Fringe of a canonical tree
  //
  // Notice that, unlike SeqHash, we don't need to hold on to the left and right Fringes, as we can always determine
  // the Fringes in O(log(n)) for each canonical tree at anytime.
  //
  def leftFringe[X](tree: SHNode[X], height: Int): List[SHNode[X]] = {
    fringeVolatile2(new LazyIndexableIterator(new LeftNodeIterator(tree,height)),0).reverse
  }

  def rightFringe[X](tree: SHNode[X], height: Int): List[SHNode[X]] = {
    fringeVolatile2(new LazyIndexableIterator(new RightNodeIterator(tree,height)),1)
  }

  // Iteratively scan and deepen the fringe 'frontier' (width=4) until the exact Fringe is found
  // We do this in order to minimally consume the LazyIndexableIterator
  def fringeVolatile2[X](elems: LazyIndexableIterator[X], direction: Byte): List[SHNode[X]] = {
    // TODO: Proof that width=4 is OK
    val width = 4
    var frontier = width
    var fringe: List[SHNode[X]] = null
    var done = false

    var ii = 0
    while (!done) {
      fringe = fringeVolatile3(elems,direction,frontier)

      if ((fringe.size + width) == frontier) { done = true }
      else { frontier = fringe.size + width }  // deepen frontier

      ii = ii + 1
    }
    fringe
  }

  // Build the left or right Fringe up to the frontier by lazily consuming the LazyIndexableIterator
  def fringeVolatile3[X](elems: LazyIndexableIterator[X], direction: Byte, frontier: Int): List[SHNode[X]] ={
    val kind: Array[Byte] = new Array(frontier+1)
    var min_frontier = frontier
    var other_direction = 1-direction
    var done = false
    var index = 1
    var bit_index = 0
    kind(0) = Fringe

    while (!done) {
      done = true

      if (index < min_frontier) {
        val e1 = elems(index)
        if (e1 != null) {
          if ((kind(index) == Unknown) && (e1.hash.bit(bit_index) == direction)) {
            kind(index) = Fringe
            index = index + 1
          }
          if (kind(index) == Unknown) { done = false }
        }
      }

      if (!done) {
        var j = index
        while (j < min_frontier) {
          if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
            val e1 = elems(j)
            val e2 = elems(j+1)

            if ((e1 != null) && (e2 != null)) {
              if ((e1.hash.bit(bit_index) == other_direction) && (e2.hash.bit(bit_index) == direction)) {
                kind(j) = Merge
                kind(j + 1) = Merge
                min_frontier = j
              }
              else { done = false }
            }
          }
          j = j + 1
        }
      }
      bit_index = bit_index + 1
    }

    var i = 0
    var fringe: List[SHNode[X]] = List()

    while (i < index) {
      fringe = elems(i) +: fringe
      i = i + 1
    }

    fringe
  }

  case class LeftFringe[X](height: Int, top: Seq[SHNode[X]], fringes: Seq[Seq[SHNode[X]]])
  case class RightFringe[X](height: Int, top: Seq[SHNode[X]], fringes: Seq[Seq[SHNode[X]]])

  // Transform a canonical tree into right-catenable LeftFringe tree
  def transformLeft[X](t: SHNode[X]): LeftFringe[X] = {
    var tt = t
    var height = 0
    var leftFringes: Seq[Seq[SHNode[X]]] = Array().seq
    var result: LeftFringe[X] = null

    while(result == null) {
      val lfringe = leftFringe(tt,height).toArray
      val lfirst = first(height,lfringe.size,tt)
      if (lfirst != null) {
        leftFringes = leftFringes :+ lfringe.toSeq
        tt = lfirst
        height = height + 1
      }
      else { result = LeftFringe(height,lfringe,leftFringes) }
    }
    result
  }

  // TODO: reduce the List->Seq->List conversions
  def first[X](hh: Int, size: Int, t: SHNode[X]): SHNode[X] = {
    val f = first2(hh,size,t,List())
    val cm = compress(f._2.reverse)
    to_tmp_tree(cm.toList)
  }

  def first2[X](hh: Int, s: Int, t: SHNode[X], st: List[SHNode[X]]): (Int,List[SHNode[X]]) = {
    if (t.height <= hh) (1,st)
    else {
      val (ls,nst) = first2(hh, s,t.left, st)
      if (ls < s) {
        val (rs,nst2) = first2(hh,s - ls,t.right, nst)
        (ls + rs,nst2)
      }
      else (ls,t.right +: nst)
    }
  }

  // TODO: reduce the List->Seq->List conversions
  def last[X](hh: Int, size: Int, t: SHNode[X]): SHNode[X] = {
    val l = last2(hh,size,t,List())
    val cm = compress(l._2)
    to_tmp_tree(cm.toList)
  }

  def last2[X](hh: Int, s: Int, t: SHNode[X], st: List[SHNode[X]]): (Int,List[SHNode[X]]) = {
    if (t.height <= hh) (1,st)
    else {
      val (rs,nst) = last2(hh, s,t.right, st)
      if (rs < s) {
        val (ls,nst2) = last2(hh,s - rs,t.left, nst)
        (ls + rs,nst2)
      }
      else (rs,t.left +: nst)
    }
  }

  // Transform a canonical tree into a left-catenable RightFringe tree
  def transformRight[X](t: SHNode[X]): RightFringe[X] = {
    var tt = t
    var height = 0
    var rightFringes: Seq[Seq[SHNode[X]]] = Array().seq
    var result: RightFringe[X] = null

    while(result == null) {
      val rfringe = rightFringe(tt,height).toArray
      val rlast = last(height,rfringe.size,tt)
      if (rlast != null) {
        rightFringes = rightFringes :+ rfringe.toSeq
        tt = rlast
        height = height + 1
      }
      else { result = RightFringe(height,rfringe,rightFringes) }
    }
    result
  }

  // Concatenate two canonical trees
  def concat[X](left: SHNode[X], right: SHNode[X]): SHNode[X] = {
    if (left == null) right
    else if (right == null) left
    else {
      val tl = transformRight(left)
      val tr = transformLeft(right)
      concat2(tl,tr)
    }
  }

  // Compresses consecutive and equal (RLE) nodes into RLE nodes
  def compress[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = {
    if (elems.size == 0) elems
    else {
      var size = elems.size
      var stack: List[SHNode[X]] = List(elems(0))
      var i = 1

      while (i < size) {
        val head = stack.head
        val elem = elems(i)
        if (head.isMultipleOf(elem)) { stack = head.combine(elem) +: stack.tail }
        else { stack = elem +: stack }
        i = i + 1
      }
      stack.reverse.toArray.toSeq
    }
  }

  def doRound[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = doRound2(compress(elems))

  def doRound2[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = {
    val N = elems.length
    val kind: Array[Byte] = new Array(N)
    val hashes: Array[Hash] = elems.map(e => e.hash).toArray

    var done = false

    var bit_index = 0
    val N1 = N - 1

    while (!done) {
      done = true
      var j = 0

      while (j < N1) {
        if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
          if ((hashes(j).bit(bit_index) == 1) && (hashes(j + 1).bit(bit_index) == 0)) {
            kind(j) = Merge
            kind(j+1) = Merge
            j = j + 1
          }
          else { done = false }
        }
        j = j + 1
      }
      bit_index = bit_index + 1
    }

    var i = 0
    var center: List[SHNode[X]] = List()

    while (i < N) {
      kind(i) match {
        case Unknown => { center = elems(i) +: center }
        case Merge => {
          center = (elems(i).combine(elems(i + 1))) +: center
          i = i + 1
        }
      }
      i = i + 1
    }

    center.toArray.reverse.toSeq
  }

  def emptyRight[X]: RightFringe[X] = RightFringe(-1,Array().seq,Array().seq)
  def emptyLeft[X]: LeftFringe[X] = LeftFringe(-1,Array().seq,Array().seq)

  // Concatenate the left and right Fringes into a canonical tree
  // TODO: optimize the concatenation (++) of intermediate Seq[X]
  def concat2[X](left: RightFringe[X], right: LeftFringe[X]): SHNode[X] ={
    var elems: Seq[SHNode[X]] = Array[SHNode[X]]().toSeq

    var height = 0
    var done = false

    while (!done) {
      if (height < left.height) { elems = left.fringes(height) ++ elems }
      else if (height == left.height) { elems = left.top ++ elems }

      if (height < right.height) { elems = elems ++ right.fringes(height) }
      else if (height == right.height) { elems = elems ++ right.top }

      if ((height >= left.height) && (height >= right.height) && (elems.length == 1)) { done = true }
      else { elems = doRound(elems) ; height = height + 1 }
    }

    elems(0)
  }

  // Split the left side of a canonical tree
  // We first split the tree into a List of sub-trees. O(log(N))
  // Then we combine those sub-trees into a non-canonical temporary tree.
  // This temporary tree is almost similar to the final tree, except that its right Fringe must be 'repaired'.
  // Most interestingly, the right Fringe can be repaired by concatenating the temporary tree with an empty tree.
  //
  // This is the key to log(N) splitting.
  //
  def leftSplit[X](h: SHNode[X], size: Int): SHNode[X] = {
    val ls = leftSplit2(h,size)
    val cm = compress(ls)
    concat2(transformRight(to_tmp_tree(cm.toList)),emptyLeft)
  }

  def leftSplit2[X](h: SHNode[X], pos: Int): List[SHNode[X]] = {
    if (pos == 0) List()
    else {
      val left = h.left
      if (pos >= left.size) left +: leftSplit2(h.right,pos-left.size)
      else leftSplit2(left,pos)
    }
  }

  // Split the right side of a canonical tree
  def rightSplit[X](h: SHNode[X], pos: Int): SHNode[X] = {
    val rs = rightSplit2(h,h.size-pos).reverse
    val cm = compress(rs)
    concat2(emptyRight,transformLeft(to_tmp_tree(cm.toList)))
  }

  def rightSplit2[X](h: SHNode[X], pos: Int): List[SHNode[X]] = {
    if (pos == 0) List()
    else {
      val right = h.right
      if (pos >= right.size) right +: rightSplit2(h.left,pos-right.size)
      else rightSplit2(right,pos)
    }
  }
}