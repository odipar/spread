package spread

//
// Copyright 2016: Robbert van Dalen
//

object SplitHash {
  import SipHash.siphash24
  import java.lang.ref.WeakReference

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

  trait SplitHash[X, SH <: SplitHash[X,SH]] extends Hashable {
    def hash: Hash                // O(1)
    def size: Int                 // O(1)
    def concat(other: SH): SH     // O(log(size))
    def split(at: Int): (SH, SH)  // O(log(size))
    def first: X                  // O(log(size))
    def last: X                   // O(log(size))
  }

  // A Hashable object
  trait Hashable {
    def hash: Hash
  }

  // An 'infinitely' expandable Hash
  trait Hash {
    def hashAt(i: Int): Int
  }

  // A canonical tree SH(Split Hash)Node
  trait SHNode[X] extends SplitHash[X,SHNode[X]] with Hash with Hashable {
    def size: Int
    def hash = this
    def concat(other: SHNode[X]) = SplitHash.concat(this,other)
    def split(at: Int) = (leftSplit(this,at),rightSplit(this,at))
    def first: X
    def last: X

    // tree traversal
    def left: SHNode[X]
    def right: SHNode[X]
    def height: Int

    // chunking to reduce memory consumption
    def chunk: SHNode[X]
    def isChunked: Boolean

    def isMultipleOf(n: SHNode[X]): Boolean = mget(this) == mget(n)
    def combine(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(mget(this),msize(this) + msize(n))
      else BinNode(this,n)
    }
    def combine2(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(mget(this),msize(this) + msize(n))
      else TempBinNode(this,n)
    }
  }

  // Utility methods to detect and deal with consecutive, equal nodes
  private def mget[X](e1: SHNode[X]): SHNode[X] = e1 match { case RLENode(h,_) => h ; case _ => e1 }
  private def msize[X](n: SHNode[X]): Int = n match { case RLENode(_,s) => s ; case _ => 1 }

  // A Leaf node that holds a single element
  trait LeafNode[X] extends SHNode[X] {
    def left = null
    def right = null
    def height = 0
    def size = 1
    def value: X
    def first = value
    def last = value
    def chunk = this
    def isChunked = true
  }

  final val m1 = 1664525
  final val m2 = 22695477
  final val m3 = 1103515245

  case class IntNode(value: Int) extends LeafNode[Int] {
    override def hashCode = siphash24(value + m1,value - m2)
    def hashAt(index: Int) = {
      if (index == 0) { hashCode }
      else if (index == 1) { siphash24(value + m2, hashCode - m3) }
      else { siphash24(hashCode + m3, hashAt(index-1) - m1) }
    }
    override def toString = value.toString
  }

  trait BNode[X] extends SHNode[X] {
    override def equals(o: Any): Boolean = {
      // BY DESIGN: fast hashcode equality check
      if (hashCode != o.hashCode) { false }
      // fast reference equality check (=true when references are equal)
      else if (this.eq(o.asInstanceOf[AnyRef])) { true }
      else {
        o match {
          case k: SHNode[X] => (left == k.left) && (right == k.right)
          case _ => false
        }
      }
    }
  }

  // A full binary node holds a hash (Int), size (Int), height (Int) and its left and right sub-trees
  case class BinNode[X](left: SHNode[X], right: SHNode[X]) extends BNode[X] {
    def first = left.first
    def last = right.last
    val csize = {         // encode isChunked property into a negative size
      val ns = left.size + right.size
      if (left.isChunked && right.isChunked) { -ns }
      else { ns }
    }
    def size = {
      val s = csize
      if (s < 0) { -s }
      else { s }
    }
    def isChunked = csize < 0
    val height = 1 + (left.height max right.height)

    override val hashCode = siphash24(left.hashCode - m1,right.hashCode + m2)

    // We just keep munging bits recursively traversing down the tree.
    //
    // The idea is that - when two unequal tree nodes collide in their hashCode -
    // then their left and right tree nodes should not equal the same hashCode
    // with exponential higher probability, given a certain hash length/depth.
    //
    // This is shaky cryptographically, but 'works' in 'practice'.
    //
    // TODO: We should have a cryptographic expert have a crack at it.

    override def hashAt(index: Int) = {
      val h = hashCode
      if (index == 0) { h }
      else if (index == 1) {
        if (h < 0) { siphash24(left.hashCode - m2,h + m3) }
        else { siphash24(h - m1,right.hashCode + m2) }
      }
      else {
        val nindex = index / 2
        siphash24(left.hash.hashAt(nindex) - m3,right.hash.hashAt(index - nindex) + m1)
      }
    }
    def chunk = {
      if (height < 6) chunkTree(this)
      else {
        if (left.isChunked) {
          if (right.isChunked)  this
          else BinNode(left,right.chunk)
        }
        else {
          if (right.isChunked) BinNode(left.chunk,right)
          else BinNode(left.chunk,right.chunk)
        }
      }
    }
    override def toString = "[" + left + "|" + right + "]"
  }

  // A RLE(Run Length Encoded) node denotes the repetition of another node
  case class RLENode[X](node: SHNode[X], multiplicity: Int) extends SHNode[X] {
    def left = {
      if (multiplicity < 4) { node }
      else { RLENode(node, multiplicity/2) }
    }
    def right = {
      if (multiplicity < 3) { node }
      else { RLENode(node,multiplicity - (multiplicity/2)) }
    }
    def size = node.size * multiplicity
    def height = node.height

    override val hashCode = siphash24(node.hashCode,multiplicity)
    override def hashAt(index: Int) = {
      if (index > 0) { siphash24(node.hash.hashAt(index),multiplicity) }
      else { hashCode }
    }
    def chunk = {
      if (!node.isChunked) { RLENode(node.chunk,multiplicity) }
      else { this }
    }
    def isChunked = node.isChunked

    def first = node.first
    def last = node.last
    override def toString = multiplicity + ":" + node
  }

  // A temporary binary node that shouldn't be part of a canonical tree
  case class TempBinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X]  {
    def error = sys.error("Internal inconsistency. Should not be called")
    val height = 1 + (left.height max right.height)
    def size = error
    def first = error
    def last = error
    def chunk = error
    def isChunked = error
    def hashAt(i: Int) = error
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
      else { null }
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
      else { null }
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

  type NBlock[X] = Array[SHNode[X]]

  // Lazily consume the Iterator, whilst caching its elements at a certain index.
  class LazyIndexableIterator[X](it: Iterator[SHNode[X]]) {
    var v: NBlock[X] = new Array(8)
    var size = 0

    def apply(i: Int): SHNode[X] = {
      if (i >= size) {
        if (i >= v.size) {
          // exhausted, so extend the array
          var vv: NBlock[X] = new Array(v.size * 2)
          v.copyToArray(vv)
          v = vv
        }
        var ii = size
        while ((ii <= i) && (it.hasNext)) {
          v(ii) = it.next
          ii = ii + 1
        }
        size = ii
      }
      if (i >= size) null
      else v(i)
    }

    def firstReversed(s: Int) = {
      val firstR: NBlock[X] = new Array(s)
      var i = 0
      while (i < s) {
        firstR(i) = v(s-i-1)
        i = i + 1
      }
      firstR
    }
  }

  // Build a temporary tree for left or right Fringe consumption only
  def to_tmp_tree[X](a: NBlock[X]): SHNode[X] = {
    if (a.size == 0) null
    else {
      val s = a.size
      var tree = a(0)
      var i = 1
      while (i < s) {
        tree = tree.combine2(a(i))
        i = i + 1
      }
      tree
    }
  }

  // Chunk a canonical tree into ChunkedNode (effectively an Array of all the tree's LeafNodes)
  def chunkTree[X](s: SHNode[X]): ChunkedNode[X] = {
    var i = new LeftNodeIterator(s,0)
    var ii = 0
    var chunk: NBlock[X] = new Array(s.size)
    while (i.hasNext) {
      chunk(ii) = i.next
      ii = ii + 1
    }
    ChunkedNode(chunk,s.hashCode,s.size,s.height)
  }

  def unchunk[X](cn: ChunkedNode[X]): SHNode[X] = {
    var s: NBlock[X] = cn.ch
    while (s.length > 1) { s = doRound(s) }
    s(0)
  }

  // A ChunkedNode that holds all the LeafNodes of the canonical tree it represents.
  // It also holds a WeakReference to its unchunked canonical tree when requested.
  // So the unchunked version can be GC'ed anytime, as it can always be rebuilt from the chunk.
  case class ChunkedNode[X](ch: NBlock[X], h: Int, size: Int, height: Int) extends BNode[X] {
    var unchunked: WeakReference[_] = null

    def isChunked = true
    def getUnchunked: SHNode[X] = {
      if (unchunked != null) {
        val u = unchunked.get
        if (u != null) u.asInstanceOf[SHNode[X]]
        else { getUnchunked2 }
      }
      else { getUnchunked2 }
    }
    def getUnchunked2: SHNode[X] = {
      val uchunk = unchunk(this)
      unchunked = new WeakReference(uchunk)
      uchunk
    }

    def left = getUnchunked.left
    def right = getUnchunked.right
    def first = getUnchunked.first
    def last = getUnchunked.last
    override def hashCode = h
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else getUnchunked.hash.hashAt(i)
    }
    def chunk = this
  }

  final val Unknown: Byte = 0
  final val Merge: Byte = 1
  final val Fringe: Byte = 2

  // Determine the left and right Fringe of a canonical tree
  //
  // Notice that, unlike SeqHash, we don't need to hold on to the left and right Fringes, as we can always determine
  // the Fringes in O(log(n)) for each canonical tree at anytime.

  def leftFringe[X](tree: SHNode[X], height: Int): NBlock[X] = {
    fringeVolatile2(new LazyIndexableIterator(new LeftNodeIterator(tree,height)),0).reverse
  }

  def rightFringe[X](tree: SHNode[X], height: Int): NBlock[X] = {
    fringeVolatile2(new LazyIndexableIterator(new RightNodeIterator(tree,height)),1)
  }

  // Iteratively scan and deepen the fringe 'frontier' (width(average)=5) until the exact Fringe is found
  // We do this in order to minimally consume the LazyIndexableIterator
  def fringeVolatile2[X](elems: LazyIndexableIterator[X], direction: Byte): NBlock[X] = {
    val width = 5
    var frontier = width
    var fringe: NBlock[X] = null

    while (fringe == null) {
      val frontier1 = frontier+1

      val kinds: Array[Byte] = new Array(frontier1) // create reuseable arrays for two fringe determinations
      val hashes: Array[Int] = new Array(frontier1)

      val fringe1 = fringeVolatile3(elems,direction,frontier,kinds,hashes)
      resetKinds(kinds)
      val fringe2 = fringeVolatile3(elems,direction,frontier1,kinds,hashes) // frontier+1 to check the edge difference

      if (fringe1 != fringe2) { frontier = frontier + width }
      else { fringe = elems.firstReversed(fringe1) }
    }
    fringe
  }

  // Build the Fringe up to the frontier by lazily consuming the LazyIndexableIterator
  def fringeVolatile3[X](elems: LazyIndexableIterator[X], direction: Byte, frontier: Int, kind: Array[Byte], hashes: Array[Int]): Int ={
    var min_frontier = frontier
    var other_direction = 1-direction
    var done = false
    var index = 1
    var bit_index = 0
    var int_index = 0
    kind(0) = Fringe

    while (!done) {
      done = true

      if (bit_index == 0) {
        // cache hashes of nodes that are unknown
        var ei = index
        var ii = bit_index >> 5
        while (ei < min_frontier && (elems(ei) != null)) {
          if (kind(ei) == Unknown) hashes(ei) = elems(ei).hash.hashAt(int_index)
          ei = ei + 1
        }
        int_index = int_index + 1
      }

      if (index < min_frontier) {
        val e1 = elems(index)
        if (e1 != null) {
          if ((kind(index) == Unknown) && (bitAt(hashes(index),bit_index) == direction)) {
            kind(index) = Fringe
            index = index + 1
          }
          if ((index < min_frontier) && (kind(index) == Unknown)) { done = false }
        }
      }

      if (!done) {
        var j = index
        var mf1 = min_frontier-1

        while (j < mf1) {
          if ((kind(j) == Unknown) && (kind(j+1) == Unknown)) {
            val e1 = elems(j)
            val e2 = elems(j+1)

            if ((e1 != null) && (e2 != null)) {
              if ((bitAt(hashes(j),bit_index) == other_direction) && (bitAt(hashes(j+1),bit_index) == direction)) {
                kind(j) = Merge
                kind(j+1) = Merge
                min_frontier = j
              }
              else { done = false }
            }
          }
          j = j + 1
        }
      }
      bit_index = (bit_index + 1) & 31
    }

    index
  }

  def resetKinds(kind: Array[Byte]): Unit = {
    var i = 0 // clean kinds
    var f = kind.size
    while (i < f) {
      kind(i) = Unknown
      i = i + 1
    }
  }

  final def bitAt(value: Int, index: Int): Byte = ((value >>> (31-index)) & 1).toByte

  case class LeftFringe[X](height: Int, top: NBlock[X], fringes: List[NBlock[X]])
  case class RightFringe[X](height: Int, top: NBlock[X], fringes: List[NBlock[X]])

  // Transform a canonical tree into right-catenable LeftFringe tree
  def transformLeft[X](t: SHNode[X]): LeftFringe[X] = {
    var tt = t
    var height = 0
    var leftFringes: List[NBlock[X]] = List()
    var result: LeftFringe[X] = null

    while(result == null) {
      val lfringe = leftFringe(tt,height)
      val lfirst = first(height,lfringe.size,tt)
      if (lfirst != null) {
        leftFringes = lfringe +: leftFringes
        tt = lfirst
        height = height + 1
      }
      else { result = LeftFringe(height,lfringe,leftFringes.reverse) }
    }
    result
  }

  def first[X](hh: Int, size: Int, t: SHNode[X]): SHNode[X] = {
    val f = first2(hh,size,t,List())
    val cm = compress(f._2.toArray.reverse)
    to_tmp_tree(cm)
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

  def last[X](hh: Int, size: Int, t: SHNode[X]): SHNode[X] = {
    val l = last2(hh,size,t,List())
    val cm = compress(l._2.toArray[SHNode[X]])
    to_tmp_tree(cm)
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
    var rightFringes: List[NBlock[X]] = List()
    var result: RightFringe[X] = null

    while(result == null) {
      val rfringe = rightFringe(tt,height)
      val rlast = last(height,rfringe.size,tt)
      if (rlast != null) {
        rightFringes = rfringe +: rightFringes
        tt = rlast
        height = height + 1
      }
      else { result = RightFringe(height,rfringe,rightFringes.reverse) }
    }
    result
  }

  // Concatenate two canonical trees
  def concat[X](left: SHNode[X], right: SHNode[X]): SHNode[X] = {
    if (left == null) { right }
    else if (right == null) { left }
    else { concat2(transformRight(left),transformLeft(right)) }
  }

  // Compresses consecutive and equal (RLE) nodes into RLE nodes
  def compress[X](elems: NBlock[X]): NBlock[X] = {
    // first check whether we need to compress
    var i = 1
    var s = elems.size
    var compress = false
    while ((i < s) && !compress) {
      if (elems(i-1).isMultipleOf(elems(i))) { compress = true }
      i = i + 1
    }
    if (compress) { compress2(elems) } // yep, we do need to compress
    else elems
  }

  def compress2[X](elems: NBlock[X]): NBlock[X] = {
    if (elems.size == 0) elems
    else {
      val size = elems.size
      var stack: List[SHNode[X]] = List(elems(0))
      var i = 1

      while (i < size) {
        val head = stack.head
        val elem = elems(i)
        if (head.isMultipleOf(elem)) {
          stack = head.combine(elem) +: stack.tail
        }
        else { stack = elem +: stack }
        i = i + 1
      }
      stack.toArray.reverse
    }
  }

  def doRound[X](elems: NBlock[X]): NBlock[X] = { doRound2(compress(elems)) }

  def doRound2[X](elems: NBlock[X]): NBlock[X] = {
    val N = elems.length
    val kind: Array[Byte] = new Array(N)
    val hashes: Array[Int] = new Array(N)
    var done = false
    var merges = 0

    var bit_index = 0
    var int_index = 0
    val N1 = N - 1

    while (!done) {
      done = true

      if (bit_index == 0) {
        // cache hashes of nodes that are unknown
        var ei = 0
        while (ei < N) {
          if (kind(ei) == Unknown) hashes(ei) = elems(ei).hash.hashAt(int_index)
          ei = ei + 1
        }
        int_index = int_index + 1
      }

      var j = 0
      while (j < N1) {
        if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
          if ((bitAt(hashes(j),bit_index) == 1) && (bitAt(hashes(j + 1),bit_index) == 0)) {
            kind(j) = Merge
            kind(j+1) = Merge
            j = j + 1
            merges = merges + 1
          }
          else { done = false }
        }
        j = j + 1
      }
      bit_index = (bit_index + 1) & 31
    }

    var i = 0
    var ii = 0
    var result: NBlock[X] = new Array(N-merges)

    while (i < N) {
      if (kind(i) == Unknown) result(ii) = elems(i)
      else {
        result(ii) = elems(i).combine(elems(i + 1))
        i = i + 1
      }
      i = i + 1
      ii = ii + 1
    }

    result
  }

  val eRight = RightFringe[Nothing](-1,Array(),List())
  val eLeft = LeftFringe[Nothing](-1,Array(),List())

  def emptyRight[X]: RightFringe[X] = eRight.asInstanceOf[RightFringe[X]]
  def emptyLeft[X]: LeftFringe[X] = eLeft.asInstanceOf[LeftFringe[X]]

  // Concatenate the left and right Fringes into a canonical tree
  // TODO: optimize the concatenation of intermediate NBLock[X]
  def concat2[X](left: RightFringe[X], right: LeftFringe[X]): SHNode[X] ={
    var elems: NBlock[X] = Array()
    var height = 0
    var done = false

    var lf = left.fringes
    var rf = right.fringes

    var lh = left.height
    var rh = right.height

    while (!done) {
      if (height < lh) {
        elems = lf.head ++ elems
        lf = lf.tail
      }
      else if (height == lh) { elems = left.top ++ elems }

      if (height < rh) {
        elems = elems ++ rf.head
        rf = rf.tail
      }
      else if (height == rh) { elems = elems ++ right.top }

      if ((height >= lh) && (height >= rh) && (elems.length == 1)) { done = true }
      else {
        elems = doRound(elems)
        height = height + 1
      }
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
    if (size <= 0) { null }
    else if (size >= h.size) { h }
    else {
      val ls = leftSplit2(h,size)
      val cm = compress(ls.toArray[SHNode[X]])
      concat2(transformRight(to_tmp_tree(cm)),emptyLeft)
    }
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
  def rightSplit[X](h: SHNode[X], size: Int): SHNode[X] = {
    if (size <= 0) { h }
    else if (size > h.size) { null }
    else {
      val rs = rightSplit2(h,h.size - size).toArray.reverse
      val cm = compress(rs)
      concat2(emptyRight,transformLeft(to_tmp_tree(cm)))
    }
  }

  def rightSplit2[X](h: SHNode[X], pos: Int): List[SHNode[X]] = {
    if (pos == 0) List()
    else {
      val right = h.right
      if (pos >= right.size) right +: rightSplit2(h.left,pos-right.size)
      else rightSplit2(right,pos)
    }
  }

  // EXPOSITION
  final def main(args: Array[String]): Unit = {

    def emptySH[X]: SHNode[X] = null
    def intNode(i: Int): SHNode[Int] = IntNode(i)

    var s1 = emptySH[Int]
    var s2 = emptySH[Int]
    var s3 = emptySH[Int]

    var i = 0
    var n = 500000
    while (i < n) {
      var k1 = intNode(i)
      var k2 = intNode(n-i-1)
      var k3 = intNode(i % 63)  // repetitions

      s1 = concat(s1,k1)
      s2 = concat(k2,s2)
      s3 = concat(s3,k3)

      i = i + 1
      if ((i % 1000) == 0) {
        println("concat i: " + i)

        s1 = s1.chunk
        s2 = s2.chunk
        s3 = s3.chunk
      }
    }

    if (s1 != s2) sys.error("Internal inconsistency")

       i = 1
    while (i < n) {
      // split into left and right
      val (ss1,ss2) = s1.split(i)
      // concatenate left and right -> should return original (unsplit) version
      val cc = ss1.concat(ss2)
      // quick check - hashCodes should be exactly the same
      if (cc.hashCode != s1.hashCode) { sys.error("Internal inconsistency") }

      // split repetition into left and right
      val (rp1,rp2) = s3.split(i)
      val ccc = rp1.concat(rp2)
      if (ccc.hashCode != s3.hashCode) { sys.error("Internal inconsistency") }

      i = i + 1

      if ((i % 1000) == 0) { println("split i: " + i) }
    }

    i = 0
    var b = 10000
    n = n / b
    var ss = emptySH[Int]

    while (i < n) {
      var ii = 0
      var sss: NBlock[Int] = new Array(b)
      var o = i * b
      println("fast block: " + o + " height: " + height(ss))

      while (ii < b) {
        sss(ii) = intNode(o + ii)
        ii = ii + 1
      }
      while (sss.size > 1) {
        sss = doRound(sss)
      }
      ss = concat(ss,sss(0))
      i = i + 1
    }
    if (s1 != ss) sys.error("Internal inconsistency")
  }

  def height[X](o: SHNode[X]) = {
    if (o == null) 0
    else o.height
  }
}

// SipHash. Modified version of https://gist.github.com/chrisvest/6989030#file-siphash-snip-scala

final object SipHash {
  final def rotl(x: Long, b: Int) = (x << b) | ( x >> (64 - b))

  final def siphash24(x1: Int, x2: Int): Int = {
    var v0 = 0x736f6d6570736575L
    var v1 = 0x646f72616e646f6dL
    var v2 = 0x6c7967656e657261L
    var v3 = 0x7465646279746573L

    def sipround() {
      v0 += v1
      v1 = rotl(v1, 13) ^ v0
      v0 = rotl(v0, 32)
      v2 += v3
      v3 = rotl(v3, 16) ^ v2
      v0 += v3
      v3 = rotl(v3, 21) ^ v0
      v2 += v1
      v1 = rotl(v1, 17) ^ v2
      v2 = rotl(v2, 32)
    }

    val m = rotl(x1.toLong,32) + x2
    v3 ^= m
    sipround
    sipround
    v0 ^= m

    v2 ^= 0xff
    sipround
    sipround
    sipround
    sipround

    val r = v0 ^ v1 ^ v2 ^ v3
    ((r >> 32) + (r & 0xffffffff)).toInt
  }
}