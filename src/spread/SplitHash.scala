package spread

import Hashing._

object SplitHash {
  //
  // SplitHash is an immutable, uniquely represented Sequence ADT (Authenticated Data Structure).
  // It is based on a novel hashing scheme that was first introduced with SeqHash.
  // (See http://www.bu.edu/hic/files/2015/01/versum-ccs14.pdf).
  //
  // Like SeqHashes, SplitHashes can be concatenated in O(log(n)).
  // But SplitHash extends SeqHash by allowing Hashes to also be split in O(log(n)).
  // It also solves SeqHash's issue with repeating nodes by applying RLE (Run Length Encoding) compression.
  // To improve cache coherence and memory bandwidth, SplitHashes can be optionally chunked into n-ary trees.
  //
  // SplitHash is the first known History-Independent(HI) ADT that holds all these properties.
  //
  // Copyright 2016: Robbert van Dalen.
  //

  trait SplitHash[X, SH <: SplitHash[X,SH]] extends Hashable {
    def hash: Hash                  // O(1)
    def size: Int                   // O(1)
    def concat(other: SH): SH       // O(log(size))
    def split(at: Int): (SH, SH)    // O(log(size))
    def first: X                    // O(log(size))
    def last: X                     // O(log(size))
    def chunk: SH                   // O(unchunked)
  }

  // A Hashable object
  trait Hashable {
    def hash: Hash
  }

  // An 'infinitely' indexable and expandable Hash that *must* obey the following property:
  // The chance that two (slightly) different objects have equal hashes at index i
  // *must* exponentially decrease at higher indices.
  //
  // Hashes that don't exhibit this property may cause SplitHash to get stuck in an infinite loop.

  trait Hash {
    def hashAt(i: Int): Int
  }

  // A canonical tree SH(Split Hash)Node
  trait SHNode[X] extends SplitHash[X,SHNode[X]] with Hash with Hashable {
    def size: Int
    def hash = this
    def concat(other: SHNode[X]) = SplitHash.concat(this,other)
    def split(at: Int) = {
      val l = leftSplit(this,at)
      val r = rightSplit(this,at)
      assert((l.size + r.size) == size)
      (l,r)
    }
    def first: X
    def last: X

    // tree traversal
    def left: SHNode[X]
    def right: SHNode[X]
    def height: Int

    // Equality check that's based on content equality, rather than structure equality (==)
    // (Used to detect node repetitions)
    def equalTo(other: SHNode[X]): Boolean = (this == other)

    // chunking
    def chunk: SHNode[X]
    def isChunked: Boolean
    def chunkHeight: Int

    def isMultipleOf(n: SHNode[X]): Boolean = mget(this).equalTo(mget(n))
    def combine(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(mget(this),msize(this) + msize(n))
      else BinNode(this,n,(size + n.size))
    }
    def combine2(n: SHNode[X]): SHNode[X] = {
      if (isMultipleOf(n)) RLENode(mget(this),msize(this) + msize(n))
      else TempBinNode(this,n)
    }

    def ++(other: SHNode[X]): SHNode[X] = this.concat(other)
  }

  // Utility methods to detect and deal with consecutive, equal nodes
  private def mget[X](e1: SHNode[X]): SHNode[X] = e1 match {
    case RLENode(h,_) => h
    case _ => e1
  }
  private def msize[X](n: SHNode[X]): Int = n match {
    case RLENode(_,s) => s
    case _ => 1
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

    def chunk = this
    def isChunked = false
    def chunkHeight = 0
  }

  case class IntNode(value: Int) extends LeafNode[Int] {
    override def hashCode = siphash24(value + magic_p1,value - magic_p2)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(value + magic_p2, hashCode * magic_p1)
      else siphash24(hashCode * magic_p3, hashAt(index-1) - magic_p2)
    }
    override def toString = value.toString
  }

  trait BNode[X] extends SHNode[X] {
    override def equalTo(other: SHNode[X]): Boolean = {
      // BY DESIGN: fast hashcode in-equality check
      if (hashCode != other.hashCode) false
      // fast reference equality check (=true when references are equal)
      else if (this.eq(other)) true
      else  ((left.equalTo(other.left)) && (right.equalTo(other.right)))
    }
  }

  var unlikely: Int = 0

  // A full binary node holds a hash (Int), size (Int), (chunk)height (Int) and its left and right sub-trees
  // There is some trickery to reduce the memory footprint and the lazy computation of hashes.
  case class BinNode[X](left: SHNode[X], right: SHNode[X], csize: Int) extends BNode[X] {

    def first = left.first
    def last = right.last
    def size = scala.math.abs(csize)
    def isChunked = csize < 0 // negative csize indicates that the node is chunked
    def heightE = 1 + (left.height max right.height)
    def chunkHeightE = 1 + (left.chunkHeight max right.chunkHeight)
    val heightEE = (heightE << 8) | chunkHeightE   // encode both heights into one Int
    def height = heightEE >> 8
    def chunkHeight = heightEE & 0xff

    private var lHash = 0  // A lazy hash = 0 (trick borrowed from the Avail Programming Language)
    override def hashCode = {
      if (lHash == 0) lHash = siphash24(left.hashCode - magic_p2,right.hashCode + magic_p3)
      lHash           // could be 0 again, but then we just recompute 0.
    }

    // We just keep munging bits recursively while traversing down the tree.
    //
    // The idea is that, when two unequal tree nodes collide in their hashCode,
    // then their left and right tree nodes should not collide with exponential
    // higher probability, given a certain hash length/depth.
    //
    // This is shaky cryptographically, but 'works' in 'practice'.
    // (That said, by using SipHash it *might* be cryptographically strong)
    //
    // TODO: We do need a cryptographic expert to have a stab at it.

    override def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) (left.hashCode - right.hashCode) ^ hashCode   // quick and dirty rehash
      else {
        // 64 bits or more are requested. This should normally not happen, unless
        // a client just wishes to calculate a bigger hash.
        unlikely = unlikely + 1
        val nindex = index / 2

        if (hashCode > 0) siphash24(left.hash.hashAt(nindex) - magic_p3,right.hash.hashAt(index - nindex) + (magic_p1 * hashCode))
        else siphash24(right.hash.hashAt(nindex) - (magic_p3 * hashCode),left.hash.hashAt(index - nindex) + magic_p1)
      }
    }
    def chunk = {
      if (isChunked) this
      else {
        val l = left.chunk
        val r = right.chunk
        val nt = BinNode(l,r,-(l.size + r.size)) // remember, 'negative' size indicates that a node is chunked.

        if (nt.chunkHeight > 5) chunkTree(nt)  // chunks of height = 6 (average size ~ 32)
        else nt
      }
    }
    override def toString = left + " " + right
  }

  // A RLE(Run Length Encoded) node denotes the repetition of another node
  case class RLENode[X](node: SHNode[X], multiplicity: Int) extends SHNode[X] {
    def left = {
      if (multiplicity < 4) node
      else RLENode(node,multiplicity/2)
    }
    def right = {
      if (multiplicity < 3) node
      else RLENode(node,multiplicity - (multiplicity/2))
    }
    def size = node.size * multiplicity
    def height = node.height
    def chunkHeight = node.chunkHeight
    override val hashCode = siphash24(node.hashCode + magic_p1 ,multiplicity - magic_p3)
    override def hashAt(index: Int) = {
      if (index > 0) siphash24(hashAt(index-1) + magic_p2 ,multiplicity - (magic_p3 * index))
      else hashCode
    }
    def chunk = {
      if (!node.isChunked) RLENode(node.chunk,multiplicity)
      else this
    }
    def isChunked = node.isChunked
    def first = node.first
    def last = node.last
    override def toString = multiplicity + ":" + node
  }

  // A temporary binary node that shouldn't be part of a canonical tree
  case class TempBinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X]  {
    def error = sys.error("Internal inconsistency. Should not be called")
    val height = 1 + (left.height max right.height) // only the height is important
    def size = error
    def first = error
    def last = error
    def chunk = error
    def isChunked = error
    def chunkHeight = error
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

  // most used type
  type NArray[X] = Array[SHNode[X]]

  // Lazily consume the Iterator, whilst caching its elements at a certain index.
  class LazyIndexableIterator[X](it: Iterator[SHNode[X]]) {
    var values: NArray[X] = new Array(8)
    var size = 0

    def apply(i: Int): SHNode[X] = {
      if (i >= size) {
        if (i >= values.length) {
          // array exhausted, so extend it
          var vv: NArray[X] = new Array(values.length * 2)
          values.copyToArray(vv)
          values = vv
        }
        var ii = size
        while ((ii <= i) && (it.hasNext)) {
          values(ii) = it.next
          ii = ii + 1
        }
        size = ii
        assert(size <= values.length)
      }

      if (i >= size) null
      else values(i)
    }

    def firstReversed(s: Int) = {
      val firstR: NArray[X] = new Array(s)
      var i = 0
      while (i < s) {
        firstR(i) = values(s-i-1)
        i = i + 1
      }
      firstR
    }
  }

  // Build a temporary tree for left or right Fringe consumption only
  def to_tmp_tree[X](subtrees: NArray[X]): SHNode[X] = {
    if (subtrees.length == 0) null
    else {
      val s = subtrees.length
      var tree = subtrees(0)
      var i = 1
      while (i < s) {
        tree = tree.combine2(subtrees(i))
        i = i + 1
      }
      tree
    }
  }

  def chunkTree[X](tree: SHNode[X]): SHNode[X] = {
    val (t,b) = chunkTree2(tree,List())
    ChunkedNode(t.toArray.toSeq,b.toArray.reverse.toSeq,tree.hashCode,tree.size,tree.height)
  }

  // TODO: imperative version
  def unchunkTree[X](nodes: Seq[SHNode[X]], tree: Seq[Boolean], i1: Int, i2: Int): (Int,Int,SHNode[X]) = {
    if (tree(i2) == false) (i1+1,i2+1,nodes(i1))
    else {
      val (ii1,ii2,l1) = unchunkTree(nodes,tree,i1,i2+1)
      val (iii1,iii2,r1) = unchunkTree(nodes,tree,ii1,ii2+1)

      (iii1,iii2,l1.combine(r1))
    }
  }

  // Chunk a canonical tree into a ChunkedNode that contains all the sub-nodes with chunkHeight = 0.
  // TODO: imperative version
  def chunkTree2[X](node: SHNode[X], treeEnc: List[Boolean]): (List[SHNode[X]], List[Boolean]) = {
    if (node.chunkHeight == 0) (List(node),false +: treeEnc)
    else {
      val (lt,ll) = chunkTree2(node.left,true +: treeEnc)
      val (rt,lll) = chunkTree2(node.right,true +: ll)
      (lt ++ rt,lll)
    }
  }

  def unchunk[X](cn: ChunkedNode[X]): SHNode[X] = {
    val (_,_,result) = unchunkTree(cn.nodes,cn.tree,0,0)

    assert(result.size == cn.size)
    assert(result.hashCode == cn.hashCode)

    result
  }

  import java.lang.ref.WeakReference

  // A ChunkedNode that holds all the LeafNodes of the canonical tree it represents.
  // When lazily requested, it also caches the unchunked canonical tree in a WeakReference,
  // The unchunked version can be GC'ed anytime, as it can always be rebuild from the originating chunk.
  // ChunkedNode turns a binary tree into a n-ary tree, thus saving a lot of memory (bandwidth)

  case class ChunkedNode[X](nodes: Seq[SHNode[X]], tree: Seq[Boolean], h: Int, size: Int, height: Int) extends BNode[X] {
    // Note that we could also decide to weakly store the unchunked version into some kind of
    // FIFOCache to avoid GC trashing. For now we just rely on the GC to clean the WeakReference
    // before memory pressure becomes too high.
    var unchunked: WeakReference[_] = null

    def isChunked = true
    def getUnchunked: SHNode[X] = {
      if (unchunked != null) {
        val u = unchunked.get
        if (u != null) u.asInstanceOf[SHNode[X]]
        else getUnchunked2
      }
      else getUnchunked2
    }
    def getUnchunked2: SHNode[X] = {
      val uchunk = unchunk(this)
      unchunked = new WeakReference(uchunk)
      uchunk
    }

    def left = getUnchunked.left
    def right = getUnchunked.right
    def first = nodes(0).first
    def last = nodes(nodes.length-1).last
    override def hashCode = h
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else getUnchunked.hashAt(i)
    }
    def chunk = this
    def chunkHeight = 0
  }

  final val Unknown: Byte = 0
  final val Merge: Byte = 1
  final val Fringe: Byte = 2

  // Determine the left and right Fringe of a canonical tree
  //
  // Notice that, unlike SeqHash, we don't need to hold on to the left and right Fringes, as we can always determine
  // the Fringes in O(log(n)) for each canonical tree at anytime.

  def leftFringe[X](tree: SHNode[X], height: Int): NArray[X] = {
    fringeVolatile2(new LazyIndexableIterator(new LeftNodeIterator(tree,height)),0).reverse
  }

  def rightFringe[X](tree: SHNode[X], height: Int): NArray[X] = {
    fringeVolatile2(new LazyIndexableIterator(new RightNodeIterator(tree,height)),1)
  }

  // Iteratively scan and deepen the fringe 'frontier' (width(average)=5) until the exact Fringe is found
  // We do this in order to minimally consume the LazyIndexableIterator
  def fringeVolatile2[X](elems: LazyIndexableIterator[X], direction: Byte): NArray[X] = {
    val width = 5
    var frontier = width
    var fringe: NArray[X] = null

    while (fringe == null) {
      val frontier1 = frontier+1

      val kinds: Array[Byte] = new Array(frontier1) // create reuseable arrays for two fringe determinations
      val hashes: Array[Int] = new Array(frontier1)

      val fringe1 = fringeVolatile3(elems,direction,frontier,kinds,hashes)
      resetKinds(kinds)
      // this is the tricky part as we *need* to check the edge difference:
      // nodes may be combined differently at edges that are off-by-one
      val fringe2 = fringeVolatile3(elems,direction,frontier1,kinds,hashes) // frontier+(off by one)
      if (fringe1 != fringe2) frontier = frontier + width // advance the frontier when there is a difference
      else fringe = elems.firstReversed(fringe1)
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

      assert(min_frontier <= frontier)

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
          if ((index < min_frontier) && (kind(index) == Unknown)) done = false
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
              else done = false
            }
          }
          j = j + 1
        }
      }
      bit_index = (bit_index + 1) & 31
    }
    index
  }

  // wipe array for re-use
  def resetKinds(kind: Array[Byte]): Unit = {
    var i = 0
    var f = kind.size
    while (i < f) {
      kind(i) = Unknown
      i = i + 1
    }
  }

  final def bitAt(value: Int, index: Int): Int = ((value >>> (31-index)) & 1)

  case class LeftFringe[X](height: Int, top: NArray[X], fringes: List[NArray[X]])
  case class RightFringe[X](height: Int, top: NArray[X], fringes: List[NArray[X]])

  // Transform a canonical tree into right-catenable LeftFringe tree
  def transformLeft[X](t: SHNode[X]): LeftFringe[X] = {
    var tt = t
    var height = 0
    var leftFringes: List[NArray[X]] = List()
    var result: LeftFringe[X] = null

    while(result == null) {
      val lfringe = leftFringe(tt,height)
      val lfirst = first(height,lfringe.size,tt)
      if (lfirst != null) {
        leftFringes = lfringe +: leftFringes
        tt = lfirst
        height = height + 1
      }
      else result = LeftFringe(height,lfringe,leftFringes.reverse)
    }
    result
  }

  // Get the first n tree nodes at a certain height
  def first[X](hh: Int, n: Int, t: SHNode[X]): SHNode[X] = {
    val f = first2(hh,n,t,List())
    val cm = compress(f._2.toArray.reverse)
    to_tmp_tree(cm)
  }

  // TODO: turn this into an iterative version
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

  // Get the last n tree nodes at a certain height
  def last[X](hh: Int, n: Int, t: SHNode[X]): SHNode[X] = {
    val l = last2(hh,n,t,List())
    val cm = compress(l._2.toArray[SHNode[X]])
    to_tmp_tree(cm)
  }

  // TODO: turn this into an iterative version
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
    var rightFringes: List[NArray[X]] = List()
    var result: RightFringe[X] = null

    while(result == null) {
      val rfringe = rightFringe(tt,height)
      val rlast = last(height,rfringe.size,tt)
      if (rlast != null) {
        rightFringes = rfringe +: rightFringes
        tt = rlast
        height = height + 1
      }
      else result = RightFringe(height,rfringe,rightFringes.reverse)
    }
    result
  }

  // Concatenate two canonical trees
  def concat[X](left: SHNode[X], right: SHNode[X]): SHNode[X] = {
    if (left == null) right
    else if (right == null) left
    else concat2(transformRight(left),transformLeft(right))
  }

  // Compresses consecutive and equal (RLE) nodes into RLE nodes
  def compress[X](elems: NArray[X]): NArray[X] = {
    // first check whether we need to compress
    var i = 1
    var s = elems.size
    var compress = false
    while ((i < s) && !compress) {
      if (elems(i-1).isMultipleOf(elems(i))) compress = true
      i = i + 1
    }
    if (compress) compress2(elems) // yep, we do need to compress
    else elems
  }

  def compress2[X](elems: NArray[X]): NArray[X] = {
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
        else stack = elem +: stack
        i = i + 1
      }
      stack.toArray.reverse
    }
  }

  def doRound[X](elems: NArray[X]): NArray[X] = doRound2(compress(elems))

  // Like SeqHash, but without the left and right fringe determination
  // Inspired by: https://github.com/jellevandenhooff/versum/blob/master/seqhash/hash.go

  def doRound2[X](elems: NArray[X]): NArray[X] = {
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
          else done = false
        }
        j = j + 1
      }
      bit_index = (bit_index + 1) & 31
    }

    var i = 0
    var ii = 0
    var result: NArray[X] = new Array(N-merges)

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

  final val eRight = RightFringe[Nothing](-1,Array(),List())
  final val eLeft = LeftFringe[Nothing](-1,Array(),List())

  final def emptyRight[X]: RightFringe[X] = eRight.asInstanceOf[RightFringe[X]]
  final def emptyLeft[X]: LeftFringe[X] = eLeft.asInstanceOf[LeftFringe[X]]

  // Concatenate the left and right Fringes into a canonical tree
  // TODO: optimize the concatenation of intermediate NArray[X]
  def concat2[X](left: RightFringe[X], right: LeftFringe[X]): SHNode[X] ={
    var elems: NArray[X] = Array()
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
      else if (height == lh) elems = left.top ++ elems

      if (height < rh) {
        elems = elems ++ rf.head
        rf = rf.tail
      }
      else if (height == rh) elems = elems ++ right.top

      if ((height >= lh) && (height >= rh) && (elems.length == 1)) done = true
      else {
        elems = doRound(elems)
        height = height + 1
      }
    }

    elems(0)
  }

  // Split the left side of a canonical tree,
  // We first split the tree into a List of sub-trees. O(log(N))
  // Then we combine those sub-trees into a non-canonical temporary tree.
  // This temporary tree is almost similar to the final tree, except that its right Fringe must be 'repaired'.
  // Most interestingly, the right Fringe can be repaired by just re-evaluating it.
  //
  // This is key to log(N) splitting.
  //
  def leftSplit[X](h: SHNode[X], size: Int): SHNode[X] = {
    if (size <= 0) null
    else if (size >= h.size) h
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
    if (size <= 0) h
    else if (size > h.size) null
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

  // *************************************
  // EXPOSITION OF ALL THE NICE PROPERTIES
  // *************************************
  final def main(args: Array[String]): Unit = {

    def emptySH[X]: SHNode[X] = null
    def intNode(i: Int): SHNode[Int] = IntNode(i)

    var s1 = emptySH[Int]
    var s2 = emptySH[Int]
    var s3 = emptySH[Int]

    var i = 0
    var n = 50000

    while (i < n) {
      var k1 = intNode(i)       // forwards
      var k2 = intNode(n-i-1)   // backwards
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
      val cc = ss1.concat(ss2).chunk
      // thus should be exactly the same. Equality check is also fast because the two trees share a lot of nodes
      if (cc != s1) sys.error("Internal inconsistency")

      // split repetition into left and right (note that repetitions are notoriously hard to get right!)
      val (rp1,rp2) = s3.split(i)
      val ccc = rp1.concat(rp2).chunk
      // Again, the concatenation of the parts should equal the original tree
      if (ccc != s3) sys.error("Internal inconsistency")

      i = i + 1
      if ((i % 1000) == 0) println("split i: " + i)
    }

    val b = n / 50
    n = n / b
    i = 0
    var ss = emptySH[Int]

    // Ultra fast block based concatenation
    while (i < n) {
      var ii = 0
      var sss: NArray[Int] = new Array(b)
      var o = i * b

      println("fast block: " + o)

      while (ii < b) {
        sss(ii) = intNode(o + ii)
        ii = ii + 1
      }

      // Build the block
      while (sss.size > 1) {
        sss = doRound(sss)
      }

      val cs = sss(0).chunk
      ss = concat(ss,cs)
      ss = ss.chunk

      i = i + 1
    }

    if (s1 != ss) sys.error("Internal inconsistency")

    // If we restrict ourselves to only concat, split, chunk and doRound, hash consumption shouldn't exceed 64 bits.
    // When unlikely does happen, you've either won the lottery or you are a cryptographer.

    println("unlikely > 64 bit consumption: " + unlikely)
  }

}
