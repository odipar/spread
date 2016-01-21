package spread

/**
  * Created by rapido on 20/01/16.
  */
object SplitHash {

  final def main(args: Array[String]): Unit = {
    var seq1: SHNode[Int] = null
    var seq2: SHNode[Int] = null

    var i = 0
    var s = 10000

    var seq3: Array[SHNode[Int]] = new Array(s)

    var k1: Double = s.toDouble
    var l = 0
    while (k1 > 1.0) {
      k1 = k1 * 0.66
      l = l + 1
    }
    println("l1: " + l)
    l = 0
    var k2: Double = s.toDouble
    while (k2 > 1.0) {
      k2 = k2 * 0.58
      l = l + 1
    }
    println("l2: " + l)

    while (i < s) {
      var k = i % 7
      //seq1 = concat(seq1,IntNode(k))
      //seq2 = concat(IntNode(s-i-1),seq2)
      seq3(i) = IntNode(k)
      i = i + 1
    }
    var ss = seq3.toSeq

    while (ss.size > 1) {
      ss = doRound(ss)
    }

    var sss = ss(0)

    i = 1

    kk = 0
    cc = 0
   while (i < s) {
      val k1 = leftSplit(sss,i)
      val k2 = rightSplit(sss,i)
      val ssss = concat(k1,k2)
      if (sss != ssss) {
        println("k1: " + k1)
        println("k2: " + k2)
        sys.error("no")
      }
      i = i + 1

      if ((i % 100) == 0) { println("i: " + i) }
    }

    //var rrr = leftSplit(sss,1)
    //var rrr = rightSplit(sss,s-150)

    println("ttm: " + ttm)
    println("h: " + sss.height)

  //  println("rrr: " + rrr)
    println("cc: " + cc)
    println("kk: " + kk)

    //println("seq1: " + seq1)
    //println("seq2: " + seq2)
    //println("seq3: " + ss(0))
  }

  trait SplitHash[X] {
    def concat(o: SplitHash[X]): SplitHash[X]
    def split(i: Long): (SplitHash[X], SplitHash[X])
    def size: Int
    //def iterator: Iterator[X]
    //def reverseIterator: Iterator[X]
    //def explode: Seq[SplitHash[X]]
    //def compress: SplitHash[X]
  }

  trait Hashable {
    def hash: Hash
  }

  trait Hash {
    def head: Int
    def bit(i: Int): Byte = {
      if (i < 32) ((head >>> (31-i)) & 1).toByte
      else sys.error("no")
    }
    override def hashCode = head
  }

  var cc: Long = 0
  var kk: Long = 0

  trait SHNode[X] extends Hashable {
    { cc = cc + 1}
    def left: SHNode[X]
    def right: SHNode[X]
    def height: Int
    def size: Int

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

  trait LeafNode[X] extends SHNode[X] {
    def left = null
    def right = null
    def height = 0
    def size = 1
  }

  case class IntNode(i: Int) extends LeafNode[Int] with Hash {
    def hash = this
    val head = { Hashing.jenkinsHash(Hashing.jenkinsHash(i) ^ 1382309) }
    override def bit(index: Int) = {
      var p = index
      var h = head
      while (p > 31) {
        val h1 = Hashing.jenkinsHash(i) ^ h
        h = Hashing.jenkinsHash(h) ^ h1
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }

    override def toString = i.toString
  }

  var ttm = 0

  case class TempBinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X]  {
    { ttm = ttm + 1 }
    def size = sys.error("should not be called")
    val height = 1 + (left.height max right.height)
    def hash = sys.error("should not be called: " + this)

    override def toString = "<" + left + "|" + right + ">"
  }

  case class BinNode[X](left: SHNode[X], right: SHNode[X]) extends SHNode[X] with Hash {
    val size = left.size + right.size
    val height = 1 + (left.height max right.height)
    val hash = this
    val head = Hashing.jenkinsHash(left.hash.head ^ Hashing.jenkinsHash(right.hash.head))

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

    override def toString = multiplicity + ":" + node
  }

  class LeftNodeIterator[X](var tree: SHNode[X], h: Int) extends Iterator[SHNode[X]] {
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
      while(leftNode.height > h) {
        nstack = leftNode +: nstack
        leftNode = leftNode.left
      }
      leftNode
    }
  }

  class RightNodeIterator[X](var tree: SHNode[X], h: Int) extends Iterator[SHNode[X]] {
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
      while (rightNode.height > h) {
        nstack = rightNode +: nstack
        rightNode = rightNode.right
      }
      rightNode
    }
  }

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

  def to_tmp_tree[X](s: List[SHNode[X]]): SHNode[X] = {
    if (s.size == 0) null
    else if (s.size == 1) s.head
    else s.head.combine2(to_tmp_tree(s.tail))
  }

  def to_tmp_tree_rev[X](s: List[SHNode[X]]): SHNode[X] = {
    if (s.size == 0) null
    else if (s.size == 1) s.head
    else s.head.combine(to_tmp_tree(s.tail))
  }


  final val Unknown: Byte = 0
  final val Merge: Byte = 1
  final val Fringe: Byte = 2

  def leftFringe[X](tree: SHNode[X], height: Int): List[SHNode[X]] = {
    fringeVolatile2(new LazyIndexableIterator(new LeftNodeIterator(tree,height)),0).reverse
  }

  def rightFringe[X](tree: SHNode[X], height: Int): List[SHNode[X]] = {
    fringeVolatile2(new LazyIndexableIterator(new RightNodeIterator(tree,height)),1)
  }

  def fringeVolatile2[X](elems: LazyIndexableIterator[X], direction: Byte): List[SHNode[X]] = {
    // TODO: Proof that this is always OK
    val width = 4
    var s = width
    var done = false
    var ns: List[SHNode[X]] = null

    var ii = 0
    while (!done) {
      ns = fringeVolatile3(elems,direction,s)
      if ((ns.size + width) == s) { done = true }
      else { s = ns.size + width }
      ii = ii + 1
    }
    ns
  }

  def fringeVolatile3[X](elems: LazyIndexableIterator[X], direction: Byte, rr: Int): List[SHNode[X]] ={
    val kind: Array[Byte] = new Array(rr+1)
    var right = rr

    var index = 1
    var bit_index = 0
    kind(0) = Fringe

    var other_direction = 1-direction

    var done = false

    while (!done) {
      done = true

      if (index < right) {
        val e1 = elems(index)
        if (e1 != null) {
          if ((kind(index) == Unknown) && (e1.hash.bit(bit_index) == direction)) {
            kind(index) = Fringe
            index = index + 1
            kk = kk + 1
          }
          if (kind(index) == Unknown) { done = false }
        }
      }

      if (!done) {
        var j = index

        while (j < right) {
          if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
            val e1 = elems(j)
            val e2 = elems(j+1)

            if ((e1 != null) && (e2 != null)) {
              if ((e1.hash.bit(bit_index) == other_direction) && (e2.hash.bit(bit_index) == direction)) {
                kind(j) = Merge
                kind(j + 1) = Merge
                right = j
                j = right
              }
              else { done = false }
            }
          }
          j = j + 1
          kk = kk + 1
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
      else {
        result = LeftFringe(height,lfringe,leftFringes)
      }
    }

    result
  }

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
      else {
        result = RightFringe(height,rfringe,rightFringes)
      }
    }

    result
  }

  def concat[X](left: SHNode[X], right: SHNode[X]): SHNode[X] = {

    if (left == null) right
    else if (right == null) left
    else {
      val tl = transformRight(left)
      val tr = transformLeft(right)

      concat2(tl,tr)
    }
  }

  def size[X](e1: SHNode[X]): Int = e1 match {
    case RLENode(_,l) => l
    case _ => 1
  }

  def get[X](e1: SHNode[X]): SHNode[X] = e1 match {
    case RLENode(h,_) => h
    case _ => e1
  }

  def compress[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = {
    if (elems.size > 0) {

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
    else elems
  }

  def doRound[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = doRound2(compress(elems))

  def doRound2[X](elems: Seq[SHNode[X]]): Seq[SHNode[X]] = {
    val N = elems.length

    val kind: Array[Byte] = elems.map(e => Unknown).toArray
    val hashes: Array[Hash] = elems.map(e => e.hash).toArray
    var done = false

    var idx = 0
    val N1 = N - 1

    while (!done) {
      done = true

      var j = 0

      while (j < N1) {
        if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
          if ((hashes(j).bit(idx) == 1) && (hashes(j + 1).bit(idx) == 0)) {
            kind(j) = Merge
            kind(j+1) = Merge
            j = j + 1
          }
          else { done = false }
        }
        j = j + 1
        kk = kk + 1
      }
      idx = idx + 1
    }

    var i = 0
    var center: List[SHNode[X]] = List()

    while (i < N) {
      kind(i) match {
        case Unknown => { center = elems(i) +: center}
        case Merge => { center = (elems(i).combine(elems(i + 1))) +: center; i = i + 1 }
      }
      i = i + 1
    }

    center.toArray.reverse.toSeq
  }

  def emptyRight[X]: RightFringe[X] = RightFringe(-1,Array().seq,Array().seq)
  def emptyLeft[X]: LeftFringe[X] = LeftFringe(-1,Array().seq,Array().seq)

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

  def leftSplit[X](h: SHNode[X], pos: Int): SHNode[X] = {
    concat2(transformRight(to_tmp_tree(leftSplit2(h,pos))),emptyLeft)
  }

  def leftSplit2[X](h: SHNode[X], pos: Int): List[SHNode[X]] = {
    if (pos == 0) List()
    else {
      val left = h.left
      val right = h.right

      if (pos >= left.size) {
        left +: leftSplit2(right,pos-left.size)
      }
      else leftSplit2(left,pos)
    }
  }

  def rightSplit[X](h: SHNode[X], pos: Int): SHNode[X] = {
    val rs = rightSplit2(h,h.size-pos).reverse
    val cm = compress(rs)
    concat2(emptyRight,transformLeft(to_tmp_tree(cm.toList)))
  }

  def rightSplit2[X](h: SHNode[X], pos: Int): List[SHNode[X]] = {
    if (pos == 0) List()
    else {
      val left = h.left
      val right = h.right

      if (pos >= right.size) {
        right +: rightSplit2(left,pos-right.size)
      }
      else rightSplit2(right,pos)
    }
  }
}