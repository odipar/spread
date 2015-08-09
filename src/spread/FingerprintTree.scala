package spread

/*
 * The following code marks the birth of a novel data structure: The Fingerprint Tree.
 * A Finger Tree is built by recursively applying a 'Chunking' algorithm, using a standard Rabin fingerprinting scheme.
 * Note that 1- or 2-level Chunking can be found in many commercial de-duplication storage solutions.
 *
 * When N-level Chunking is applied, it results in a Tree structure, with following two amazing properties:
 *  - It is a B-Tree
 *  - it has a canonical representation
 *
 *  In addition, repetitions like the following:
 * XXXXXXXXXXXXXXXXXAAAAAAAAA
 * are RLEN compressed and recursively rechunked, to prevent excessive chunk sizes due to low entropy.
 * Details can be found in the below code.
 * 
 * Note another interesting property:
 * 
 * Small changes to the input X, will only create log(|X|) new Chunks. 
 * It is due to this property that de-duplication actually works in practice.
 * 
 * So in theory, it should be possible to create incremental immutable versions of the following algorithms:
 * - concatenate two Fingerprint Trees A and B, in O(B*log(max(|A|,|B|))
 * - cut a Fingerprint Tree X at index I, in O(B*log(|X|) §
 *       
 *       .. with B the average Block size(=31).
 * 
 * However, in practice, it might prove difficult to find implementations of these algorithms.
 * 
 * Copyright 2015: Robbert van Dalen.
 *
 */

object FingerprintTree {

  import scala.reflect.ClassTag

  final def main(args: Array[String]): Unit = {


    var i = 1
    var s = 1000
    var ii = s - 1
    var r: SeqHash = empty
    var r2: SeqHash = empty
    var r3: SeqHash = empty

    while (i < s) {
      val kk = (i / 10)
      r = merge(r,create(IntHashable(kk)))
      r2 = merge(r2,create(IntHashable(kk)))

      if ((i % 5) == 0) {
        val r4 = merge(r3,r2)
        r2 = empty
        r3 = r4
      }
      i = i + 1
      ii = ii - 1
    }

    r3 = merge(r3,r2)

    println("r: " + r)
    println("r3: " + r3)

    println("r == r3: " + (r == r3))
  }

  /* Constants for Rabin fingerprints */
  val multiplier = 1664525

  /* The fingerprint window size */
  val wsize = 5

  /* mm = pow(m,wsize) */
  val mm = {
    var i = 0 ; var m = multiplier
    while (i < wsize) {
      m = m * multiplier ; i = i + 1
    }
    m
  }

  /* Jenkins hash of an Int */
  final def jenkinsHash(k: Int): Int = {
    var key: Int = k
    key = ~key + (key << 15) // key = (key << 15) - key - 1;
    key = key ^ (key >>> 12)
    key = key + (key << 2)
    key = key ^ (key >>> 4)
    key = key * 2057 // key = (key + (key << 3)) + (key << 11);
    key = key ^ (key >>> 16)
    key
  }

  final def jh(k: Any): Int = jenkinsHash(k.hashCode)

  /*
   * Build the canonical Tree through recursive chunking, until the output size is smaller than the window size.
   *
   */

  def chunk[X: ClassTag](a: Seq[X]): Seq[_] = {
    var r: Seq[_] = a
    while (r.size > wsize) { r = partition(r,16) }
    r
  }
  /*
   * Partition a Sequence of Objects into a Sequence of Chunks using Rabin fingerprinting (rolling hash)
   * Chunks that are too big (due to repetitions/low entropy) are compressed and again (recursively) partitioned.
   */
  def partition[X: ClassTag](a: Seq[X], csize: Int): Seq[Chunk[_]] = {
    var r: List[Chunk[_]] = List()
    var size = a.length

    var searchPosition = 1
    var hash = 0
    var position = 0
    val max2 = Integer.MAX_VALUE / 2

    while (searchPosition < size) {
      val wi = searchPosition - position

      // compute the next hash of the advancing window
      hash = (hash + jh(a(searchPosition))) * multiplier
      if (searchPosition > wsize) { hash = hash - (jh(a(searchPosition - wsize)) * mm) }

      // make hash a positive int
      val hh = max2 + (hash / 2)

      if ((hh % csize) == 0) {
        r = r :+ createChunk(copy(a,position,wi), hh, wsize)
        position = searchPosition
      }

      searchPosition = searchPosition + 1
    }

    // The remainder is also a Chunk
    val wi = searchPosition - position
    if ((wi) > 0) { r = r :+ createChunk(copy(a,position,wi), hash, wsize) }

    r.toArray.toSeq
  }

  def copy[X : ClassTag](s: Seq[X], i: Int, w: Int): Seq[X] = s.slice(i,i+w)

  def createChunk[X: ClassTag](a: Seq[X], rhash: Int, ws: Int): Chunk[_] = {
    if (a.size < (ws * 2)) Chunk(a)  // within bounds
    else {                           // the Chunk is too big
      val rs = compress(a,16,2)      // compress
      val ks = partition(rs,16)      // and chunk the compressed chunk

      if ((ks.size) > ws * 2) sys.error("The chance of this happening should be very small")
      else Chunk(ks)
    }
  }

  case class Chunk[X : ClassTag](chunk: Seq[X]) {
    override def toString = {
      var r = "<"
      for (i <- chunk) { r = r + i.toString }
      r + ">"
    }
  }

  trait CompressedChunk[X]

  case class Repetition[X](rep: Seq[X],size: Int) extends CompressedChunk[X]{
    override def toString ={
      var r = "{" + size + ":"
      for (i <- rep) { r = r + i.toString }
      r + "}"
    }
  }

  case class Block[X](block: Seq[X]) extends CompressedChunk[X]{
    override def toString ={
      var r = "["+block.size+":"
      for (i <- block) { r = r + i.toString }
      r + "]"
    }
  }

  /*
   * Run-Length-Encoding-N (RLEN) compression. This version also encodes repetitions of *multiple* N < W elements.
   * RLE(N) compression has the important property that local changes to the input result in local changes
   * to the encoded output.
   *
   * For example:
   *
   * AAAAXXXABCABCYPO is encoded in {4:A}{3:X}{6:ABC}[3:YPO]
   *
   * and the slightly altered version:
   *
   * AABAAXXXABCABCYPO is encoded in {2:A}[1:B]{2:A}{3:X}{6:ABC}[3:YPO]
   *
   */

  def compress[X: ClassTag](data: Seq[X], windowLength: Int, minimumRunLength: Int): Seq[CompressedChunk[X]] = {
    // The compressed output is a 'Stack' of CompressedString[X]s with its head being the top of the Stack
    var output: List[CompressedChunk[X]] = List()
    var size = data.length
    var position = 0

    while (position < size) {
      var bestMatchLength = 0
      var bestDistance = 0
      var searchPosition = position + 1
      var maxPos = size min (searchPosition + windowLength)

      // find the best repeating match, given the window length
      while (searchPosition < maxPos) {
        var positionMatch = position
        var searchMatch = searchPosition

        while ((searchMatch < size) && (data(searchMatch) == data(positionMatch))) {
          positionMatch = positionMatch + 1
          searchMatch = searchMatch + 1
        }
        // We have found a repeating pattern, iff
        //  - the positionMatch is greater or equal to the searchPosition
        //  - the matchLength = (searchMatch - position) is greater than the best length found earlier
        if ((positionMatch >= searchPosition) && ((searchMatch - position) > bestMatchLength)) {
          bestDistance = searchPosition - position
          bestMatchLength = searchMatch - position
        }
        searchPosition = searchPosition + 1
      }

      if (bestMatchLength > 0) {
        // encode as a Repetition
        output = append(output,Repetition(data.slice(position,position+bestDistance),bestMatchLength))
        position = position + bestMatchLength
      }
      else {
        // encode as a Block
        output = append(output,Block(Seq(data(position))))
        position = position + 1
      }
    }

    // Reverse output to an Sequence of CompressedString[X]
    output.reverse.toArray.toSeq
  }


  /*
   * Appends a CompressedChunk[X] to the output List, merging a Block with another Block or Repetition where possible.
   */

  def append[X](a: List[CompressedChunk[X]],i: CompressedChunk[X]): List[CompressedChunk[X]] ={
    if (!a.isEmpty) {
      a.head match {
        case Block(aa) => i match {
          case Block(ii) => Block(aa ++ ii) +: a.tail  // merge two Blocks
          case _ => i +: a
        }
        case _ => i +: a
      }
    }
    else List(i)
  }

  /*

  type Hashable interface {
	ads.ADS

	CombineWith(other Hashable, c comp.C) Hashable
}

const (
	unknown int = iota
	mergeLeft
	mergeRight
	leftFringe
	rightFringe
)

type round struct {
	leftFringe, center, rightFringe []Hashable
}
   */

  case class CharHashable(i: Char) extends Hashable with Hash {
    def height = 0
    def size = 1
    def head = Hashing.jenkinsHash(i) ^ i
    def bit(index: Int) = {
      var p = index
      var h = head
      if (p > 31) {
        h = Hashing.jenkinsHash(h) ^ i
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }
    def hash: Hash = this
    override def toString = i.toString
  }

  case class IntHashable(i: Int) extends Hashable with Hash {
    def height = 0
    def size = 1
    def head = { Hashing.jenkinsHash(i) }
    def bit(index: Int) = {
      var p = index
      var h = head
      while (p > 31) {
        val h1 = Hashing.jenkinsHash(i) ^ h
        h = Hashing.jenkinsHash(h) ^ h1
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }
    def hash: Hash = this
    override def toString = i.toString
  }


  case class HPair(h1: Hashable, h2: Hashable) extends Hashable with Hash {
    def height = (h1.height max h2.height) + 1
    val size = h1.size + h2.size
    val head = Hashing.jenkinsHash(h1.hash.head ^ Hashing.jenkinsHash(h2.hash.head))
    def bit(index: Int): Byte = {
      var p = index
      var h = head
      while (p > 31) {
        h = Hashing.jenkinsHash(h)
        p = p - 32
      }
      ((h >>> (31-p)) & 1).toByte
    }
    def hash: Hash = this

    override def toString = "[" + h1 + "," + h2 + "]"
  }

  val counts: scala.collection.mutable.HashSet[Hashable] = scala.collection.mutable.HashSet()
  var ccounts: Long = 0
  var dd = false

  def count(h: Hashable): Unit = {
    if (!counts.contains(h)) {
      if (dd) {
        println("h: " + h)
      }
      counts.add(h) ; ccounts = ccounts + 1  }
  }

  trait Hashable {
    def hash: Hash
    def combineWith(other: Hashable): Hashable = {
      val h = HPair(this,other) ; count(h) ; h
    }
    def height: Int
    def size: Int
  }

  trait Hash {
    def head: Int
    def bit(i: Int): Byte
  }

  case class RLE(h: Hashable, l: Int) extends Hashable {
    def size = h.size * l
    def hash = HPair(h,IntHashable(l)).hash
    def height = h.height + 1
    override def toString = l + ":" + h
  }

  final val Unknown: Byte = 0
  final val LeftMerge: Byte = 1
  final val RightMerge: Byte = 2
  final val LeftFringe: Byte = 3
  final val RightFringe: Byte = 4

  def multipleOf(e1: Hashable, e2: Hashable): Boolean = {
    (e1,e2) match {
      case (RLE(h1,_),RLE(h2,_)) => h1 == h2
      case (h1,RLE(h2,_)) => h1 == h2
      case (RLE(h1,_),h2) => h1 == h2
      case _ => e1 == e2
    }
  }

  def size(e1: Hashable): Int = e1 match {
    case RLE(_,l) => l
    case _ => 1
  }

  def get(e1: Hashable): Hashable = e1 match {
    case RLE(h,_) => h
    case _ => e1
  }

  def compress(elems: Seq[Hashable]): Seq[Hashable] = {
    var j = 0
    var N1 = elems.length - 1
    var relems: Seq[Hashable] = Array[Hashable]()

    while (j < N1) {
      if (multipleOf(elems(j),elems(j+1))) {
        var k = j+1
        var tot = size(elems(j))
        var elem = elems(j)

        while ((k <= N1) && multipleOf(elems(k),elem)) {
          tot = tot + size(elems(k))
          k = k + 1
        }
        relems = relems :+ RLE(get(elem),tot)
        j = k-1
      }
      else { relems = relems :+ elems(j) }

      j = j + 1
    }
    if (j <= N1) { relems = relems :+ elems(j ) }

    relems
  }

  case class Round(leftFringe: Seq[Hashable], center: Seq[Hashable], rightFringe: Seq[Hashable]) {
    override def toString ={
      var s = "\n"
      s = s + "lfringe: " + leftFringe + "\n"
      s = s + "center : " + center + "\n"
      s = s + "rfringe: " + rightFringe
      s
    }
  }

  def doRound2(elems: Seq[Hashable], volatileLeft: Boolean, volatileRight: Boolean): Round ={
    doRound(compress(elems),volatileLeft,volatileRight)
  }

  def doRound(elems: Seq[Hashable], volatileLeft: Boolean, volatileRight: Boolean): Round = {
    val N = elems.length

    val kind: Array[Byte] = elems.map(e => Unknown).toArray
    val hashes: Array[Hash] = elems.map(e => e.hash).toArray
    var done = false

    var idx = 0

    val N1 = N - 1
    var left = 0
    var right = N1

    if (volatileLeft) {
      if (N > 0) {
        kind(0) = LeftFringe
        left = left + 1
      }
    }
    if (volatileRight) {
      if (N > 1) {
        kind(N1) = RightFringe
        right = right - 1
      }
    }

    while (!done) {
      done = true

      if (volatileLeft) {
        if ((left < N) && (kind(left) == Unknown) && (hashes(left).bit(idx) == 0)) {
          kind(left) = LeftFringe ; left = left + 1
        }
        if ((left < N) && (kind(left) == Unknown)) { done = false }
      }

      if (volatileRight) {
        if ((right >= 0) && (kind(right) == Unknown) && (hashes(right).bit(idx) == 1)) {
          kind(right) = RightFringe ; right = right - 1
        }
        if ((right >= 0) && (kind(right) == Unknown)) { done = false }
      }

      var j = 0

      while (j < N1) {
        if ((kind(j) == Unknown) && (kind(j + 1) == Unknown)) {
          if ((hashes(j).bit(idx) == 1) && (hashes(j + 1).bit(idx) == 0)) { kind(j) = LeftMerge ; kind(j+1) = RightMerge ; j = j + 1 }
          else { done = false }
        }
        j = j + 1
      }
      idx = idx + 1
    }

    var i = 0
    var center: Array[Hashable] = Array()
    var leftFringe: Array[Hashable] = Array()
    var rightFringe: Array[Hashable] = Array()

    while (i < N) {
      kind(i) match {
        case Unknown => {
          center = center :+ elems(i)
        }
        case LeftMerge => {
          center = center :+ (elems(i).combineWith(elems(i + 1))); i = i + 1
        }
        case LeftFringe => {
          leftFringe = leftFringe :+ elems(i)
        }
        case RightFringe => {
          rightFringe = rightFringe :+ elems(i)
        }
      }
      i = i + 1
    }

    Round(leftFringe.toSeq,center.toSeq,rightFringe.toSeq)
  }

  val empty: SeqHash = SeqHash(0,Array[Seq[Hashable]]().toSeq,Array[Hashable]().toSeq,Array[Seq[Hashable]]().toSeq)
  def create(h: Hashable): SeqHash = SeqHash(0,Array[Seq[Hashable]]().toSeq,Array[Hashable](h).toSeq,Array[Seq[Hashable]]().toSeq)


  case class SeqHash(height: Int, leftFringes: Seq[Seq[Hashable]], top: Seq[Hashable], rightFringes: Seq[Seq[Hashable]]){
    def isEmpty = (top.length == 0) && (height == 0)
    override def toString ={
      var s = "h: " + height + "\n"
      s = s + "l: " + leftFringes + "\n"
      s = s + "t: " + top.toSeq + "\n"
      s = s + "r: " + rightFringes.reverse
      s
    }
  }

  def merge(left: SeqHash, right: SeqHash): SeqHash = {
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else {
      var top: Seq[Hashable] = Array[Hashable]().toSeq
      var leftFringes: Seq[Seq[Hashable]] = Array[Seq[Hashable]]().toSeq
      var rightFringes: Seq[Seq[Hashable]] = Array[Seq[Hashable]]().toSeq
      var elems: Seq[Hashable] = Array[Hashable]().toSeq

      var height = 0
      var done = false

      while (!done) {

        if (height < left.height) { elems = left.rightFringes(height) ++ elems }
        else if (height == left.height) { elems = left.top ++ elems }

        if (height < right.height) { elems = elems ++ right.leftFringes(height) }
        else if (height == right.height) { elems = elems ++ right.top }

        if ((height >= left.height) && (height >= right.height) && (elems.length == 0)) { done = true }
        else {
          val round = doRound2(elems,height >= left.height,height >= right.height)
          elems = round.center

          if (height < left.height) { leftFringes = leftFringes :+ left.leftFringes(height) }
          else { leftFringes = leftFringes :+ round.leftFringe }

          if (height < right.height) { rightFringes = rightFringes :+ right.rightFringes(height) }
          else { rightFringes = rightFringes :+ round.rightFringe }

          height = height + 1
        }
      }

      height = height - 1
      top = leftFringes(height) ++ rightFringes(height)
      leftFringes = leftFringes.splitAt(height)._1
      rightFringes = rightFringes.splitAt(height)._1

      SeqHash(height,leftFringes,top,rightFringes)
    }
  }

}


// Every second node
// A_A_A_
// 01010

