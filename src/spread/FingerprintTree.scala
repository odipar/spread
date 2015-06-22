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
 * XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
 * are RLE compressed and recursively rechunked, to prevent excessive chunk sizes due to low entropy.
 * Details can be found in the below code.
 * 
 * Note another interesting property:
 * 
 * Small changes to the input X, will only create log(|X|) new Chunks. 
 * It is due to this property that de-duplication actually works in practice.
 * 
 * So in theory, it should be possible to create incremental immutable versions of the following algorithms:
 * - concatenate two Fingerprint Trees A and B, in O(B*log(max(|A|,|B|))
 * - cut a Fingerprint Tree X at index I, in O(B*log(|X|)
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
    var result: Vector[Char] = Vector()

 result = result ++ "The following code marks the birth of a novel data structure: The Fingerprint Tree. A Finger Tree is built by recursively applying a 'Chunking' algorithm, using a standard Rabin fingerprinting scheme. Note that 1- or 2-level Chunking can be found in many commercial de-duplication storage solutions. When N-level Chunking is applied, it results in a Tree structure, with following two amazing properties: - It is a B-Tree - it has a canonical representation In addition, repetitions like the following: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA are RLE compressed and recursively rechunked, to prevent excessive chunk sizes due to low entropy. Details can be found in the below code. Note another interesting property: Small changes to the input X, will only create log(|X|) new Chunks. It is due to this property that de-duplication actually works in practice. So in theory, it should be possible to create incremental immutable versions of the following algorithms: - concatenate two Fingerprint Trees A and B, in O(Blog(max(|A|,|B|)) - cut a Fingerprint Tree X at index I, in O(Blog(|X|)  .. with B the average Block size(=31). However, in practice, it might prove difficult to find implementations of these algorithms. Copyright 2015: Robbert van Dalen."

    val aa = chunk(result)
    val bb = chunk(result.updated(50,"%"))

    println("aa: " + aa)
    println("bb: " + bb)
  }

  /* Constants for Rabin fingerprints */
  val multiplier = 1664525

  /* The fingerprint window size */
  val wsize = 31

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
    while (r.size > wsize) { r = partition(r) }
    r
  }
  /*
   * Partition a Sequence of Objects into a Sequence of Chunks using Rabin fingerprinting (rolling hash)
   * Chunks that are too big (due to repetitions/low entropy) are compressed and again (recursively) partitioned.
   */
  def partition[X: ClassTag](a: Seq[X]): Seq[Chunk[_]] = {
    var r: List[Chunk[_]] = List()
    var size = a.length

    var searchPosition = 0
    var hash = 0
    var position = 0

    while (searchPosition < size) {
      val wi = searchPosition - position

      // Found the cut point of a Chunk
      if (((hash >> 9) & 15) == 7) {
        r = r :+ createChunk(copy(a,position,wi), hash, wsize)
        position = searchPosition
      }

      // compute the next hash of the advancing window
      hash = (hash + jh(a(searchPosition))) * multiplier
      if (searchPosition >= wsize) { hash = hash - (jh(a(searchPosition - wsize)) * mm) }

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
      val ks = partition(rs)         // and chunk the compressed chunk

      if ((ks.size) > ws * 2) sys.error("The chance of this happening should be very small")
      else Chunk(ks)
    }
  }

  case class Chunk[X : ClassTag](chunk: Seq[X]) {
    override def toString = {
      var r = "<" + chunk.size + ":"
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
   * Run-Length-Encoding (RLE) compression. This version also encodes repetitions of *multiple* elements.
   * RLE compression has the important property that local changes to the input result in local changes
   * to the encoded output.
   *
   * For example:
   *
   * AAAAXXXABCABCYPO  is encoded in {4:A}{3:X}{6:ABC}[3:YPO]
   *
   * and the slightly altered version:
   *
   * AABAAXXXABCABCYPO is encoded in [6:AABAAX]{2:X}{6:ABC}[3:YPO]
   *
   */

  def compress[X: ClassTag](data: Seq[X], windowLength: Int, minimumRunLength: Int): Seq[CompressedChunk[X]] = {
    // The compressed output is a 'Stack' of CompressedString[X]s with its head being the top of the Stack
    var output: List[CompressedChunk[X]] = List()
    var size = data.length
    var position = 0

    while (position < size) {
      var matchLength = 0
      var bestMatchLength = 0
      var bestDistance = 0
      var searchPosition = 0 max (position - windowLength)

      // find the best repeating match, given the window length
      while (searchPosition < position) {
        var positionMatch = position
        var searchMatch = searchPosition

        while ((positionMatch < size) && (data(searchMatch) == data(positionMatch))) {
          matchLength = matchLength + 1
          positionMatch = positionMatch + 1
          searchMatch = searchMatch + 1
        }

        // We have found a repeating pattern, iff
        //  - the searchMatch is greater or equal to the current position
        //  - the matchLength is greater than the best length found
        if ((searchMatch >= position) && (matchLength > bestMatchLength)) {
          bestMatchLength = matchLength
          bestDistance = position - searchPosition
        }
        searchPosition = searchPosition + 1
        matchLength = 0
      }

      if (bestMatchLength >= minimumRunLength) {
        // encode as a Repetition when we have reached the minimum repetition length
        output = append(output,Repetition(data.slice(position - bestDistance,position),bestMatchLength))
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
          case Repetition(seq,size) => {
            if (aa == seq) Repetition(seq,size + aa.size) +: a.tail  // merge a Repetition with a Block
            else i +: a
          }
        }
        case _ => i +: a
      }
    }
    else List(i)
  }
}
