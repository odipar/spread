package org.spread.core

//
// Bagwell's Ideal Hash Tree with 'infinite' hashes (nibble based)
//
// Copyright 2016: Robbert van Dalen
//

object SetHash {
  import Hashing._
  import Reference._

  trait SetHash[SH <: SetHash[SH]] extends Hashable {
    def put(e: Hashable): SH
    def hash(e: Hashable): FiniteHash   // Returns the minimum unique FiniteHash to get back to the original Hashable
    def get(h: FiniteHash): Hashable
    // TODO: remove
  }

  case class HashNode(a: Array[Hashable], bits: Int) extends SetHash[HashNode] with Hash {
    def parts = a.clone
    def put(o: Hashable): HashNode = put(o,0)
    def put(o: Hashable ,nibble: Int): HashNode = {
      // TODO: Correct Type magic should prevent this
      if (o.isInstanceOf[HashNode]) { sys.error("NOT ALLOWED") }

      val i = getIndex(o,nibble)
      val ai = getArrayIndex(i)

      get2(ai) match {
        case n: HashNode => {
          // deeper
          val na = parts
          na(ai) = n.put(o,nibble + 1)
          HashNode(na,bits)
        }
        case null => {
          // new
          val aai = Integer.bitCount(bits & ((2 << i) - 1))
          val sp = a.splitAt(aai)
          // TODO: Optimize
          HashNode(sp._1 ++ Array(o) ++ sp._2, bits | (1 << i))
        }
        case x => {
          if (o != x) {
            // collision
            val na = parts
            na(ai) = HashNode(Array(),0).put(o,nibble + 1).put(a(ai),nibble + 1)
            HashNode(na,bits)
          }
          else this // the same object has been re-stored (hash-consing)
        }
      }
    }
    def get(o: Hashable, nibble: Int): Hashable = get2(getArrayIndex(getIndex(o,nibble))) match {
      case n: HashNode => n.get(o,nibble + 1)
      case x => {
        if (x == o) x
        else null
      }
    }

    def get(id: FiniteHash): Hashable = get(id,0)
    def get(id: FiniteHash, nibble: Int): Hashable = get2(getArrayIndex(getIndex(id,nibble))) match {
      case n: HashNode => n.get(id,nibble + 1)
      case x => x
    }

    def hash(o: Hashable) = {
      val r: List[Int] = id(o,0)
      if (r == null) null
      else {
        val s = r.size
        if (s == 1) IntHash(r.head)
        else if (s == 2) LongHash(r.head,r.tail.head)
        else ArrayHash(r.toArray)
      }
    }

    def id(o: Hashable, nibble: Int): List[Int] = get2(getArrayIndex(getIndex(o,nibble))) match {
      case n: HashNode => {
        if ((nibble & 3) == 0) {
          val r = n.id(o,nibble+1)
          if (r != null) {
            o.hash.hashAt(nibble >> 2) +: r
          }
          else null
        }
        else n.id(o,nibble + 1)
      }
      case x => {
        if (x == o) {
          if ((nibble & 3) == 0) List(o.hash.hashAt(nibble >> 2))
          else List()
        }
        else null
      }
    }

    def getIndex(o: Hashable, nibble: Int): Int = (o.hash.hashAt(nibble >> 2) >> ((nibble & 3) * 4)) & 0xf
    def getIndex(o: FiniteHash, nibble: Int): Int = (o.hashAt(nibble >> 2) >> ((nibble & 3) * 4)) & 0xf
    def getArrayIndex(i: Int): Int ={
      val b = bits & ((2 << i) - 1)
      if (((bits >> i) & 1) == 0) -1
      else Integer.bitCount(b) - 1
    }
    def get2(i: Int): Hashable = {
      if (i < 0) null
      else a(i)
    }

    import Hashing._

    // 64 bit hashes
    var lHash1: Long = 0
    override def hashCode = longHashCode.toInt

    def hash = this
    def longHashCode: Long = {
      if (lHash1 == 0) {
        var i = 0
        var s = a.length
        var h1 = magic_p2
        var h2 = magic_p3

        while (i < s) {
          h1 = siphash24(h1,a(i).hashCode)
          h2 = siphash24(a(i).hash.hashAt(1),h2)
          i = i + 1
        }
        lHash1 = (rotl(h1.toLong,32)) ^ h2
      }
      lHash1
    }

    override def hashAt(i: Int) = {
      if (i == 0) hashCode
      else if (i == 1) rotl(longHashCode,32).toInt
      else sys.error("DON'T KNOW WHAT TO DO YET")
    }

    override def toString = (a.toSeq,bits).toString
  }
}
