package org.spread.core.splithash

object Hashing {

  // A Hashable object
  trait Hashable {
    def hash: Hash
    def hashParts: Array[Hashable]
  }

  // An 'infinitely' indexable and expandable Hash that *must* obey the following property:
  // The chance that two (slightly) different objects have equal hashes at index i
  // *must* exponentially decrease at higher indices.
  //
  // Hashes that don't exhibit this property may cause SplitHash to get stuck in an infinite loop.

  trait Hash {
    def hashAt(i: Int): Int
  }

  trait FiniteHash extends Hash {
    def size: Int
    def error = sys.error("hash out of bounds")
  }

  case class IntHash(h: Int) extends FiniteHash {
    override def hashCode = h
    def size = 1
    def hashAt(i: Int) = {
      if (i == 0) h
      else error
    }
  }

  case class LongHash(h1: Int, h2: Int) extends FiniteHash {
    override def hashCode = h1
    def size = 2
    def hashAt(i: Int) = {
      if (i == 0) h1
      else if (i == 1) h2
      else error
    }
  }

  case class ArrayHash(a: Array[Int]) extends FiniteHash {
    def size = a.size
    override def hashCode = a(0)
    def hashAt(i: Int) = {
      if (i < a.size) a(i)
      else error
    }
  }

  // Magic relative primes
  final val magic_p1 = 1664525
  final val magic_p2 = 22695477
  final val magic_p3 = 1103515245

  final def rotl(x: Long, b: Int): Long = (x << b) | (x >>> -b)

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

    val m = rotl(x1, 32) + x2 // combine the input ints into one long

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

    (rotl(r, 32) ^ r).toInt // munch the long into an int
  }
}