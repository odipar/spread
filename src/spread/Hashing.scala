package spread

object Hashing {

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
  final def jh(k: Int): Int = jenkinsHash(k)

  trait PriorityHasher[X] {
    def hash(x: X): Int
  }

  object IntPriorityHasher extends PriorityHasher[Int] {
    def hash(x: Int) = jenkinsHash(x)
  }

  implicit val intHasher = IntPriorityHasher

  // An IDStream should be consistent and unique, given a value object (which can be self)

  trait IDStream {
    def current: Int = Int.MinValue
    def next: IDStream = NullID
  }

  object NullID extends IDStream

  case class IntID(i: Int) extends IDStream {
    override def current = i
  }

  case class LongID1(l: Long) extends IDStream {
    override def current = l.toInt
    override def next = LongID2(l)
  }

  case class LongID2(l: Long) extends IDStream {
    override def current = (l >>> 32).toInt
    override def next = NullID
  }

  def string_value(s: String, i: Int) : Int = {
    if (i >= s.length) Int.MinValue
    else s(i)
  }

  case class StringID(s: String) extends IDStream {
    override def current = {
      var s1 = string_value(s,0)
      var s2 = string_value(s,1)

      (s2 << 16) + s1
    }
    override def next = {
      if (s.length >= 2) StringIDNext(2,s)
      else NullID
    }
  }

  case class StringIDNext(val pos: Int, s: String) extends IDStream {
    override def current = {
      var s1 = string_value(s,pos)
      var s2 = string_value(s,pos+1)

      (s2 << 16) + s1
    }
    override def next = {
      if (s.length >= pos) StringIDNext(pos+2,s)
      else NullID
    }
  }

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

    ((r >> 32) ^ (r & 0xffffffff)).toInt
  }
}