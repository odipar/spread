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
}