package spread

object Hashing {

  final def jenkinsHash(k: Int) = {
    var key = k
    key = ~key + (key << 15) // key = (key << 15) - key - 1;
    key = key ^ (key >>> 12)
    key = key + (key << 2)
    key = key ^ (key >>> 4)
    key = key * 2057 // key = (key + (key << 3)) + (key << 11);
    key = key ^ (key >>> 16)
    key
  }

  trait PriorityHasher[X] {
    def hash(x: X): Int
  }

  object IntPriorityHasher extends PriorityHasher[Int] {
    def hash(x: Int) = jenkinsHash(x)
  }

  implicit val intHasher = IntPriorityHasher
}
