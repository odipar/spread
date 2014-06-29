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
}
