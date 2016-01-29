package spread

object Hashing {

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

    val m = rotl(x1,32) + x2   // combine the input ints into one long

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

    (rotl(r,32) ^ r).toInt  // munch the long into an int
  }
}