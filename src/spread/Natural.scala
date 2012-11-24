package spread

import java.math.BigInteger

// Naturals (Nat) 1,2,3,4,5... - NO ZEROs!

object Natural {
  private val ONE = BigInteger.ONE

  def nOne[O]: NatImpl[O] = One()

  def create[O](n: BigInteger): NatImpl[O] = if (n.equals(One().size)) One() ; else BiggerThanOne(n)

  trait Nat[O,N <: Nat[O,N]] {
    def self: N

    def simplify: N
    def half: N
    def div(o: N): N            // gcd div
    def add(o: N): N
    def mul(o: N): N
    def diff1(o: N): N          // absolute difference + 1
    def compare(o: N): Int

    def +(o: N) = add(o)
    def *(o: N) = mul(o)
    def >(o: N) = compare(o) > 0
    def <(o: N) = compare(o) > 0
    def ==(o: N) = compare(o) == 0
    def >=(o: N) = compare(o) >= 0
    def <=(o: N) = compare(o) <= 0

    def one = diff1(self)
    def isOne = self == one
    def even = self == (half + half)
    def inc = self + one

    def gcd(o: N): N = {
      if (self == o) self
      else if (self.isOne) self
      else if (o.isOne) o
      else {
        if (self.even) {
          if (!o.even) self.half.gcd(o).simplify
          else { val oo = self.half.gcd(o.half).simplify ; (oo+oo).simplify }
        }
        else if (o.even) self.gcd(o.half).simplify
        else {
          val diff = one.diff1(self.diff1(o)).simplify  // diff(u,v) always greater than 0
          val diff2 = diff.half.simplify
          if (self > o) diff2.gcd(o).simplify
          else diff2.gcd(self).simplify
        }
      }
    }
  }

  trait NatImpl[O] extends Nat[O,NatImpl[O]] {
    def self = this
    def size: BigInteger

    type NI  = NatImpl[O]

    def simplify = self
    def half: NI = if (size.and(ONE).compareTo(ONE) != 0) create(size.shiftRight(1)) ; else create(size.shiftRight(1).add(ONE))
    def div(o: NI) = create(size.divide(size.gcd(o.size)))
    def add(o: NI) = create(size.add(o.size))
    def mul(o: NI) = create(size.multiply(o.size))
    def diff1(o: NI) = create(size.subtract(o.size).abs.add(ONE))
    def compare(o: NI) = size.compareTo(o.size)

    override def toString = size.toString
  }

  case class One[O]() extends NatImpl[O] {
    val size = ONE
  }

  case class BiggerThanOne[O](size: BigInteger) extends NatImpl[O]
}
