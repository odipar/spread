package spread

// Numerator/Denominator or Rationals (Nat\Nat)/(Nat\Nat) - NO ZEROs!

object Rational {
  import Natural._
  import Integer._

   def rOne[O]: RatPImpl[O,NatImpl[O],IntPImpl[O,NatImpl[O]]] = create(iOne,iOne)

  def create[O,N <: Nat[O,N],I <: IntP[O,N,I]](n: I, d: I): RatPImpl[O,N,I] = IPair(n,d)

  trait RatP[O,N <: Nat[O,N],I <: IntP[O,N,I], Q <: RatP[O,N,I,Q]] extends IntP[O,N,Q] {

    def numerator: I
    def denominator: I

    def inverse: Q

    def unary_~ = inverse
  }

  trait RatPImpl[O,N <: Nat[O,N], I <: IntP[O,N,I]] extends RatP[O,N,I,RatPImpl[O,N,I]] {
    def self = this
    type Q = RatPImpl[O,N,I]

    def credit = sys.error("not yet")
    def debit = sys.error("not yet")

    def half = {
      // TODO: fishy
      create(numerator,denominator + denominator)
    }
    def div(o: Q) = (this * o.inverse).simplify

    override def gcd(o: Q) = {
      val gcd = (numerator * o.denominator).gcd(denominator*o.numerator).simplify
      val bd =  (denominator * o.denominator).simplify
      create(gcd,bd)
    }
    def add(o: Q) = create((numerator * o.denominator) + (denominator * o.numerator),denominator * o.denominator)
    def mul(o: Q) = create(numerator * o.numerator, denominator * o.denominator)
    def compare(o: Q) = {
      (numerator * o.denominator).compare(denominator * o.numerator)
    }
    def negate = create(numerator.negate,denominator)
    def inverse = create(denominator,numerator)
    def simplify = {
      val n = numerator.simplify
      val d = denominator.simplify
      val gcd = n.gcd(d).simplify
      create(n.div(gcd),d.div(gcd))
    }
    def simplify2: Q  = {
      val n = numerator.simplify
      val d = denominator.simplify
      create(n,d)
    }
    def diff1(o: Q) = {
      val ss = self.simplify2
      val so = o.simplify2
      val d = ss.denominator.simplify
      val one1: Q = create(d,d)
      val c = ss.compare(so)
      if (c == 0) one1
      else if (c > 0) {
        ((ss + (-so)) + one).simplify2
      }
      else {
        ((so + (-ss)) + one).simplify2
      }
    }

    override def toString = numerator+"/"+denominator
  }

  case class IPair[O,N <: Nat[O,N], I <: IntP[O,N,I]](numerator: I, denominator: I) extends RatPImpl[O,N,I]
}