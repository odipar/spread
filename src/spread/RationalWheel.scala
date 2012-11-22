package spread

import java.math.BigInteger

/* Wheels: On Division by Zero, Jesper Carlstrom
 see: http://www2.math.su.se/reports/2001/11/2001-11.ps
 */


object RationalWheel {

  type Q = RatW

  trait RatW {
    def numerator: BigInteger
    def denominator: BigInteger

    def negate: RatW
    def inverse: RatW
    def add(o: RatW): RatW
    def multiply(o: RatW): RatW

    def create(n: BigInteger, d: BigInteger): RatW

    def unary_-() = negate
    def unary_~() = inverse
    def +(o: RatW) = add(o)
    def *(o: RatW) = multiply(o)
    def /(o: RatW) = this * ~o
  }

  trait RatWImpl extends RatW {
    def negate = create(numerator.negate,denominator)
    def inverse = create(denominator,numerator)
    def add(o: RatW) = {
      val n = numerator.multiply(o.denominator).add(denominator.multiply(o.numerator))
      val d = denominator.multiply(o.denominator)
      create(n,d)
    }
    def multiply(o: RatW) = {
      val n = numerator.multiply(o.numerator)
      val d = denominator.multiply(o.denominator)
      create(n,d)
    }

    def create(n: BigInteger, d: BigInteger) = (n.signum,d.signum) match {
      case (0,0) => ZeroZeroW
      case (0,_) => ZeroW
      case (_,0) => NegInfW
      case (_,-1) => create(n.negate,d.negate)
      case (_,_) => {
        val gcd = n.gcd(d)
        val nn = n.divide(gcd)
        val dd = d.divide(gcd)
        if (nn.equals(dd)) OneW
        else if (dd.equals(BigInteger.ONE)) NatW(nn)
        else RatWI(nn,dd)
      }
    }

    override def toString = numerator+"/"+denominator
  }

  case object ZeroW extends RatWImpl {
    val numerator = BigInteger.ZERO
    val denominator = BigInteger.ONE
    override def toString = "0"
  }

  case object OneW extends RatWImpl {
    val numerator = BigInteger.ONE
    val denominator = BigInteger.ONE
    override def toString = "1"
  }

  case object InfW extends RatWImpl {
    val numerator = BigInteger.ONE
    val denominator = BigInteger.ZERO
    override def toString = "Infinity"
  }

  case object ZeroZeroW extends RatWImpl {
    val numerator = BigInteger.ZERO
    val denominator = BigInteger.ZERO
    override def toString = "Bottom"
  }

  case class NatW(numerator: BigInteger) extends RatWImpl {
    val denominator = BigInteger.ONE
    override def toString = ""+numerator
  }

  case class RatWI(numerator: BigInteger, denominator: BigInteger) extends RatWImpl

  implicit def longToRatW(i: Long): RatW = NatW(BigInteger.valueOf(i))
}
