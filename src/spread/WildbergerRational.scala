package spread

import java.math.BigInteger

object WildbergerRational {

  implicit def longToWRat(i: Long): WRat = WNat(BigInteger.valueOf(i))

  type R = WRat

  trait WRat {
    def numerator: BigInteger
    def denominator: BigInteger

    def negate: WRat
    def inverse: WRat
    def add(o: WRat): WRat
    def multiply(o: WRat): WRat

    def create(n: BigInteger, d: BigInteger): WRat

    def unary_-() = negate
    def unary_~() = inverse
    def +(o: WRat): WRat = add(o)
    def *(o: WRat): WRat = multiply(o)
    def /(o: WRat): WRat = this * ~o
  }

  trait WRatImpl extends WRat {
    def negate = create(numerator.negate,denominator)
    def inverse = create(denominator,numerator)
    def add(o: WRat) = {
      val n = numerator.multiply(o.denominator).add(denominator.multiply(o.numerator))
      val d = denominator.multiply(o.denominator)
      create(n,d)
    }
    def multiply(o: WRat) = {
      val n = numerator.multiply(o.numerator)
      val d = denominator.multiply(o.denominator)
      create(n,d)
    }

    def create(n: BigInteger, d: BigInteger) = (n.signum,d.signum) match {
      case (0,0) => WZeroZero
      case (0,_) => WZero
      case (-1,0) => WNegInf
      case (1,0) => WPosInf
      case (_,-1) => create(n.negate,d.negate)
      case (_,_) => {
        val gcd = n.gcd(d)
        val nn = n.divide(gcd)
        val dd = d.divide(gcd)
        if (nn.equals(dd)) WOne
        else if (dd.equals(BigInteger.ONE)) WNat(nn)
        else WRatI(nn,dd)
      }
    }

    override def toString = numerator+"/"+denominator
  }

  case object WZero extends WRatImpl {
    val numerator = BigInteger.ZERO
    val denominator = BigInteger.ONE
    override def toString = "0"
  }

  case object WOne extends WRatImpl {
    val numerator = BigInteger.ONE
    val denominator = BigInteger.ONE
    override def toString = ""+numerator
  }

  case object WPosInf extends WRatImpl {
    val numerator = BigInteger.ONE
    val denominator = BigInteger.ZERO
    override def toString = "Inf"
  }

  case object WNegInf extends WRatImpl {
    val numerator = BigInteger.ONE.negate
    val denominator = BigInteger.ZERO
    override def toString = "-Inf"
  }

  case object WZeroZero extends WRatImpl {
    val numerator = BigInteger.ZERO
    val denominator = BigInteger.ZERO
  }

  case class WNat(numerator: BigInteger) extends WRatImpl {
    val denominator = BigInteger.ONE
    override def toString = ""+numerator
  }

  case class WRatI(numerator: BigInteger, denominator: BigInteger) extends WRatImpl
}
