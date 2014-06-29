package spread

import spread.Hashing._

/*
  Copyright 2014: Robbert van Dalen
 */

object IncrementalArithmetic {
  import scala.language.implicitConversions
  import IncrementalMemoization._
  import java.math.BigInteger

  type I = Expr[Int]

  /* syntactic sugar */
  trait IExpr extends I {
    def origin: I
    def +(o: I): I = %(add,origin,o)
    def ++(o: I): I = %%(add,origin,o)
    def -(o: I): I = %(sub,origin,o)
    def --(o: I): I = %%(sub,origin,o)
    def *(o: I): I = %(mul,origin,o)
    def **(o: I): I = %%(mul,origin,o)
  }

  case class II(eval: Int) extends IExpr {
    def origin = this
    def stage = 0
    override def toString = "" + eval
    override def hashCode = jh(eval)
  }

  implicit def toI(i: Int): I = II(i)

  /* syntactic sugar */
  private case class IWrap(origin: I) extends IExpr {
    def error = sys.error("IWrap should not be used directly")
    override def stage = error
    override def eval = error
    override def reduce(s: Int, c: Context) = error
  }

  implicit def toIWrap(i: I): IExpr = i match {
    case ii: IExpr => ii
    case _ => IWrap(i)
  }

  type BI = ((I, I) => I)

  object add extends BI { def apply(a: I, b: I): I = a.eval + b.eval ; override def toString = "+" }
  object sub extends BI { def apply(a: I, b: I): I = a.eval - b.eval ; override def toString = "-" }
  object mul extends BI { def apply(a: I, b: I): I = a.eval * b.eval ; override def toString = "*" }
}
