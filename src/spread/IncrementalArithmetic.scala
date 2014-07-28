package spread

import spread.Hashing._

/*
  Copyright 2014: Robbert van Dalen
 */

object IncrementalArithmetic {
  import scala.language.implicitConversions
  import IncrementalMemoization._

  type I = Expr[Int]

  trait IExpr extends Expr[Int] {
    def origin: I
    def ++(o: I): I = %(add, origin, o)
    def --(o: I): I = %(sub, origin, o)
    def **(o: I): I = %(mul, origin, o)
  }

  type F0I = F0[Int]

  case class II(value: Int) extends IExpr with F0I {
    def contains[X](x: X) = BFalse
    def set[X,O](x: X, e: Expr[O]) = this
    def origin = this
    override def toString = "" + value
    override def hashCode = Hashing.jenkinsHash(value)
  }

  implicit def toI(i: Int) = II(i)

  private case class IWrap(origin: I) extends IExpr {
    def contains[X](x: X) = error
    def set[X,O](x: X, e: Expr[O]) = error
    def eval = error
    def unquote = error
    def error = sys.error("IWrap should not be used directly")
  }

  implicit def toIWrap(i: I): IExpr = i match {
    case ii: IExpr => ii
    case _ => IWrap(i)
  }

  object add extends FA2[Int,Int,Int] {
    def apply(a: F0I, b: F0I) = a.value + b.value
    override def toString = "++"
  }

  object sub extends FA2[Int,Int,Int] {
    def apply(a: F0I, b: F0I) = a.value - b.value
    override def toString = "--"
  }

  object mul extends FA2[Int,Int,Int] {
    def apply(a: F0I, b: F0I) = a.value * b.value
    override def toString = "**"
  }

  case class WI(i: Int) {
    def unary_! = II(i)
  }

  implicit def toWI(i: Int): WI = WI(i)
}
