package spread

import spread.Hashing._

/*
  Copyright 2014: Robbert van Dalen
 */

object IncrementalArithmetic {
  import scala.reflect.runtime.universe.TypeTag
  import scala.language.implicitConversions
  import IncrementalMemoization._

  type I = Expr[Int]

  trait IExpr extends Expr[Int] {
    def origin: I
    def +(o: I): I = add(origin, o)
    def ++(o: I): I = %(add, origin, o)
    def -(o: I): I =  sub(origin, o)
    def --(o: I): I = %(sub, origin, o)
    def *(o: I): I = mul(origin, o)
    def **(o: I): I = %(mul, origin, o)
  }

  type F0I = F0[Int]

  case class II(evalValue: Int) extends IExpr with F0I {
    def containsQuotes = false
    def unquote = this

    def contains[X](x: X) = BFalse
    def set[X,O: TypeTag](x: X, e: Expr[O]) = this
    def origin = this
    override def toString = "" + evalValue
    override def hashCode = Hashing.jenkinsHash(evalValue)
  }

  implicit def toI(i: Int) = II(i)

  private case class IWrap(origin: I) extends IExpr {
    def containsQuotes = error
    def unquote = error

    def contains[X](x: X) = error
    def set[X,O: TypeTag](x: X, e: Expr[O]) = error
    def eval = error
    def error = sys.error("IWrap should not be used directly")
  }

  implicit def toIWrap(i: I): IExpr = i match {
    case ii: IExpr => ii
    case _ => IWrap(i)
  }

  object add extends FA2[Int,Int,Int] with Infix {
    def apply(a: F0I, b: F0I) = a.evalValue + b.evalValue
    override def toString = "++"
  }

  object sub extends FA2[Int,Int,Int] with Infix  {
    def apply(a: F0I, b: F0I) = a.evalValue - b.evalValue
    override def toString = "--"
  }

  object mul extends FA2[Int,Int,Int] with Infix {
    def apply(a: F0I, b: F0I) = a.evalValue * b.evalValue
    override def toString = "**"
  }

  case class WI(i: Int) {
    def unary_~ = II(i)
  }

  implicit def toWI(i: Int): WI = WI(i)
}
