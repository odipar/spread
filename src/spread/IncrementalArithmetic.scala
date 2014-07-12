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

  trait IExpr extends I {
    def origin: I
    def ++(o: I): I = mem(%(add, origin, o))
    def +++(o: I): I = mem(%(add2, origin, o))
    def --(o: I): I = mem(%(sub, origin, o))
    def ---(o: I): I = mem(%(sub2, origin, o))
    def **(o: I): I = mem(%(mul, origin, o))
    def ***(o: I): I = mem(%(mul, origin, o))
  }

  case class II(eval: Int) extends IExpr {
    def origin = this
    override def toString = "" + eval
    override def hashCode = Hashing.jenkinsHash(eval)
  }

  implicit def toI(i: Int) = II(i)

  private case class IWrap(origin: I) extends IExpr {
    def error = sys.error("IWrap should not be used directly")
    override def eval = error
    override def reduce = error
  }

  implicit def toIWrap(i: I): IExpr = i match {
    case ii: IExpr => ii
    case _ => IWrap(i)
  }

  val add = new ((I, I) => I) {
    def apply(a: I, b: I): I = a.eval + b.eval
    override def toString = "++"
  }

  val add2 = new ((I, I) => I) {
    def apply(a: I, b: I): I = %%(add, a, b)
    override def toString = "+++"
  }

  val sub = new ((I, I) => I) {
    def apply(a: I, b: I): I = a.eval - b.eval
    override def toString = "--"
  }

  val sub2 = new ((I, I) => I) {
    def apply(a: I, b: I): I = %%(sub, a, b)
    override def toString = "---"
  }

  val mul = new ((I, I) => I) {
    def apply(a: I, b: I): I = a.eval * b.eval
    override def toString = "**"
  }

  val mul2 = new ((I, I) => I) {
    def apply(a: I, b: I): I = %%(mul, a, b)
    override def toString = "***"
  }

  case class WI(i: Int) {
    def unary_! = II(i)
  }

  implicit def toWI(i: Int): WI = WI(i)
}
