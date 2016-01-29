package spread

import spread.Spread._
import Hashing._

//
// Integer expressions + evaluation
//
// Copyright 2016: Robbert van Dalen
//

object SpreadArithmetic {

  type I = Expr[Int]
  type FI0 = F0[Int]
  
  trait IntExpr extends I {
    def unwrap: I
    def +(o: I): I = F2(add,unwrap,o)
    def *(o: I): I = F2(mul,unwrap,o)
    def /(o: I): I = F2(div,unwrap,o)
  }

  case class IExpr(i: Int) extends FI0 with IntExpr {
    def unwrap = this
    def value = i
    def lazyHash = siphash24(value - magic_p1,value + magic_p2)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(value + magic_p3, hashCode * magic_p2)
      else siphash24(hashCode * magic_p2, hashAt(index-1) - magic_p1)
    }
    override def toString = i.toString
  }

  trait BinOp extends FA2[Int,Int,Int]

  trait add2 extends BinOp {
    def apply(o1: FI0, o2: FI0) = IExpr(o1.value + o2.value)
    override def toString = "+"
    def codeHash = 0
  }
  trait mul2 extends BinOp {
    def apply(o1: FI0, o2: FI0) = IExpr(o1.value * o2.value)
    override def toString = "*"
    def codeHash = 1
  }
  trait div2 extends BinOp {
    def apply(o1: FI0, o2: FI0) = IExpr(o1.value / o2.value)
    override def toString = "/"
    def codeHash = 2
  }

  object add extends add2
  object mul extends mul2
  object div extends div2

  case class IWrap(unwrap: I) extends IntExpr {
    def error = sys.error("Wrapper object. Should not be called.")
    def eval = unwrap
    def lazyHash = error
    def hashAt(i: Int) = error
  }

  def wrap(i: I): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: I): IntExpr = wrap(i)

}
