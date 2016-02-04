package spread

//
// Integer expressions + evaluation + DSL
//
// Copyright 2016: Robbert van Dalen
//

import spread.Spread._
import Hashing._
import scala.language.implicitConversions

object SpreadArithmetic {

  type _Int = Expr[Int]
  type $Int = F0[Int]

  trait IntExpr extends _Int {
    def unwrap: _Int

    def !+(o: _Int): _Int = F2(add,unwrap,o)
    def !-(o: _Int): _Int = F2(sub,unwrap,o)
    def !*(o: _Int): _Int = F2(mul,unwrap,o)
    def !/(o: _Int): _Int = F2(div,unwrap,o)

    // TODO: MORE primitives
  }

  case class IExpr(value: Int) extends $Int with IntExpr {
    def unwrap = this
    def lazyHash = siphash24(value - magic_p1,value + magic_p2)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(value + magic_p3, hashCode * magic_p2)
      else siphash24(hashCode * magic_p2, hashAt(index-1) - magic_p1)
    }
    def parts = Array()
    override def toString = value.toString
  }

  trait BinIntOp extends FA2[Int,Int,Int] with InfixOperator

  trait add2 extends BinIntOp {
    def apply(o1: $Int, o2: $Int) = IExpr(o1.value + o2.value)
    override def toString = "!+"
  }

  trait sub2 extends BinIntOp {
    def apply(o1: $Int, o2: $Int) = IExpr(o1.value - o2.value)
    override def toString = "!-"
  }

  trait mul2 extends BinIntOp {
    def apply(o1: $Int, o2: $Int) = IExpr(o1.value * o2.value)
    override def toString = "!*"
  }

  trait div2 extends BinIntOp {
    def apply(o1: $Int, o2: $Int) = IExpr(o1.value / o2.value)
    override def toString = "!/"
  }

  object add extends add2
  object sub extends sub2
  object mul extends mul2
  object div extends div2

  case class IWrap(unwrap: _Int) extends IntExpr {
    def error = sys.error("Wrapper object. Should not be called.")
    def lazyHash = error
    def hashAt(i: Int) = error
    def parts = error
  }

  def wrap(i: _Int): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  // Automatic conversion to bootstrap the DSL
  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: _Int): IntExpr = wrap(i)
}
