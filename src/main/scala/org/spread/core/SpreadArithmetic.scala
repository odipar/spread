package org.spread.core

//
// Integer expressions + evaluation + DSL
//
// Copyright 2016: Robbert van Dalen
//

import org.spread.core.Hashing._
import org.spread.core.Spread._

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag

object SpreadArithmetic {

  type _Int = Expr[Int]
  type $Int = F0[Int]

  trait IntExpr extends _Int {
    def unwrap: _Int

    def !+(o: _Int): _Int = %(add,unwrap,o)
    def !-(o: _Int): _Int = %(sub,unwrap,o)
    def !*(o: _Int): _Int = %(mul,unwrap,o)
    def !/(o: _Int): _Int = %(div,unwrap,o)

    // TODO: MORE primitives
  }

  case class IVarExpr(s: Symbol)(implicit t2: TypeTag[Int]) extends IntExpr with Variable[Int] {
    def t = t2
    def unwrap = this
    def _unquote = this
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
    def _unquote = this
    def _bindVariable[Y: TypeTag](s: Symbol, x: Expr[Y]) = this
    override def toString = value.toString
  }

  trait BinIntOp extends FA2[Int,Int,Int] with InfixOperator

  trait add2 extends BinIntOp {
    def apply2(o1: $Int, o2: $Int) = IExpr(o1.value + o2.value)
    override def toString = "!+"
  }

  trait sub2 extends BinIntOp {
    def apply2(o1: $Int, o2: $Int) = IExpr(o1.value - o2.value)
    override def toString = "!-"
  }

  trait mul2 extends BinIntOp {
    def apply2(o1: $Int, o2: $Int) = IExpr(o1.value * o2.value)
    override def toString = "!*"
  }

  trait div2 extends BinIntOp {
    def apply2(o1: $Int, o2: $Int) = IExpr(o1.value / o2.value)
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
    def _unquote = error
    def _bindVariable[Y : TypeTag](s: Symbol, x: Expr[Y]) = error
  }

  def wrap(i: _Int): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  // Automatic conversion to bootstrap the DSL
  implicit def toVarExpr(s: Symbol)(implicit t2: TypeTag[Int]): IntExpr = IVarExpr(s)
  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: _Int): IntExpr = wrap(i)
}
