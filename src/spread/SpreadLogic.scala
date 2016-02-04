package spread

import Spread._
import Hashing._
import scala.language.implicitConversions

//
// Boolean expressions + evaluation + DSL
//
// Copyright 2016: Robbert van Dalen
//

object SpreadLogic{
  type _Boolean = Expr[Boolean]
  type $Boolean = F0[Boolean]

  trait BooleanExpr extends _Boolean {
    def unwrap: _Boolean

    def !&&(o: _Boolean): _Boolean = F2(and,unwrap,o)
    def !||(o: _Boolean): _Boolean = F2(or,unwrap,o)
    def !^^(o: _Boolean): _Boolean = F2(xor,unwrap,o)

    // TODO: MORE primitives
  }

  trait BinBoolOp extends FA2[Boolean,Boolean,Boolean] with InfixOperator

  trait BExpr extends $Boolean with BooleanExpr {
    def asInt: Int
    def unwrap = this
    def lazyHash = siphash24(asInt * magic_p2,asInt + magic_p2)
    def hashAt(index: Int) = {
      if (index == 0) hashCode
      else if (index == 1) siphash24(asInt * magic_p3, hashCode - magic_p2)
      else siphash24(hashCode - magic_p2, hashAt(index-1) + magic_p1)
    }
    def parts = Array()
    override def toString = value.toString
  }

  object True extends BExpr {
    def asInt = magic_p1
    def value = true
    override def toString = "true"
  }

  object False extends BExpr {
    def asInt = magic_p2
    def value = false
    override def toString = "false"
  }

  def bexpr(b: Boolean): BExpr = {
    if (b) True
    else False
  }

  trait and2 extends BinBoolOp {
    def apply(o1: $Boolean, o2: $Boolean) = bexpr(o1.value & o2.value)
    override def toString = "!&&"
  }

  trait or2 extends BinBoolOp {
    def apply(o1: $Boolean, o2: $Boolean) = bexpr(o1.value | o2.value)
    override def toString = "!||"
  }

  trait xor2 extends BinBoolOp {
    def apply(o1: $Boolean, o2: $Boolean) = bexpr(o1.value ^ o2.value)
    override def toString = "!^^"
  }

  object and extends and2
  object or extends or2
  object xor extends xor2

  case class BWrap(unwrap: _Boolean) extends BooleanExpr {
    def error = sys.error("Wrapper object. Should not be called.")
    def lazyHash = error
    def hashAt(i: Int) = error
    def parts = error
  }

  def wrap(i: _Boolean): BooleanExpr = i match {
    case w: BWrap => w
    case _ => BWrap(i)
  }

  // Automatic conversion to bootstrap the DSL
  implicit def toBooleanExpr(b: Boolean): BooleanExpr = bexpr(b)
  implicit def toBooleanExpr2(b: _Boolean): BooleanExpr = wrap(b)
}
