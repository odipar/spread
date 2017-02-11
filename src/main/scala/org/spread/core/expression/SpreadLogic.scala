package org.spread.core.expression

import org.spread.core.expression.Spread._
import org.spread.core.splithash.Hashing._

import scala.language.implicitConversions
import scala.reflect.runtime.universe.TypeTag

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

    def !&&(o: _Boolean): _Boolean = %(and,unwrap,o)
    def !||(o: _Boolean): _Boolean = %(or,unwrap,o)
    def !^^(o: _Boolean): _Boolean = %(xor,unwrap,o)

    def !?[X,Y](e1: Expr[X], e2: Expr[X]): Expr[X] = %(then3[X],unwrap,Either(e1,e2))
    // TODO: MORE primitives
  }
  def !!(o: _Boolean): _Boolean = %(not,o)

  trait BinBoolOp extends FA2[Boolean,Boolean,Boolean] with InfixOperator

  case class Either[X](left: Expr[X], right: Expr[X]) extends F0[X] with HashedExpr[X] {
    val lazyHash = siphash24(left.hashCode * magic_p2 - magic_p3,right.hashCode * magic_p2 + magic_p1)
    def value = sys.error("There is no choice made")
    def hashAt(i: Int) = {
      if (i == 0) hashCode
      else if (i == 1)  left.hashCode ^ right.hashCode + hashCode
      else siphash24(right.hash.hashAt(i-1) * magic_p2 - hashCode,right.hash.hashAt(i-1) - magic_p2 + hashCode)
    }
    def parts = Array(left,right)
    override def _depth = (left.depth max right.depth) + 1
    override def _containsQuote = left.containsQuote || left.containsQuote
    override def _containsVariable = left.containsVariable || right.containsVariable
    def _unquote = {
      if (containsQuote) Either(left._unquote,right._unquote)
      else this
    }
    def _bindVariable[Y : TypeTag](s: Symbol, x: Expr[Y]) = {
      if (containsVariable) Either(left._bindVariable(s,x),right._bindVariable(s,x))
      else this
    }
  }

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
    def _unquote = this
    def _bindVariable[Y : TypeTag](s: Symbol, x: Expr[Y]) = this
    override def toString = value.toString
  }

  object True extends BExpr {
    def asInt = magic_p1
    def value = true
    override def toString = "true"
  }

  object False extends BExpr {
    def asInt = magic_p3
    def value = false
    override def toString = "false"
  }

  def bexpr(b: Boolean): BExpr = {
    if (b) True
    else False
  }

  trait and2 extends BinBoolOp {
    def apply2(o1: $Boolean, o2: $Boolean) = bexpr(o1.value & o2.value)
    override def toString = "!&&"
  }

  trait or2 extends BinBoolOp {
    def apply2(o1: $Boolean, o2: $Boolean) = bexpr(o1.value | o2.value)
    override def toString = "!||"
  }

  trait xor2 extends BinBoolOp {
    def apply2(o1: $Boolean, o2: $Boolean) = bexpr(o1.value ^ o2.value)
    override def toString = "!^^"
  }

  trait not2 extends FA1[Boolean,Boolean] {
    def apply2(o1: $Boolean) = bexpr(!o1.value)
    override def toString = "!!"
  }

  trait then2[X] extends FA2[Boolean,X,X] {
    def apply2(c: $Boolean, e: F0[X]): Expr[X] = e match {
      case Either(e1,e2) => {
        if (c.value) e1
        else e2
      }
      case _ => e
    }
    override def toString = "!?"
  }

  object and extends and2
  object or extends or2
  object xor extends xor2
  object not extends not2
  object then22 extends then2[Nothing]

  def then3[X] = then22.asInstanceOf[then2[X]]

  case class BWrap(unwrap: _Boolean) extends BooleanExpr {
    def error = sys.error("Wrapper object. Should not be called.")
    def lazyHash = error
    def hashAt(i: Int) = error
    def parts = error
    def _unquote = error
    def _bindVariable[Y : TypeTag](s: Symbol, x: Expr[Y]) = error
  }

  def wrap(i: _Boolean): BooleanExpr = i match {
    case w: BWrap => w
    case _ => BWrap(i)
  }

  case class BVarExpr(s: Symbol)(implicit t2: TypeTag[Boolean]) extends BooleanExpr with Variable[Boolean] {
    def t = t2
    def unwrap = this
    def _unquote = this
  }

  // Automatic conversion to bootstrap the DSL
  implicit def toBVarExpr(s: Symbol)(implicit t2: TypeTag[Boolean]): BooleanExpr = BVarExpr(s)
  implicit def toBooleanExpr(b: Boolean): BooleanExpr = bexpr(b)
  implicit def toBooleanExpr2(b: _Boolean): BooleanExpr = wrap(b)
}
