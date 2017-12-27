package org.spread.core.experiment.expression

import java.lang.ref.WeakReference

import org.spread.core.splithash.Hashing.siphash24

import scala.collection.mutable

object Spread {

  import scala.language.implicitConversions

  trait Expr[+V] {
    def eval(c: Context): Expr[V] = c.eval(this)
    def evalImpl(c: Context): Expr[V] = this

    def rest: HashList[Expr[_]] = defaultRest
    def head: Expr[V] = this
  }

  trait Context {
    def eval[V](e: Expr[V]): Expr[V] = e.evalImpl(this)
  }

  trait Operator { override def toString: String  = this.getClass().getSimpleName().replace("$", "")}
  trait InfixOperator extends Operator
  trait PostfixOperator extends Operator

  def %[A, X](f: FA1[A, X], a: Expr[A]): Expr[X] = F1(f, a)
  def %[A, B, X](f: FA2[A, B, X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f, a, b)
  def %[A, B, C, X](f: FA3[A, B, C, X], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = F3(f, a, b, c)

  trait HashList[E] {
    def head: E
    def tail: HashList[E]

    def append(e: E): HashList[E] = Cons(e, this)
    def toList: List[E]
  }

  case class Empty[E]() extends HashList[E] {
    def error: Nothing= sys.error("empty list")
    def head: E = error
    def tail: HashList[E] = error
    def toList: List[E] = List()
  }

  case class Cons[E](head: E, tail: HashList[E]) extends HashList[E] {
    def toList: List[E] = head +: tail.toList
    override val hashCode: Int = siphash24(head.hashCode, tail.hashCode)
  }

  val defaultRest: HashList[Expr[_]] = Empty()

  case class Trace[V](override val head: Expr[V], override val rest: HashList[Expr[_]]) extends Expr[V] {
    override def evalImpl(c: Context): Expr[V] = {
      val e2 = head.eval(c)
      if (e2 != head) combine(e2, this)
      else this
    }
    override def toString: String = {
      "[" + rest.toList.map(_.toString).reduce((x, y) => x + " => " + y) + " => " + head + "]"
    }
    override val hashCode: Int = siphash24(head.hashCode, rest.hashCode)
  }

  def combine[V](e1: Expr[V], e2: Expr[V]): Trace[V] = Trace(e1.head, e1.rest append e2)

  case class Fail[E, F](f: F) extends Expr[E]

  trait FA0[V] extends Expr[V] {
    def value: V
    def unary_! : V = value
    override def evalImpl(c: Context): Expr[V] = this
  }


  case class AnExpr[V](value: V) extends FA0[V] {
    override def toString: String = value.toString
  }

  /*trait FA1[A, X] extends (Expr[A] => Expr[X]) with Operator {
    def apply(a: Expr[A]): Expr[X] = a match {
      case (a: FA0[A]) => apply2(a)
      case (a: Fail[A, _]) => Fail(a.f)
      case _ => F1(this, a)
    }
    def apply2(a: FA0[A]): Expr[X]
  } */

  trait FA1[A, X] extends (A => Expr[X]) with Operator {
    def apply(a: Expr[A]): Expr[X] = apply2(a)
    def apply2(a: Expr[A]): Expr[X] = a match {
      case (a: FA0[A]) => apply(a.value)
      case (a: Fail[A, _]) => Fail(a.f)
      case _ => F1(this, a)
    }
  }

  trait FA2[A, B, X] extends ((A, B) => Expr[X]) with Operator {
    def apply(a: Expr[A], b: Expr[B]): Expr[X] = apply2(a,b)
    def apply2(a: Expr[A], b: Expr[B]): Expr[X] = (a, b) match {
      case (a: FA0[A], b: FA0[B]) => apply(a.value, b.value)
      case (a: Fail[A, _], _) => Fail(a.f)
      case (_, b: Fail[B, _]) => Fail(b.f)
      case _ => F2(this, a, b)
    }
  }

  trait FA3[A, B, C, X] extends ((A,B,C) => Expr[X]) with Operator {
    def apply(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = apply2(a, b, c)
    def apply2(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = (a, b, c) match {
      case (a: FA0[A], b: FA0[B], c: FA0[C]) => apply(a.value, b.value, c.value)
      case (a: Fail[A, _], _, _) => Fail(a.f)
      case (_, b: Fail[B, _], _) => Fail(b.f)
      case (_, _, c: Fail[C, _]) => Fail(c.f)
      case _ => F3(this, a, b, c)
    }
  }

  trait ExprImpl[V] extends Expr[V] with Product {
    def eval2(e: Expr[V], c: Context): Expr[V] = {
      val ee = e.eval(c)
      combine(ee, this)
    }
  }

  case class F1[A, X](f: FA1[A, X], v1: Expr[A]) extends ExprImpl[X] {
    override def evalImpl(c: Context): Expr[X] = {
      val e1 = v1.head.eval(c)
      val ff = F1(f, e1)
      if (ff != this) eval2(ff, c)
      else {
        val ff = f(e1)
        if (ff != this) eval2(ff, c)
        else this
      }
    }
    override val hashCode: Int = siphash24(f.hashCode, v1.hashCode)

    override def toString: String = {
      if (f.isInstanceOf[PostfixOperator]) v1 + "." + f
      else f + "(" + v1 + ")"
    }
  }

  case class F2[A, B, X](f: FA2[A, B, X], v1: Expr[A], v2: Expr[B]) extends ExprImpl[X] {
    override def evalImpl(c: Context): Expr[X] = {
      val e1 = v1.head.eval(c)
      val e2 = v2.head.eval(c)
      val ff = F2(f, e1, e2)
      if (ff != this) eval2(ff, c)
      else {
        val ff = f(e1, e2)
        if (ff != this) eval2(ff, c)
        else this
      }
    }
    override val hashCode: Int = siphash24(siphash24(f.hashCode, v1.hashCode), v2.hashCode)
    override def toString: String = f match {
      case i: InfixOperator => "(" + v1 + " " + f + " " + v2 + ")"
      case _ => f + "(" + v1 + "," + v2 + ")"
    }
  }

  case class F3[A, B, C, X](f: FA3[A, B, C, X], v1: Expr[A], v2: Expr[B], v3: Expr[C]) extends ExprImpl[X] {
    override def evalImpl(c: Context): Expr[X] = {
      val e1 = v1.head.eval(c)
      val e2 = v2.head.eval(c)
      val e3 = v3.head.eval(c)
      val ff = F3(f, e1, e2, e3)
      if (ff != this) eval2(ff, c)
      else {
        val ff = f(e1, e2, e3)
        if (ff != this) eval2(ff, c)
        else this
      }
    }
    override val hashCode: Int = siphash24(siphash24(siphash24(f.hashCode, v1.hashCode), v2.hashCode), v3.hashCode)
    override def toString: String = f + "(" + v1 + "," + v2 + "," + v3 + ")"
  }

  object NullContext extends Context

  class StrongMemoizationContext(var m: mutable.HashMap[Expr[_], Expr[_]]) extends Context {
    override def eval[V](e: Expr[V]): Expr[V] = {
      m.get(e) match {
        case None => {
          val ee = e.evalImpl(this)
          m += (e -> ee)
          ee
        }
        case Some(x) => x.asInstanceOf[Expr[V]]
      }
    }
  }

  class WeakMemoizationContext(var m: mutable.WeakHashMap[Expr[_], WeakReference[Expr[_]]]) extends Context {
    override def eval[V](e: Expr[V]): Expr[V] = {
      m.get(e) match {
        case None => {
          val ee = e.evalImpl(this)
          m += (e -> new WeakReference(ee))
          ee
        }
        case Some(x) => {
          val ref = x.get()

          if (ref == null) {
            m -= e
            eval(e)
          }
          else ref.asInstanceOf[Expr[V]]
        }
      }
    }
  }

  type _Int = Expr[Int]
  type $Int = FA0[Int]

  trait IntExpr extends _Int {
    def unwrap: _Int

    def !+(o: _Int): _Int = %(add, unwrap, o)
    def !-(o: _Int): _Int = %(sub, unwrap, o)
    def !*(o: _Int): _Int = %(mul, unwrap, o)
    def !/(o: _Int): _Int = %(div, unwrap, o)
  }

  case class IExpr(value: Int) extends $Int with IntExpr {
    def unwrap: _Int = this
    override def toString: String = value.toString
  }

  trait BinIntOp extends FA2[Int, Int, Int] with InfixOperator

  trait add2 extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 + o2)
    override def toString = "!+"
  }

  trait sub2 extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 - o2)
    override def toString = "!-"
  }

  trait mul2 extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 * o2)
    override def toString = "!*"
  }

  trait div2 extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 / o2)
    override def toString = "!/"
  }

  object add extends add2
  object sub extends sub2
  object mul extends mul2
  object div extends div2

  case class IWrap(unwrap: _Int) extends IntExpr

  def wrap(i: _Int): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: _Int): IntExpr = wrap(i)
  implicit def toExpr[X](x: X): Expr[X] = AnExpr[X](x)

  object fac extends FA1[Int, Int] {
    def apply(x: Int): _Int = {
      if (x <= 1) 1
      else %(fac, x - 1) !* x
    }
  }

  object fac2 extends FA1[Int, Int] {
    def apply(x: Int): _Int = {
      if (x <= 1) 1
      else %(fac, x - 1) !* x
    }
  }

  object fib extends FA1[Int, Int] {
    def apply(x: Int): _Int = {
      if (x <= 1) 1
      else %(fib, x - 1) !+ %(fib, x - 2)
    }
  }

  final def main(args: Array[String]): Unit = {
    val f = %(fib, 30)
    val c = new WeakMemoizationContext(mutable.WeakHashMap())

    var k = f.eval(c)

    System.gc()

    println("k: " + k.head)
    println("c: " + c.m.size)
  }
}

