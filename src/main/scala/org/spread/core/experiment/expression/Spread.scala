package org.spread.core.experiment.expression

import org.spread.core.splithash.Hashing.siphash24

object Spread {

  import scala.language.implicitConversions

  trait Context {
    def eval[V](e: Expr[V]): (Expr[V],Context) = e.evalImpl(this)
  }

  trait Operator { override def toString = this.getClass().getSimpleName().replace("$","") }
  trait InfixOperator extends Operator
  trait PostfixOperator extends Operator

  def %[A,X](f: FA1[A,X], a: Expr[A]): Expr[X] = F1(f,a)
  def %[A,B,X](f: FA2[A,B,X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f,a,b)
  def %[A,B,C,X](f: FA3[A,B,C,X], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = F3(f,a,b,c)

  trait HashList[E] {
    def head: E
    def tail: HashList[E]

    def append(e: E): HashList[E] = Cons(e,this)
    def toList: List[E]
  }

  case class Empty[E]() extends HashList[E] {
    def error = sys.error("empty list")
    def head = error
    def tail = error
    def toList = List()
  }

  case class Cons[E](head: E, tail: HashList[E]) extends HashList[E] {
    def toList = head +: tail.toList
    override val hashCode: Int = siphash24(head.hashCode,tail.hashCode)
  }

  val defaultRest: HashList[Expr[_]] = Empty()

  trait Expr[+V] {
    def eval(c: Context): (Expr[V],Context) = c.eval(this)
    def evalImpl(c: Context): (Expr[V],Context) = (this,c)

    def rest: HashList[Expr[_]] = defaultRest
    def head: Expr[V] = this
  }

  case class Trace[V](override val head: Expr[V], override val rest: HashList[Expr[_]]) extends Expr[V] {
    override def evalImpl(c: Context): (Expr[V], Context) = {
      val (e2,c1) = head.eval(c)
      if (e2 != head) (combine(e2,this),c1)
      else (this,c)
    }
    override def toString = "[" + rest.toList.map(_.toString).reduce((x,y) => x + " => " + y) + " => " + head + "]"
    override val hashCode = siphash24(head.hashCode,rest.hashCode)
  }

  def combine[V](e1: Expr[V],e2: Expr[V]): Trace[V] = Trace(e1.head,e1.rest append e2)

  case class Fail[E,F](f: F) extends Expr[E]

  trait FA0[V] extends Expr[V] {
    def value: V
    def unary_! = value
    override def evalImpl(c: Context) = (this,c)
  }

  trait FA1[A,X] extends (Expr[A] => Expr[X]) with Operator {
    def apply(a: Expr[A]): Expr[X] = a match {
      case (a: FA0[A]) => apply2(a)
      case (a: Fail[A,_]) => Fail(a.f)
      case _ => F1(this,a)
    }
    def apply2(a: FA0[A]): Expr[X]
  }

  trait FA2[A,B,X] extends ((Expr[A],Expr[B]) => Expr[X]) with Operator {
    def apply(a: Expr[A],b: Expr[B]): Expr[X] = (a,b) match {
      case (a: FA0[A],b: FA0[B]) => apply2(a,b)
      case (a: Fail[A,_],_) => Fail(a.f)
      case (_, b: Fail[B,_]) => Fail(b.f)
      case _ => F2(this,a,b)
    }
    def apply2(a: FA0[A],b: FA0[B]): Expr[X]
  }

  trait FA3[A,B,C,X] extends ((Expr[A],Expr[B],Expr[C]) => Expr[X]) with Operator {
    def apply(a: Expr[A],b: Expr[B],c: Expr[C]): Expr[X] = (a,b,c) match {
      case (a: FA0[A],b: FA0[B],c: FA0[C]) => apply2(a,b,c)
      case (a: Fail[A,_],_,_) => Fail(a.f)
      case (_,b: Fail[B,_],_) => Fail(b.f)
      case (_,_,c: Fail[C,_]) => Fail(c.f)
      case _ => F3(this,a,b,c)
    }
    def apply2(a: FA0[A],b: FA0[B],c: FA0[C]): Expr[X]
  }

  trait ExprImpl[V] extends Expr[V] with Product {
    def eval2(e: Expr[V], c: Context) = {
      val (ee,c2) = e.eval(c)
      (combine(ee,this),c2)
    }
  }

  case class F1[A,X](f: FA1[A,X],v1: Expr[A]) extends ExprImpl[X] {
    override def evalImpl(c: Context) = {
      val (e1,c1) = v1.head.eval(c)
      val ff = F1(f,e1)
      if (ff != this) eval2(ff,c1)
      else {
        val ff = f(e1)
        if (ff != this) eval2(ff,c1)
        else (this,c1)
      }
    }
    override val hashCode = siphash24(f.hashCode,v1.hashCode)

    override def toString = {
      if (f.isInstanceOf[PostfixOperator]) v1 + "." + f
      else f+"("+v1+")"
    }
  }

  case class F2[A,B,X](f: FA2[A,B,X],v1: Expr[A],v2: Expr[B]) extends ExprImpl[X] {
    override def evalImpl(c: Context) = {
      val (e1,c1) = v1.head.eval(c)
      val (e2,c2) = v2.head.eval(c1)
      val ff = F2(f,e1,e2)
      if (ff != this) eval2(ff,c2)
      else {
        val ff = f(e1,e2)
        if (ff != this) eval2(ff,c2)
        else (this,c2)
      }
    }
    override val hashCode = siphash24(siphash24(f.hashCode,v1.hashCode),v2.hashCode)
    override def toString = f match {
      case i: InfixOperator => "(" + v1 + " " + f + " " + v2 + ")"
      case _ =>  f + "(" + v1 + "," + v2 + ")"
    }
  }

  case class F3[A,B,C,X](f: FA3[A,B,C,X],v1: Expr[A],v2: Expr[B],v3: Expr[C]) extends ExprImpl[X] {
    override def evalImpl(c: Context) = {
      val (e1,c1) = v1.head.eval(c)
      val (e2,c2) = v2.head.eval(c1)
      val (e3,c3) = v3.head.eval(c2)
      val ff = F3(f,e1,e2,e3)
      if (ff != this) eval2(ff,c3)
      else {
        val ff = f(e1,e2,e3)
        if (ff != this) eval2(ff,c3)
        else (this,c3)
      }
    }
    override val hashCode = siphash24(siphash24(siphash24(f.hashCode,v1.hashCode),v2.hashCode),v3.hashCode)
    override def toString = f + "(" + v1 + "," + v2 + "," + v3 +")"
  }

  object NullContext extends Context

  case class MemozationContext(m: Map[Expr[_],Expr[_]]) extends Context {
    override def eval[V](e: Expr[V]): (Expr[V],Context) = {
      m.get(e) match {
        case None => {
          val (ee,MemozationContext(m2)) = e.evalImpl(this)
          (ee,MemozationContext(m2 + (e->ee)))
        }
        case Some(x) => (x.asInstanceOf[Expr[V]],this)
      }
    }
  }

  type _Int = Expr[Int]
  type $Int = FA0[Int]

  trait IntExpr extends _Int {
    def unwrap: _Int

    def !+(o: _Int): _Int = %(add,unwrap,o)
    def !-(o: _Int): _Int = %(sub,unwrap,o)
    def !*(o: _Int): _Int = %(mul,unwrap,o)
    def !/(o: _Int): _Int = %(div,unwrap,o)
  }

  case class IExpr(value: Int) extends $Int with IntExpr {
    def unwrap = this
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

  case class IWrap(unwrap: _Int) extends IntExpr

  def wrap(i: _Int): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: _Int): IntExpr = wrap(i)

  object fac extends FA1[Int,Int] {
    def apply2(x: $Int) = {
      if (!x <= 1) 1
      else %(fac,!x - 1) !* x
    }
  }

  object fib extends FA1[Int,Int] {
    def apply2(x: $Int) = {
      if (!x <= 1) 1
      else %(fib,!x-1) !+ %(fib,!x-2)
    }
  }

  final def main(args: Array[String]): Unit = {
    val f = %(fac,10)

    println("f: " + f.eval(NullContext)._1)

  }
}

