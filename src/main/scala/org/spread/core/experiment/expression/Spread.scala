package org.spread.core.experiment.expression

import java.lang.ref.WeakReference

import org.spread.core.experiment.sequence.{Storage, Test}
import org.spread.core.experiment.sequence.Storage._
import org.spread.core.splithash.Hashing.siphash24

import scala.collection.mutable
import scala.util.DynamicVariable

object Spread {

  import scala.language.implicitConversions

  val dynamicContext: DynamicVariable[Context] = new DynamicVariable(new StrongStoringMemoizationContext())

  def withContext[S](c: Context)(thunk: => S): S = dynamicContext.withValue(c)(thunk)
  def context: Context = dynamicContext.value
  
  trait Expr[+V] extends Storable[Expr[V]] {
    def eval: Expr[V] = {
      mcontext.eval(this)
    }
    def evalImpl: Expr[V] = this

    def isFailure: Boolean
    def isValue: Boolean

    def rest: HashList[V] = empty.asInstanceOf[Empty[V]]
    def head: Expr[V] = this

    def store: Expr[V]
    def mcontext: Context = context
  }

  val empty: Empty[_] = Empty()

  case class RefExpr[V](ref: Ref, isFailure: Boolean, isValue: Boolean) extends Expr[V] with RefObject[Expr[V]] {
    override def evalImpl: Expr[V] = resolve.evalImpl

    override def rest: HashList[V] = resolve.rest
    override def head: Expr[V] = resolve.head
    def store: Expr[V] = this
  }
  
  trait Context {
    def eval[V](e: Expr[V]): Expr[V] = e.evalImpl
    def contains[V](e: Expr[V]): Boolean = false
  }

  trait Operator {
    def value[X](e: Expr[X]): X = e.asInstanceOf[FA0[X]].value
    override def toString: String  = this.getClass.getSimpleName.replace("$", "")
  }
  trait InfixOperator extends Operator
  trait PostfixOperator extends Operator

  def %[A, X](f: FA1[A, X], a: Expr[A]): Expr[X] = F1(f, a)
  def %[A, B, X](f: FA2[A, B, X], a: Expr[A], b: Expr[B]): Expr[X] = F2(f, a, b)
  def %[A, B, C, X](f: FA3[A, B, C, X], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = F3(f, a, b, c)

  trait HashList[+E] extends Storable[HashList[E]] {
    def size: Int
    def head: Expr[E]
    def tail: HashList[E]

    def append[E2 >: E](e: Expr[E2]): HashList[E2] = Cons(e, this)
    def toList: List[Expr[E]]
    def store: HashList[E] = this
  }

  case class RefHashList[E](ref: Ref, size: Int) extends HashList[E] with RefObject[HashList[E]] {
    def head: Expr[E] = resolve.head
    def tail: HashList[E] = resolve.tail
    def toList: List[Expr[E]] = resolve.toList
  }

  case class Empty[E]() extends HashList[E] {
    def size = 0
    def error: Nothing= sys.error("empty list")
    def head: Expr[E] = error
    def tail: HashList[E] = error
    def toList: List[Expr[E]] = List()
  }

  case class Cons[E](head: Expr[E], tail: HashList[E]) extends HashList[E] {
    def size: Int = tail.size + 1
    def toList: List[Expr[E]] = head +: tail.toList
    override val hashCode: Int = siphash24(head.hashCode, -tail.hashCode)
    def ref1(r: HashList[E]): HashList[E] = Cons(head.store, tail.store)
    def ref2(r: Ref): HashList[E] = RefHashList(r, size)
    override def store: HashList[E] = storage.put(this, ref1, ref2)
  }

  val defaultRest: HashList[Expr[_]] = Empty()

  case class Trace[V](override val head: Expr[V], override val rest: HashList[V]) extends Expr[V] {
    override def evalImpl: Expr[V] = {
      val e2 = head.eval
      if (e2 != head) combine(e2, this)
      else this
    }

    def isFailure: Boolean = head.isFailure
    def isValue: Boolean = head.isValue
    
    override def store: Expr[V] = Trace(head.store, rest.store)

    override def toString: String = {
      "[" + rest.toList.map(_.toString).reduce((x, y) => x + " => " + y) + " => " + head + "]"
    }
    override val hashCode: Int = siphash24(-head.hashCode, rest.hashCode)
  }

  def combine[V](e1: Expr[V], e2: Expr[V]): Trace[V] = Trace(e1.head, e1.rest.append(e2))

  case class Fail[E, F](f: F) extends Expr[E] {
    def isFailure: Boolean = true
    def isValue: Boolean = false
    def store: Expr[E] = this
  }

  trait FA0[V] extends Expr[V] {
    def value: V
    def unary_! : V = value
    override def evalImpl: Expr[V] = this

    def isFailure: Boolean = false
    def isValue: Boolean = true
  }

  case class AnExpr[V](value: V) extends FA0[V] {
    override def toString: String = "$" + value.toString
    def store: Expr[V] = this
  }

  trait FA1[A, X] extends (A => Expr[X]) with Operator {
    def apply(a: Expr[A]): Expr[X] = apply2(a)
    def apply2(a: Expr[A]): Expr[X] = {
      if (a.isValue) apply(value(a))
      else if (a.isFailure) Fail(a.asInstanceOf[Fail[A, _]].f)
      else F1(this, a)
    }
  }

  trait FA2[A, B, X] extends ((A, B) => Expr[X]) with Operator {
    def apply(a: Expr[A], b: Expr[B]): Expr[X] = apply2(a,b)
    def apply2(a: Expr[A], b: Expr[B]): Expr[X] = {
      if (a.isValue && b.isValue) apply(value(a), value(b))
      else if (a.isFailure || b.isFailure) {
        (a, b) match {
          case (a: Fail[A, _], _) => Fail(a.f)
          case (_, b: Fail[B, _]) => Fail(b.f)
          case _  => ???
        }
      }
      else F2(this, a, b)
    }
  }

  trait FA3[A, B, C, X] extends ((A,B,C) => Expr[X]) with Operator {
    def apply(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = apply2(a, b, c)
    def apply2(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = {
      if (a.isValue && b.isValue && c.isValue) apply(value(a), value(b), value(c))
      else if (a.isFailure || b.isFailure || c.isFailure) {
        (a, b, c) match {
          case (a: Fail[A, _], _, _) => Fail(a.f)
          case (_, b: Fail[B, _], _) => Fail(b.f)
          case (_, _, c: Fail[C, _]) => Fail(c.f)
          case _  => ???
        }
      }
      else F3(this, a, b, c)
    }
  }
  
  trait ExprImpl[V] extends Expr[V] with Product {
    def eval2(e: Expr[V]): Expr[V] = {
      val ee = e.eval
      combine(ee, this)
    }

    def isFailure: Boolean = false
    def isValue: Boolean = false
  }

  case class F1[A, X](f: FA1[A, X], v1: Expr[A]) extends ExprImpl[X] {
    override def evalImpl: Expr[X] = {
      val e1 = v1.head.eval
      val ff = F1(f, e1)
      if (ff != this) eval2(ff)
      else {
        val ff = f(e1)
        if (ff != this) eval2(ff)
        else this
      }
    }

    override val hashCode: Int = siphash24(-f.hashCode, v1.hashCode)

    def ref1(r: Expr[X]): Expr[X] = F1(f, v1.store)
    def ref2(r: Ref): Expr[X] = RefExpr(r, isFailure, isValue)
    override def store: Expr[X] = storage.put(this, ref1, ref2)

    override def toString: String = {
      if (f.isInstanceOf[PostfixOperator]) v1 + "." + f
      else f + "(" + v1 + ")"
    }
  }

  case class F2[A, B, X](f: FA2[A, B, X], v1: Expr[A], v2: Expr[B]) extends ExprImpl[X] {
    override def evalImpl: Expr[X] = {
      val e1 = v1.head.eval
      val e2 = v2.head.eval
      val ff = F2(f, e1, e2)
      if (ff != this) eval2(ff)
      else {
        val ff = f(e1, e2)
        if (ff != this) eval2(ff)
        else this
      }
    }

    def ref1(r: Expr[X]): Expr[X] = F2(f, v1.store, v2.store)
    def ref2(r: Ref): Expr[X] = RefExpr(r, isFailure, isValue)
    override def store: Expr[X] = storage.put(this, ref1, ref2)

    override val hashCode: Int = siphash24(siphash24(f.hashCode, v1.hashCode), v2.hashCode)
    override def toString: String = f match {
      case i: InfixOperator => "(" + v1 + " " + f + " " + v2 + ")"
      case _ => f + "(" + v1 + "," + v2 + ")"
    }
  }

  case class F3[A, B, C, X](f: FA3[A, B, C, X], v1: Expr[A], v2: Expr[B], v3: Expr[C]) extends ExprImpl[X] {
    override def evalImpl: Expr[X] = {
      val e1 = v1.head.eval
      val e2 = v2.head.eval
      val e3 = v3.head.eval
      val ff = F3(f, e1, e2, e3)
      if (ff != this) eval2(ff)
      else {
        val ff = f(e1, e2, e3)
        if (ff != this) eval2(ff)
        else this
      }
    }

    def ref1(r: Expr[X]): Expr[X] = F3(f, v1.store, v2.store, v3.store)
    def ref2(r: Ref): Expr[X] = RefExpr(r, isFailure, isValue)
    override def store: Expr[X] = storage.put(this, ref1, ref2)
    override val hashCode: Int = siphash24(siphash24(siphash24(f.hashCode, v1.hashCode), v2.hashCode), v3.hashCode)
    override def toString: String = f + "(" + v1 + "," + v2 + "," + v3 + ")"
  }

  object NullContext extends Context {}

  class StoringMemoizationContext() extends Context {
    var m: mutable.HashMap[Expr[_], Expr[_]] = mutable.HashMap()

    override def eval[V](e: Expr[V]): Expr[V] = {
      val es = e.store

      m.get(es) match {
        case None => {
          val ee = e.evalImpl
          val ees = ee.store
          //if ((e.rest.size == 0) && (ee.rest.size > 3)) {
            m += (es -> ees)
          //}
          ees
        }
        case Some(x) => {
          x.asInstanceOf[Expr[V]]
        }
      }
    }
  }

  case class StackedContext(o1: Context, o2: Context) extends Context() {
    override def eval[V](e: Expr[V]): Expr[V] = {
      if (o1.contains(e)) o1.eval(e)
      else o2.eval(e)
    }
    override def contains[V](e: Expr[V]): Boolean = o1.contains(e) || o2.contains(e)
  }
  
  case class StrongStoringMemoizationContext() extends Context {
    var m: mutable.HashMap[Expr[_], Expr[_]] = mutable.HashMap()
    
    override def eval[V](e: Expr[V]): Expr[V] = {
      val es = e.store
      //val es = e
      m.get(es) match {
        case None => {
          val ee = e.evalImpl
          //val ees = ee
          val ees = ee.store

          if (ees.rest.size > 0) m += (es -> ees)
          
          ees
      }
        case Some(x) =>  {
          x.asInstanceOf[Expr[V]]
        }
      }
    }

    override def contains[V](e: Expr[V]): Boolean = m.contains(e)
  }

  case class WeakMemoizationContext() extends Context {
    var m: mutable.WeakHashMap[Expr[_], WeakReference[Expr[_]]] = mutable.WeakHashMap()
    
    override def eval[V](e: Expr[V]): Expr[V] = {
      m.get(e) match {
        case None => {
          val ee = e.evalImpl
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

    def ref1(o: Expr[Int]): Expr[Int] = this
    def ref2(r: Ref) = RefExpr(r, isFailure, isValue)
    def store: Expr[Int] = storage.put(this, (x: Expr[Int]) => this, ref2)
  }

  trait BinIntOp extends FA2[Int, Int, Int] with InfixOperator

  case class add2() extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 + o2)
    override def toString = "!+"
  }

  case class sub2() extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 - o2)
    override def toString = "!-"
  }

  case class mul2() extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 * o2)
    override def toString = "!*"
  }

  case class div2()  extends BinIntOp {
    def apply(o1: Int, o2: Int) = IExpr(o1 / o2)
    override def toString = "!/"
  }

  val add = add2()
  val sub = sub2()
  val mul = mul2()
  val div = div2()

  case class IWrap(unwrap: _Int) extends IntExpr {
    def isFailure: Boolean = false
    def isValue: Boolean = false
    def store: Expr[Int] = this
  }

  def wrap(i: _Int): IntExpr = i match {
    case w: IWrap => w
    case _ => IWrap(i)
  }

  implicit def toIntExpr(i: Int): IntExpr = IExpr(i)
  implicit def toIntExpr2(i: _Int): IntExpr = wrap(i)
  implicit def toExpr[X](x: X): Expr[X] = AnExpr[X](x)

  case class fac2() extends FA1[Int, Int] {
    def apply(x: Int): _Int = {
      if (x <= 1) 1
      else %(fac, x - 1) !* x
    }
  }

  val fac = fac2()

  case class fib2() extends FA1[Int, Int] {
    def apply(x: Int): _Int = {
      if (x <= 1) 1
      else %(fib, x - 1) !+ %(fib, x - 2)
    }
  }

  val fib = fib2()

  final def main(args: Array[String]): Unit = {
    val f = %(fib, 30)
    //val f = %(fac, 10)

    Storage.withStorage(InMemoryStorage()) {
      withContext(StrongStoringMemoizationContext()) {
        var k = f.eval

        val m = context.asInstanceOf[StrongStoringMemoizationContext].m
        for (k <- m.keySet) {
          println("key: " + k)
          println("value: " + m(k))
          println
        }

        println("k: " + k.head)
      }
    }
  }
}

