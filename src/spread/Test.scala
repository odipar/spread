package spread

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import scala.collection.immutable._
  import Hashing._

  final def main(args: Array[String]): Unit = {
    import NewExpr._

    val ii = %(fac,3)
    val i = %(fib,ii)
    var k = 0

    println(reduce(1,i).reduce(2).reduce(1).reduce(1).expr)

    println(fac.hashCode)
  }

  object NewExpr {
    import scala.language.implicitConversions

    object fac extends (I=>I) {
      def apply(i: I): I = {
        if (i.eval < 2) 1
        else i * %(fac,i.eval - 1)
      }
      override def toString = "fac"
    }

    object fib extends (I=>I) {
      def apply(i: I): I = {
        val ii = i.eval
        if (ii < 2) 1
        else %(fib,ii.eval-1) + %(fib,ii.eval-2)
      }
      override def toString = "fib"
    }

    trait Context {
      def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E])
      def merge(o: Context): Context = o
    }

    def reduce[E](s: Int, e: Expr[E]) = ExprContext(MemContext(Map()),e).reduce(s)

    case class ExprContext[E](context: Context, expr: Expr[E]) {
      def reduce(s: Int): ExprContext[E] = { val (c,e) = context.reduce(s,expr) ; ExprContext(c,e) }
    }

    case class MemContext(m: Map[(Int,Expr[_]),Expr[_]]) extends Context
    {
      // TODO: optimize - one map per stage
      def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E]) = {
        val se = (s,e)
        if (m.contains(se)) (this,m.get(se).get.asInstanceOf[Expr[E]])
        else {
          val (MemContext(mm),ee: Expr[E]) = e.reduce(s,this)
          (MemContext(mm.updated(se,ee)),ee)
        }
      }
    }

    def fullReduce[E](s: Int, c: Context, e: Expr[E]): (Context, Expr[E]) = {
      val (cc,ee) = c.reduce(s,e)
      if  (ee == e) (cc,ee)
      else fullReduce(s,cc,ee)
    }

    trait Expr[E] {
      def eval: E
      def stage: Int
      def reduce(s: Int, c: Context): (Context,Expr[E]) = (c,this)
    }

    def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = { F1(1,f,a) }
    def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = { F2(1,f,a,b) }

    type I = Expr[Int]

    case class II(eval: Int) extends IExpr {
      def origin = this
      def stage = 0
      override def toString = "" + eval
      override def hashCode = jh(eval)
    }

    implicit def toI(i: Int): I = II(i)

    private case class IWrap(origin: I) extends IExpr {
      def error = sys.error("IWrap should not be used directly")
      override def stage = error
      override def eval = error
      override def reduce(s: Int, c: Context) = error
    }

    implicit def toIWrap(i: I): IExpr = i match {
      case ii: IExpr => ii
      case _ => IWrap(i)
    }

    trait IExpr extends I {
      def origin: I
      def +(o: I): I = F2(2,add,origin,o)
      def -(o: I): I = F2(2,sub,origin,o)
      def *(o: I): I = F2(2,mul,origin,o)
    }

    val add = new ((I, I) => I) {
      def apply(a: I, b: I): I = a.eval + b.eval
      override def toString = "+"
    }

    val sub = new ((I, I) => I) {
      def apply(a: I, b: I): I = a.eval - b.eval
      override def toString = "-"
    }

    val mul = new ((I, I) => I) {
      def apply(a: I, b: I): I = a.eval * b.eval
      override def toString = "*"
    }

    trait E

    object NE extends E
    case class EE(e: E) extends E

    case class F1[A, R](ss: Int, f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
      def eval = { f(a).eval }
      val stage = ss max a.stage // TODO: optimize by using 15 bits for ss and 15 bits for stage
      override def reduce(s: Int, c: Context) = {
        val (ca,ea) = fullReduce(s,c,a)
        if (((a.stage == 0) && (ea.stage == 0))  && (s >= stage)) (ca,f(ea))
        else (ca,F1(ss,f,ea))
      }
      override val hashCode = jh(jh(jh(stage) ^ Hashing.jh(f)) + jh(a))
      override def toString = f + "(" + a + ")"
    }

    case class F2[A,B,R](ss: Int, f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
      def eval = { f(a,b).eval }
      val stage = ss max a.stage max b.stage
      override def reduce(s: Int, c: Context) = {
        val (ca,ea) = fullReduce(s,c,a)
        val (cb,eb) = fullReduce(s,ca,b)
        if (((a.stage == 0) && (ea.stage == 0)) && ((b.stage == 0) && (eb.stage == 0)) && (s >= stage)) {
          (cb,f(ea,eb))
        }
        else
        {
          // TODO: could be optimized to if (ea == a and eb == b) then this
          (cb,F2(ss,f,ea,eb))
        }
      }
      override val hashCode = jh(jh(jh(jh(stage) ^ Hashing.jh(f)) - jh(a)) ^ jh(b))
      override def toString = "(" + a + " " + f + " " + b + ")"
    }
  }

  // each expression has a stage, which is the maximum of it's children stages + the stage of the expression itself
  // when evaluating an expression against a evaluation stage, the stage should be higher or equal to the expression.

  // if all arguments are in stage 0 and the evaluation stage is >= to the function stage, the function 'fires'
  // it *must* be that the evaluation of stage 0 expressions is O(1)

  // how to calculate the minimum stage that will have another reduction?
  // returning the memoization of another stage if the memoized version has a lower stage.
  // i.e. Map[Expr[_],SortedMap[Int,Expr[_]]
  // do a binary search in the sorted map

}

