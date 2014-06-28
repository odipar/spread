package spread

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import scala.collection.immutable._

  final def main(args: Array[String]): Unit = {
    import NewExpr._

    val i = fib(30)
    var k = 0
    while (k < 100000) {
      MemContext(Map()).reduce(i)._2
      //println ("ff: " + ff)
      k = k +1
    }
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

    var ff = 0

    object fib extends (I=>I) {
      def apply(i: I): I = {
        val ii = i.eval
        if (ii < 2) 1
        else %(fib,ii-1) + %(fib,ii-2)
      }
      override def toString = "fib"
    }

    trait Context {
      def reduce[E](e: Expr[E]): (Context,Expr[E])
      def merge(o: Context): Context = o
    }

    case class MemContext(m: Map[Expr[_],Expr[_]]) extends Context
    {
      def reduce[E](e: Expr[E]): (Context,Expr[E]) = {
        if (m.contains(e)) (this,m.get(e).get.asInstanceOf[Expr[E]])
        else {
          val (MemContext(mm),ee: Expr[E]) = e.reduce(this)
          (MemContext(mm.updated(e,ee)),ee)
        }
      }
    }

    def fullReduce[E](c: Context, e: Expr[E]): (Context, Expr[E]) = {
      val (cc,ee) = c.reduce(e) ; if (ee == e) (cc,ee) ; else fullReduce(cc,ee)
    }

    trait Expr[E] {
      def eval: E
      def reduce(c: Context): (Context,Expr[E]) = (c,this)
    }

    def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = { F1(f,a) }
    def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = { F2(f,a,b) }

    type I = Expr[Int]

    case class II(eval: Int) extends IExpr {
      def origin = this
      override def toString = "" + eval
      override def hashCode = Hashing.jenkinsHash(eval)
    }

    implicit def toI(i: Int): I = II(i)

    private case class IWrap(origin: I) extends IExpr {
      def error = sys.error("IWrap should not be used directly")
      override def eval = error
      override def reduce(c: Context) = error
    }

    implicit def toIWrap(i: I): IExpr = i match {
      case ii: IExpr => ii
      case _ => IWrap(i)
    }

    trait IExpr extends I {
      def origin: I
      def +(o: I): I = %(add, origin, o)
      def -(o: I): I = %(sub, origin, o)
      def *(o: I): I = %(mul, origin, o)
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

    case class F1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
      def eval = { f(a).eval }
      override def reduce(c: Context) = {
        val (ca,ea) = fullReduce(c,a)
        if (ea == a) (ca,f(a))
        else (ca,F1(f,ea))
      }
      override def toString = f + "(" + a + ")"
    }

    case class F2[A,B,R](f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
      def eval = { f(a,b).eval }
      override def reduce(c: Context) = {
        val (ca,ea) = fullReduce(c,a)
        val (cb,eb) = fullReduce(ca,b)
        if ((ea == a) && (eb == b)) (cb,f(a,b))
        else (cb,F2(f,ea,eb))
      }
      override def toString = "(" + a + " " + f + " " + b + ")"
    }
  }

      // each expression has a stages which is the maximum of it's children stages + the stage of the expression itself
      // when evaluating an expression against a stage, the stage should be higher or equal to the expression.

  // stage 0 -> Nullary Functions: Integer, String, etc
  // stage 1 -> N-Ary Functions
  // stage 2 -> Generating Functions
  // stage 3 -> Generating Generating Functions

  // only stage 0 arguments 'fire' functions.
  // it *must* be that the evaluation of stage 0 expressions is O(1)

}

