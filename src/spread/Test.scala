package spread

import java.lang.ref.WeakReference
import scala.collection.mutable.WeakHashMap

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import scala.language.implicitConversions

  import IncrementalMemoization._
  import scala.language.implicitConversions
  import IncrementalArithmetic._

  import IncrementalTreap._

  object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (!i < 2) 1
      else i ** %(fac,!i - 1)
    }
    override def toString = "fac"
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (!i < 2) i
      else (%(fib,!i-1)) ++ (%(fib,!i-2))
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    var a = (('a~1 ++ 2) ** (3 ++ 4))
    var b =  ((5 ++ 6) ** (7 ++ 8))

    var c = a.replace('a,2)
    println("c: " + c)

     System.gc()
    println("size: " + rt.keySet.size)

    for (i <- rt.keySet) {
     println(i + " ==> " + rt(i).get)
    }
  }

  /*def contains[E,X](expr: Expr[E], x: X) = {
    def c = Contains[E,X]()
    %(c,expr,Quote(ei(x)))
  } */

  /*case class Contains[E,X](a: Expr[E], bb: X) extends F2[E,X,Boolean] with ((Expr[E],Expr[X]) => Expr[Boolean]) {
    def f = this
    def b = ei(bb)

    def reduce = {
      this
    }
    def apply(e: Expr[E], x: Expr[X]): Expr[Boolean] = true

    override def toString = a + " ? " + bb
  } */
 /* def contains[X,E](x: X, expr: Expr[X]) = expr match {
    case Quote(xx) => Quote(Contains(x,xx))
    case v: Var[_,E] => {
      if (v.label == x) trace(expr,true)
      else trace(expr,Contains(x,v.expr))
    }
    case t: Trace[E] => {
      trace(expr,Contains(x,t.to))
    }
    case f1: F1[_,E] => {
      trace(expr,Contains(x,f1.a))
    }
    case f2: F2[_,_,E] => {
      val aa = Contains(x,f2.a)
      val bb = Contains(x,f2.b)

      trace(expr,aa ||| bb)
    }
    case f3: F3[_,_,_,E] => {
      val aa = contains(x,f3.a)
      val bb = contains(x,f3.b)
      val cc = contains(x,f3.c)

      trace(expr, aa ||| bb ||| cc)
    }
    case _ => false
  }       */

  // ((1 ++ 2) ** (1 ++ 2)).search(1)
  def search[X](e: Expr[X]): Set[Expr[X]] = sys.error("no")
}

