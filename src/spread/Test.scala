package spread

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import language.implicitConversions
  import IncrementalMemoization._
  import IncrementalTreap._
  import IncrementalArithmetic._

  object fac extends (I=>I) {
    def apply(i: I): I = {
      if (!i < 2) 1.$
      else i ** %(fac,!i-1)
    }
    override def toString = "fac"
  }

  object fib extends (I=>I) {
    def apply(i: I): I = {
      if (!i < 3) 1
      else %(fib,!i-1) ++ %(fib,!i-2)
    }
    override def toString = "fib"
  }

  val put = put1(intord)
  final def main(args: Array[String]): Unit = {
    var i1 = (1.$ + 2) * (3.$ + 4)
    println(i1.replace(3.$,9))
  }

  def parentPaths(e: Expr[ _], t: Expr[_]) : List[Expr[_]] = {
    var c: List[List[Expr[_]]] = List(List(e))
    var done = false

    while (!done) {
      var nc: List[List[Expr[_]]] = List()
      var iter = false
      for (tl <- c) {
        val h = tl.head
        if (h == t) {
           done = true
           nc = List(tl)
        }
        else
        {
          val p = getParents(tl.head)
          if (!p.isEmpty) {
            iter = true
            for (pp <- p.keys) {
              var k = tl.::(pp)
              nc = k +: nc
            }
          }
        }
      }
      c = nc
      if (!iter) { done = true }
    }

    c.head
  }
}
