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
      if (!i < 2) (1~1).\
      else ((!i)~i).\ ** %(fac,!i - 1)
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
    var a = (1 ++ 2)
    var b =  (('a~5 ++ 6) ** (7 ++ 'b~8))
    //var c = b.replace('a,2).replace('b,3)
    var e = fullRed(%(fac,5)).replace(1,2)
    println("c: " + e)

     System.gc()
    println("size: " + rt.keySet.size)

    for (i <- rt.keySet) {
     println(i + " ==> " + rt(i).get)
    }
  }

  def search[X](e: Expr[X]): Set[Expr[X]] = sys.error("no")
}

