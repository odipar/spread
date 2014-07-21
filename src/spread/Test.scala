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
    var f = %(fac,10)
    var r = f.eval.eval.eval

    println("r: " + r)
    for (i <- vt.keySet) {
      println(i)
    }

    var f2 = %(fac,5)
    f = null
    System.gc()
    println("GC")
    for (i <- vt.keySet) {
      println(i)
    }
  }

}

