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
    val p = intord
    var t1 = T(0)
    var i = 1
    while (i < 100) {
      t1 = p.join(t1,T(i))
      i = i + 1
    }

    var t2 = T(99)
    i = 98
    while (i >= 0) {
      t2 = p.join(T(i),t2)
      i = i - 1
    }

    val t3 = t1
    val t4 = t2

    val r1 = fullRed(t3)
    val r2 = fullRed(t4)

    println("r1: " + r1)
    println("r2: " + r2)
    println("r1 eq r2 = " + (r1 eq r2))

    System.gc()

    println("size: " + vt.size)
    for (i <- vt.keySet) {
      println(i)
    }
  }

  def search[X](e: Expr[X]): Set[Expr[X]] = sys.error("no")
}

