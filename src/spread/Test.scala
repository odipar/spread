package spread

import java.lang.ref.WeakReference
import scala.collection.mutable.WeakHashMap

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {

  import scala.language.implicitConversions
  import IncrementalMemoization._
  import IncrementalTreapMap._
  import IncrementalArithmetic._

  object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (~i < 2) 1
      else  i * %(fac,~i - 1)
    }
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I) = {
      if (~i < 2) i
      else %(fib,i - 1) + %(fib,i - 2)
    }
  }

  object sum extends FA1[TreapMap[Int,Int],Int] {
    def apply(t: F0[TreapMap[Int,Int]]) = {
      val tt = ~t
      val l = tt.left
      val r = tt.right

      if (l.isEmpty) {
        if (r.isEmpty) tt.value
        else tt.value ++ %(sum,tt.right)
      }
      else {
        if (r.isEmpty) %(sum,tt.left) ++ tt.value
        else %(sum,tt.left) ++ tt.value ++ %(sum,tt.right)
      }
    }
  }


  object g extends (I => I) with Func {
    def apply(x: I) = 2 ** x
  }

  object h extends (I => I) with Func {
    def apply(x: I) = x ** x
  }

  object f extends ((I,I) => I) with Func {
    def apply(x: I,y: I) = %(g,x) + %(h,y)
  }

  final def main(args: Array[String]): Unit = {
    var e: TreapSet[Int] = empty
    implicit val pif = ISetPrioFactory[Int]()

    var i = 0
    while (i < 100) {
      e = e.put(SetEntry(i))
      i = i + 1
    }
    val r = %(sum,e)
    val (c1,e1) = fullRed(cc,r)
    println("e1: " + e1)
    e = e.put(SetEntry(i))
    val r2 = %(sum,e)
    val (c2,e2) = fullRed(c1,r2)
    println("e2: " + e2)
  }


  trait RF[A,B] extends ((A,RF[A,B]) => B)

  object RF extends RF[Int,Int] {
    def apply(i: Int, f: RF[Int,Int]): Int = {
      if (i < 1) 1
      else i * f(i-1,f)
    }
  }

  val z = RF(10,RF)

}
