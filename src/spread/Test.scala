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
      else i ** %(fac,~i - 1)
    }
    override def toString = "fac"
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (~i < 2) i
      else %(fib,~i-1) ++ %(fib,~i-2)
    }
    override def toString = "fib"
  }

  object sum extends FA1[TreapMap[Int,Int], Int] {
    def apply(t: F0[TreapMap[Int,Int]]): I = {
      val tt = ~t
      val l = tt.left
      val r = tt.right

      if (l.isEmpty) {
        if (r.isEmpty) tt.value
        else tt.value ++ %(sum,tt.right)
      }
      else {
        if (r.isEmpty) tt.value ++ %(sum,tt.left)
        else %(sum,tt.left) ++ tt.value ++ %(sum,tt.right)
      }
    }
    override def toString = "sum"
  }

  implicit val pif = ISetPrioFactory[Int]()

  object f extends ((I,I) => I) {
    def apply(x: I, y: I): I = {
      %(g,x) ++ %(h,y)
    }
    override def toString = "f"
  }

  object g extends (I => I) {
    def apply(x: I): I = {
      2 ** x
    }
    override def toString = "g"
  }

  object h extends (I => I) {
    def apply(x: I): I = {
      x ** x
    }
    override def toString = "h"
  }

  final def main(args: Array[String]): Unit = {
    val c1 = %(f,5,20)
    val c2 = %(f,10,20)

    println("c1: " + fullRed(c1))
    println("c2: " + fullRed(c2))
    /*var e: TreapMap[Int,Int] = empty

    var i = 0
    println("start")
    while (i < 10) {
      e = e.put(i,i)
      i = i + 1
    }
    println("done")
    val r = %(sum,e)
    println("r: " + r.hashCode)
    e = e.put(i,i)
    val r2 = %(sum,e)
    println("r2: " + fullRed(r2))*/
  }

}

