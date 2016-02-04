package spread

import spread.SpreadArithmetic._
import spread.SpreadLogic._
import spread.Spread._
import SplitHash._
import scala.collection.mutable.WeakHashMap
import scala.collection.immutable.HashMap
import SetHash._

// EXPOSITION:
//
// Authenticated re-usable computations = authenticated spreadsheets?
//
// Copyright 2016: Robbert van Dalen
//

object Test {

  val scontext = StrongMemoizationContext(HashMap())
  val wcontext = WeakMemoizationContext(WeakHashMap())
  val econtext = EmptyContext

  final def main(args: Array[String]): Unit ={
    {
      val s1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
      val s2 = s1 ! s1
      val (s3,s4) = s2.split(12)
      val (s5,s6) = s2.split(2)
      val s7 = s4 ! s5
      val s8 = 5 ! 6 ! 7 ! 8 ! 1 ! 2
      println(s7 == s8) // true
    }

    {
      val a = true !|| false
      val b = true !&& false
      val c = a !^^ b
      println(c.fullEval)
    }

    val seq1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
    val seq2 = 1 ! 2 ! 3 ! 9 ! 5 ! 6 ! 7 ! 8

    val sum1 = %(sum,expr(seq1))
    val sum2 = %(sum,expr(seq2))

    traceReuse = true

    var (r1,_) = fullEval(sum1,wcontext)
    println(r1)

    var (r2,_) = fullEval(sum2,wcontext)
    println(r2)

    val fib1 = %(Test.fib2,6)

    var (f1,_) = fullEval(fib1,econtext)
    println("slow: " + f1)

    var (f2,_) = fullEval(fib1,wcontext)
    println("fast: " + f2)

    if (f1 != f2) { sys.error("Internal inconsistency") }  // the traces must be structurally equal

    println()

    val fac1 = %(fac,5)
    var (fc1,_) = fullEval(fac1,wcontext)
    println("fac(5): " + fc1)

    val fac2 = %(fac,7)
    var (fc2,_) = fullEval(fac2,wcontext)
    println("fac(7): " + fc2.head)
    println()

    val fib2 = %(Test.fib2,25)
    var (f3,_) = fullEval(fib2,wcontext)
    println("fib(25): " + f3.head)
    println("trace size: " + f3.trace.size)
  }

  object fac extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else i !* {
        if (!i < 5) ~%(fac,!i - 1) // We only capture the crypto hash for fac(i), i < 5
        else %(fac,!i - 1)         // For i >= 5 we capture the full trace
      }
    }
  }

  object fib extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)
    }
  }

  object fib2 extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else if (!i < 5) ~%(fib,i)
      else %(fib2,!i - 1) !+ %(fib2,!i - 2)
    }
  }


  type _SplitHash[X] = SHNode[X]
  type $SplitHash[X] = F0[SHNode[X]]

  // We could easily have implemented sum with a generic fold
  // But for now we just explicitly show how to use the DSL and API
  object sum extends FA1[_SplitHash[Int],Int]{
    def apply(s: $SplitHash[Int]) ={
      val ss = !s
      if (ss.size == 1) ss.last
      else {
        val parts = ss.splitParts
        var ssum = %(sum,expr(parts(0)))
        var i = 1
        while (i < parts.length) {
          ssum = ssum !+ %(sum,expr(parts(i)))
          i = i + 1
        }
        ssum
      }
    }
  }
}
