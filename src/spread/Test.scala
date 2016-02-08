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
  final def main(args: Array[String]): Unit = {

    val l = ClassLoader.instance    // SPREAD installs a custom ClassLoader to authenticate all the JVM bytecode
    val c = l.loadClass(new SubTest().getClass.getName)
    val i = c.newInstance

    val m = c.getMethod("main", classOf[Array[String]])
    val params: Array[String] = null
    m.invoke(i, params)
  }

  class SubTest {
    val scontext = StrongMemoizationContext(HashMap())
    val wcontext = WeakMemoizationContext(WeakHashMap())
    val econtext = EmptyContext

    final def main(args: Array[String]): Unit =
    {
      {
        val s1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
        val s2 = s1 ! s1
        val (s3,s4) = s2.split(12)
        val (s5,s6) = s2.split(2)
        val s7 = s4 ! s5
        val s8 = 5 ! 6 ! 7 ! 8 ! 1 ! 2
        println(s7 == s8) // true
        println
      }
      {
        val a = (1 !+ 2) !* (3 !+ 4)
        val b = a.fullEval
        val c = b !+ b
        val d = a !+ a
        println(c.fullEval == d.fullEval)
        println("b: " + b)
        println
      }
      {
        val a = ('a !+ 2) !* (3 !+ 4)
        val b = (5 !+ 6) !* (7 !+ 8)
        val c = a !+ b
        println(c.bind('a,2).fullEval)
        println
      }
      {
        val a = (1 !+ 'a) !* (3 !+ 4.quote)
        val b = a.unquote.bind('a,(7 !- 5)).fullEval
        val c = a.unquote.fullEval
        val d = c.bind('a,(7 !- 5))
        println(a)
        println
        println(b)
        println
        println(c)
        println
        println(d.fullEval)
        println
      }
      {
        val seq1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
        val seq2 = 1 ! 2 ! 3 ! 9 ! 5 ! 6 ! 7 ! 8

        val sum1 = %(sum,expr(seq1))
        val sum2 = %(sum,expr(seq2))

        traceReuse = true

        var (r1,_) = sum1.fullEval(wcontext)
        println(r1)

        var (r2,_) = sum2.fullEval(wcontext)
        println(r2)
        println
      }
      {
        traceReuse = true

        val fib1 = %(fib2,6)

        var (f1,_) = fib1.fullEval(econtext)
        println("slow: " + f1)

        var (f2,_) = fib1.fullEval(wcontext)
        println("fast: " + f2)

        if (f1 != f2) {
          sys.error("Internal inconsistency")
        } // the traces must be structurally equal
        println()
      }
      {
        val fac1 = %(fac,5)
        var (fc1,_) = fac1.fullEval(wcontext)
        println("fac(5): " + fc1)

        val fac22 = %(fac,7)
        var (fc2,_) = fac22.fullEval(wcontext)
        println("fac(7): " + fc2.head)
        println()

        val fac33 = %(fac2,5)
        var (fc3,_) = fac33.fullEval(wcontext)
        println("fac2(5): " + fc3)
        println()
        val (fc4,_) = fc3.unquote.fullEval(wcontext)
        println("fac2(5).unquote: " + fc4)
        println
      }
    }

    object fac extends FA1[Int,Int]{
      def apply2(i: $Int) ={
        if (!i < 2) 1
        else i !* {
          if (!i < 5) ~%(fac,!i - 1) // We only capture the crypto hash for fac(i), i < 5
          else %(fac,!i - 1) // For i >= 5 we capture the full trace
        }
      }
    }

    object fac2 extends FA1[Int,Int]{
      // Quoted + if then else
      def apply2(i: $Int) ={
        (i !== 1) !?(           // if (i == 1)
          1.quote,              // then quote 1
          i !* %(fac2,i !- 1)   // else i * fac(i-1)
        )
      }
    }

    object fib extends FA1[Int,Int]{
      def apply2(i: $Int) ={
        if (!i < 2) 1
        else %(fib,!i - 1) !+ %(fib,!i - 2)
      }
    }

    object fib2 extends FA1[Int,Int]{
      def apply2(i: $Int) ={
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
      def apply2(s: $SplitHash[Int]) ={
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
}
