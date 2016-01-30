package spread

import spread.SpreadArithmetic._
import spread.Spread._
import SplitHash._
import scala.collection.mutable.WeakHashMap

// EXPOSITION:
//
// Authenticated re-usable computations = authenticated spreadsheets?
//
// Copyright 2016: Robbert van Dalen
//

object Test {

  val wcontext = WeakMemoizationContext(new WeakHashMap())
  val econtext = EmptyContext

  final def main(args: Array[String]): Unit = {

    val k1 = 1 :: 2 :: 3 :: 4 :: 5 :: 6
    val k2 = 1 :: 2 :: 3 :: 8 :: 5 :: 6

    val sum1 = %(sum,expr(k1))
    val sum2 = %(sum,expr(k2))

    traceReuse = true

    val (r1,_) = fullEval(sum1,wcontext)
    println(r1)

    var (r2,_) = fullEval(sum2,wcontext)
    println(r2)

    val fib1 = %(fib,5)

    var (s1,c) = fullEval(fib1,econtext)
    println("slow: " + s1)

    var (s2,c2) = fullEval(fib1,wcontext)
    println("fast: " + s2)

    val fib2 = %(fib,8)
    var (s3,c3) = fullEval(fib2,wcontext)
    println("fib(8): " + s3.head)
    println("trace size: " + s3.trace.size)

    if (s1 != s2) { sys.error("Internal inconsistency") }  // the traces must be structurally equal
  }

  object fac extends FA1[Int,Int] {
    def apply(i: II): I = {

      if (!i == 0) 1
      else i !* %(fac,!i - 1)

    }
    override def toString = "fac"
    def codeID = 100
  }

  object fib extends FA1[Int,Int] {
    def apply(i: II) = {

      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)

    }
    override def toString = "fib"
    def codeID = 101
  }

  type INode = SHNode[Int]
  type FINode = F0[SHNode[Int]]

  object sum extends FA1[INode,Int] {
    def apply(s: FINode) = {

      val ss = !s
      if (ss.size == 1) ss.last
      else {
        val parts = ss.parts
        var ssum = %(sum,expr(parts(0)))
        var i = 1
        while (i < parts.length) {
          ssum = ssum !+ %(sum,expr(parts(i)))
          i = i + 1
        }
        ssum
      }

    }
    override def toString = "sum"
    def codeID = 1000
  }
}
