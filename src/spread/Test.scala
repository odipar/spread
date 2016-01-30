package spread

import spread.SpreadArithmetic._
import spread.Spread._
import SplitHash._
import scala.collection.mutable.WeakHashMap

//
// Copyright 2016: Robbert van Dalen
//

object Test {

  object fac extends FA1[Int,Int] {
    def apply(i: II): I = {

      if (!i == 0) 1
      else i !* %(fac,!i - 1)

    }
    override def toString = "fac"
    def codeHash = 100
  }

  object fib extends FA1[Int,Int] {
    def apply(i: II) = {

      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)

    }
    override def toString = "fib"
    def codeHash = 101
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
    def codeHash = 1000
  }


  val wcontext = WeakMemoizationContext(new WeakHashMap())

  // Authenticated re-usable computations = authenticated spreadsheets!
  final def main(args: Array[String]): Unit = {

    val k1 = 1 :: 2 :: 3 :: 4 :: 5 :: 6
    val k2 = 1 :: 2 :: 3 :: 8 :: 5 :: 6

    val e1 = %(sum,expr(k1))
    val e2 = %(sum,expr(k2))

    traceReuse = true
    val (r1,_) = fullEval(e1,wcontext)
    println(r1)
    var (r2,_) = fullEval(e2,wcontext)
    println(r2)

    val fib1 = %(fib,5)

    var (s1,c) = fullEval(fib1,EmptyContext)
    println("slow: " + s1)

    var (s2,c2) = fullEval(fib1,wcontext)
    println("fast: " + s2)

    val fib2 = %(fib,8)
    var (s3,c3) = fullEval(fib2,wcontext)
    println("fib(8): " + s3.head)
    println("trace size: " + s3.trace.size)

    if (s1 != s2) { sys.error("Internal inconsistency") }  // the traces must be structurally equal
  }
}
