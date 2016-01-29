package spread

import spread.SpreadArithmetic._
import spread.Spread._

//
// Copyright 2016: Robbert van Dalen
//

object Test {

  object fac extends FA1[Int,Int] {
    def codeHash = 100
    def apply(i: FI0): I = {
      if (i.value == 0) 1
      else i * %(fac,i.value-1)
    }
    override def toString = "fac"
  }

  object fib extends FA1[Int,Int] {
    def codeHash = 101
    def apply(i: FI0) = {
      val v = i.value

      if (v < 2) 1
      else %(fib,v-1) + %(fib,v-2)
    }
    override def toString = "fib"
  }

  object fib2 extends FA1[Int,Int] {
    def codeHash = 102
    def apply(i: FI0) = {
      val v = i.value

      if (v < 2) 1
      else F1(fib,v-1) + F1(fib,v-2)
    }
    override def toString = "fib2"
  }

  // Authenticated fibonacci with- and without memoization
  final def main(args: Array[String]): Unit = {

    val e = ((1:I) + 2) * ((3:I) + 4)
    val (r,_) = fullEval(e,EmptyContext)
    println(r)

    val fib27 = %(fib,27)

    val (s,c) = fullEval(fib27,EmptyContext)
    println("slow: " + s.trace.size)

    val (s2,c2) = fullEval(fib27,MapMemoizationContext(Map()))
    println("fast: " + s2.trace.size)

    if (s != s2) { sys.error("Internal inconsistency") }  // the traces should be structurally
  }
}
