package spread

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import IncrementalMemoization._
  import IncrementalArithmetic._

  object fac extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) 1
      else i * %(fac,i.eval - 1)
    }
    override def toString = "fac"
  }

  object fib extends (I=>I) {
    def apply(i: I): I = {
      val ii = i.eval
      if (ii < 2) 1
      else %(fib,ii.eval-1) + %(fib,ii.eval-2)
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    val e1 = %(fac, 10)
    println(reduce(1, e1).expr.maxStage)
  }
}

