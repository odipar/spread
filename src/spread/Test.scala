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
      else i ** %(fac,i.eval - 1)
    }
    override def toString = "fac"
  }

  object fib extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) 1
      else %(fib,i.eval-1) + %(fib,i.eval-2)
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    val e1 = (1:I) + 2
    val e2 = (1:I) + 4
    val ee = e1 * e2
    println(spread(ee).reduce(1).expr)
  }
}

