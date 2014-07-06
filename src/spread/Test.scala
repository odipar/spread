package spread

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import IncrementalMemoization._
  import IncrementalArithmetic._
  import IncrementalTreap._

  object fac extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) 1
      else i ** %(fac,i.eval - 1)
    }
    override def toString = "fac"
  }

  object fib extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) i
      else %(fib,i.eval-1) + %(fib,i.eval-2)
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    val e1 = V("a",1) ++ 2
    val e2 = (3:I) ++ V("b",4)
    val e3 = e1 ** e2
    val e4 = e3("a",1,3)
    val e5 = e3("a",1,4)
    val e6 = e3("a",1,5)
    val e7 = e3("a",1,6)
    val e9 = (e4 ++ e5) ++ (e6 ++ e7)
    println(e9)

    println("e3: " + spread(e9).reduce(1))

  }


}

