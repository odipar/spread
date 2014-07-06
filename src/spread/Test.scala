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
      if (i.eval < 2) Var("y",1)
      else i ** %(fac,i.eval - 1)
    }
    override def toString = "fac"
  }

  object fib extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) i
      else %(fib,i.eval-1) ++ %(fib,i.eval-2)
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    val x = "x" // argument x
    val y = "y" // argument y

    val e1 = Var(x,1) + 2
    val e2 = Var(y,3) + 4

    val e3 = e1 * e2

    val e4 = e3(x,2)
    val e5 = e3(x,3)

    val e6 = e3(y,1)
    val e7 = e3(y,2)

    val e9 = (e4 + e5) + (e6 + e7)

    val e10 = %(fac,Var(x,10))

    println(spread(e10).$.$(y,5))
  }

}

