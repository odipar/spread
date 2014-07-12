package spread

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {
  import IncrementalMemoization._
  import scala.language.implicitConversions
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
      else %(fib,i.eval-1) ++ %(fib,i.eval-2)
    }
    override def toString = "fib"
  }

  final def main(args: Array[String]): Unit = {
    /* val kk = 'e~('a~1 ++ 'b~2) ** 'f~('c~3 ++ 'd~4)
    val kkk = ('a~1 ++ 2) ** (3 ++ 4)
    println(spread(kkk).trace)

    val id = StringID("abcde")
    val id2 = StringID("abcde")

    println("id: " + id.next.current)
    println("id2: " + id2.next.current) */
    val l = labelExpr(1)
    var e = (l ++ 2) ** (3 ++ 4)

    println(e.reduce.reduce)
    //println(parentPaths(e,l))
  }
}

