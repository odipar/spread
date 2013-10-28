package spread

/*

 Copyright 2013: Robbert van Dalen

 */

object Test {
  import IncrementalMemoization._

  // Proof of concept works!
  final def main(args: Array[String]): Unit = {}
  {
    var f1 = $(fib,100)
    println("f1: " + f1())
    println("tot: " + totalt)

    var f2 = $(fib,110)
    println("f2: " + f2())
    println("tot: " + totalt)

    f1 = null
    f2 = null
    System.gc()

    var f3 = $(fib,100)
    println("f3: " + f3())
    println("tot: " + totalt)
  }
}
