package spread

import scala.collection.mutable.WeakHashMap
import java.lang.ref.WeakReference

/*
 Copyright 2013: Robbert van Dalen
 */

object Test {
  import language.implicitConversions
  import IncrementalMemoization._
  import IncrementalTreap._
  import IncrementalArithmetic._
  import IncrementalTreeView._

  import javax.swing.tree._
  import javax.swing._
  import java.awt._

  object fac extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 2) 1
      else i * %(fac,i.eval-1)
    }
    override def toString = "fac"
  }
  object fib extends (I=>I) {
    def apply(i: I): I = {
      if (i.eval < 3) 1
      else %(fib,i.eval-1) ++ %(fib,i.eval-2)
    }
    override def toString = "fib"
  }
  final def main(args: Array[String]): Unit =
  {
    val f = %(fib,10)
    println(f.reduce.reduce)
  }

}
