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

  val fac: (I => I) = new (I => I) {
    def apply(i: I): I = {
      if (i.eval < 2) 1
      else i ** %(fac,i.eval-1)
    }
    override def toString = "fac"
  }
  final def main(args: Array[String]): Unit =
  {
    val f = %(fac,10)
    println("fac: " + f.reduce)
  }

}
