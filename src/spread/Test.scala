package spread

import spread.SpreadArithmetic._
import spread.Spread._
import SplitHash._
import scala.collection.mutable.WeakHashMap
import SetHash._

// EXPOSITION:
//
// Authenticated re-usable computations = authenticated spreadsheets?
//
// Copyright 2016: Robbert van Dalen
//

object Test {

  val wcontext = WeakMemoizationContext(new WeakHashMap())
  val econtext = EmptyContext

  final def main(args: Array[String]): Unit = {

    val seq1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
    val seq2 = 1 ! 2 ! 3 ! 9 ! 5 ! 6 ! 7 ! 8

    val sum1 = %(sum,expr(seq1))
    val sum2 = %(sum,expr(seq2))

    traceReuse = true

    val (r1,_) = fullEval(sum1,wcontext)
    println(r1)

    var (r2,_) = fullEval(sum2,wcontext)
    println(r2)

    val fib1 = %(fib,5)

    var (f1,_) = fullEval(fib1,econtext)
    println("slow: " + f1)

    var (f2,_) = fullEval(fib1,wcontext)
    println("fast: " + f2)

    if (f1 != f2) { sys.error("Internal inconsistency") }  // the traces must be structurally equal

    println()

    val fac1 = %(fac,5)
    var (fc1,_) = fullEval(fac1,wcontext)
    println("fac(5): " + fc1.head)

    val fac2 = %(fac,7)
    var (fc2,_) = fullEval(fac2,wcontext)
    println("fac(7): " + fc2.head)
    println()

    val fib2 = %(fib,15)
    var (f3,_) = fullEval(fib2,wcontext)
    println("fib(8): " + f3.head)
    println("trace size: " + f3.trace.size)

    // Some other radical stuff - NOT YET DONE
    var st = HashNode(Array(),0)
    val m = store(f3.trace,st)
    println("st: " + m.size)
  }

  object fac extends FA1[Int,Int] {
    def apply(i: II): I = {

      if (!i == 0) 1
      else i !* %(fac,!i - 1)

    }
    override def toString = "fac"
    def codeID = 100
  }

  object fib extends FA1[Int,Int] {
    def apply(i: II) = {

      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)

    }
    override def toString = "fib"
    def codeID = 200
  }

  type INode = SHNode[Int]
  type FINode = F0[SHNode[Int]]

  // We could easily have implemented sum with a generic fold
  // But for now we just explicitly show how to use the DSL and API
  object sum extends FA1[INode,Int] {
    def apply(s: FINode) = {

      val ss = !s
      if (ss.size == 1) ss.last
      else {
        val parts = ss.splitParts
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
    def codeID = 1000
  }

  import Hashing._

  def store[X](node: Hashable, h: HashNode): Map[FiniteHash,Hashable] = {
    val hh = store2(node,h)
    store3(node,hh,Map())
  }

  def store2[X](node: Hashable, h: HashNode): HashNode = {
    var hh = h
    for (p <- node.parts) hh = store2(p,hh.put(node).put(p))
    hh
  }

  def store3[X](node: Hashable, h: HashNode, m: Map[FiniteHash,Hashable]): Map[FiniteHash,Hashable] = {
    var mm = m
    for (p <- node.parts)  mm = store3(p,h,mm + (h.hash(p)->p) + (h.hash(node)->node))
    mm
  }
}
