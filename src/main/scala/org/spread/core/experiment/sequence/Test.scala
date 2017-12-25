package org.spread.core.experiment.sequence

import org.spread.core.experiment.expression.Spread.FA1
import org.spread.core.experiment.sequence.Sequence.Seq
import org.spread.core.experiment.expression.Spread._
import org.spread.core.language.Annotation.sp

object Test {
  import TreeSequence._
  import PairedSequence._

  final def main(args: Array[String]): Unit = {
    val longFactory = emptyTree[Long]

    val s1 = longFactory.createSeq((0 until 200).map(x => x.toLong).toArray)
    val s2 = s1 ++ longFactory.createSeq((10 until 20).map(x => -x.toLong).toArray)

    val r1 = F1[TreeSeq[Long], Range](rangeOfSeq(), s1)
    val r2 = F1[TreeSeq[Long], Range](rangeOfSeq(), s2)

    val c = MemozationContext(Map())
    //val c = NullContext
    
    println("start")
    println("nodes: " + nodes)

    val (e1, c2) = r1.eval(c)
    println("e1: " + e1)

    val (e2, c3) = r2.eval(c2)
    println("e2: " + e2)

    println("end")
    println("nodes: " + nodes)

  }

  case class Range(min: Long, max: Long)

  /*case class rangeOfSeqWithinRange[S <: Seq[Long, S]]() extends FA2[S, Range, Range] {
    def apply2(x: FA0[S], r: FA0[Range]): Expr[Range] = {
      val xx = !x
      var rr = !r

      if (rr.max >= xx.size) rangeOfSeqWithinRange[S](x, r)
      else r
    }
  } */
  case class rangeOfSeq[S <: Seq[Long, S]]() extends FA1[S, Range] {
    def apply2(x: FA0[S]): Expr[Range] = {
      val xx = !x
      
      if (xx.parts.length == 1) {
        val values = xx.toArray
        println("xx: " + xx)
        Range(values.min, values.max)
      }
      else %(rangeOfRanges, xx.parts.map(x => %(this, x)).toList)
    }
  }

  object rangeOfRanges extends FA1[List[Expr[Range]], Range] {
    def apply2(x: FA0[List[Expr[Range]]]): Expr[Range] = {
      val xx = !x

      if (xx.length == 1) xx(0)
      else {
        val (l, r) = xx.splitAt(xx.length / 2)
        %(mergeRanges, %(rangeOfRanges, l), %(rangeOfRanges, r))
      }
    }
  }

  object mergeRanges extends FA2[Range, Range, Range] {
    def apply2(r1: FA0[Range], r2: FA0[Range]): Expr[Range] = {
      val rr1 = !r1
      val rr2 = !r2

      Range(rr1.min min rr2.min, rr1.max max rr2.max)
    }
  }
}
