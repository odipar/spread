package org.spread.core.experiment.sequence

import org.spread.core.experiment.expression.Spread.FA1
import org.spread.core.experiment.sequence.Sequence.Seq
import org.spread.core.experiment.expression.Spread._
import org.spread.core.language.Annotation.sp
import spire.algebra.Order

import scala.collection.immutable.HashMap
import scala.collection.mutable


object Test {
  import TreeSequence._

  final def main(args: Array[String]): Unit = {
    val longFactory = emptyTree[Long]

    val s1 = longFactory.createSeq((1000 until 20000000).map(x => x.toLong).toArray)
    val s2 = longFactory.createSeq((-20000000 until 1010).map(x => x.toLong).toArray)
    val s3 = s2 ++ longFactory.createSeq((1010 until 1020).map(x => x.toLong).toArray)
    
    val ct = new StrongMemoizationContext(mutable.HashMap())

    println("start 1")
    
    val r1 = rangeOfSeq()(s1).eval(ct)
    val r2 = rangeOfSeq()(s2).eval(ct)

    println("start 2")
    val c1 = cartesianProduct()((s1, 0L), (s2, 0L))
    val e1 = c1.eval(ct)

    println("start 3")
    val c2 = cartesianProduct()((s1, 0L), (s3, 0L))
    val e2 = c2.eval(ct)
    
    println("e1: " + e1.head)
    println("e2: " + e2.head)

  }

  case class Range(start: Long, end: Long) {
    def union(o: Range): Range = Range(start min o.start, end max o.end)
    def intersect(o: Range): Range = Range(start max o.start, end min o.end)
    def isEmpty: Boolean = start > end
  }
  
  type ASeq = Seq[X, S] forSome { type X; type S <: Seq[X, S] }

  trait Constraint[X, S1 <: Seq[X, S1], S2 <: Seq[X, S2]]

  trait Index {
    def size: Long
    def parts: Array[Index]
  }

  object EmptyIndex extends Index {
    def size: Long = 0
    def parts: Array[Index] = Array()
    override def toString: String = "~"
  }

  case class LeafIndex(indices: Array[Long]) extends Index {
    def size: Long = indices.length
    def parts: Array[Index] = Array(this)
    override def toString: String = indices.foldLeft("<")((x, y) => x + " " + y) + ">"
  }

  case class RangeIndex(from: Long, to: Long) extends Index {
    def size: Long = to - from
    def parts: Array[Index] = Array(this)
  }

  def branchIndex(parts: List[Index]): Index = {
    var result: List[Index] = List()
    for (p <- parts) p match {
      case EmptyIndex =>
      case _ => result = p +: result
    }

    val s = result.size

    if (s == 0) EmptyIndex
    else if (s == 1) result.head
    else BranchIndex(result.reverse.toArray)
  }

  case class BranchIndex(parts: Array[Index]) extends Index {
    val size: Long = parts.map(x => x.size).sum
    override def toString: String = parts.foldLeft("|")((x, y) => x + " " + y) + "|"
  }


  case class cartesianProduct[S1 <: Seq[Long, S1], S2 <: Seq[Long, S2]]() extends FA2[(S1, Long), (S2, Long), (Index, Index)] {
    def apply(s1: (S1, Long), s2: (S2, Long)): Expr[(Index, Index)] = {
      val r1 = %(combine[S1](), s1._1, %(rangeOfSeq[S1](), s1._1), s1._2)
      val r2 = %(combine[S2](), s2._1, %(rangeOfSeq[S2](), s2._1), s2._2)

      cartesianProduct2()(r1, r2)
    }
  }

  var ct: Long = 0
  
  case class cartesianProduct2[S1 <: Seq[Long, S1], S2 <: Seq[Long, S2]]() extends FA2[(S1, Range, Long), (S2, Range, Long), (Index, Index)] {
    def apply(ss1: (S1, Range, Long), ss2: (S2, Range, Long)): Expr[(Index, Index)] = {
      val s1 = ss1._1
      val s2 = ss2._1

      if ((ss1._2 intersect ss2._2).isEmpty) (EmptyIndex, EmptyIndex)
      else {
        if (s1.parts.length == 1) {
          if (s2.parts.length == 1) {
            val o1 = ss1._3
            val o2 = ss2._3

            val v1: Array[Long] = s1.toArray
            val v2: Array[Long] = s2.toArray

            var i1: List[Long] = List()
            var i2: List[Long] = List()

            for (x1 <- v1.indices) {
              for (x2 <- v2.indices) {
                ct = ct + 1
                if (v1(x1) == v2(x2)) {
                  i1 = (o1 + x1.toLong) +: i1
                  i2 = (o2 + x2.toLong) +: i2
                }
              }
            }

            (LeafIndex(i1.reverse.toArray), LeafIndex(i2.reverse.toArray))
          }
          else swap()(cartesianProduct2()(ss2, ss1))
        }
        else {
          var offset: Long = ss1._3
          val products = s1.parts.toList.map(
            x => {
              val prod = %(cartesianProduct[S1, S2](), (x, offset), (s2, ss2._3))
              offset += x.size
              prod
            }
          )
          %(branchIndex, flatten()(products))
        }
      }
    }
  }

  case class swap[@sp A, @sp B]() extends FA1[(A, B), (B, A)] {
    def apply(i: (A, B)): Expr[(B, A)] = (i._2, i._1)
  }

  object branchIndex extends FA1[List[(Index, Index)], (Index, Index)] {
    def apply(a: List[(Index, Index)]): Expr[(Index, Index)] = {
      (branchIndex(a.map(x => x._1)), branchIndex(a.map(x => x._2)))
    }
  }

  case class flatten[@sp X]() extends FA1[List[Expr[X]], List[X]] {
    val el: element[X] = element[X]()
    def apply(a: List[Expr[X]]): Expr[List[X]] = {
      if (a.isEmpty) List()
      else concat()(el(a.head), this(a.tail))
    }
  }

  case class concat[@sp X]() extends FA2[List[X], List[X], List[X]] {
    def apply(x1: List[X], x2: List[X]): Expr[List[X]] = x1 ++ x2
  }

  case class element[@sp X]() extends FA1[X, List[X]] {
    def apply(x: X): Expr[List[X]] = List(x)
  }

  case class combine[S <: Seq[Long, S]]() extends FA3[S, Range, Long, (S, Range, Long)] {
    def apply(s: S, i: Range, o: Long): Expr[(S, Range, Long)] = (s, i, o)
  }

  case class rangeOfSeq[S <: Seq[Long, S]]() extends FA1[S, Range] {
    def apply(x: S): Expr[Range] = {

      if (x.parts.length == 1) {
        val values = x.toArray
        Range(values.min, values.max)
      }
      else %(rangeOfRanges, x.parts.map(p => %(this, p)).toList)
    }
  }

  object rangeOfRanges extends FA1[List[Expr[Range]], Range] {
    def apply(x: List[Expr[Range]]): Expr[Range] = {

      if (x.lengthCompare(1) == 0) x.head
      else %(mergeRanges, x.head, %(rangeOfRanges, x.tail))
    }
  }

  object mergeRanges extends FA2[Range, Range, Range] {
    def apply(r1: Range, r2: Range): Expr[Range] = r1.union(r2)
  }
}
