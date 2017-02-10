package org.spread.core.sequence

import org.spread.core.algorithm.Combine._
import org.spread.core.annotation.Annotation.{Statistics, createStats}
import org.spread.core.sequence.Sequence._
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._

object PairedSequence {

  trait PairedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,S1 <: Seq[X,S1],S2 <: Seq[Y,S2], S <: PairedSeq[X,Y,S1,S2,S]]
    extends SeqImpl[(X,Y),S] {

    {assert(left.size == right.size)}

    def left: S1
    def right: S2
    def create(l: S1,r: S2): S
    def emptySeq: S = create(left.emptySeq,right.emptySeq)
    def size: Long = left.size
    def split(o: Long): (S,S) = {
      val (ll,lr) = left.split(o)
      val (rl,rr) = right.split(o)
      (create(ll,rl),create(lr,rr))
    }
    def append[SS <: S](o: SS): S = create(left.append(o.left),right.append(o.right))
    def equalTo[SS <: S](o: SS): Boolean = left.equals(o.left) && right.equals(o.right)
  }

  case class BinSeqImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, S1 <: Seq[X,S1],S2 <: Seq[Y,S2]]
  (left: S1, right: S2) extends PairedSeq[X,Y,S1,S2,BinSeqImpl[X,Y,S1,S2]] {
    type TC = NoContext
    def context = NoContext

    type S = BinSeqImpl[X,Y,S1,S2]
    val height = (left.height max right.height) + 1
    def self: S = this
    def create(l: S1, r: S2): S = BinSeqImpl(l,r)
    def some = (left.some,right.some)
  }

  case class DualOrdering[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (x: Ordering[X], y: Ordering[Y]) extends Ordering[(X,Y)] {

    def compare(o1: (X,Y), o2: (X,Y)): Int = {
      val c = x.compare(o1._1,o2._1)
      if (c == 0) y.compare(o1._2,o2._2)
      else c
    }
  }
  
  case class OrderingBinContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (o: DualOrdering[X,Y])  extends OrderingContext[(X,Y)]() { def ord = o }

  case class AnnPairedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,XA,YA,AA,S1 <: AnnotatedSeq[X,XA,S1],S2 <: AnnotatedSeq[Y,YA,S2]]
  (left: S1, right: S2)(implicit c: OrderingBinContext[X,Y], f:(XA,YA)=>AA)
    extends PairedSeq[X,Y,S1,S2,AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]]
      with AnnotatedSeq[(X,Y),AA,AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]] {
    type S = AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]
    type TC = OrderingBinContext[X,Y]

    val height = (left.height max right.height) + 1
    def self: S = this
    def context = c
    def some = (left.some,right.some)
    def create(l: S1, r: S2): S = AnnPairedSeq(l,r)
    def annotation = f(left.annotation,right.annotation)
    def annotationRange(start: Long, end: Long) = f(left.annotationRange(start,end),right.annotationRange(start,end))
  }

  implicit def ordBinContext[X,Y](implicit xo: Ordering[X], yo: Ordering[Y]): OrderingBinContext[X,Y] = {
    OrderingBinContext(DualOrdering(xo,yo))
  }

  implicit def dualStats[X,Y]: (Statistics[X],Statistics[Y]) => Statistics[(X,Y)] = {
    (x: Statistics[X],y: Statistics[Y]) => createStats(
      (x.lowerBound,y.lowerBound),
      (x.upperBound,y.upperBound),
      (x.first,y.first),
      (x.last,y.last),
      x.sorted && y.sorted
    )
  }

  final def main(args: Array[String]): Unit = {
    val factory = EmptyAnnotatedTreeSeq[Int,Statistics[Int]]()
    val factory2 = EmptyAnnotatedTreeSeq[Double,Statistics[Double]]()
    var b = factory.emptySeq
    var b2 = factory2.emptySeq

    for (i <- 1 to 4) { b = b :+ -i }
    for (i <- 1 to 4 ) { b2 = b2 :+ i }

    val p = b && b2    // combine
    val p2 = p ++ p    // append
    //val p3 = p2 :+: p2 // union
    //println("p3: " + sort3(p3))
  }

}
