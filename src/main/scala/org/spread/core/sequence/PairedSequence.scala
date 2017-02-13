package org.spread.core.sequence

import org.spread.core.algorithm.Combine._
import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint._
import org.spread.core.sequence.Sequence._
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.RangedSequence.EmptyLongTreeSeq

import scala.language.{existentials, implicitConversions}

object PairedSequence {

  case class DualOrdering[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (x: Ordering[X], y: Ordering[Y]) extends Ordering[(X,Y)] {

    def compare(o1: (X,Y), o2: (X,Y)): Int = {
      val c = x.compare(o1._1,o2._1)
      if (c == 0) y.compare(o1._2,o2._2)
      else c
    }
  }

  case class OrderingBinContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (o: DualOrdering[X,Y]) extends OrderingContext[(X,Y)]() { def ord = o }

  trait PairedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,S1 <: Seq[X,S1,CX],S2 <: Seq[Y,S2,CY], CX <: Context[X], CY <: Context[Y], S <: PairedSeq[X,Y,S1,S2,CX,CY,S,TC], TC <: Context[(X,Y)]]
     extends SeqImpl[(X,Y),S,TC] {

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

   case class BinSeqImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, S1 <: Seq[X,S1,CX],S2 <: Seq[Y,S2,CY], CX <: Context[X], CY <: Context[Y]]
   (left: S1, right: S2) extends PairedSeq[X,Y,S1,S2,CX,CY,BinSeqImpl[X,Y,S1,S2,CX,CY],NoContext] {

     type S = BinSeqImpl[X,Y,S1,S2,CX,CY]

     def context = NoContext
     val height = (left.height max right.height) + 1
     def self: S = this
     def create(l: S1, r: S2): S = BinSeqImpl(l,r)
     def some = (left.some,right.some)
   }

  /*
      extends PairedSeq[X,Y,S1,S2,AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]]
      with AnnotatedSeq[(X,Y),AA,AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]] {
   */

   case class AnnPairedSeq[
   @specialized(Int,Long,Double) X,
   @specialized(Int,Long,Double) Y,
   XA,
   YA,
   AA,
   SX <: AnnotatedSeq[X,XA,SX,CX],
   SY <: AnnotatedSeq[Y,YA,SY,CY],
   CX <: AnnotationContext[X,XA],
   CY <: AnnotationContext[Y,YA],
   TC <: AnnotationContext[(X,Y),AA]]
   (left: SX, right: SY)(implicit c: TC, f:(XA,YA)=>AA)
     extends PairedSeq[X,Y,SX,SY,CX,CY,AnnPairedSeq[X,Y,XA,YA,AA,SX,SY,CX,CY,TC],TC]
       with AnnotatedSeq[(X,Y),AA,AnnPairedSeq[X,Y,XA,YA,AA,SX,SY,CX,CY,TC],TC] {

     type S = AnnPairedSeq[X,Y,XA,YA,AA,SX,SY,CX,CY,TC]

     val height = (left.height max right.height) + 1
     def self: S = this
     def context = c
     def some = (left.some,right.some)
     def create(l: SX, r: SY): S = AnnPairedSeq(l,r)
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

  /* type OSEQ[X,XA,S <: AnnotatedSeq[X,XA,S]] = AnnotatedSeq[X,XA,S]
   type BinRel[X,Y,XA <: PropValue,YA <: PropValue,AA,S1 <: OSEQ[X,XA,S1],S2 <: OSEQ[Y,YA,S2]] = AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]
   type EREL = BinRel[X,Y,XA,YA,AA,S1,S2] forSome {
     type X ;  type XA <: PropValue ; type Y ; type YA <: PropValue ; type AA
     type S1 <: OSEQ[X,XA,S1] ; type S2 <: OSEQ[Y,YA,S2] ; type S <: AnnPairedSeq[X,Y,XA,YA,AA,S1,S2]
   }*/

   /*sealed trait ColumnPos

   object LeftCol extends ColumnPos

   object RightCol extends ColumnPos

   sealed trait RelCol[X,XA] {
     def id: Symbol
     def column: ColumnPos
   }

   case class LeftCol[Y,YA](id: Symbol) extends RelCol[Y,YA] {
     def column = LeftCol
     override def toString: String = id + ".L"
   }

   case class RightCol[X,XA](id: Symbol) extends RelCol[X,XA] {
     def column = RightCol
     override def toString: String = id + ".R"
   }

   sealed trait RCol[X,XA <: PropValue] {
     def rel: EREL
     def column: ColumnPos
     def withID(s: Symbol): RelCol[X,XA]
   }

   case class RightRCol[X,Y,XA <: PropValue,YA <: PropValue,AA,S1 <: OSEQ[X,XA,S1],S2 <: OSEQ[Y,YA,S2]]
   (r: BinRel[X,Y,XA,YA,AA,S1,S2]) extends RCol[Y,YA] {
     def rel: EREL = r
     def column = RightCol
     def withID(s: Symbol): RelCol[Y,YA] = RightCol(s)
   }

   case class LeftRCol[X,Y,XA <: PropValue,YA <: PropValue,AA,S1 <: OSEQ[X,XA,S1],S2 <: OSEQ[Y,YA,S2]]
   (r: BinRel[X,Y,XA,YA,AA,S1,S2]) extends RCol[X,XA] {
     def rel: EREL = r
     def column = LeftCol
     def withID(s: Symbol): RelCol[X,XA] = LeftCol(s)
   }

   implicit def annotator[X](implicit ord: Ordering[X]): StatisticsAnnotator[X] = StatisticsAnnotator[X]()

   def createPairedSeq[X,Y]
   (x: Array[X], y: Array[Y])(implicit xc: OrderingTreeContext[X,Statistics[X]], yc: OrderingTreeContext[Y,Statistics[Y]], oc: OrderingBinContext[X,Y], f:(Statistics[X],Statistics[Y])=>Statistics[(X,Y)]) = {
     val xx = createAnnSeq(x)
     val yy = createAnnSeq(y)
     xx && yy
   }

   def createAnnSeq[X](x: Array[X])(implicit xc: OrderingTreeContext[X,Statistics[X]]) = {
     EmptyAnnotatedTreeSeq[X,Statistics[X]]().createSeq(x)
   }

   def createSeq[X](x: Array[X])(implicit xc: AnnTreeContext[X,NoAnnotation]) = {
     EmptyAnnotatedTreeSeq[X,NoAnnotation]().createSeq(x)
   }       */

   final def main(args: Array[String]): Unit = {
     val factory = EmptyAnnotatedTreeSeq[Int,Statistics[Int],OrderingTreeContext[Int,Statistics[Int]]]()
     val factory2 = factory //EmptyAnnotatedTreeSeq[Double,Statistics[Double],OrderingTreeContext[Double,Statistics[Double]]]()

     var b = factory.emptySeq
     var b2 = factory2.emptySeq

     for (i <- 1 to 4) { b = b :+ -i }
     for (i <- 1 to 4 ) { b2 = b2 :+ i }

     //val p = b combine b2    // combine
     /*val p2 = p ++ p    // append
     val p3 = p2 :+: p2 // union
     println("p3: " + p3.sort) */
   }

}
