package org.spread.core.sequence

import org.spread.core.annotation.Annotation._

import scala.reflect.ClassTag

object Sequence {
  
  trait Context[@specialized(Int,Long,Double) +X]
  trait OrderingContext[@specialized(Int,Long,Double) X] extends Context[X] { def ord: Ordering[X] }
  
  trait Seq[@specialized(Int,Long,Double) +X,S <: Seq[X,S]] {
    type TC <: Context[X]

    def self: S
    def context: TC
    
    def size: Long

    def emptySeq: S
    def append[SS <: S](o: SS): S
    def split(o: Long): (S,S)
    def equalTo[SS <: S](o: SS): Boolean
  }

  trait SeqImpl[@specialized(Int,Long,Double) X,S <: SeqImpl[X,S]] extends Seq[X,S] {
    def combine[@specialized(Int,Long,Double) Y,SY <: Seq[Y,SY]](o: Seq[Y,SY]): BinSeqImpl[X,Y,S,SY] = BinSeqImpl[X,Y,S,SY](self,o.asInstanceOf[SY])
  }

  trait NoContext extends Context[Nothing]
  object NoContext extends NoContext

  case class VectorSeq[@specialized(Int,Long,Double) X: ClassTag](x: Vector[X]) extends SeqImpl[X,VectorSeq[X]] {
    type S = VectorSeq[X]
    type TC = NoContext

    def context = NoContext
    def self: S = this
    def size: Long = x.length

    def emptySeq = VectorSeq(Vector())
    def append[SS <: S](o: SS): S = VectorSeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (VectorSeq(l),VectorSeq(r)) }
    def equalTo[SS <: S](o: SS): Boolean = x.equals(o.x)
  }

  type ASEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]

  trait AnnotatedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: ASEQ[X,A,S]]
    extends SeqImpl[X,S] {
    def annotation: A
    def annotationRange(start: Long, end: Long): A
  }

  trait AnnotatedSeqImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeqImpl[X,A,S]]
    extends AnnotatedSeq[X,A,S] {

    type AS <: ASeq[X,A,S]

    def sequence: S#AS
    def create(s: S#AS): S

    def append[S2 <: S](o: S2): S = create(sequence.append(o.sequence)(self))
    def equalTo[S2 <: S](o: S2): Boolean = sequence.equalToTree(o.sequence)(self)
    def split(o: Long) = { val (l,r) = sequence.split(o)(self) ; (create(l),create(r)) }

    def annotation: A = sequence.annotation(self)
    def annotationRange(start: Long, end: Long) = sequence.annotationRange(start,end)(self)
    def size = sequence.size

    def combine[@specialized(Int,Long,Double) Y,@specialized(Int,Long,Double) YA,SY <: AnnotatedSeqImpl[Y,YA,SY],AA,C <: Context[(X,Y)]]
    (o: AnnotatedSeqImpl[Y,YA,SY])(implicit c: C, f: (A,YA)=>AA) = {
      AnnotatedBinSeq[X,Y,A,YA,AA,C,S,SY](self,o.asInstanceOf[SY])
    }
  }

  type OASEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S] { type TC <: OrderingContext[X] }

  trait OrderedAnnSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: OrderedAnnSeq[X,A,S]]
  extends AnnotatedSeqImpl[X,A,S] {
    type TC <: OrderingContext[X]
  }

  trait ASeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeqImpl[X,A,S]] {
    def size: Long
    def append[AS <: S#AS](o: AS)(implicit s: S): S#AS
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def split(o: Long)(implicit s: S): (S#AS,S#AS)
    def annotation(implicit s: S): A
    def annotationRange(start: Long, end: Long)(implicit c: S): A
  }

  trait BinSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,S1 <: Seq[X,S1],S2 <: Seq[Y,S2], S <: BinSeq[X,Y,S1,S2,S]]
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
  (left: S1, right: S2) extends BinSeq[X,Y,S1,S2,BinSeqImpl[X,Y,S1,S2]] {
    type TC = NoContext
    def context = NoContext
    
    type S = BinSeqImpl[X,Y,S1,S2]
    def self: S = this
    def create(l: S1, r: S2): S = BinSeqImpl(l,r)
  }

  trait BinContext[X,Y] extends Context[(X,Y)]

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

  case class AnnotatedBinSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,XA,YA,AA,C <: Context[(X,Y)],S1 <: AnnotatedSeq[X,XA,S1],S2 <: AnnotatedSeq[Y,YA,S2]]
  (left: S1, right: S2)(implicit c: C, f:(XA,YA)=>AA)
    extends BinSeq[X,Y,S1,S2,AnnotatedBinSeq[X,Y,XA,YA,AA,C,S1,S2]]
      with AnnotatedSeq[(X,Y),AA,AnnotatedBinSeq[X,Y,XA,YA,AA,C,S1,S2]] {
    type S = AnnotatedBinSeq[X,Y,XA,YA,AA,C,S1,S2]
    type TC = C

    def self: S = this
    def context = c
    def create(l: S1, r: S2): S = AnnotatedBinSeq(l,r)
    def annotation = f(left.annotation,right.annotation)
    def annotationRange(start: Long, end: Long) = f(left.annotationRange(start,end),right.annotationRange(start,end))
  }

  implicit def ordBinContext[X,Y](implicit xo: Ordering[X], yo: Ordering[Y]): OrderingContext[(X,Y)] = {
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
}