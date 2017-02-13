package org.spread.core.sequence
import Sequence._
import org.spread.core.constraint.Constraint.EqualProp
import org.spread.core.sequence.PairedSequence.OrderingBinContext
import scala.language.{existentials, implicitConversions}

object AnnotatedSequence {
  type ASEQ[X,A,S <: AnnotatedSeq[X,A,S,TC], TC <: AnnotationContext[X,A]] = AnnotatedSeq[X,A,S,TC]

  trait AnnotationContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends Context[X] {
    def equal: EqualProp[A]
  }
  trait OrderingAnnContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
    extends OrderingContext[X] with AnnotationContext[X,A]
  
  trait AnnotatedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: ASEQ[X,A,S,TC], TC <: AnnotationContext[X,A]]
    extends SeqImpl[X,S,TC] {

    def annotation: A
    def annotationRange(start: Long, end: Long): A
  }

  trait AnnotatedSeqImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) XA,SX <: AnnotatedSeqImpl[X,XA,SX,CX], CX <: AnnotationContext[X,XA]]
    extends AnnotatedSeq[X,XA,SX,CX] {

    type AS <: ASeq[X,XA,SX,CX]

    def sequence: SX#AS
    def create(s: SX#AS): SX

    def append[S2 <: SX](o: S2): SX = create(sequence.append(o.sequence)(self))
    def equalTo[S2 <: SX](o: S2): Boolean = sequence.equalToTree(o.sequence)(self)
    def split(o: Long) = { val (l,r) = sequence.split(o)(self) ; (create(l),create(r)) }
    def some = sequence.some

    def annotation: XA = sequence.annotation(self)
    def annotationRange(start: Long, end: Long) = sequence.annotationRange(start,end)(self)
    def size = sequence.size
    def height = sequence.height + 1
    def combine[@specialized(Int,Long,Double) Y,@specialized(Int,Long,Double) YA,SY <: AnnotatedSeq[Y,YA,SY,CY],AA,CY <: AnnotationContext[Y,YA],C <: AnnotationContext[(X,Y),AA]]
    (o: AnnotatedSeq[Y,YA,SY,CY])(implicit c: C, f: (XA,YA)=>AA) = {
      PairedSequence.AnnPairedSeq[X,Y,XA,YA,AA,SX,SY,CX,CY,C](self,o.asInstanceOf[SY])
    }
   /* def &&[@specialized(Int,Long,Double) Y,@specialized(Int,Long,Double) YA,SY <: AnnotatedSeq[Y,YA,SY,TC],AA]
    (o: AnnotatedSeq[Y,YA,SY,TC])(implicit c: OrderingBinContext[X,Y], f: (A,YA)=>AA) = combine(o) */
  }

  type OASEQ[X,A,S <: AnnotatedSeq[X,A,S,TC], TC <: OrderingAnnContext[X,A]] = AnnotatedSeq[X,A,S,TC]

  trait OrderedAnnSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: OrderedAnnSeq[X,A,S,TC], TC <: OrderingAnnContext[X,A]]
    extends AnnotatedSeqImpl[X,A,S,TC]

  trait ASeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeqImpl[X,A,S,TC], TC <: AnnotationContext[X,A]] {
    def size: Long
    def height: Int
    def some: X
    def append[AS <: S#AS](o: AS)(implicit s: S): S#AS
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def split(o: Long)(implicit s: S): (S#AS,S#AS)
    def annotation(implicit s: S): A
    def annotationRange(start: Long, end: Long)(implicit c: S): A
  }
}
