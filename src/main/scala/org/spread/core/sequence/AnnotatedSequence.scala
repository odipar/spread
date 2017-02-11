package org.spread.core.sequence
import Sequence._
import org.spread.core.constraint.Constraint.EqualProp
import org.spread.core.sequence.PairedSequence.OrderingBinContext
import scala.language.{existentials, implicitConversions}

object AnnotatedSequence {
  type ASEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]

  trait OrderingAnnContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends OrderingContext[X] {
    def equal: EqualProp[A]
  }
  
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
    def some = sequence.some

    def annotation: A = sequence.annotation(self)
    def annotationRange(start: Long, end: Long) = sequence.annotationRange(start,end)(self)
    def size = sequence.size
    def height = sequence.height + 1
    def combine[@specialized(Int,Long,Double) Y,@specialized(Int,Long,Double) YA,SY <: AnnotatedSeq[Y,YA,SY],AA]
    (o: AnnotatedSeq[Y,YA,SY])(implicit c: OrderingBinContext[X,Y], f: (A,YA)=>AA) = {
      PairedSequence.AnnPairedSeq[X,Y,A,YA,AA,S,SY](self,o.asInstanceOf[SY])
    }
    def &&[@specialized(Int,Long,Double) Y,@specialized(Int,Long,Double) YA,SY <: AnnotatedSeq[Y,YA,SY],AA]
    (o: AnnotatedSeq[Y,YA,SY])(implicit c: OrderingBinContext[X,Y], f: (A,YA)=>AA) = combine(o)
  }

  type OASEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S] { type TC <: OrderingContext[X] }

  trait OrderedAnnSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: OrderedAnnSeq[X,A,S]]
    extends AnnotatedSeqImpl[X,A,S] {
    type TC <: OrderingContext[X]
  }

  trait ASeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeqImpl[X,A,S]] {
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
