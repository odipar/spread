package org.spread.core.sequence

import org.spread.core.annotation.Annotation.{Annotated, Annotator, RangeAnnotator}
import org.spread.core.constraint.Constraint.EqualProp
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence._
import org.spread.core.sequence.Sequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

/**
  * Created by rapido on 24/02/17.
  */
object AnnotatedSequence {

  trait AnnotatedSeq[@sp X,A,S <: AnnotatedSeq[X,A,S]] extends SeqImpl[X,S] with Annotated[A] {
    def annotationRange(start: Long, end: Long): A
    def approxAnnotationRange(start: Long, end: Long): A
    def annotationsForRange(start: Long, end: Long, a: Array[A])
    def equal: EqualProp[A]
  }

  trait AnnOrdSeq[@sp X,A,S <: AnnOrdSeq[X,A,S]] extends AnnotatedSeq[X,A,S] with OrderingSeq[X,S]

  // Annotated Seq with representation
  trait AnnSeqWithRepr[@sp X,A,S <: AnnSeqWithRepr[X,A,S]] extends AnnotatedSeq[X,A,S] {
    type AS <: AnnSeqRepr[X,A,S]

    def repr: S#AS

    def create(s: S#AS): S
    def append[S2 <: S](o: S2): S = create(repr.append(o.repr)(self))
    def split(o: Long): (S,S) = { val (l,r) = repr.split(o)(self); (create(l),create(r)) }
    def equalTo[S2 <: S](o: S2): Boolean = repr.equalToTree(o.repr)(self)
    def annotation = repr.annotation
    def annotationRange(start: Long, end: Long) = repr.annotationRange(start,end)(self)
    def approxAnnotationRange(start: Long, end: Long) = repr.approxAnnotationRange(start,end)(self)
    def annotationsForRange(start: Long, end: Long, a: Array[A]) = repr.annotationsForRange(start,end,a)(self)
    def size = repr.size
    def height = repr.height+1
    def first = repr.first(self)
    def last = repr.last(self)
    def apply(i: Long) = repr.apply(i)(self)
  }

  trait AnnOrdSeqWithRepr[@sp X,A,S <: AnnOrdSeqWithRepr[X,A,S]]
    extends AnnSeqWithRepr[X,A,S] with AnnOrdSeq[X,A,S]

  trait AnnSeqRepr[@sp X,A,S <: AnnSeqWithRepr[X,A,S]] extends Annotated[A] {
    def size: Long
    def height: Int
    def append[AS <: S#AS](o: AS)(implicit c: S): S#AS
    def split(o: Long)(implicit c: S): (S#AS,S#AS)
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def annotationRange(start: Long, end: Long)(implicit c: S): A
    def approxAnnotationRange(start: Long, end: Long)(implicit c: S): A
    def annotationsForRange(start: Long, end: Long, a: Array[A])(implicit c: S)
    def first(implicit c: S): X
    def last(implicit c: S): X
    def apply(i: Long)(implicit c: S): X
  }

  trait AnnotationContext[@sp X,A]
  {
    type ANN <: Annotator[X,A]
    def ann: ANN
    def eq: EqualProp[A]
    def xTag: ClassTag[X]
    def aTag: ClassTag[A]
  }

  class AnnContextImpl[@sp X,A]
  (val ann: Annotator[X,A], val eq: EqualProp[A], val xTag: ClassTag[X], val aTag: ClassTag[A]) extends AnnotationContext[X,A] {
    type ANN = Annotator[X,A]
  }

  trait AnnotationOrderingContext[@sp X,A] extends AnnotationContext[X,A]
  {
    def ord: Order[X]
  }

  trait RangedAnnotationOrderingContext[@sp X,A] extends AnnotationOrderingContext[X,A] {
    type ANN <: RangeAnnotator[X,A]
  }

  class AnnOrdContextImpl[@sp X,A]
  (val ann: Annotator[X,A], val eq: EqualProp[A], val ord: Order[X], val xTag: ClassTag[X], val aTag: ClassTag[A])
    extends AnnotationOrderingContext[X,A] {
    type ANN = Annotator[X,A]
  }

  class RangedAnnOrdContextImpl[@sp X,A]
  (val ann: RangeAnnotator[X,A], val eq: EqualProp[A], val ord: Order[X], val xTag: ClassTag[X], val aTag: ClassTag[A])
    extends RangedAnnotationOrderingContext[X,A] {
    type ANN = RangeAnnotator[X,A]
  }
}
