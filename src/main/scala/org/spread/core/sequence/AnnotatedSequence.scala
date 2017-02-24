package org.spread.core.sequence

import cats.Order
import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.constraint.Constraint.EqualProp
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence._
import org.spread.core.sequence.Sequence._

import scala.reflect.ClassTag

/**
  * Created by rapido on 24/02/17.
  */
object AnnotatedSequence {

  trait AnnotatedSeq[@sp X,A,S <: AnnotatedSeq[X,A,S]] extends SeqImpl[X,S] {
    def annotation: A
    def annotationRange(start: Long, end: Long): A
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
    def annotation = repr.annotation(self)
    def annotationRange(start: Long, end: Long) = repr.annotationRange(start,end)(self)
    def size = repr.size
    def height = repr.height+1
    def first = repr.first(self)
    def last = repr.last(self)
    def apply(i: Long) = repr.apply(i)(self)
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A = repr.annotate(annotator)
  }

  trait AnnOrdSeqWithRepr[@sp X,A,S <: AnnOrdSeqWithRepr[X,A,S]]
    extends AnnSeqWithRepr[X,A,S] with AnnOrdSeq[X,A,S]

  trait AnnSeqRepr[@sp X,A,S <: AnnSeqWithRepr[X,A,S]] {
    def size: Long
    def height: Int
    def append[AS <: S#AS](o: AS)(implicit c: S): S#AS
    def split(o: Long)(implicit c: S): (S#AS,S#AS)
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def annotation(implicit c: S): A
    def annotationRange(start: Long, end: Long)(implicit c: S): A
    def first(implicit c: S): X
    def last(implicit c: S): X
    def apply(i: Long)(implicit c: S): X
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A
  }

  trait AnnotationContext[@sp X,A]
  {
    def ann: Annotator[X,A]
    def eq: EqualProp[A]
    def xTag: ClassTag[X]
    def aTag: ClassTag[A]
  }

  case class AnnContextImpl[X,A]
  (ann: Annotator[X,A], eq: EqualProp[A], xTag: ClassTag[X], aTag: ClassTag[A]) extends AnnotationContext[X,A]

  trait AnnotationOrderingContext[X,A] extends AnnotationContext[X,A]
  {
    def ord: Order[X]
  }

  case class AnnOrdContextImpl[X,A]
  (ann: Annotator[X,A], eq: EqualProp[A], ord: Order[X], xTag: ClassTag[X], aTag: ClassTag[A])
    extends AnnotationOrderingContext[X,A]

}
