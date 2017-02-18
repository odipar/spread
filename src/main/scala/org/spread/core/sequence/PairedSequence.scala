package org.spread.core.sequence

import org.spread.core.constraint.Constraint.PropValue
import org.spread.core.sequence.Sequence._
import scala.language.{existentials, implicitConversions}

object PairedSequence {
  case class PairSeqImpl[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2]](left: S1, right: S2)
    extends PairedSeq[X1,X2,S1,S2,PairSeqImpl[X1,X2,S1,S2]] {

    type S = PairSeqImpl[X1,X2,S1,S2]
    def create(l: S1, r: S2): S = PairSeqImpl(l,r)
    def self = this
  }

  case class OrdPairSeqImpl[X1,X2,S1 <: OrderingSeq[X1,S1], S2 <: OrderingSeq[X2,S2]]
  (left: S1, right: S2)(implicit ord: Ordering[(X1,X2)])
    extends PairedSeq[X1,X2,S1,S2,OrdPairSeqImpl[X1,X2,S1,S2]] with OrderingSeq[(X1,X2),OrdPairSeqImpl[X1,X2,S1,S2]] {

    type S = OrdPairSeqImpl[X1,X2,S1,S2]
    type ORD = Ordering[(X1,X2)]

    def ordering = ord
    def create(l: S1, r: S2): S = OrdPairSeqImpl[X1,X2,S1,S2](l,r)(ord)
    def self = this
  }

  trait AnnPairSeq[X1,X2,A1,A2,S1 <: AnnotatedSeq[X1,A1,S1], S2 <: AnnotatedSeq[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
    extends PairedSeq[X1,X2,S1,S2,S] with AnnotatedSeq[(X1,X2),(A1,A2),S] {
    def annotationRange(start: Long, end: Long) = (left.annotationRange(start,end),right.annotationRange(start,end))
    def equal = ???
  }

  case class AnnPairSeqImpl[X1,X2,A1,A2,S1 <: AnnotatedSeq[X1,A1,S1], S2 <: AnnotatedSeq[X2,A2,S2]]
  (left: S1, right: S2) extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]

    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnPairSeqImpl(left,right)
    def self = this
  }

  case class AnnOrdBinSeqImpl[X1,X2,A1,A2,S1 <: AnnotatedSeq[X1,A1,S1], S2 <: AnnotatedSeq[X2,A2,S2]]
  (left: S1, right: S2)(implicit ord: Ordering[(X1,X2)])
    extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnOrdBinSeqImpl[X1,X2,A1,A2,S1,S2]] with
      OrderingSeq[(X1,X2),AnnOrdBinSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnOrdBinSeqImpl[X1,X2,A1,A2,S1,S2]
    type ORD = Ordering[(X1,X2)]

    def ordering = ord
    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnOrdBinSeqImpl[X1,X2,A1,A2,S1,S2](l,r)(ord)
    def self = this
  }

  implicit class Combiner[X1,S1 <: Seq[X1,S1]](val s1: Seq[X1,S1]) extends AnyVal {
    def combine[X2,S2 <: Seq[X2,S2]](s2: Seq[X2,S2]) = PairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
  }

  implicit class OrdCombiner[X1,S1 <: OrderingSeq[X1,S1]](val s1: OrderingSeq[X1,S1]) extends AnyVal {
    def combineOrd[X2,S2 <: OrderingSeq[X2,S2]](s2: OrderingSeq[X2,S2])(implicit ord: Ordering[(X1,X2)]) = {
      OrdPairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
    }
  }

  implicit class AnnCombiner[X1,A1,S1 <: AnnotatedSeq[X1,A1,S1]](val s1: AnnotatedSeq[X1,A1,S1]) extends AnyVal {
    def combineAnn[X2,A2,S2 <: AnnotatedSeq[X2,A2,S2]](s2: AnnotatedSeq[X2,A2,S2]) = {
      AnnPairSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
    }
  }

  implicit class AnnOrdCombiner[X1,A1,S1 <: AnnOrdSeq[X1,A1,S1]](val s1: AnnOrdSeq[X1,A1,S1]) extends AnyVal {
    def combineAnnOrd[X2,A2,S2 <: AnnOrdSeq[X2,A2,S2]](s2: AnnOrdSeq[X2,A2,S2])(implicit ord: Ordering[(X1,X2)]) = {
      AnnOrdBinSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
    }
  }

  type OSEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]
  type BinRel[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2],S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]] = AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  type EREL = BinRel[X1,X2,A1,A2,S1,S2,S] forSome {
    type X1
    type X2
    type A1 <: PropValue
    type A2 <: PropValue
    type S1 <: OSEQ[X1,A1,S1]
    type S2 <: OSEQ[X2,A2,S2]
    type S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  }
  
  sealed trait ColumnPos

  object LeftCol extends ColumnPos

  object RightCol extends ColumnPos

  sealed trait RelCol[X,XA] {
    def id: Symbol
    def column: ColumnPos
  }

  case class LeftCol[X1,A1](id: Symbol) extends RelCol[X1,A1] {
    def column = LeftCol
    override def toString: String = id + ".L"
  }

  case class RightCol[X2,A2](id: Symbol) extends RelCol[X2,A2] {
    def column = RightCol
    override def toString: String = id + ".R"
  }

  sealed trait RCol[X,A <: PropValue] {
    def rel: EREL
    def column: ColumnPos
    def withID(s: Symbol): RelCol[X,A]
  }

  case class RightRCol[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (r: BinRel[X1,X2,A1,A2,S1,S2,S]) extends RCol[X2,A2] {
    def rel: EREL = r
    def column = RightCol
    def withID(s: Symbol): RelCol[X2,A2] = RightCol[X2,A2](s)
  }

  case class LeftRCol[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (r: BinRel[X1,X2,A1,A2,S1,S2,S]) extends RCol[X1,A1] {
    def rel: EREL = r
    def column = LeftCol
    def withID(s: Symbol): RelCol[X1,A1] = LeftCol[X1,A1](s)
  }
}
