package org.spread.core.sequence

import cats.Order
import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.constraint.Constraint.PropValue
import org.spread.core.sequence.Sequence._

import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.OrderingSequence._

import scala.reflect.ClassTag

object PairedSequence {
  trait PairedSeq[@sp X1,@sp X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S]]
    extends Seq[(X1,X2),S] {

    def left: S1
    def right: S2

    def L = left
    def R = right

    def emptySeq = create(left.emptySeq,right.emptySeq)
    def create(l: S1, r: S2): S
    def append[S2 <: S](o: S2): S = create(left append o.left, right append o.right)
    def split(o: Long) = {
      val (ll,lr) = left.split(o)
      val (rl,rr) = right.split(o)
      (create(ll,rl),create(lr,rr))
    }
    def equalTo[S2 <: S](o: S2): Boolean = left.equals(o.left) && right.equals(o.right)
    def size = left.size
    def height = (left.height max right.height) + 1
    def first = (left.first,right.first)
    def last = (left.last,right.last)
    def apply(i: Long) = (left(i),right(i))
    def annotate[@sp A: ClassTag](annotator: Annotator[(X1,X2),A]): A = ???
  }

  case class PairSeqImpl[@sp X1,@sp X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2]](left: S1, right: S2)
    extends PairedSeq[X1,X2,S1,S2,PairSeqImpl[X1,X2,S1,S2]] {

    type S = PairSeqImpl[X1,X2,S1,S2]
    def create(l: S1, r: S2): S = PairSeqImpl(l,r)
    def self = this
  }

  case class OrdPairSeqImpl[@sp X1,@sp X2,S1 <: OrderingSeq[X1,S1], S2 <: OrderingSeq[X2,S2]]
  (left: S1, right: S2)(implicit ord: Order[(X1,X2)])
    extends PairedSeq[X1,X2,S1,S2,OrdPairSeqImpl[X1,X2,S1,S2]] with OrderingSeq[(X1,X2),OrdPairSeqImpl[X1,X2,S1,S2]] {

    type S = OrdPairSeqImpl[X1,X2,S1,S2]
    type ORD = Order[(X1,X2)]

    def sort = defaultSort
    def ordering = ord
    def create(l: S1, r: S2): S = OrdPairSeqImpl[X1,X2,S1,S2](l,r)(ord)
    def self = this
  }

  type AS[@sp X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]

  trait AnnPairSeq
  [@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
    extends PairedSeq[X1,X2,S1,S2,S] with AnnotatedSeq[(X1,X2),(A1,A2),S] {
    def annotationRange(start: Long, end: Long) = (left.annotationRange(start,end),right.annotationRange(start,end))
    def equal = ???
  }

  case class AnnPairSeqImpl[@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2]]
  (left: S1, right: S2) extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]

    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnPairSeqImpl(left,right)
    def self = this
  }

  case class AnnOrdPairSeqImpl[@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2]]
  (left: S1, right: S2)(implicit ord: Order[(X1,X2)])
    extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]] with
      AnnOrdSeq[(X1,X2),(A1,A2),AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]
    type ORD = Order[(X1,X2)]

    def sort = defaultSort
    def ordering = ord
    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2](l,r)(ord)
    def self = this
  }

  trait Prio4Combiner {
    trait Combiner4[@sp X1,S1 <: Seq[X1,S1]] {
      def s1: Seq[X1,S1]
      def &&[@sp X2,S2 <: Seq[X2,S2]](s2: Seq[X2,S2]) = {
        PairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
      }
    }
    implicit class SimpleCombiner[@sp X1,S1 <: Seq[X1,S1]](val s1: Seq[X1,S1]) extends Combiner4[X1,S1]
  }

  trait Prio3Combiner extends Prio4Combiner {
    trait Combiner3[@sp X1,S1 <: OrderingSeq[X1,S1]] extends Combiner4[X1,S1] {
      def s1: OrderingSeq[X1,S1]
      def &&[@sp X2,S2 <: OrderingSeq[X2,S2]](s2: OrderingSeq[X2,S2])(implicit ord: Order[(X1,X2)]) = {
        OrdPairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
      }
    }
    implicit class OrdCombiner[@sp X1,S1 <: OrderingSeq[X1,S1]](val s1: OrderingSeq[X1,S1])
      extends Combiner3[X1,S1]
  }

  trait Prio2Combiner extends Prio3Combiner {
    trait Combiner2[@sp X1,A1,S1 <: AnnotatedSeq[X1,A1,S1]] extends Combiner4[X1,S1] {
      def s1: AnnotatedSeq[X1,A1,S1]
      def &&[@sp X2,A2,S2 <: AnnotatedSeq[X2,A2,S2]](s2: AnnotatedSeq[X2,A2,S2]) = {
        AnnPairSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
      }
    }
    implicit class AnnCombiner[@sp X1,A1,S1 <: AnnotatedSeq[X1,A1,S1]](val s1: AnnotatedSeq[X1,A1,S1])
     extends Combiner2[X1,A1,S1]
  }

  trait Prio1Combiner extends Prio2Combiner {
    trait Combiner1[@sp X1,A1,S1 <: AnnOrdSeq[X1,A1,S1]] extends Combiner2[X1,A1,S1] with Combiner3[X1,S1] {
      def s1: AnnOrdSeq[X1,A1,S1]
      def &&[@sp X2,A2,S2 <: AnnOrdSeq[X2,A2,S2]]
      (s2: AnnOrdSeq[X2,A2,S2])(implicit ord: Order[(X1,X2)]) = {
        AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2])
      }
    }
    implicit class AnnOrdCombiner[@sp X1,A1,S1 <: AnnOrdSeq[X1,A1,S1]](val s1: AnnOrdSeq[X1,A1,S1])
    extends Combiner1[X1,A1,S1]
  }

  object Combiner extends Prio1Combiner // combining (&&) AnnOrdSeq has highest prio (Prio1)
  
  type ASEQ[@sp X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]
  type BinRel[
    @sp X1,
    @sp X2,
    A1 <: PropValue,
    A2 <: PropValue,
    S1 <: ASEQ[X1,A1,S1],
    S2 <: ASEQ[X2,A2,S2],
    S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]] = AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  
  type EREL = BinRel[X1,X2,A1,A2,S1,S2,S] forSome {
    type X1
    type X2
    type A1 <: PropValue
    type A2 <: PropValue
    type S1 <: ASEQ[X1,A1,S1]
    type S2 <: ASEQ[X2,A2,S2]
    type S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  }
  
  case class Selector[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S],XX,SS <: Seq[XX,SS]]
  (n: Symbol,s: S)(implicit f: S => SS) {
    def apply(): SS = f(s)
    def apply(o: S): SS = f(o)
  }

  // AnnSelector[_,_,_,_,EREL,X,A,_]
  case class AnnSelector[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S],XX,AA,SS <: AnnotatedSeq[XX,AA,SS]]
  (n: Symbol,s: S)(implicit f: S => SS) {
    def function = f
    //def apply(): SS = f(s)
    def apply(o: S): SS = f(o)
  }
  
  trait Prio2Selector {
    implicit class Select[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S]]
    (s: PairedSeq[X1,X2,S1,S2,S]) {
      def select[XX,SS <: Seq[XX,SS]](f: PairedSeq[X1,X2,S1,S2,S] => Seq[XX,SS],n: Symbol) = {
        Selector[X1,X2,S1,S2,S,XX,SS](n,s.asInstanceOf[S])(f.asInstanceOf[S=>SS])
      }
    }
  }
  trait Prio1Selector extends Prio2Selector {
    implicit class AnnSelect[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S]]
    (s: PairedSeq[X1,X2,S1,S2,S]) {
      def select[XX,AA,SS <: AnnotatedSeq[XX,AA,SS]](f: PairedSeq[X1,X2,S1,S2,S] => AnnotatedSeq[XX,AA,SS],n: Symbol) = {
        AnnSelector[X1,X2,S1,S2,S,XX,AA,SS](n,s.asInstanceOf[S])(f.asInstanceOf[S=>SS])
      }
    }
  }
  object Selector extends Prio1Selector
}

