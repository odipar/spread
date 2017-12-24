package org.spread.core.sequence

import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.constraint.Constraint.PropValue
import org.spread.core.sequence.Sequence._

import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.OrderingSequence._

import scala.language.{existentials, implicitConversions}

import scala.reflect.ClassTag

object PairedSequence {
  trait PairedSeq[@sp X1,@sp X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S]]
    extends Seq[(X1,X2),S] {

    { assert(left.size == right.size) }
    
    def left: S1
    def right: S2

    def L = left
    def R = right

    def emptySeq = create(left.emptySeq,right.emptySeq)
    def create(l: S1, r: S2): S

    implicit def xTag = left.tag
    implicit def yTag = right.tag
    
    def createSeq(a: Array[(X1,X2)]) = {
      // TODO: optimize
      val x1 = a.map(_._1).toArray
      val x2 = a.map(_._2).toArray

      create(left.createSeq(x1),right.createSeq(x2))
    }

    def toArray: Array[(X1,X2)] = {
      val x1 = left.toArray
      val x2 = right.toArray
      val result = new Array[(X1,X2)](x1.length)

      var i = 0
      var s = x1.length
      while (i < s) {
        result(i) = (x1(i),x2(i))
        i = i + 1
      }
      result
    }

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
  }

  case class PairSeqImpl[@sp X1,@sp X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2]](left: S1, right: S2, tag: ClassTag[(X1,X2)])
    extends PairedSeq[X1,X2,S1,S2,PairSeqImpl[X1,X2,S1,S2]] {

    type S = PairSeqImpl[X1,X2,S1,S2]

    def create(l: S1, r: S2): S = PairSeqImpl(l,r,tag)
    def self = this
  }

  case class OrdPairSeqImpl[@sp X1,@sp X2,S1 <: OrderingSeq[X1,S1], S2 <: OrderingSeq[X2,S2]]
  (left: S1, right: S2, tag: ClassTag[(X1,X2)])(implicit ord: Order[(X1,X2)])
    extends PairedSeq[X1,X2,S1,S2,OrdPairSeqImpl[X1,X2,S1,S2]] with OrderingSeq[(X1,X2),OrdPairSeqImpl[X1,X2,S1,S2]] {

    type S = OrdPairSeqImpl[X1,X2,S1,S2]
    type ORD = Order[(X1,X2)]

    def sort = defaultSort
    def ordering = ord
    def create(l: S1, r: S2): S = OrdPairSeqImpl[X1,X2,S1,S2](l,r,tag)(ord)
    def self = this
  }

  type AS[@sp X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]

  trait AnnPairSeq
  [@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
    extends PairedSeq[X1,X2,S1,S2,S] with AnnotatedSeq[(X1,X2),(A1,A2),S] {
    def annotationRange(start: Long, end: Long) = (left.annotationRange(start,end),right.annotationRange(start,end))
    def approxAnnotationRange(start: Long, end: Long) = (left.annotationRange(start,end),right.annotationRange(start,end))
    def annotationsForRange(start: Long, end: Long, a: Array[(A1,A2)]): Unit = { ??? }
    def equal = ???
  }

  case class AnnPairSeqImpl[@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2]]
  (left: S1, right: S2, tag: ClassTag[(X1,X2)]) extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnPairSeqImpl[X1,X2,A1,A2,S1,S2]

    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnPairSeqImpl(left,right,tag)
    def self = this
  }

  case class AnnOrdPairSeqImpl[@sp X1,@sp X2,A1,A2,S1 <: AS[X1,A1,S1], S2 <: AS[X2,A2,S2]]
  (left: S1, right: S2, tag: ClassTag[(X1,X2)])(implicit ord: Order[(X1,X2)])
    extends AnnPairSeq[X1,X2,A1,A2,S1,S2,AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]] with
      AnnOrdSeq[(X1,X2),(A1,A2),AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]] {

    type S = AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2]
    type ORD = Order[(X1,X2)]

    def sort = defaultSort
    def ordering = ord
    def annotation = (left.annotation,right.annotation)
    def create(l: S1, r: S2): S = AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2](l,r,tag)(ord)
    def self = this
  }

  trait Prio4Combiner {
    trait Combiner4[@sp X1,S1 <: Seq[X1,S1]] {
      def s1: Seq[X1,S1]
      def &&[@sp X2,S2 <: Seq[X2,S2]](s2: Seq[X2,S2]) = {
        PairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2],implicitly[ClassTag[(X1,X2)]])
      }
    }
    implicit class SimpleCombiner[@sp X1,S1 <: Seq[X1,S1]](val s1: Seq[X1,S1]) extends Combiner4[X1,S1]
  }

  trait Prio3Combiner extends Prio4Combiner {
    trait Combiner3[@sp X1,S1 <: OrderingSeq[X1,S1]] extends Combiner4[X1,S1] {
      def s1: OrderingSeq[X1,S1]
      def &&[@sp X2,S2 <: OrderingSeq[X2,S2]](s2: OrderingSeq[X2,S2])(implicit ord: Order[(X1,X2)]) = {
        OrdPairSeqImpl[X1,X2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2],implicitly[ClassTag[(X1,X2)]])
      }
    }
    implicit class OrdCombiner[@sp X1,S1 <: OrderingSeq[X1,S1]](val s1: OrderingSeq[X1,S1])
      extends Combiner3[X1,S1]
  }

  trait Prio2Combiner extends Prio3Combiner {
    trait Combiner2[@sp X1,A1,S1 <: AnnotatedSeq[X1,A1,S1]] extends Combiner4[X1,S1] {
      def s1: AnnotatedSeq[X1,A1,S1]
      def &&[@sp X2,A2,S2 <: AnnotatedSeq[X2,A2,S2]](s2: AnnotatedSeq[X2,A2,S2]) = {
        AnnPairSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2],implicitly[ClassTag[(X1,X2)]])
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
        AnnOrdPairSeqImpl[X1,X2,A1,A2,S1,S2](s1.asInstanceOf[S1],s2.asInstanceOf[S2],implicitly[ClassTag[(X1,X2)]])
      }
    }
    implicit class AnnOrdCombiner[@sp X1,A1,S1 <: AnnOrdSeq[X1,A1,S1]](val s1: AnnOrdSeq[X1,A1,S1])
    extends Combiner1[X1,A1,S1]
  }

  object Combiner extends Prio1Combiner // combining (&&) AnnOrdSeq has highest prio (Prio1)

  // Selector has identity
  class Selector[X1,@sp X2, S1 <: Seq[X1,S1], S2 <: Seq[X2,S2]]
  (seq: S1, f: S1 => S2) {
    def copy: Selector[X1,X2,S1,S2] = new Selector(seq,f)
    def asSeq: Seq[X1,S1] = seq
    def apply(): S2 = f(seq)
    def apply(o: S1): S2 = f(o)
  }

  // AnnSelector has identity
  class AnnSelector[X1,@sp X2, A, S1 <: Seq[X1,S1], S2 <: AnnotatedSeq[X2,A,S2]]
  (seq: S1, f: S1 => S2) {
    def copy: AnnSelector[X1,X2,A,S1,S2] = new AnnSelector(seq,f)
    def asSeq: Seq[X1,S1] = seq
    def apply(): S2 = f(seq)
    def apply(o: S1): S2 = f(o)
  }

  trait Prio3Selector {
    implicit class Select3[@sp X1,A,S1 <: AnnotatedSeq[X1,A,S1]](seq: AnnotatedSeq[X1,A,S1]) {
      def selectSame = new AnnSelector[X1,X1,A,S1,S1](seq.asInstanceOf[S1],(x=>x))
    }                                                                  
  }

  trait Prio2Selector extends Prio3Selector {
    implicit class Select2[X1,S1 <: Seq[X1,S1]]
    (s: Seq[X1,S1]) {
      def select[@sp X2,S2 <: Seq[X2,S2]](f: S1 => Seq[X2,S2]) = {
        new Selector[X1,X2,S1,S2](s.asInstanceOf[S1], f.asInstanceOf[S1=>S2])
      }
    }
  }

  trait Prio1Selector extends Prio2Selector {
    implicit class Select1[X1,S1 <: Seq[X1,S1]]
    (seq: Seq[X1,S1]) {
      def select[@sp X2,A,S2 <: AnnotatedSeq[X2,A,S2]]
      (f: S1 => AnnotatedSeq[X2,A,S2]) = {
        new AnnSelector[X1,X2,A,S1,S2](seq.asInstanceOf[S1], f.asInstanceOf[S1 => S2])
      }
    }

  }

  object Selector extends Prio1Selector
}

