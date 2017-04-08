package org.spread.core.sequence

import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint.{EqualNoAnn, EqualProp, EqualStatP}
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.OrderingSequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import spire.implicits._

object RangedSequence {

  type SL = Statistics[Long]

  trait LongTreeSeqImpl[A]
    extends AnnTreeSeq[Long,A] {

    def self = this
    def emptySeq: AnnTreeSeq[Long,A] = EmptyLongTreeSeq[A]()(context)
    def create(s: AnnTreeSeq[Long,A]#AS) = FullLongTreeSeq[A](s)(context)
    def create(lowerBound: Long,upperBound: Long) = FullLongTreeSeq[A](createRange(lowerBound,upperBound)(self))(context)

    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS) = {
      if (lowerBound > upperBound) c.empty
      else LongLeafRange[A](lowerBound,upperBound,c.annotator.range(lowerBound,upperBound))
    }

    override def sort = repr match {
      case l: LongLeafRange[A] => this
      case _ => defaultSort2
    }
    
    override def createTree[ARR <: Array[SAS]](a: ARR)(implicit c: SS): SAS = {
      var i = 0
      for (ii <- 1 until a.length) {
        if (canMerge(a(i),a(ii))) { a(i) = merge(a(i),a(ii)) }
        else { i = i + 1 ; a(i) = a(ii) }
      }
      if (i == (a.length-1)) { super.createTree(a) }
      else { super.createTree(a.splitAt(i+1)._1) }  // TODO: optimize array copy
    }

    def canMerge(s1: SAS, s2: SAS): Boolean = {
      if (s1.isInstanceOf[LongLeafRange[A]] && s2.isInstanceOf[LongLeafRange[A]]) {
        val l1 = s1.asInstanceOf[LongLeafRange[A]]
        val l2 = s2.asInstanceOf[LongLeafRange[A]]
        ((l1.upperBound+1)==l2.lowerBound)
      }
      else false
    }

    def merge(s1: SAS, s2: SAS)(implicit c: SS): SAS = {
      val l1 = s1.asInstanceOf[LongLeafRange[A]]
      val l2 = s2.asInstanceOf[LongLeafRange[A]]
      createRange(l1.lowerBound,l2.upperBound)
    }
    
    override def createLeaf(a: Array[Long])(implicit c: SS) = {
      if (a.length == 0) c.empty
      else {
        var sequential = true
        for (i <- 1 until a.length) { if ((a(i-1)+1) != a(i)) sequential = false }
        if (sequential) { createRange(a(0),a(a.length-1)) }
        else BSeqLeafImpl(a,c.annotator.manyX(a))
      }
    }
  }

  case class EmptyLongTreeSeq[A](implicit c: RangedAnnotationOrderingContext[Long,A]) extends LongTreeSeqImpl[A] {
    def repr = empty
    def context = c
  }

  case class FullLongTreeSeq[A](repr: AnnTreeSeq[Long,A]#AS)(implicit c: RangedAnnotationOrderingContext[Long,A])
    extends LongTreeSeqImpl[A] {
    def context = c
  }

  case class LongLeafRange[A](lowerBound: Long,upperBound: Long, ann: A)
    extends BSeqLeaf[Long,A]{
    { assert(lowerBound <= upperBound) }

    def annotation = ann
    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS): SAS = {
      if (lowerBound > upperBound) c.empty
      else LongLeafRange(lowerBound,upperBound,c.annotator.range(lowerBound,upperBound))
    }

    def split(i: Long)(implicit c: SS) = {
      if (i < 0) (c.empty,this)
      else if (i >= size) (this,c.empty)
      else {
        val l = createRange(lowerBound,lowerBound + i - 1)
        val r = createRange(lowerBound + i,upperBound)
        (l,r)
      }
    }
    
    def annotationRange(start: Long,end: Long)(implicit c: SS): A = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else c.annotator.range(lowerBound + start,lowerBound + end)
    }

    def approxAnnotationRange(start: Long,end: Long)(implicit c: SS): A = {
      if (end >= size) approxAnnotationRange(start,size - 1)
      else if (start < 0) approxAnnotationRange(0,end)
      else c.annotator.range(lowerBound + start,lowerBound + end)
    }

    def intoArray(dest: Array[Long], i: Int) = {
      var ri = i
      var ii = lowerBound.toInt
      while (ii <= upperBound) {
        dest(ri) = ii
        ri = ri + 1
        ii = ii + 1
      }
      ri
    }
    def equalToTree[AAS <: SAS](o: AAS)(implicit c: SS): Boolean = o match {
      case LongLeafRange(l,u,_) => (lowerBound == l) && (upperBound == u)
      case _ => c.equalTo(this,o)
    }
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = o match {
      case LongLeafRange(l,u,_) => {
        if ((upperBound+1) == l) createRange(lowerBound,u)
        else c.append(this ,o)
      }
      case _ => c.append(this,o)
    }

    def sorted = true
    def size = upperBound - lowerBound + 1
    def toArray = (lowerBound to upperBound).toArray
    def first = lowerBound
    def last = upperBound
    def first(implicit c: SS) = lowerBound
    def last(implicit c: SS) = upperBound
    def apply(i: Long)(implicit c: SS) = lowerBound+i
    def isValid = true
    def getLeaf(i: Long): (BSeqLeaf[Long,A],Int) = {
      (this,i.toInt)
    }
    def getLeaf2(i: Long): BSeqLeaf[Long,A] = this
    override def toString = "L"+lowerBound + ":" + upperBound
  }

  type LSEQ[A] = AnnTreeSeq[Long,A]

  def longSeqFactory[A](implicit ann: RangeAnnotator[Long,A], ca: ClassTag[A], eq: EqualProp[A]): LSEQ[A] = {
    val ord = implicitly[Order[Long]]
    val cx = implicitly[ClassTag[Long]]
    EmptyLongTreeSeq()(new RangedAnnOrdContextImpl[Long,A](ann,eq,ord,cx,ca))
  }

  def defaultLongSeqFactory = {
    val ann = NoAnnotator[Long]
    val ca = implicitly[ClassTag[NoAnnotation]]
    val eq = EqualNoAnn
    longSeqFactory(ann,ca,eq)
  }

  def defaultAnnLongSeqFactory = {
    val ann = StatisticsAnnotator[Long]()
    val ca = implicitly[ClassTag[SL]]
    val eq = EqualStatP()(ann)
    longSeqFactory(ann,ca,eq)
  }

  def createRange(lb: Long, ub: Long): LSEQ[NoAnnotation] = defaultLongSeqFactory.asInstanceOf[LongTreeSeqImpl[NoAnnotation]].create(lb,ub)
  def createAnnRange(lb: Long, ub: Long): LSEQ[SL] = defaultAnnLongSeqFactory.asInstanceOf[LongTreeSeqImpl[SL]].create(lb,ub)
}
