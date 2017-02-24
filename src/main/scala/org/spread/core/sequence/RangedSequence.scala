package org.spread.core.sequence

import cats.Order
import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint.EqualStatP
import org.spread.core.sequence.AnnotatedTreeSequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp
import cats.instances.all._
import org.spread.core.sequence.AnnotatedSequence._

object RangedSequence {

  type SL = Statistics[Long]

  trait LongTreeSeqImpl
    extends AnnTreeSeq[Long,SL] {
    type C = AnnotationOrderingContext[Long,SL]

    def self = this
    def emptySeq: AnnTreeSeq[Long,SL] = EmptyLongTreeSeq()(context)
    def create(s: AnnTreeSeq[Long,SL]#AS) = FullLongTreeSeq(s)(context)
    def create(lowerBound: Long,upperBound: Long) = FullLongTreeSeq(createRange(lowerBound,upperBound)(self))(context)

    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS) = {
      if (lowerBound > upperBound) c.empty
      else LongLeafRange(lowerBound,upperBound)
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
      if (s1.isInstanceOf[LongLeafRange] && s2.isInstanceOf[LongLeafRange]) {
        val l1 = s1.asInstanceOf[LongLeafRange]
        val l2 = s2.asInstanceOf[LongLeafRange]
        ((l1.upperBound+1)==l2.lowerBound)
      }
      else false
    }

    def merge(s1: SAS, s2: SAS): SAS = {
      val l1 = s1.asInstanceOf[LongLeafRange]
      val l2 = s2.asInstanceOf[LongLeafRange]
      LongLeafRange(l1.lowerBound,l2.upperBound)
    }
    
    override def createLeaf(a: Array[Long])(implicit c: SS) = {
      if (a.length == 0) c.empty
      else {
        var sequential = true
        for (i <- 1 until a.length) { if ((a(i-1)+1) != a(i)) sequential = false }
        if (sequential) LongLeafRange(a(0),a(a.length-1))
        else BSeqLeafImpl(a,c.annotator.manyX(a))
      }
    }

    override def sort = {
      if (annotation.sorted) this
      else super.sort
    }

    override def first = annotation.first
    override def last = annotation.last
  }

  case class EmptyLongTreeSeq(implicit c: AnnotationOrderingContext[Long,SL]) extends LongTreeSeqImpl {
    def repr = empty
    def context = c
  }

  case class FullLongTreeSeq(repr: AnnTreeSeq[Long,SL]#AS)(implicit c: AnnotationOrderingContext[Long,SL])
    extends LongTreeSeqImpl {
    def context = c
  }

  case class LongLeafRange(lowerBound: Long,upperBound: Long)
    extends BSeqLeaf[Long,SL] with Statistics[Long] {
    { assert(lowerBound <= upperBound) }

    def annotation(implicit c: SS) = this
    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS) = {
      if (lowerBound > upperBound) c.empty
      else LongLeafRange(lowerBound,upperBound)
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
    def annotationRange(start: Long,end: Long)(implicit c: SS): SL = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else StatisticsImpl2(lowerBound + start,lowerBound + end)
    }
    def equalToTree[AAS <: SAS](o: AAS)(implicit c: SS): Boolean = o match {
      case LongLeafRange(l,u) => (lowerBound == l) && (upperBound == u)
      case _ => c.equalTo(this,o)
    }
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = o match {
      case LongLeafRange(l,u) => {
        if ((upperBound+1) == l) LongLeafRange(lowerBound,u)
        else c.append(this ,o)
      }
      case _ => c.append(this,o)
    }

    def sorted = true
    def size = upperBound - lowerBound + 1
    def toArray = (lowerBound to upperBound).toArray
    def annotation(implicit c: AnnotationContext[Long,SL]) = this
    def first = lowerBound
    def last = upperBound
    def first(implicit c: SS) = lowerBound
    def last(implicit c: SS) = upperBound
    def apply(i: Long)(implicit c: SS) = lowerBound+i
    def isValid = true
    def annotate[@sp A: ClassTag](annotator: Annotator[Long,A]): A = {
      var i = lowerBound
      var a = annotator.one(i)
      while (i < upperBound) {
        a = annotator.append(a,annotator.one(i))
        i = i + 1
      }
      a
    }
    override def toString = "L"+lowerBound + ":" + upperBound
  }

  type LSEQ = AnnTreeSeq[Long,Statistics[Long]]

  val longSeqFactory: LSEQ = {
    val ann = StatisticsAnnotator[Long]()
    val eq = EqualStatP()(ann)
    val ord = implicitly[Order[Long]]
    val cx = implicitly[ClassTag[Long]]
    val ca = implicitly[ClassTag[Statistics[Long]]]
    EmptyLongTreeSeq()(AnnOrdContextImpl(ann,eq,ord,cx,ca))
  }
  def createRange(lb: Long, ub: Long): LSEQ = longSeqFactory.asInstanceOf[EmptyLongTreeSeq].create(lb,ub)

}
