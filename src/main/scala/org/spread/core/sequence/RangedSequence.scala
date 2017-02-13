package org.spread.core.sequence

import org.spread.core.annotation.Annotation.{Statistics, StatisticsImpl2}
import org.spread.core.sequence.AnnotatedTreeSequence._

object RangedSequence {

  type SL = Statistics[Long]
  
  trait LongTreeSeqImpl[TC <: OrderingTreeContext[Long,SL]]
    extends AnnTreeSeq[Long,SL,TC] {

    def create(s: AnnTreeSeq[Long,SL,TC]#AS) = FullLongTreeSeq(s)(context)
    def create(lowerBound: Long,upperBound: Long) = FullLongTreeSeq(createRange(lowerBound,upperBound)(self))(context)
    
    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS): SAS = {
      if (lowerBound > upperBound) c.empty
      else LongLeafRange(lowerBound,upperBound)
    }

    override def createLeaf(a: Array[Long])(implicit c: SS): SAS = {
      if (a.length == 0) c.empty
      else {
        var sequential = true
        for (i <- 1 until a.length) { if ((a(i-1)+1) != a(i)) sequential = false }
        if (sequential) LongLeafRange(a(0),a(a.length-1))
        else BSeqLeafImpl(a,c.annotator.manyX(a))
      }
    }
  }

  case class EmptyLongTreeSeq[TC <: OrderingTreeContext[Long,SL]]
  ()(implicit c: TC) extends LongTreeSeqImpl[TC] {

    def context = c
    def sequence = empty
  }

  case class FullLongTreeSeq[TC <: OrderingTreeContext[Long,SL]]
  (sequence: AnnTreeSeq[Long,SL,TC]#AS)(implicit c: TC) extends LongTreeSeqImpl[TC] {

    def context = c
  }
  
  case class LongLeafRange[TC <: OrderingTreeContext[Long,SL]](lowerBound: Long,upperBound: Long)
    extends BSeqLeaf[Long,SL,TC] with Statistics[Long] {
    { assert(lowerBound <= upperBound) }

    def some = (lowerBound + upperBound)/2
    def annotation(implicit c: SS) = this
    def createRange(lowerBound: Long,upperBound: Long)(implicit c: SS): SAS = {
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
    def annotation(implicit c: OrderingTreeContext[Long,SL]) = this
    def first = lowerBound
    def last = upperBound
    def isValid = true
    override def toString = "L"+lowerBound + ":" + upperBound
  }

  type LSEQ = AnnTreeSeq[Long,Statistics[Long],OrderingTreeContext[Long,SL]]

  val emptyLSEQ: LSEQ = EmptyLongTreeSeq()
  def createRange(lb: Long, ub: Long): LSEQ = EmptyLongTreeSeq[OrderingTreeContext[Long,SL]]().create(lb,ub)

  final def main(args: Array[String]): Unit = {
    val factory = EmptyAnnotatedTreeSeq[Long,Statistics[Long],OrderingTreeContext[Long,SL]]()
    val factory2 = EmptyLongTreeSeq[OrderingTreeContext[Long,SL]]()
    val a = factory.createSeq(Array(1,2,3))
    val b = factory2.create(10,100)
    val c = factory2.create(101,105)
    val d = a.append(b)
    println("d: " + d.annotation)
  }
}
