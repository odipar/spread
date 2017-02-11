package org.spread.core.sequence


import Sequence._
import AnnotatedSequence._
import org.spread.core.annotation.Annotation._

import scala.reflect.ClassTag
import org.spread.core.algorithm.Combine._
import org.spread.core.constraint.Constraint.EqualProp

object AnnotatedTreeSequence {
  
  trait OrderingTreeContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends OrderingAnnContext[X,A]
  {
    def annotator: Annotator[X,A]
    def xTag: ClassTag[X]
    def aTag: ClassTag[A]
  }

  trait AnnTreeSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
    extends OrderedAnnSeq[X,A,AnnTreeSeq[X,A]]  {

    type TC <: OrderingTreeContext[X,A]
    type AS = BSeqTr[X,A]
    type SS = AnnTreeSeq[X,A]
    type SAS = SS#AS

    def self = this
    def context: TC

    implicit def ord: Ordering[X] = context.ord
    implicit def xTag: ClassTag[X] = context.xTag
    implicit def aTag: ClassTag[A] = context.aTag

    def minWidth = 16
    final def maxWidth = minWidth*4

    def annotator: Annotator[X,A] = context.annotator
    def empty: SAS = EmptySeq()

    def createSeq(a: Array[X]): SS = {
      if (a.length <= maxWidth) create(createLeaf(a)(this))
      else {
        val (l,r) = a.splitAt(a.length/2)
        createSeq(l).append(createSeq(r))
      }
    }
    
    def emptySeq = create(empty)
    def :+ (x: X) = append(createSeq(Array(x)))

    def createTree[ARR <: Array[SAS]](a: ARR)(implicit c: SS): SAS = {
      val sz = new Array[Long](a.length)
      val an = new Array[A](a.length)

      var ts: Long = 0
      for (i <- a.indices) {ts = ts + a(i).size; sz(i) = ts; an(i) = a(i).annotation}
      BSeqTreeImpl(a,sz,annotator.manyA(an))
    }
    def createPair[AA1 <: SAS, AA2 <: SAS](ss1: AA1,ss2: AA2)(implicit c: SS): SAS = {
      createTree(Array(ss1,ss2)) // TODO: create specialized Pair
    }
    def createLeaf(a: Array[X])(implicit c: SS) = {
      if (a.length == 0) empty
      else BSeqLeafImpl(a,annotator.manyX(a))
    }
    def appendTrees(o1: BSeqTree[X,A],o2: BSeqTree[X,A])(implicit c: SS): SAS = {
      assert(o1.height == o2.height) // only append trees with same height
      if (o1.childCount >= minWidth && o2.childCount >= minWidth) createPair(o1,o2)
      else {
        val merged = o1.childs ++ o2.childs
        if (merged.length <= maxWidth) createTree(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createTree(l),createTree(r))}
      }
    }
    def appendLeafs(o1: BSeqLeaf[X,A],o2: BSeqLeaf[X,A])(implicit c: SS): SAS = {
      assert((o1.height == 0) && (o2.height == 0))
      if (o1.size >= minWidth && o2.size >= minWidth) createPair(o1,o2)
      else {
        val merged = o1.toArray ++ o2.toArray
        if (merged.length <= maxWidth) createLeaf(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createLeaf(l),createLeaf(r))}
      }
    }
    def asTree(t: SAS): BSeqTree[X,A] = t.asInstanceOf[BSeqTree[X,A]]
    def asLeaf(l: SAS): BSeqLeaf[X,A] = l.asInstanceOf[BSeqLeaf[X,A]]

    def append[AA1 <: SAS, AA2 <: SAS](ss1: AA1,ss2: AA2)(implicit c: SS): SAS = {
      if (ss2.size == 0) ss1
      else if (ss1.size == 0) ss2
      else if ((ss1.height == 0) && (ss2.height == 0)) appendLeafs(asLeaf(ss1),asLeaf(ss2))
      else if (ss1.height == ss2.height) appendTrees(asTree(ss1),asTree(ss2))
      else if (ss1.height > ss2.height) {
        val s1 = asTree(ss1)
        val newLast = s1.lastChild.append(ss2)
        if (newLast.height == s1.height) append(s1.withoutLastChild,newLast)
        else s1.replaceLastChild(newLast)
      }
      else {
        val s2 = asTree(ss2)
        val newFirst = ss1.append(s2.firstChild)
        if (newFirst.height == s2.height) append(newFirst,s2.withoutFirstChild)
        else s2.replaceFirstChild(newFirst)
      }
    }

    def equalTo[AA1 <: SAS, AA2 <: SAS](s1: AA1, s2: AA2)(implicit c: SS): Boolean = {
      // optimize with valueRange
      if (s1 == s2) true
      else if (s1.size != s2.size) false
      else {
        if (s1.size == 1) asLeaf(s1).first == asLeaf(s2).first
        else {
          val i = (s1.size + 1) / 2
          val (l1,r1) = s1.split(i)
          val (l2,r2) = s2.split(i)
          l1.equalToTree(l2) && r1.equalToTree(r2)
        }
      }
    }
  }

  trait BSeqTr[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends ASeq[X,A,AnnTreeSeq[X,A]]
  {
    type AS = BSeqTr[X,A]
    type SS = AnnTreeSeq[X,A]
    type SAS = SS#AS
  }

  trait BSeqTree[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends BSeqTr[X,A] {
    def childs: Array[SAS]
    def sizes: Array[Long]
    val height = childs(0).height + 1
    def size = sizes(sizes.length - 1)
    def childCount = childs.length
    def firstChild = childs(0)
    def lastChild = childs(childCount - 1)
    def childAt(i: Int): SAS = childs(i)
    def setChild(i: Int,s: SAS)(implicit c: SS): SAS = {val nc = childs.clone; nc(i) = s; c.createTree(nc)}
    def replaceFirstChild(first: SAS)(implicit c: SS): SAS = setChild(0,first)
    def replaceLastChild(last: SAS)(implicit c: SS): SAS = setChild(childCount - 1,last)
    def withoutFirstChild(implicit c: SS): SAS = c.createTree(childs.slice(1,childCount))
    def withoutLastChild(implicit c: SS): SAS = c.createTree(childs.slice(0,childCount - 1))
    def offsetForChild(index: Int) = {
      if (index == 0) 0.toLong
      else sizes(index - 1)
    }
    def childAtIndex(index: Long) = {
      if (index < size && index >= 0) {var i = 0; while (sizes(i) <= index) {i = i + 1}; i}
      else sys.error("index out of bounds")
    }
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = c.append(this,o)
    def equalToTree[AS <: SAS](o: AS)(implicit c: SS): Boolean = c.equalTo(this,o)

    def split(i: Long)(implicit c: SS): (SAS,SAS) = {
      if (i >= size) (this,c.empty)
      else if (i < 0) (c.empty,this)
      else {
        val sIndex = childAtIndex(i)
        val offset = offsetForChild(sIndex)
        var (left,right) = childAt(sIndex).split(i - offset)
        for (i <- 0 until sIndex) left = childAt(sIndex - i - 1).append(left)
        for (i <- (sIndex + 1) until childCount) right = right.append(childAt(i))
        (left,right)
      }
    }
    def annotationRange(start: Long,end: Long)(implicit c: SS): A = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else {
        val startIndex = childAtIndex(start)
        val startOffset = offsetForChild(startIndex)
        val endIndex = childAtIndex(end)
        val endOffset = offsetForChild(endIndex)
        val startChild = childAt(startIndex)
        if (startIndex == endIndex) startChild.annotationRange(start - startOffset,end - endOffset)
        else {
          val endChild = childAt(endIndex)
          var ann = startChild.annotationRange(start - startOffset,startChild.size)
          for (i <- (startIndex + 1) until endIndex) ann = c.annotator.append(ann,childAt(i).annotation)
          c.annotator.append(ann,endChild.annotationRange(0,end - endOffset))
        }
      }
    }
    def equalTo(o: SAS)(implicit c: SS): Boolean = c.equalTo(this,o)
    override def toString = childs.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class EmptySeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]() extends BSeqTr[X,A] {
    def annotation(implicit c: SS) = c.annotator.none
    def size = 0.toLong
    def height = -1
    def some = sys.error("Empty sequence")
    def split(i: Long)(implicit c: SS) = (this,this)
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = o
    def annotationRange(start: Long,end: Long)(implicit c: SS) = c.annotator.none
    def equalToTree[AAS <: SAS](o: AAS)(implicit c: SS): Boolean = (o.size == 0)
    override def toString = "<>"
  }

  trait BSeqLeaf[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] extends BSeqTr[X,A]  {
    def toArray: Array[X]
    def height = 0
    def first: X
    def last: X
    override def toString = toArray.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class BSeqLeafImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A](array: Array[X],ann: A)
    extends BSeqLeaf[X,A] {
    def annotation(implicit c: SS) = ann
    def toArray = array
    def size = array.length
    def some = array(array.length/2)
    def split(i: Long)(implicit c: SS) = {
      if (i >= size) (this,c.empty)
      else if (i < 0) (c.empty,this)
      else {
        val (left: Array[X],right: Array[X]) = array.splitAt(i.toInt)
        (c.createLeaf(left),c.createLeaf(right))
      }
    }
    def annotationRange(start: Long,end: Long)(implicit c: SS): A = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else c.annotator.manyX(array.slice(start.toInt,end.toInt + 1)) // TODO: Optimize
    }
    def equalToTree[AAS <: SAS](o: AAS)(implicit c: SS): Boolean = c.equalTo(this,o)
    def first = array(0)
    def last = array(array.length - 1)
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = c.append(this,o)
  }

  case class BSeqTreeImpl
  [@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A, ARR <: Array[AnnTreeSeq[X,A]#AS]]
  (childs: ARR,sizes: Array[Long],ann: A) extends BSeqTree[X,A] {
    def some = childs(childs.length/2).some
    def annotation(implicit c: SS) = ann
  }

  case class DefaultTreeContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
  ()(implicit o: Ordering[X], xt: ClassTag[X], at: ClassTag[A], ann: Annotator[X,A], eq: EqualProp[A]) extends OrderingTreeContext[X,A] {
    def ord = o
    def annotator = ann
    def xTag = xt
    def aTag = at
    def equal = eq
  }

  trait AnnotatedTreeSeqImpl[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
    extends AnnTreeSeq[X,A] {

    type TC = OrderingTreeContext[X,A]
    def create(s: AnnTreeSeq[X,A]#AS) = FullAnnotatedTreeSeq(s)(context)
  }

  case class EmptyAnnotatedTreeSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
  ()(implicit c: OrderingTreeContext[X,A]) extends AnnotatedTreeSeqImpl[X,A] {

    def context = c
    def sequence = empty
  }

  case class FullAnnotatedTreeSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
  (sequence: AnnTreeSeq[X,A]#AS)(implicit c: OrderingTreeContext[X,A]) extends AnnotatedTreeSeqImpl[X,A] {

    def context = c
  }

  implicit def annotator[@specialized(Int,Long,Double) X](implicit ord: Ordering[X]): StatisticsAnnotator[X] = {
    StatisticsAnnotator[X]()
  }

  implicit def treeContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
    (implicit ord: Ordering[X], xt: ClassTag[X], at: ClassTag[A], ann: Annotator[X,A], eq: EqualProp[A]) = {
    
    DefaultTreeContext()(ord,xt,at,ann,eq)
  }
}
