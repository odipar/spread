package org.spread.core.sequence

import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint.EqualP

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// An annotated BTree implementation with a branching factor between 16 and 64
//
// Inspired by Avail's TreeTuple:
// https://github.com/AvailLang/Avail/blob/master/src/com/avail/descriptor/TreeTupleDescriptor.java
//
// Copyright 2016: Robbert van Dalen
//

object Sequence {

  trait BSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,C <: Context[X,A,C]] {
    type S = BSeq[X,A,C]

    def append(o: S)(implicit c: C): S = c.append(this,o)
    def split(i: Long)(implicit c: C): (S,S) = c.split(this,i)
    def annotationRange(start: Long, end: Long)(implicit c: C): A = c.annotationRange(this,start,end)

    def size: Long
    def level: Int
    def annotation: A
  }

  trait Context[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A, C <: Context[X,A,C]] {
    type S = BSeq[X,A,C]

    def minWidth = 16
    def maxWidth = 64

    implicit def context: C
    implicit def ordering: Ordering[X]
    implicit def annotator: Annotator[X,A]
    implicit def classTagX: ClassTag[X]
    implicit def classTagA: ClassTag[A]

    val empty: S = EmptySeq(annotator)
    def createLeaf(a: Array[X]): S = {
      if (a.length == 0) empty
      else BSeqArray(a,annotator.manyX(a))
    }

    def createTree(a: Array[S]): S= {
      val sz = new Array[Long](a.length)
      val an = new Array[A](a.length)
      var ts: Long = 0
      for (i <- a.indices) { ts = ts + a(i).size; sz(i) = ts; an(i) = a(i).annotation }
      BSeqTree(a,sz,annotator.manyA(an))
    }

    def annotationRange(s: S, start: Long, end: Long): A = {
      if (end >= s.size) annotationRange(s,start,s.size-1)
      else if (start < 0) annotationRange(s,0,end)
      else if ((start == 0) && (end == (s.size-1))) s.annotation
      else s match {
        case e: EmptySeq[X,A,C] => annotator.none
        case l: BSeqArray[X,A,C] => {  annotator.manyX(l.array.slice(start.toInt,end.toInt+1)) } // TODO: Optimize
        case t: BSeqTree[X,A,C] => {
          val startIndex = t.childAtIndex(start)
          val startOffset = t.offsetForChild(startIndex)

          val endIndex = t.childAtIndex(end)
          val endOffset = t.offsetForChild(endIndex)

          val startChild = t.childAt(startIndex)
          val endChild = t.childAt(endIndex)

          if (startIndex == endIndex) startChild.annotationRange(start-startOffset,end-endOffset)
          else {
            var ann = startChild.annotationRange(start-startOffset,startChild.size - 1)
            for (i <- (startIndex + 1) until endIndex) ann = annotator.append(ann,t.childAt(i).annotation)
            annotator.append(ann,endChild.annotationRange(0,end-endOffset))
          }
        }
      }
    }

    def append(ss1: S, ss2: S): S = {
      if (ss2.size == 0) ss1
      else if (ss1.size == 0) ss2
      else if ((ss1.level == 0) && (ss2.level == 0)) appendLeafs(asLeaf(ss1),asLeaf(ss2))
      else if (ss1.level == ss2.level) appendTrees(asTree(ss1),asTree(ss2))
      else if (ss1.level > ss2.level) {
        val s1 = asTree(ss1)
        val newLast = s1.lastChild.append(ss2)
        if (newLast.level == s1.level) append(s1.withoutLastChild,newLast)
        else s1.replaceLastChild(newLast)
      }
      else {
        val s2 = asTree(ss2)
        val newFirst = ss1.append(s2.firstChild)
        if (newFirst.level == s2.level) append(newFirst,s2.withoutFirstChild)
        else s2.replaceFirstChild(newFirst)
      }
    }

    def split(ss: S, i: Long): (S,S) = ss match {
      case e: EmptySeq[X,A,C] => (empty,empty)
      case l: BSeqArray[X,A,C] => {
        val (left,right) = l.array.splitAt(i.toInt)
        (createLeaf(left),createLeaf(right))
      }
      case t: BSeqTree[X,A,C] => {
        if (i >= t.size) (t,empty)
        else if (i < 0) (empty,t)
        else {
          val sIndex = t.childAtIndex(i)
          val offset = t.offsetForChild(sIndex)
          var (left,right) = t.childAt(sIndex).split(i - offset)
          for (i <- 0 until sIndex) left = t.childAt(sIndex - i - 1).append(left)
          for (i <- (sIndex + 1) until t.childCount) right = right.append(t.childAt(i))
          (left,right)
        }
      }
    }

    def appendTrees(o1: BSeqTree[X,A,C],o2: BSeqTree[X,A,C]): BSeq[X,A,C] = {
      if (o1.childCount >= minWidth && o2.childCount >= minWidth) createPair(o1,o2)
      else {
        val merged = o1.childs ++ o2.childs
        if ((o1.childCount + o2.childCount) <= maxWidth) createTree(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createTree(l),createTree(r))}
      }
    }

    def appendLeafs(o1: BSeqArray[X,A,C],o2: BSeqArray[X,A,C]): BSeq[X,A,C] = {
        val merged = o1.array ++ o2.array
        if ((o1.size + o2.size) <= maxWidth) createLeaf(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createLeaf(l),createLeaf(r))}
    }

    def createPair(v1: BSeq[X,A,C],v2: BSeq[X,A,C]): BSeq[X,A,C] = createTree(Array(v1,v2)) // TODO: create specialized Pair
    def asTree(t: S): BSeqTree[X,A,C] = t.asInstanceOf[BSeqTree[X,A,C]]
    def asLeaf(l: S): BSeqArray[X,A,C] = l.asInstanceOf[BSeqArray[X,A,C]]
  }

  case class BSeqTree[X,A,C <: Context[X,A,C]]
  (childs: Array[BSeq[X,A,C]],sizes: Array[Long], annotation: A) extends BSeq[X,A,C] {
    val level = childs(0).level + 1
    def size = sizes(sizes.length - 1)
    def childCount = childs.length
    def firstChild = childs(0)
    def lastChild = childs(childCount - 1)
    def childAt(i: Int) = childs(i)
    def setChild(i: Int,s: S)(implicit c: C) = { val nc = childs.clone; nc(i) = s; c.createTree(nc) }
    def replaceFirstChild(first: S)(implicit c: C) = setChild(0,first)
    def replaceLastChild(last: S)(implicit c: C) = setChild(childCount - 1,last)
    def withoutFirstChild(implicit c: C) = c.createTree(childs.slice(1,childCount))
    def withoutLastChild(implicit c: C) = c.createTree(childs.slice(0,childCount - 1))

    def offsetForChild(index: Int) = {
      if (index == 0) 0.toLong
      else sizes(index - 1)
    }

    def childAtIndex(index: Long) = {
      if (index < size && index >= 0) { var i = 0 ; while (sizes(i) < index) { i = i + 1 }; i }
      else sys.error("index out of bounds")
    }
    override def toString = childs.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class EmptySeq[X,A,C <: Context[X,A,C]](annotator: Annotator[X,A]) extends BSeq[X,A,C] {
    def annotation = annotator.none
    def size = 0.toLong
    def level = -1
    override def toString = "<>"
  }

  case class BSeqArray[@specialized(Int,Long,Double) X:ClassTag,A,C <: Context[X,A,C]]
  (array: Array[X], annotation: A) extends BSeq[X,A,C] {
    def size = array.length
    def level = 0
    override def toString = array.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  final def seq[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) A,C <: Context[X,A,C]]
  (x: X)(implicit c: Context[X,A,C]): BSeq[X,A,C] = c.createLeaf(Array[X](x))
  final def seqArray[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) A,C <: Context[X,A,C]]
  (x: Array[X])(implicit c: Context[X,A,C]): BSeq[X,A,C] = c.createLeaf(x)

  case class DefaultContext[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A]
  (implicit ord: Ordering[X], ann: Annotator[X,A], xTag: ClassTag[X], aTag: ClassTag[A])
    extends Context[X,A,DefaultContext[X,A]] {
    implicit def ordering = ord
    implicit def annotator = ann
    implicit def classTagX = xTag
    implicit def classTagA = aTag
    implicit def context = this

    // Cache the equality constraint propagator
    implicit val equalProp: EqualP[X] = EqualP[X]()
  }

  def defaultContext[@specialized(Int,Long,Double) X]
  (implicit ord: Ordering[X], xTag: ClassTag[X], aTag: ClassTag[Statistics[X]]) = {
    DefaultContext()(ord,StatisticsAnnotator[X](),xTag,aTag)
  }

  implicit val intContext = defaultContext[Int]
  implicit val longContext = defaultContext[Long]
  implicit val doubleContext = defaultContext[Double]

  final def main(args: Array[String]): Unit = {
   val s = 10000
    val s2 = 1

    var r = seq(0)
    for (i <- 1 until s) {
      r = r.append(seq(i))
    }

    var rr = r
    for (k <- 1 until s2) {
      rr = rr.append(r)
      if ((rr.size % s2) == 0) { println("rr.size: " + rr.size)}
    }
    println("rr.annotationRange: " + rr.annotationRange(50,900))
  }
}