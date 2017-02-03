package org.spread.core.sequence

import org.spread.core.algorithm.Combine
//import org.spread.core.algorithm.Combine.Combiner
import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint.{EqualProp, EqualStatP}

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// An annotated BTree implementation with a branching factor between 16 and 64
//
// Copyright 2017: Robbert van Dalen
//

object Sequence {

  trait BSeq[X,XA,SX <: BSeq[X,XA,SX,XC], XC <: Context[X,XA,SX,XC]] {
    def self: SX
    def append(o: SX)(implicit c: XC): SX
    def split(i: Long)(implicit c: XC): (SX,SX)
    def annotationRange(start: Long,end: Long)(implicit c: XC): XA
    def equalTo(o: SX)(implicit c: XC): Boolean
    def size: Long
    def height: Int
    def annotation(implicit c: XC): XA
  }

  trait Context[X,A,S <: BSeq[X,A,S,C], C <: Context[X,A,S,C]] {
    def empty: S
    implicit def annotator: Annotator[X,A]
    def create(x: Array[X]): S
  }

  trait OrderingContext[X,A,S <: BSeq[X,A,S,C], C <: OrderingContext[X,A,S,C]] extends Context[X,A,S,C] {
    implicit def ordering: Ordering[X]
    implicit def equal: EqualProp[A]
  }

  trait BSeqTr[X,A, C <: TreeContext[X,A,C]] extends BSeq[X,A,BSeqTr[X,A,C],C] {
    def self: S = this
    type S = BSeqTr[X,A,C]
  }

  trait TreeContext[X,A, C <: TreeContext[X,A,C]] extends Context[X,A,BSeqTr[X,A,C],C] {
    type S = BSeqTr[X,A,C]

    type BTREE = BSeqTree[X,A,C]
    type BLEAF = BSeqLeaf[X,A,C]

    def minWidth = 16
    def maxWidth = 64

    implicit def context: C
    implicit def annotator: Annotator[X,A]
    // constructors
    def empty: S
    def create(x: Array[X]) = createLeaf(x)
    def createLeaf(a: Array[X]): S
    def createTree(a: Array[S]): S
    def appendTrees(o1: BTREE,o2: BTREE): S
    def appendLeafs(o1: BLEAF,o2: BLEAF): S
    def asTree(t: S): BTREE = t.asInstanceOf[BTREE]
    def asLeaf(l: S): BLEAF = l.asInstanceOf[BLEAF]
    def equalTo(s1: S,s2: S) = {
      // optimize with valueRange
      if (s1 == s2) true
      else if (s1.size != s2.size) false
      else {
        if (s1.size == 1) asLeaf(s1).first == asLeaf(s2).first
        else {
          val i = (s1.size + 1) / 2
          val (l1,r1) = s1.split(i)
          val (l2,r2) = s2.split(i)
          l1.equalTo(l2) && r1.equalTo(r2)
        }
      }
    }
    // Inspired by Avail's TreeTuple:
    // https://github.com/AvailLang/Avail/blob/master/src/com/avail/descriptor/TreeTupleDescriptor.java
    def append(ss1: S,ss2: S): S = {
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
        val s2: BSeqTree[X,A,C] = asTree(ss2)
        val newFirst = ss1.append(s2.firstChild)
        if (newFirst.height == s2.height) append(newFirst,s2.withoutFirstChild)
        else s2.replaceFirstChild(newFirst)
      }
    }
    def createPair(v1: S,v2: S): S = createTree(Array(v1,v2)) // TODO: create specialized Pair
  }

  trait BSeqTree[X,A,C <: TreeContext[X,A,C]] extends BSeqTr[X,A,C] {
    def childs: Array[BSeqTr[X,A,C]]
    def sizes: Array[Long]
    val height = childs(0).height + 1
    def size = sizes(sizes.length - 1)
    def childCount = childs.length
    def firstChild = childs(0)
    def lastChild = childs(childCount - 1)
    def childAt(i: Int) = childs(i)
    def setChild(i: Int,s: S)(implicit c: C) = {val nc = childs.clone; nc(i) = s; c.createTree(nc)}
    def replaceFirstChild(first: S)(implicit c: C) = setChild(0,first)
    def replaceLastChild(last: S)(implicit c: C) = setChild(childCount - 1,last)
    def withoutFirstChild(implicit c: C) = c.createTree(childs.slice(1,childCount))
    def withoutLastChild(implicit c: C) = c.createTree(childs.slice(0,childCount - 1))
    def offsetForChild(index: Int) = {
      if (index == 0) 0.toLong
      else sizes(index - 1)
    }
    def childAtIndex(index: Long) = {
      if (index < size && index >= 0) {var i = 0; while (sizes(i) <= index) {i = i + 1}; i}
      else sys.error("index out of bounds")
    }
    def append(o: S)(implicit c: C): S = c.append(this,o)
    def split(i: Long)(implicit c: C): (S,S) = {
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
    def annotationRange(start: Long,end: Long)(implicit c: C): A = {
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
    def equalTo(o: S)(implicit c: C): Boolean = c.equalTo(this,o)
    override def toString = childs.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class EmptySeq[X,A,C <: TreeContext[X,A,C]](annotator: Annotator[X,A]) extends BSeqTr[X,A,C] {
    def annotation(implicit c: C) = annotator.none
    def size = 0.toLong
    def height = -1
    def split(i: Long)(implicit c: C) = (this,this)
    def append(o: S)(implicit c: C): S = o
    def annotationRange(start: Long,end: Long)(implicit c: C) = c.annotator.none
    def equalTo(o: S)(implicit c: C): Boolean = (o.size == 0)
    override def toString = "<>"
  }

  trait BSeqLeaf[X,A,C <: TreeContext[X,A,C]] extends BSeqTr[X,A,C]  {
    def toArray: Array[X]
    def height = 0
    def first: X
    def last: X
    override def toString = toArray.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class BSeqLeafImpl[X,A, C <: TreeContext[X,A,C]](array: Array[X],ann: A) extends BSeqLeaf[X,A,C] {
    def annotation(implicit c: C) = ann
    def toArray = array
    def size = array.length
    def split(i: Long)(implicit c: C) = {
      if (i >= size) (this,c.empty)
      else if (i < 0) (c.empty,this)
      else {
        val (left: Array[X],right: Array[X]) = array.splitAt(i.toInt)
        (c.createLeaf(left),c.createLeaf(right))
      }
    }
    def annotationRange(start: Long,end: Long)(implicit c: C): A = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else c.annotator.manyX(array.slice(start.toInt,end.toInt + 1)) // TODO: Optimize
    }
    def equalTo(o: S)(implicit c: C): Boolean = c.equalTo(this,o)
    def first = array(0)
    def last = array(array.length - 1)
    def append(o: S)(implicit c: C): S = c.append(this,o)
  }

  case class BSeqTreeImpl[X,A,C <: TreeContext[X,A,C]](childs: Array[BSeqTr[X,A,C]],sizes: Array[Long],ann: A)
    extends BSeqTree[X,A,C] {
    def annotation(implicit c: C) = ann
  }

  trait OrderingTreeContext[X,A]
    extends TreeContext[X,A,OrderingTreeContext[X,A]] with OrderingContext[X,A,BSeqTr[X,A,OrderingTreeContext[X,A]],OrderingTreeContext[X,A]] {
    type C = OrderingTreeContext[X,A]
    implicit def xTag: ClassTag[X]
    implicit def aTag: ClassTag[A]
    val empty: S = EmptySeq(annotator) // Allocate only one EmptySeq for re-use
    def createTree(a: Array[S]): S = {
      val sz = new Array[Long](a.length)
      val an = new Array[A](a.length)

      var ts: Long = 0
      for (i <- a.indices) {ts = ts + a(i).size; sz(i) = ts; an(i) = a(i).annotation}
      BSeqTreeImpl(a,sz,annotator.manyA(an))
    }
    def appendTrees(o1: BTREE,o2: BTREE): S = {
      assert(o1.height == o2.height) // only append trees with same height
      if (o1.childCount >= minWidth && o2.childCount >= minWidth) createPair(o1,o2)
      else {
        val merged = o1.childs ++ o2.childs
        if (merged.length <= maxWidth) createTree(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createTree(l),createTree(r))}
      }
    }
    def appendLeafs(o1: BLEAF,o2: BLEAF): S = {
      assert((o1.height == 0) && (o2.height == 0))
      if (o1.size >= minWidth && o2.size >= minWidth) createPair(o1,o2)
      else {
        val merged = o1.toArray ++ o2.toArray
        if (merged.length <= maxWidth) createLeaf(merged)
        else {val (l,r) = merged.splitAt((merged.length + 1) >> 1); createPair(createLeaf(l),createLeaf(r))}
      }
    }
  }

  case class DefaultOrderingTreeContext[X,A]
  (implicit ordering: Ordering[X], equal: EqualProp[A], ann: Annotator[X,A],xTag: ClassTag[X],aTag: ClassTag[A]) extends OrderingTreeContext[X,A] {
    implicit def annotator = ann
    implicit def context = this
    def createLeaf(a: Array[X]) = {
      if (a.length == 0) empty
      else BSeqLeafImpl(a,annotator.manyX(a))
    }
  }

  def createRange(lowerBound: Long,upperBound: Long)(implicit c: OrderingTreeContext[Long,Statistics[Long]]) = {
    if (lowerBound > upperBound) c.empty
    else LongLeafRange(lowerBound,upperBound)
  }

  case class LongLeafRange(lowerBound: Long,upperBound: Long)
    extends BSeqLeaf[Long,Statistics[Long],OrderingTreeContext[Long,Statistics[Long]]] with Statistics[Long] {
    { assert(lowerBound <= upperBound) }

    def split(i: Long)(implicit c: OrderingTreeContext[Long,Statistics[Long]]) = {
      if (i < 0) (c.empty,this)
      else if (i >= size) (this,c.empty)
      else {
        val l = createRange(lowerBound,lowerBound + i - 1)
        val r = createRange(lowerBound + i,upperBound)
        (l,r)
      }
    }
    def annotationRange(start: Long,end: Long)(implicit c: OrderingTreeContext[Long,Statistics[Long]]): Statistics[Long] = {
      if (end >= size) annotationRange(start,size - 1)
      else if (start < 0) annotationRange(0,end)
      else StatisticsImpl2(lowerBound + start,lowerBound + end)
    }
    def equalTo(o: S)(implicit c: OrderingTreeContext[Long,Statistics[Long]]): Boolean = o match {
      case LongLeafRange(l,u) => (lowerBound == l) && (upperBound == u)
      case _ => c.equalTo(this,o)
    }
    def append(o: S)(implicit c: OrderingTreeContext[Long,Statistics[Long]]): S = o match {
      case LongLeafRange(l,u) => {
        if ((upperBound+1) == l) LongLeafRange(lowerBound,u)
        else c.append(this ,o)
      }
      case _ => c.append(this,o)
    }
    def sorted = true
    def size = upperBound - lowerBound + 1
    def toArray = (lowerBound to upperBound).toArray
    def annotation(implicit c: OrderingTreeContext[Long,Statistics[Long]]) = this
    def first = lowerBound
    def last = upperBound
    def isValid = true
    override def toString = lowerBound + ".." + upperBound
  }

  case class LongTreeContext(implicit ordering: Ordering[Long],equal: EqualProp[Statistics[Long]], xTag: ClassTag[Long],aTag: ClassTag[Statistics[Long]])
    extends OrderingTreeContext[Long,Statistics[Long]] {
    implicit def annotator = StatisticsAnnotator[Long]
    implicit def context = this

    def createLeaf(a: Array[Long]) = {
      if (a.length == 0) empty
      else {
        var sequential = true
        for (i <- 1 until a.length) { if ((a(i-1)+1) != a(i)) sequential = false }
        if (sequential) LongLeafRange(a(0),a(a.length-1))
        else BSeqLeafImpl(a,annotator.manyX(a))
      }
    }
  }

  def defaultContext[X]
  (implicit ord: Ordering[X], equal: EqualProp[Statistics[X]], xTag: ClassTag[X],aTag: ClassTag[Statistics[X]]): DefaultOrderingTreeContext[X,Statistics[X]] = {
    DefaultOrderingTreeContext()(ord,equal,StatisticsAnnotator[X](),xTag,aTag)
  }
  implicit def equalStaticProp[X](implicit o: Ordering[X]): EqualProp[Statistics[X]] = EqualStatP[X]()(StatisticsAnnotator[X]())
  implicit val intContext: DefaultOrderingTreeContext[Int,Statistics[Int]] = defaultContext[Int]
  implicit val longContext: LongTreeContext = LongTreeContext()
  implicit val doubleContext: DefaultOrderingTreeContext[Double,Statistics[Double]] = defaultContext[Double]

  val longBSeqContext = LongTreeContext()

  type STAT[X] = Statistics[X]
  type OC[X,XA,SX <: BSeq[X,XA,SX,XC] ,XC <: OrderingContext[X,XA,SX,XC]] = OrderingContext[X,XA,SX,XC]
  type OTC[X] = OrderingTreeContext[X,STAT[X]]

  case class DualOrdering[X,Y](x: Ordering[X], y: Ordering[Y]) extends Ordering[(X,Y)] {
    def compare(o1: (X,Y), o2: (X,Y)): Int = {
      val c = x.compare(o1._1,o2._1)
      if (c == 0) y.compare(o1._2,o2._2)
      else c
    }
  }

  case class BinContext[X,XA,Y,YA,AA,SX <: BSeq[X,XA,SX,XC], SY <: BSeq[Y,YA,SY,YC],XC <: OC[X,XA,SX,XC], YC <: OC[Y,YA,SY,YC]](left: XC, right: YC, f: (XA,YA) => AA)
  extends OrderingContext[(X,Y),AA,BinSeq[(X,Y),AA,BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]],BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]] {
    val ordering = DualOrdering(left.ordering,right.ordering)
    def error = sys.error("BinContext is a phantom context except for ordering")
    def equal = error
    def empty = error
    implicit def annotator: Annotator[(X,Y),AA] = error
    def create(a: Array[(X,Y)]) = error
  }

  trait BinSeq[X,A,C <: Context[X,A,BinSeq[X,A,C],C]] extends BSeq[X,A,BinSeq[X,A,C],C]

  trait SSeq[X,XA,SQX <: SSeq[X,XA,SQX,SX,XC], SX <: BSeq[X,XA,SX,XC], XC <: OC[X,XA,SX,XC]] {
    def self: SQX
    implicit def context: XC
    def append(o: SQX): SQX
    def split(i: Long): (SQX,SQX)
    def empty: SQX
    def annotationRange(start: Long,end: Long): XA
    def equalTo(o: SQX): Boolean
    def size: Long
    def height: Int
    def annotation: XA
    final def ordering: Ordering[X] = context.ordering
    def combine[Y,YA,AA,SQY <: SSeq[Y,YA,SQY,SY,YC],SY <: BSeq[Y,YA,SY,YC], YC <: OC[Y,YA,SY,YC]]
    (o: SSeq[Y,YA,SQY,SY,YC])(implicit c: BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]) = {
      SSeqBin[X,XA,Y,YA,AA,SQX,SQY,SX,SY,XC,YC](self,o.asInstanceOf[SQY])
    }

    // BELOW DOESN'T WORK? THAT'S WHY WE NEED A CAST (MAY BE SOLVE THIS WITH AN EXISTENTIAL TYPE)
    //
    // def combine[Y,YA,AA,SQY <: SSeq[Y,YA,SQY,SY,YC],SY <: BSeq[Y,YA,SY,YC], YC <: OC[Y,YA,SY,YC]]
    // (o: SQY)(implicit c: BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]) = {
    //   SSeqBin[X,XA,Y,YA,AA,SQX,SQY,SX,SY,XC,YC](self,o)
    // }
  }

  case class SSeqImpl[X,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]](seq: S,c: C) extends SSeq[X,A,SSeqImpl[X,A,S,C],S,C] {
    def self = this
    type SS = SSeqImpl[X,A,S,C]

    implicit def context = c
    def append(o: SS): SS = SSeqImpl(seq.append(o.seq),c)
    def split(i: Long): (SS,SS) = {
      val (left,right) = seq.split(i)
      (SSeqImpl(left,c),SSeqImpl(right,c))
    }
    def annotationRange(start: Long,end: Long): A = seq.annotationRange(start,end)
    def equalTo(o: SS): Boolean = seq.equalTo(o.seq)
    def size: Long = seq.size
    def height: Int = seq.height
    def annotation: A = seq.annotation
    def empty = SSeqImpl(c.empty,c)
    override def toString = "[" + seq + "]"
  }

  case class SSeqBin[X,XA,Y,YA,AA,SQX <: SSeq[X,XA,SQX,SX,XC], SQY <: SSeq[Y,YA,SQY,SY,YC], SX <: BSeq[X,XA,SX,XC], SY <: BSeq[Y,YA,SY,YC],XC <: OC[X,XA,SX,XC], YC <: OC[Y,YA,SY,YC]]
  (left: SQX, right: SQY)(implicit c: BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]) extends SSeq[(X,Y),AA,SSeqBin[X,XA,Y,YA,AA,SQX,SQY,SX,SY,XC,YC],BinSeq[(X,Y),AA,BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]],BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC]] {
    type SS = SSeqBin[X,XA,Y,YA,AA,SQX,SQY,SX,SY,XC,YC]

    def self = this
    def context: BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC] = c
    def append(o: SS): SS = {
      SSeqBin(left.append(o.left),right.append(o.right))
    }
     def split(i: Long): (SS,SS) = {
       val (ll,lr) = left.split(i)
       val (rl,rr) = right.split(i)
       (SSeqBin(ll,rl),SSeqBin(lr,rr))
    }
    def annotationRange(start: Long,end: Long): AA = {
      context.f(left.annotationRange(start,end),right.annotationRange(start,end))
    }
    def equalTo(o: SS): Boolean = left.equalTo(o.left) && right.equalTo(o.right)
    def size: Long = left.size
    def height: Int = left.height max right.height
    def annotation: AA = context.f(left.annotation,right.annotation)
    def empty = SSeqBin(left.empty,right.empty)
    override def toString = "[" + left + "_" + right + "]"
  }

  final def sseq[X,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]]
  (s: S)(implicit context: C): SSeqImpl[X,A,S,C] = SSeqImpl(s,context)

  final def seq[X: ClassTag,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]]
  (x: X)(implicit c: C): SSeqImpl[X,A,S,C] = sseq[X,A,S,C](seq2[X,A,S,C](x))

  final def seq2[X: ClassTag,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]]
  (x: X)(implicit c: C) = c.create(Array[X](x))

  final def seqArray[X: ClassTag,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]]
  (x: Array[X])(implicit c: C): SSeqImpl[X,A,S,C] = sseq[X,A,S,C](seqArray2[X,A,S,C](x))

  final def seqstat[X: ClassTag](x: Array[X])(implicit c: OTC[X]): SSeqImpl[X,STAT[X],BSeqTr[X,STAT[X],OTC[X]],OTC[X]] = {
    seqArray[X,STAT[X],BSeqTr[X,STAT[X],OTC[X]],OTC[X]](x)
  }

  def createRelArray[X: ClassTag, Y: ClassTag](x: Array[X], y: Array[Y])(implicit c1: OTC[X], c2: OTC[Y]) = {
    seqstat[X](x).combine(seqstat[Y](y))
  }

  final def seqArray2[X: ClassTag,A,S <: BSeq[X,A,S,C], C <: OC[X,A,S,C]]
  (x: Array[X])(implicit c: C): S = {
    if (x.length <= 0) c.empty
    else if (x.length <= 32) c.create(x)
    else {
      val (l,r) = x.splitAt((x.length + 1) / 2)
      seqArray2[X,A,S,C](l).append(seqArray2[X,A,S,C](r))
    }
  }

  implicit def toBinContext[X,XA,Y,YA,AA,SX <: BSeq[X,XA,SX,XC], SY <: BSeq[Y,YA,SY,YC],XC <: OC[X,XA,SX,XC], YC <: OC[Y,YA,SY,YC]](implicit xc: XC, yc: YC, f: (XA,YA) => AA): BinContext[X,XA,Y,YA,AA,SX,SY,XC,YC] = BinContext(xc,yc,f)

  implicit def dualStats[X,Y]: (Statistics[X],Statistics[Y]) => Statistics[(X,Y)] = {
    (x: Statistics[X],y: Statistics[Y]) => createStats(
      (x.lowerBound,y.lowerBound),
      (x.upperBound,y.upperBound),
      (x.first,y.first),
      (x.last,y.last),
      x.sorted && y.sorted
    )
  }

  final def main(args: Array[String]): Unit = {
    val s = 10000

    var r = seqstat[Int](Array())  // empty sequence with statistics
    var rr = seqstat[Int](Array())

    for (i <- 0 until s) {
      r = r.append(seq(i))
      rr = rr.append(seq(80-i))
    }

    val z = rr.combine(r)
    val zz = z.combine(z)
    val zzz = zz.combine(zz)
    val zzzz = zzz.combine(zzz)
    /*val zzzzz = zzzz.combine(zzzz)
    val zzzzzz = zzzzz.combine(zzzzz)
    val zzzzzzz = zzzzzz.combine(zzzzzz)*/

    println("start")
    println("zzzz: " + Combine.sort(zzzz).size)
    println("end")
  }


}