package org.spread.core.sequence

import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint._
import org.spread.core.sequence.Sequence._
import org.spread.core.sequence.OrderingSequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.OrderingSequence.Union

object AnnotatedTreeSequence {

  trait AnnTreeSeq[@sp X,A] extends AnnOrdSeqWithRepr[X,A,AnnTreeSeq[X,A]] {
    type C <: AnnotationOrderingContext[X,A]
    type AS = BSeqTr[X,A]
    type SS = AnnTreeSeq[X,A]
    type SAS = SS#AS

    def context: C
    def annotator = context.ann
    def ordering = context.ord
    def equal = context.eq

    def tag = context.xTag
    def empty: SAS = EmptySeq()

    implicit def xTag: ClassTag[X] = context.xTag
    implicit def aTag: ClassTag[A] = context.aTag

    
    @inline final def minWidth = 16
    @inline final def maxWidth = minWidth*4

    def createSeq(a: Array[X]): AnnTreeSeq[X,A] = createSeq(a,0,a.length)
    def toArray = {
      if (size < Int.MaxValue) {
        val array = new Array[X](size.toInt)
        repr.intoArray(array,0)
        array
      }
      else sys.error("sequence to big")
    }

    def createSeq(a: Array[X], start: Int, size: Int): AnnTreeSeq[X,A] = {
      if (size <= maxWidth) {
        val b = new Array[X](size.toInt)
        val end = start + size
        var i = start
        var ii = 0
        while (i < end) {
          b(ii) = a(i)
          i = i + 1
          ii = ii + 1
        }
        create(createLeaf(b)(this))
      }
      else {
        val ms = size / 2
        createSeq(a,start,ms).append(createSeq(a,start+ms,size - ms))
      }
    }
    def createLeaf(a: Array[X])(implicit c: SS) = {
      if (a.length == 0) empty
      else BSeqLeafImpl(a,annotator.manyX(a))
    }

    def createTree[ARR <: Array[SAS]](a: ARR)(implicit c: SS): SAS = {
      val sz = new Array[Long](a.length)
      val an = new Array[A](a.length)

      var ts: Long = 0
      
      var i = 0
      var s = a.length

      while (i < s) {
        ts = ts + a(i).size; sz(i) = ts; an(i) = a(i).annotation
        i = i + 1
      }
      
      BSeqTreeImpl(a,sz,annotator.manyA(an))
    }

    def createPair[AA1 <: SAS, AA2 <: SAS](ss1: AA1,ss2: AA2)(implicit c: SS): SAS = {
      createTree(Array(ss1,ss2)) // TODO: create specialized Pair
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
        val tsize = o1.size + o2.size
        if (tsize <= maxWidth) createLeaf(o1.toArray ++ o2.toArray)
        else {
          if (o1.size > o2.size) {
            val (l,r) = o1.split(o1.size/2)
            createPair(l,r.append(o2))
          }
          else {
            val (l,r) = o2.split(o2.size/2)
            createPair(o1.append(l),r)
          }
        }

      }
    }

    def asTree(t: SAS): BSeqTree[X,A] = t.asInstanceOf[BSeqTree[X,A]]
    def asLeaf(l: SAS): BSeqLeaf[X,A] = l.asInstanceOf[BSeqLeaf[X,A]]

    def sort = {
      if (size <= 1) this
      else if (size <= (64*64)) {
        val a = toArray

        spire.math.Sorting.mergeSort(a)(ordering,tag)
        createSeq(a)
      }
      else repr match {
        case l: BSeqLeaf[X,A] => {
          var a = toArray
          spire.math.Sorting.mergeSort(a)(ordering,tag)

          create(createLeaf(a)(this))
        }
        case t: BSeqTree[X,A] => {
          val childs: Array[SS] = t.childs.map(x => create(x))
          sort(childs,0,childs.size)
        }
      }
    }

    def sort(s: Array[SS], start: Int, size: Int): SS = {
      if (size == 1) { s(start).sort }
      else {
        val m = size / 2
        combineSorted(sort(s,start,m),(sort(s,start+m,size-m)),Union)
      }
    }
    
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
      // TODO: optimize this
      if (s1 eq s2) true
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
    override def iterator: SequenceIterator[X] = TreeSeqIterator[X,A]()(self)
  }

  trait BSeqTr[X,A] extends AnnSeqRepr[X,A,AnnTreeSeq[X,A]]
  {
    type AS = BSeqTr[X,A]
    type SS = AnnTreeSeq[X,A]
    type SAS = SS#AS

    def getLeaf(i: Long): (BSeqLeaf[X,A],Int)
    def getLeaf2(i: Long): BSeqLeaf[X,A]
    def intoArray(dest: Array[X], i: Int): Int
  }

  trait BSeqTree[@sp X,A] extends BSeqTr[X,A] {
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
    def first(implicit c: SS) = childs(0).first
    def last(implicit c: SS) = childs(childs.length-1).last
    def apply(i: Long)(implicit c: SS) = {
      val cc = childAtIndex(i)
      val o = offsetForChild(cc)
      childAt(cc)(i-o)
    }
    def getLeaf(i: Long): (BSeqLeaf[X,A],Int) = {
      val cc = childAtIndex(i)
      val o = offsetForChild(cc)
      childAt(cc).getLeaf(i-o)
    }
    def getLeaf2(i: Long): BSeqLeaf[X,A] = {
      val cc = childAtIndex(i)
      val o = offsetForChild(cc)
      childAt(cc).getLeaf2(i-o)
    }
    override def toString = childs.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class EmptySeq[@sp X,A]() extends BSeqTr[X,A] {
    def error = sys.error("empty")
    def annotation(implicit c: SS) = c.annotator.none
    def size = 0.toLong
    def height = -1
    def split(i: Long)(implicit c: SS) = (this,this)
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = o
    def annotationRange(start: Long,end: Long)(implicit c: SS) = c.annotator.none
    def equalToTree[AAS <: SAS](o: AAS)(implicit c: SS): Boolean = (o.size == 0)
    def first(implicit c: SS) = error
    def last(implicit c: SS) = error
    def apply(i: Long)(implicit c: SS) = error
    def getLeaf(i: Long): (BSeqLeaf[X,A],Int) = error
    def getLeaf2(i: Long): BSeqLeaf[X,A] = error
    def intoArray(dest: Array[X], i: Int) = i
    override def toString = "<>"
  }

  trait BSeqLeaf[@sp X,A] extends BSeqTr[X,A]  {
    def toArray: Array[X]
    def height = 0
    override def toString = toArray.foldLeft("<")((x,y) => x + " " + y) + " >"
  }

  case class BSeqLeafImpl[@sp X,A](array: Array[X],ann: A)
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
    def first(implicit c: SS) = array(0)
    def last(implicit c: SS) = array(array.length - 1)
    def append[AAS <: SAS](o: AAS)(implicit c: SS): SAS = c.append(this,o)
    def apply(i: Long)(implicit c: SS) = array(i.toInt)
    def getLeaf(i: Long): (BSeqLeaf[X,A],Int) = (this,i.toInt)
    def getLeaf2(i: Long): BSeqLeaf[X,A] = this
    def intoArray(dest: Array[X], i: Int): Int = {
      array.copyToArray(dest,i,array.length)
      array.length + i
    }
  }

  case class BSeqTreeImpl[@sp X,A, ARR <: Array[AnnTreeSeq[X,A]#AS]]
  (childs: ARR,sizes: Array[Long],ann: A) extends BSeqTree[X,A] {
    def annotation(implicit c: SS) = ann
    def intoArray(dest: Array[X], i: Int): Int = {
      var ii = i
      var s = childs.length
      var idx = 0
      while (idx < s) {
        ii = childs(idx).intoArray(dest,ii)
        idx = idx + 1
      }
      ii
    }
  }

  trait AnnTreeSeqImpl[@sp X,A] extends AnnTreeSeq[X,A] {
    type C = AnnotationOrderingContext[X,A]
    def self = this
    def emptySeq = EmptyAnnotatedTreeSeq()(context)
    def create(s: AnnTreeSeq[X,A]#AS) = FullAnnotatedTreeSeq(s)(context)
    implicit def xtag = context.xTag
  }

  case class EmptyAnnotatedTreeSeq[@sp X,A]
  (implicit c: AnnotationOrderingContext[X,A]) extends AnnTreeSeqImpl[X,A] {
    def context = c
    def repr = empty
  }

  case class FullAnnotatedTreeSeq[@sp X,A]
  (repr: AnnTreeSeq[X,A]#AS)(implicit c: AnnotationOrderingContext[X,A]) extends AnnTreeSeqImpl[X,A] {
    def context = c
  }

  def seqFactory[@sp X](implicit ord: Order[X], ct: ClassTag[X], ca: ClassTag[Statistics[X]]) = {
    val ann = StatisticsAnnotator[X]()
    val eq = EqualStatP()(ann)
    EmptyAnnotatedTreeSeq()(AnnOrdContextImpl(ann,eq,ord,ct,ca))
  }

  def defaultSeqFactory[@sp X](implicit ord: Order[X], ct: ClassTag[X]) = {
    val ann = NoAnnotator[X]
    val ca = implicitly[ClassTag[NoAnnotation]]
    val eq = EqualNoAnn
    EmptyAnnotatedTreeSeq()(AnnOrdContextImpl(ann,eq,ord,ct,ca))
  }


  private case class TreeSeqIterator[@sp X,@sp A](implicit seq: AnnTreeSeq[X,A]) extends SequenceIterator[X] {
    var pos: Long = 0
    var li: Int = seq.repr.getLeaf(0)._2
    var leaf: BSeqLeaf[X,A] = seq.repr.getLeaf(0)._1

    val size = seq.size
    var lsize: Long = seq.repr.getLeaf(0)._1.size

    def next: X = {
      if (li < lsize) {
        val v = leaf(li) ; li = li + 1
        v
      }
      else {
        pos = pos + lsize
        if (pos < size) {
          val (newleaf,newli) = seq.repr.getLeaf(pos)
          leaf = newleaf
          li = newli
          lsize = leaf.size
          next
        }
        else sys.error("no more elements")
      }
    }
    def position = pos
    def hasNext = {
      if (li < lsize) true
      else (pos+lsize) < size
    }
    def goto(i: Long) = {
      val (newleaf,newli) = seq.repr.getLeaf(pos)
      leaf = newleaf
      li = newli
      lsize = leaf.size
      pos = i
    }
  }

  def insertionSort[@sp X](input: Array[X], ordering: Order[X]): Array[X] = {
    var i = 1
    while  (i < input.length) {
      var j = i
      var tmp = input(j)
      while (j > 0) {
        if (ordering.lt(input(j),input(j-1))) {
          tmp = input(j)
          input(j) = input(j - 1)
          input(j - 1) = tmp
        }
        j = j - 1
      }
      i = i + 1
    }
    input
  }
}
