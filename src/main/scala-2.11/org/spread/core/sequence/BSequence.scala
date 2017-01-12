package org.spread.core.sequence
import scala.reflect.ClassTag

object BSequence{

  trait MyFold[@specialized(Int,Long,Double) Y, @specialized(Int,Long,Double) X] {
    def apply(y: Y, x: X): Y
  }

  trait BSeq[@specialized(Int,Long,Double) X] {
    def size: Long
    def append(o: BSeq[X]): BSeq[X]
    def slice(start: Long,end: Long): BSeq[X]
    def level: Int
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: MyFold[Y,X]): Y
  }

  final def minWidthOfNonRoot = 16
  final def maxWidth = 64

  def bseq[X](a: Array[BSeq[X]]) ={
    val sz = new Array[Long](a.length)
    var ts: Long = 0
    var i = 0
    var s = a.length
    while (i < s) {
      val aa = a(i)
      ts = ts + aa.size
      sz(i) = ts
      i = i + 1
    }
    BSeqTree(a,sz)
  }
  def bseq[X](a: Array[BSeq[X]],s: Array[Long]) = BSeqTree(a,s)

  /* nicked from Avail's TreeTuple: by Mark van Gulik */
  case class BSeqTree[X](childs: Array[BSeq[X]],sizes: Array[Long]) extends BSeq[X]{
    val size = sizes(sizes.length - 1)
    val level = childs(0).level + 1
    def childAt(i: Int): BSeq[X] = childs(i - 1)
    def append(o: BSeq[X]): BSeq[X] = appendAtLeastOneTree(this,o)
    def slice(start: Long,end: Long) = sliceTree(this,start,end)
    override def toString ={
      var r = "< "
      for (i <- childs) {
        r = r + i.toString + " "
      }
      r + ">"
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](sf: Y, f: MyFold[Y,X]) = {
      var i = 0
      val s = childs.length
      val ch = childs
      var fold = sf
      while (i < s) {
        fold = ch(i).fold(fold,f)
        i = i + 1
      }
      fold
    }
  }

  def childSubscriptForIndex[X](v1: BSeqTree[X],index: Long): Int ={
    val sizes = v1.sizes
    var i = 0
    if (index <= v1.size && index >= 1) {
      while (v1.sizes(i) < index) i = i + 1
      i + 1
    }
    else sys.error("index out of bounds")
  }
  def appendSameLevel[X](v1: BSeqTree[X],v2: BSeqTree[X]) ={
    val level = v1.level
    assert(level == v2.level)
    val count1 = v1.childs.length
    val count2 = v2.childs.length
    val tcount = count1 + count2
    if (count1 >= minWidthOfNonRoot && count2 >= minWidthOfNonRoot) createPair(v1,v2)
    else if (tcount <= maxWidth) {
      val nc: Array[BSeq[X]] = new Array(tcount)
      v1.childs.copyToArray(nc,0)
      v2.childs.copyToArray(nc,count1)
      bseq(nc)
    }
    else {
      val r = v1.childs ++ v2.childs
      val (left,right) = r.splitAt((tcount + 1) >> 1)
      createPair(bseq(left),bseq(right))
    }
  }
  def appendAtLeastOneTree[@specialized(Int,Long,Double) X](vv1: BSeq[X],vv2: BSeq[X]): BSeqTree[X] ={
    val size1 = vv1.size
    val size2 = vv2.size
    val level1 = vv1.level
    val level2 = vv2.level
    if (level1 == level2) appendSameLevel(vv1.asInstanceOf[BSeqTree[X]],vv2.asInstanceOf[BSeqTree[X]])
    else if (level1 > level2) {
      val v1 = vv1.asInstanceOf[BSeqTree[X]]
      val childCount1 = v1.childs.length
      val oldLast = v1.childAt(childCount1)
      val newLast = oldLast.append(vv2)
      if (newLast.level == level1) {
        val nc: Array[BSeq[X]] = new Array(childCount1 - 1)
        v1.childs.copyToArray(nc,0)
        appendSameLevel(bseq(nc),newLast.asInstanceOf[BSeqTree[X]])
      }
      else {
        val nc = v1.childs.clone()
        nc(childCount1 - 1) = newLast
        bseq(nc)
      }
    }
    else {
      val v2 = vv2.asInstanceOf[BSeqTree[X]]
      val childCount2 = v2.childs.length
      val oldFirst = v2.childAt(1)
      val newFirst = vv1.append(oldFirst)
      if (newFirst.level == level2) {
        appendSameLevel(newFirst.asInstanceOf[BSeqTree[X]],bseq(v2.childs.tail))
      }
      else {
        val nc = v2.childs.clone()
        nc(0) = newFirst
        bseq(nc)
      }
    }
  }
  def offsetForChildSubscript[X](v1: BSeqTree[X],index: Int): Long ={
    if (index == 1) 0
    else v1.sizes(index - 2)
  }
  def sliceTree[@specialized(Int,Long,Double) X](v1: BSeqTree[X],start: Long,end: Long): BSeq[X] ={
    if (start == 1 && end == v1.size) v1
    else {
      val lowChildIndex = childSubscriptForIndex(v1,start)
      val highChildIndex = childSubscriptForIndex(v1,end)
      if (lowChildIndex == highChildIndex) {
        val offset = offsetForChildSubscript(v1,lowChildIndex)
        v1.childAt(lowChildIndex).slice(start - offset,end - offset)
      }
      else {
        val leftOffset = offsetForChildSubscript(v1,lowChildIndex)
        val rightOffset = offsetForChildSubscript(v1,highChildIndex)
        val leftPart = v1.childAt(lowChildIndex).slice(start - leftOffset,offsetForChildSubscript(v1,lowChildIndex + 1) - leftOffset)
        val rightPart = v1.childAt(highChildIndex).slice(1,end - rightOffset)
        var accumulator = leftPart
        var childIndex = lowChildIndex + 1
        while (childIndex < highChildIndex) {
          accumulator = accumulator.append(v1.childAt(childIndex))
          childIndex = childIndex + 1
        }
        accumulator.append(rightPart)
      }
    }
  }
  def createPair[X](v1: BSeqTree[X],v2: BSeqTree[X]) ={
    val c: Array[BSeq[X]] = Array(v1,v2)
    bseq(c)
  }

  case class ArrayBlockSeq[@specialized(Int,Long,Double) X: ClassTag](a: Array[X]) extends BSeq[X]{
    def size: Long = a.length
    def first = a(0)
    def last = a(size.toInt - 1)
    def level: Int = 0
    def append(o: BSeq[X]): BSeq[X] ={
      val ts = size + o.size
      if (ts <= maxWidth) {
        o match {
          case ArrayBlockSeq(x) => ArrayBlockSeq(a ++ x)
          case BSeqLeaf(x) => ArrayBlockSeq(a :+ x)
          case _ => sys.error("not yet")
        }
      }
      else {
        if (this.level == 0 && o.level == 0) {
          val t: Array[BSeq[X]] = Array(this,o)
          bseq(t)
        }
        else appendAtLeastOneTree(this,o)
      }
    }
    def slice(start: Long,end: Long): BSeq[X] ={
      val s = start max 1
      val e = end min size
      ArrayBlockSeq(a.slice(s.toInt - 1,e.toInt))
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: MyFold[Y,X]) = {
      afold(a,s,f)
    }
    override def toString ={
      var r = "< "
      for (i <- a) {
        r = r + i.toString + " "
      }
      r + ">"
    }
  }

  def afold[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y](x: Array[X], sf: Y, f: MyFold[Y,X]): Y = {
    val s = x.length
    var i = 0
    var fold = sf
    while (i < s) {
      fold = f(fold,x(i))
      i = i + 1
    }
    fold
  }

  case class BSeqLeaf[@specialized(Int,Long,Double) X: ClassTag](x: X) extends BSeq[X]{
    def size = 1
    def level = 0
    def append(o: BSeq[X]): BSeq[X] ={
      ArrayBlockSeq(Array[X](x)).append(o)
    }
    def slice(start: Long,end: Long): BSeq[X] ={
      val s = start max 1
      val e = end min 1
      if (s == e && s == 1) this
      else sys.error("no slice: empty vector?")
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: MyFold[Y,X]) = f(s,x)
    override def toString = x.toString
  }

  object Sum extends MyFold[Long,Int] {
    def apply(a: Long, i: Int): Long = {
      (a + i)
    }
  }
  final def main(args: Array[String]): Unit ={
    var a = ArrayBlockSeq(Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))
    var b: BSeq[Int] = ArrayBlockSeq(Array())

    for (i <- 0 to 10000000/4) {
      b = b.append(a)
      if ((i % 100000) == 0) { println("b: " + b.level + " size: " + b.size) }
    }

    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)
    println("b: " + b.level + " size: " + b.size)
    b = b.append(b)

    println("sum: " + b.fold[Long](0,Sum))

    /*val aa: Array[Int] = new Array(512000204)

    val s = aa.size
    var k = 0
    var sum: Long = 0
    while (k < s) {
      aa(k) = k
      k = k + 1
    }

    for (i <- 1 to 10) {
      val s = afold[Int,Long](aa,0,Sum)
      println("s: "+ s)
      println("i: "+ i)
    }

    for (i <- 1 to 10) {
      var k = 0
      val s = aa.size
      var sum: Long = 0
      while (k < s) {
        sum = sum + aa(k)
        k = k + 1
      }
      println("sum: "+ sum)
      println("i: "+ i)*/

  }
}