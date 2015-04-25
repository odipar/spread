package spread

import spread.IncrementalArithmetic._

object IncrementalVector{

  import scala.reflect.ClassTag
  import IncrementalMemoization._

  var ic: Long = 0
  var iic: Long = 0
  var iiic: Long = 0

  trait IVector[@specialized(Int,Long,Double) X] {
    {
      ic = ic + 1
    }
    def size: Int
    def level: Int
    def append(o: IVector[X]): IVector[X]
    def split: Array[IVector[X]]
    def slice(start: Int, end: Int): IVector[X]
    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y]
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y): Y
  }

  final def minWidthOfNonRoot = 16
  final def maxWidth = 64

  val sz = new Array[Int](0)

  def treevector[X](a: Array[IVector[X]]) = {
    val sz = new Array[Int](a.length)
    var ts = 0
    var i = 0;
    var s = a.length
    while (i < s) {
      val aa = a(i)
      ts = ts + aa.size
      sz(i) = ts
      i = i + 1
    }
    TreeVector(a,sz)
  }

  def treevector[X](a: Array[IVector[X]], s: Array[Int]) = TreeVector(a,s)

  /* nicked from Avail's TreeTuple: by Mark van Gulik */
  case class TreeVector[X](childs: Array[IVector[X]], sizes: Array[Int]) extends IVector[X] {
    { iic = iic + 1 }
    val size = sizes(sizes.length-1)

    val level = childs(0).level + 1
    def childAt(i: Int): IVector[X] = childs(i-1)
    def append(o: IVector[X]): IVector[X] = appendAtLeastOneTree(this,o)
    def slice(start: Int, end: Int) = sliceTree(this,start,end)
    def split = childs
    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = {
      val nr = new Array[IVector[Y]](childs.length)
      var i = 0
      val s = childs.length
      val ch = childs
      while (i < s) {
        nr(i) = ch(i).map(f)
        i = i + 1
      }
      treevector(nr,sizes)
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](sf: Y, f: (Y,X) => Y) = {
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

    override def toString = {
      var r = "< "
      for (i <- childs) {
        r = r + i.toString + " "
      }
      r + ">"
    }
  }

  def amap[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y](x: Array[X], y: Array[Y], f: X => Y): Array[Y] = {
    val s = x.length
    var i = 0

    while (i < s) {
      y(i) = f(x(i))
      i = i + 1
    }
    y
  }

  def afold[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y](x: Array[X], sf: Y, f: (Y,X) => Y): Y = {
    val s = x.length
    var i = 0
    var fold = sf
    while (i < s) {
      fold = f(fold,x(i))
      i = i + 1
    }
    fold
  }

  def childSubscriptForIndex[X](v1: TreeVector[X], index: Int): Int = {
    val sizes = v1.sizes
    var i = 0

    if (index <= v1.size && index >= 1) {
      while (v1.sizes(i) < index) i = i + 1
      i + 1
    }
    else sys.error("index out of bounds")
  }

  def appendSameLevel[X](v1: TreeVector[X], v2: TreeVector[X]) = {
    val level = v1.level

    assert (level == v2.level)

    val count1 = v1.childs.length
    val count2 = v2.childs.length
    val tcount = count1 + count2

    if (count1 >= minWidthOfNonRoot && count2 >= minWidthOfNonRoot) createPair(v1,v2)
    else if (tcount <= maxWidth) {
      val nc: Array[IVector[X]] = new Array(tcount)
      v1.childs.copyToArray(nc,0)
      v2.childs.copyToArray(nc,count1)
      treevector(nc)
    }
    else {
      val r = v1.childs ++ v2.childs
      val (left,right) = r.splitAt((tcount + 1) >> 1)
      createPair(treevector(left),treevector(right))
    }
  }

  def appendAtLeastOneTree[@specialized(Int,Long,Double) X](vv1: IVector[X], vv2: IVector[X]): TreeVector[X] = {

    val size1 = vv1.size
    val size2 = vv2.size
    val level1 = vv1.level
    val level2 = vv2.level

    if (level1 == level2) appendSameLevel(vv1.asInstanceOf[TreeVector[X]], vv2.asInstanceOf[TreeVector[X]])
    else if (level1 > level2) {
      val v1 = vv1.asInstanceOf[TreeVector[X]]
      val childCount1 = v1.childs.length
      val oldLast = v1.childAt(childCount1)
      val newLast = oldLast.append(vv2)

      if (newLast.level == level1) {
        val nc: Array[IVector[X]] = new Array(childCount1-1)
        v1.childs.copyToArray(nc,0)
        appendSameLevel(treevector(nc),newLast.asInstanceOf[TreeVector[X]])
      }
      else {
        val nc = v1.childs.clone()
        nc(childCount1-1) = newLast
        treevector(nc)
      }
    }
    else {
      val v2 = vv2.asInstanceOf[TreeVector[X]]
      val childCount2 = v2.childs.length
      val oldFirst = v2.childAt(1)
      val newFirst = vv1.append(oldFirst)

      if (newFirst.level == level2) {
        appendSameLevel(newFirst.asInstanceOf[TreeVector[X]],treevector(v2.childs.tail))
      }
      else {
        val nc = v2.childs.clone()
        nc(0) = newFirst
        treevector(nc)
      }
    }
  }

  def offsetForChildSubscript[X](v1: TreeVector[X], index: Int): Int = {
    if (index == 1) 0
    else v1.sizes(index-2)
  }

  def sliceTree[X](v1: TreeVector[X], start: Int, end: Int): IVector[X] = {
    if (start == 1 && end == v1.size) v1
    else {
      val lowChildIndex = childSubscriptForIndex(v1, start)
      val highChildIndex = childSubscriptForIndex(v1, end)

      if (lowChildIndex == highChildIndex)
      {
        val offset = offsetForChildSubscript(v1,lowChildIndex)
        v1.childAt(lowChildIndex).slice(start - offset, end - offset)
      }
      else {
        val leftOffset = offsetForChildSubscript(v1,lowChildIndex)
        val rightOffset = offsetForChildSubscript(v1,highChildIndex)

        val leftPart = v1.childAt(lowChildIndex).slice(start - leftOffset, offsetForChildSubscript(v1,lowChildIndex+1) - leftOffset)
        val rightPart = v1.childAt(highChildIndex).slice(1,end - rightOffset)

        var accumulator = leftPart
        var childIndex = lowChildIndex + 1

        if (lowChildIndex + 5 < highChildIndex)
        {
          val nc: Array[IVector[X]] = new Array(highChildIndex-lowChildIndex-1)
          var dst = 0
          while (childIndex < highChildIndex) {
            nc(dst) = v1.childAt(childIndex)
            dst = dst + 1
            childIndex = childIndex + 1
          }
          accumulator = accumulator.append(treevector(nc))
        }
        else
        {
          while (childIndex < highChildIndex) {
            accumulator = accumulator.append(v1.childAt(childIndex))
            childIndex = childIndex + 1
          }
        }
        accumulator.append(rightPart)
      }
    }
  }
  var pp: Long = 0

  def createPair[X](v1: TreeVector[X], v2: TreeVector[X]) = {
    { pp = pp + 1 }

    val c: Array[IVector[X]] = Array(v1,v2)
    treevector(c)
  }

  case class ArrayVector[@specialized(Int,Long,Double) X: ClassTag](a: Array[X]) extends IVector[X] {
    def size: Int = a.length
    def first = a(0)
    def last = a(size.toInt - 1)
    def level: Int = 0
    def split = Array()
    def append(o: IVector[X]): IVector[X] = {
      val ts = size + o.size
      if (ts <= 64) {
        o match {
          case ArrayVector(x) => ArrayVector(a ++ x)
          case LeafVector(x) => ArrayVector(a :+ x)
          case _ => sys.error("not yet")
        }
      }
      else {
        if (this.level == 0 && o.level == 0) {
          val t: Array[IVector[X]] = Array(this,o)
          treevector(t)
        }
        else appendAtLeastOneTree(this,o)
      }
    }

    def slice(start: Int, end: Int): IVector[X] = {
      val s = start max 1
      val e = end min size

      ArrayVector(a.slice(s-1,e))
    }


    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = {
      ArrayVector(amap(a,new Array[Y](size),f))
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y) = {
      afold(a,s,f)
    }

    override def toString = {
      var r = "< "
      for (i <- a) {
        r = r + i.toString + " "
      }
      r + ">"
    }
  }

  case class LeafVector[@specialized(Int,Long,Double) X: ClassTag](x: X) extends IVector[X] {
    def size = 1
    def level = 0

    def append(o: IVector[X]): IVector[X] = {
      ArrayVector(Array[X](x)).append(o)
    }

    def split = Array()
    def slice(start: Int, end: Int): IVector[X] = {
      val s = start max 1
      val e = end min 1

      if (s == e && s == 1) this
      else sys.error("no slice: empty vector?")
    }

    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = LeafVector(f(x))
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y) = f(s,x)

    override def toString = x.toString
  }

  object Add2 extends (Double => Double) {
    final def apply(d: Double) : Double = {
      d + 1.0
    }
  }

  object Add3 extends ((Double,Double) => Double) {
    final def apply(a1: Double, a2: Double) = a1 + a2
  }

  object Add4 extends ((Int,Int) => Int) {
    final def apply(a1: Int, a2: Int) = {
      iiic = iiic + 1

      a1 + a2
    }
  }

  object Concatenate extends ((IVector[Double],Double) => IVector[Double]) {
    final def apply(a1: IVector[Double], a2: Double): IVector[Double] = {
      a1.append(LeafVector(a2))
    }
  }

  object Mul3 extends (Int => Int) {
    final def apply(d: Int) : Int = {
      d * 3
    }
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I) = {
      if (~i < 2) i
      else %(fib,i - 1) + %(fib,i - 2)
    }
    override def toString = "fib"
  }

  object sum extends FA1[IVector[Int],Int] {
    def apply(v: F0[IVector[Int]]): I = {
        val vv = ~v
        if (vv.level == 0) vv.fold(0,Add4)
        else {
          val spl: Array[IVector[Int]] = vv.split
          var i = 1
          var s = spl.size
          var r = %(sum,ei(spl(0)))

          while (i < s) {
            r = r + %(sum,ei(spl(i)))
            i = i + 1
          }

          r
        }
    }
  }

  final def main(args: Array[String]): Unit = {
    var k = new Array[Int](500)
    var i = 0

    while (i < k.length) {
      k(i) = i
      i = i + 1
    }
    println(part(k))
  }

  abstract class Foreach[@specialized(Int,Long,Double) X, @specialized(Int,Long,Double) Y] extends (Array[X] => Y) {
    def apply(a: Array[X]): Y
  }

  /*case class FMap[X,Y: ClassTag](f: X => Y) extends Foreach[X,Array[Y]] {
    def apply(a: Array[X]): Array[Y] = {

    }
  } */

  object mp extends (Unit => Int) {
    def apply(a: Unit) = a.hashCode
  }

  // Canonical B-Tree
  trait CBTree

  val multiplier = 1664525

  val wsize = 16

  val mm = {
    var i = 0
    var m = multiplier
    while (i < wsize) {
      m = m * multiplier
      i = i + 1
    }
    m
  }

  def part[X: ClassTag](a: Array[X]) = {
    import Hashing.jh

    var r: List[ArrayVector[X]] = List()
    var i = 0
    var s = a.length
    var h: Int = 0
    var w = wsize
    var ii = 0

    while (i < s) {
      h = (h + jh(jh(a(i)))) * multiplier
      if (i >= wsize) {
        h = h - (jh(jh(a(i-w))) * mm)
      }

      if (((h >> 5) & 7) == 3) {
        println("i: " + i)

        val na: Array[X] = new Array[X](i-ii)
        r = r :+ new ArrayVector(na)
        ii = i
      }

      i = i + 1
    }

    val na: Array[X] = new Array[X](i-ii)
    r = r :+ new ArrayVector(na)

    r
  }
}
