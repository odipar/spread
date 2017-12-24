package org.spread.core.sequence

import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.OrderingSequence._
import org.spread.core.sequence.AnnotatedTreeSequence.insertionSort

import scala.reflect.ClassTag
import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.Sequence.SequenceIterator
import org.spread.core.sequence.OrderingSequence._


object ArraySequence {

  case class ArraySeq[@sp X](x: Array[X])(implicit ord: Order[X], xt: ClassTag[X]) extends OrderingSeq[X,ArraySeq[X]] {
    type S = ArraySeq[X]

    def self: S = this
    def size: Long = x.length
    def height = 0

    def tag = xt

    def ordering = ord

    def sort = {
      if (size < (64*64)){
        val a = x.clone
        spire.math.Sorting.mergeSort(a)
        ArraySeq(a)
      }
      else defaultSort
    }
    def emptySeq = ArraySeq(Array())
    def createSeq(a: Array[X]) = ArraySeq(a)
    def toArray = x.clone
    def append[S2 <: S](o: S2): S = ArraySeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (ArraySeq(l),ArraySeq(r)) }
    def equalTo[S2 <: S](o: S2): Boolean = {
      val s = x.length
      val ox = o.x

      if (s == ox.length) {
        var i = 0
        var eq = true
        while ((i < s) && eq) { eq = (x(i) == ox(i)) ; i = i + 1}
        eq
      }
      else false
    }
    def apply(i: Long) = x(i.toInt)
    def first = x(0)
    def last = x(x.length-1)
  }
}
