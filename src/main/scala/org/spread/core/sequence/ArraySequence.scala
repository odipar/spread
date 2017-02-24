package org.spread.core.sequence

import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.Sequence._

import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp

object ArraySequence {
  case class ArraySeq[@sp X: ClassTag](x: Array[X]) extends SeqImpl[X,ArraySeq[X]] {
    type S = ArraySeq[X]

    def self: S = this
    def size: Long = x.length
    def height = 0

    def emptySeq = ArraySeq(Array())
    def append[S2 <: S](o: S2): S = ArraySeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (ArraySeq(l),ArraySeq(r)) }
    def equalTo[S2 <: S](o: S2): Boolean = {
      val s = x.length
      val ox = o.x

      val i = 0
      var eq = true
      while ((i < s) || !eq) { eq = (x(i) ==  ox(i)) }
      eq
    }
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A = annotator.manyX(x)
    def apply(i: Long) = x(i.toInt)
    def first = x(0)
    def last = x(x.length-1)
  }
}
