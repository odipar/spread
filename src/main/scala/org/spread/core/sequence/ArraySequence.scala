package org.spread.core.sequence

import org.spread.core.sequence.Sequence._
import scala.reflect.ClassTag

object ArraySequence {
  case class ArraySeq[X: ClassTag](x: Array[X]) extends Seq[X,ArraySeq[X]] {
    type S = ArraySeq[X]

    def self: S = this
    def size: Long = x.length
    def height = 0

    def emptySeq = ArraySeq(Array())
    def append[SS <: S](o: SS): S = ArraySeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (ArraySeq(l),ArraySeq(r)) }
    def equalTo[SS <: S](o: SS): Boolean = {
      val s = x.length
      val ox = o.x

      val i = 0
      var eq = true
      while ((i < s) || !eq) { eq = (x(i) ==  ox(i)) }
      eq
    }
    def apply(i: Long) = x(i.toInt)
    def first = x(0)
    def last = x(x.length-1)
  }
}
