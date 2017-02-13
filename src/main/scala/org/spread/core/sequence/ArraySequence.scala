package org.spread.core.sequence

import org.spread.core.sequence.Sequence._

import scala.reflect.ClassTag

object ArraySequence {

  case class ArraySeq[@specialized(Int,Long,Double) X: ClassTag](x: Array[X]) extends SeqImpl[X,ArraySeq[X],NoContext] {
    type S = ArraySeq[X]
    type TC = NoContext

    def context = NoContext
    def self: S = this
    def size: Long = x.length
    def height = 0

    def some = x(x.length/2)
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
  }
}
