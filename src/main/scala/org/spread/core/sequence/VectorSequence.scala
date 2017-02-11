package org.spread.core.sequence

import org.spread.core.sequence.Sequence._
import scala.reflect.ClassTag

object VectorSequence {

  case class VectorSeq[@specialized(Int,Long,Double) X: ClassTag](x: Vector[X]) extends SeqImpl[X,VectorSeq[X]] {
    type S = VectorSeq[X]
    type TC = NoContext

    def context = NoContext
    def self: S = this
    def size: Long = x.length
    def height = 0
    
    def emptySeq = VectorSeq(Vector())
    def some = x(x.size/2)
    def append[SS <: S](o: SS): S = VectorSeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (VectorSeq(l),VectorSeq(r)) }
    def equalTo[SS <: S](o: SS): Boolean = x.equals(o.x)
  }
}
