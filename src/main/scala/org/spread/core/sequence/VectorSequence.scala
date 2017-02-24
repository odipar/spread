package org.spread.core.sequence

import cats.Order
import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.Sequence._

import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence.OrderingSeq

object VectorSequence {
  case class VectorSeq[@sp X: ClassTag](x: Vector[X])(implicit ord: Order[X])
    extends OrderingSeq[X,VectorSeq[X]] {
    type S = VectorSeq[X]

    def ordering = ord

    def self: S = this
    def size: Long = x.length
    def height = 0

    def sort = defaultSort
    def emptySeq = VectorSeq(Vector())
    def append[S2 <: S](o: S2): S = VectorSeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (VectorSeq(l),VectorSeq(r)) }
    def equalTo[S2 <: S](o: S2): Boolean = x.equals(o.x)
    def first = x(0)
    def last = x.last
    def apply(i: Long) = x(i.toInt)
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A = ???
  }
}
