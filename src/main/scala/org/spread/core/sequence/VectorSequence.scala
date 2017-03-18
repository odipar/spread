package org.spread.core.sequence

import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.Sequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence._

object VectorSequence {
  case class VectorSeq[@sp X](x: Vector[X])(implicit ord: Order[X], ct: ClassTag[X])
    extends OrderingSeq[X,VectorSeq[X]] {
    type S = VectorSeq[X]

    def ordering = ord

    def self: S = this
    def size: Long = x.length
    def height = 0

    def tag = ct
    def createSeq(a: Array[X]) = VectorSeq[X](a.toVector)
    
    def sort = defaultSort
    def emptySeq = VectorSeq(Vector())
    def toArray = x.toArray
    def append[S2 <: S](o: S2): S = VectorSeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (VectorSeq(l),VectorSeq(r)) }
    def equalTo[S2 <: S](o: S2): Boolean = x.equals(o.x)
    def first = x(0)
    def last = x.last
    def apply(i: Long) = x(i.toInt)
  }
}
