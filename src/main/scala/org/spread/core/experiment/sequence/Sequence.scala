package org.spread.core.experiment.sequence

import org.spread.core.language.Annotation.sp

object Sequence {
  import scala.reflect.ClassTag
  
  trait Seq[@sp X, S <: Seq[X, S]] {
    def self: S
    def emptySeq: S

    def parts: Array[S]

    def append[S2 <: S](o: S2): S
    def split(o: Long): (S,S)
    def equalTo[S2 <: S](o: S2): Boolean

    def size: Long
    def height: Int

    def apply(i: Long): X
    def first: X
    def last: X

    def tag: ClassTag[X]
    def createSeq(a: Array[X]): S
    def toArray: Array[X]

    def isEmpty: Boolean = (size == 0)
    def ++[S2 <: S](o: S2): S = append(o)
  }
}
