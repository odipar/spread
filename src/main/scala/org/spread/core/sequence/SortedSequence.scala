package org.spread.core.sequence

import Sequence._
import org.spread.core.sequence.ArraySequence.ArraySeq

import scala.reflect.ClassTag

/**
  * Created by rapido on 07/02/17.
  */
/*object SortedSequence {
  case class SortedSequence[X, SS <: Seq[X,SS]](x: SS) extends SeqImpl[X,SortedSequence[X,SS]] {
      type S = SortedSequence[X,SS]
      type TC = NoContext

      def context = NoContext
      def self: S = this

      def size: Long = x.size
      def height = x.height+1
      def some = x.some
    
      def emptySeq = SortedSequence(x.emptySeq)
      def append[X2 >: X, SS >: S <: Seq[X2,SS]](o: Seq[X2,SS]): Seq[X2,SS]= SortedSequence(x.append(o.x))
      def split(o: Long) = { val (l,r) = x.split(o) ; (SortedSequence(l),SortedSequence(r)) }
      def equalTo[SS <: S](o: SS): Boolean = sys.error("no yet")
  }
} */
