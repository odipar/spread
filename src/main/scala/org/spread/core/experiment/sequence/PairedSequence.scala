package org.spread.core.experiment.sequence

import org.spread.core.language.Annotation.sp

object PairedSequence {

  import org.spread.core.experiment.sequence.Sequence.Seq
  import scala.reflect.ClassTag

  /*trait PairedSeq[@sp X1, @sp X2, S1 <: Seq[X1, S1], S2 <: Seq[X2, S2], S <: PairedSeq[X1, X2, S1, S2, S]]
    extends Seq[(X1, X2), S] {

    {assert(left.size == right.size)}

    def left: S1
    def right: S2
    def L = left
    def R = right
    def emptySeq = create(left.emptySeq, right.emptySeq)
    def create(l: S1, r: S2): S
    implicit def xTag = left.tag
    implicit def yTag = right.tag

    def createSeq(a: Array[(X1, X2)]) = {
      // TODO: optimize
      val x1 = a.map(_._1).toArray
      val x2 = a.map(_._2).toArray

      create(left.createSeq(x1), right.createSeq(x2))
    }

    def toArray: Array[(X1, X2)] = {
      val x1 = left.toArray
      val x2 = right.toArray
      val result = new Array[(X1, X2)](x1.length)

      var i = 0
      var s = x1.length
      while (i < s) {
        result(i) = (x1(i), x2(i))
        i = i + 1
      }
      result
    }

    def append[S2 <: S](o: S2): S = create(left append o.left, right append o.right)

    def split(o: Long) = {
      val (ll, lr) = left.split(o)
      val (rl, rr) = right.split(o)
      (create(ll, rl), create(lr, rr))
    }

    def equalTo[S2 <: S](o: S2): Boolean = left.equals(o.left) && right.equals(o.right)
    def size = left.size
    def height = (left.height max right.height) + 1
    def first = (left.first, right.first)
    def last = (left.last, right.last)
    def apply(i: Long) = (left(i), right(i))
  }

  case class PairSeqImpl[@sp X1, @sp X2, S1 <: Seq[X1, S1], S2 <: Seq[X2, S2]](left: S1, right: S2)
                                                                              (implicit t: ClassTag[(X1, X2)])
    extends PairedSeq[X1, X2, S1, S2, PairSeqImpl[X1, X2, S1, S2]] {

    type S = PairSeqImpl[X1, X2, S1, S2]

    def create(l: S1, r: S2): S = PairSeqImpl(l, r)
    def tag = t
    def self = this
    def parts = ???
  }

  def pair[@sp X1, @sp X2, S1 <: Seq[X1, S1], S2 <: Seq[X2, S2]]
  (left: Seq[X1, S1], right: Seq[X2, S2])(implicit t: ClassTag[(X1, X2)]) = {
    PairSeqImpl[X1, X2, S1, S2](left.asInstanceOf[S1], right.asInstanceOf[S2])
  }   */
}
