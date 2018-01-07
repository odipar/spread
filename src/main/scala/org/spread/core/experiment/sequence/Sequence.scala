package org.spread.core.experiment.sequence

import org.spread.core.experiment.sequence.Container.{ArrayContainer, Container}
import org.spread.core.language.Annotation.sp
import org.spread.core.splithash.Hashing

object Sequence {

  import scala.reflect.ClassTag

  trait Seq[@sp X, S <: Seq[X, S]] extends Container[X, S] {
    type SS <: ArraySeq[X, SS]

    def self: S

    def append[S2 <: S](o: S2): S
    def split(o: Long): (S, S)
    def equalTo[S2 <: S](o: S2): Boolean

    def size: Long
    def height: Int

    def apply(i: Long): X
    def first: X
    def last: X

    def parts: Array[S]
    def whole: SS

    def emptySeq: S
    def createSeq(a: Array[X]): S

    def isEmpty: Boolean = size == 0
    def ++[S2 <: S](o: S2): S = append(o)
  }

  trait SeqContext[@sp X, S <: Seq[X, S]]

  trait ArraySeq[@sp X, AS <: ArraySeq[X, AS]] extends ArrayContainer[X, AS] {
    def size: Int
    def apply(i: Int): X
    def append(o: AS)(implicit c: ClassTag[X]): AS
    def splitAt(i: Int): (AS, AS)
    def toArray: Array[X]
    def ++(o: AS)(implicit c: ClassTag[X]): AS = append(o)
  }

  case class ArraySeqImpl[@sp X](x: Array[X]) extends ArraySeq[X, ArraySeqImpl[X]] {
    def map[@sp Y](f: X => Y)(implicit c: ClassTag[Y]): ArraySeqImpl[Y] = ArraySeqImpl(x.map(f).toArray)
    def size: Int = x.length
    def apply(i: Int): X = x(i)
    def append(o: ArraySeqImpl[X])(implicit c: ClassTag[X]): ArraySeqImpl[X] = ArraySeqImpl(x ++ o.x)
    def splitAt(i: Int): (ArraySeqImpl[X], ArraySeqImpl[X]) = {
      val (l, r) = x.splitAt(i)
      (ArraySeqImpl(l), ArraySeqImpl(r))
    }
    def toArray: Array[X] = x
    override def equals(o: Any): Boolean = o match {
      case a: ArraySeqImpl[X] => {
        val a1 = x
        val a2 = a.x

        if (a1.length != a2.length) false
        else if (a1 eq a2) true
        else {
          val s = a1.length
          var i = 0
          var result = true

          while ((i < s) && result) {
            result = a1(i) == a2(i)
            i += 1
          }
          result
        }
      }
      case _ => false
    }
    override lazy val hashCode: Int = {
      var hash: Int = Hashing.siphash24(x.length, -x.length)
      var s = x.length
      var i = 0
      while (i < s) {
        hash = Hashing.siphash24(hash, x(i).hashCode)
        i += 1
      }
      hash
    }
  }
}
