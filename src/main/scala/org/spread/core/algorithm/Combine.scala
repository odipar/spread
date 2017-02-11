package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.AnnotatedSequence._
import scala.language.{existentials, implicitConversions}

object Combine {
  type SEQSTAT[@specialized(Int,Long,Double) X,S <: OASEQ[X,Statistics[X],S]] = OASEQ[X,Statistics[X],S]
  
  private def sort2[@specialized(Int,Long,Double) X,S <: SEQSTAT[X,S]](x: SEQSTAT[X,S]) = {
    Combiner[X,S]().sort(x.asInstanceOf[S])
  }
  private def union2[@specialized(Int,Long,Double) X,S <: SEQSTAT[X,S]](s1: SEQSTAT[X,S],s2: SEQSTAT[X,S]) = {
    Combiner[X,S]().union(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }
  private def difference2[@specialized(Int,Long,Double) X,S <: SEQSTAT[X,S]](s1: SEQSTAT[X,S],s2: SEQSTAT[X,S]) = {
    Combiner[X,S]().difference(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }
  private def intersect2[@specialized(Int,Long,Double) X,S <: SEQSTAT[X,S]](s1: SEQSTAT[X,S],s2: SEQSTAT[X,S]) = {
    Combiner[X,S]().difference(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }
  
  case class CombineSyntax[X, S <: SEQSTAT[X,S]](s: SEQSTAT[X,S]) {
    def sort = sort2(s)
    
    def union(o: SEQSTAT[X,S]) = union2(s,o)
    def difference(o: SEQSTAT[X,S]) = difference2(s,o)
    def intersect(o: SEQSTAT[X,S]) = intersect2(s,o)

    def :+:(o: SEQSTAT[X,S]) = union(o)
    def :*:(o: SEQSTAT[X,S]) = intersect(o)
    def :^:(o: SEQSTAT[X,S]) = difference(o)
  }

  implicit def syntax[@specialized(Int,Long,Double) X, S <: SEQSTAT[X,S]](s: SEQSTAT[X,S]): CombineSyntax[X,S] = {
    CombineSyntax(s)
  }

  private case class Combiner[@specialized(Int,Long,Double) X,S <: SEQSTAT[X,S]]() {
    type SQ = S
    
    def sort(r: SQ): SQ = {
      if (r.size == 0) r
      else if (r.annotation.sorted) r
      else {
        val (left,right) = r.split(r.size / 2)
        combine_(sort(left),sort(right),Union)
      }
    }

    // TODO: do a range search, and than split (also for bigger and same)
    def smaller(s: SQ,elem: X): SQ = {
      val ord = s.context.ord
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.lt(ann.last,elem)) s
      else if (ord.gteq(ann.first,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        smaller(l,elem).append(smaller(r,elem))
      }
    }

    def bigger(s: SQ,elem: X): SQ = {
      val ord = s.context.ord
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.gt(ann.first,elem)) s
      else if (ord.lteq(ann.last,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        bigger(l,elem).append(bigger(r,elem))
      }
    }

    def same(s: SQ,elem: X): SQ = {
      val ord = s.context.ord
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.equiv(ann.first,elem) && ord.equiv(ann.last,elem)) s
      else if (ord.gt(ann.first,elem)) s.emptySeq
      else if (ord.lt(ann.last,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        same(l,elem).append(same(r,elem))
      }
    }

    def repeat(s1: SQ,m: Int): SQ = {
      if (m == 0) s1.emptySeq
      else if (m == 1) s1
      else {
        val m2 = m / 2
        val sr = repeat(s1,m2)
        sr.append(sr).append(repeat(s1,m - (m2 * 2)))
      }
    }

    def multiply(s1: SQ,m: Int): SQ = {
      if (s1.size == 0) s1
      else if (s1.size == 1) repeat(s1,m)
      else {
        val (l,r) = s1.split(s1.size / 2)
        multiply(l,m).append(multiply(r,m))
      }
    }

    trait MergeOperator {
      def equal(s1: SQ): SQ
      def equalElems(s1: SQ,s2: SQ): SQ
      def append(s1: SQ,s2: SQ): SQ
    }

    case object Union extends MergeOperator {
      def equal(s1: SQ) = multiply(s1,2)
      def equalElems(s1: SQ,s2: SQ) = s1.append(s2)
      def append(s1: SQ,s2: SQ) = s1.append(s2)
    }

    case object Difference extends MergeOperator {
      def equal(s1: SQ) = s1.emptySeq
      def equalElems(s1: SQ,s2: SQ) = {
        val s = Math.abs(s1.size - s2.size)
        s1.split(s)._1
      }
      def append(s1: SQ,s2: SQ) = s1.append(s2)
    }

    case object Intersect extends MergeOperator {
      def equal(s1: SQ) = s1
      def equalElems(s1: SQ,s2: SQ) = s1.split(s1.size min s2.size)._1
      def append(s1: SQ,s2: SQ) = s1.emptySeq
    }

    def union(s1: SQ,s2: SQ): SQ = combine(s1,s2,Union)
    def difference(s1: SQ,s2: SQ): SQ = combine(s1,s2,Difference)
    def intersect(s1: SQ,s2: SQ): SQ = combine(s1,s2,Intersect)
    def combine(s1: SQ,s2: SQ,op: MergeOperator): SQ = combine_(sort(s1),sort(s2),op)

    def combine_(s1: SQ,s2: SQ,op: MergeOperator): SQ = {
      if (s1.size == 0) op.append(s1,s2)
      else if (s2.size == 0) op.append(s1,s2)
      else if (s1.equalTo(s2)) op.equal(s1)
      else {
        val ann1 = s1.annotation
        val ann2 = s2.annotation
        val ord = s1.context.ord
        if (ord.lt(ann1.last,ann2.first)) op.append(s1,s2)
        else if (ord.lt(ann2.last,ann1.first)) op.append(s2,s1)
        else if (s1.size == 1 && s2.size == 1) {
          val c = ord.compare(ann1.first,ann2.first)
          if (c == 0) op.equal(s1)
          if (c < 0) op.append(s1,s2)
          else op.append(s2,s1)
        }
        else {
          if (s1.size >= s2.size) {
            val elem = s1.some
            val smaller1 = smaller(s1,elem)
            val same1 = same(s1,elem)
            val bigger1 = bigger(s1,elem)
            val smaller2 = smaller(s2,elem)
            val same2 = same(s2,elem)
            val bigger2 = bigger(s2,elem)
            val ml = combine_(smaller1,smaller2,op)
            val mm = op.equalElems(same1,same2)
            val mr = combine_(bigger1,bigger2,op)
            ml.append(mm).append(mr)
          }
          else combine_(s2,s1,op)
        }
      }
    } 
  }
}
