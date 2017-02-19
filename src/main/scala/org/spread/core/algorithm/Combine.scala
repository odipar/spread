package org.spread.core.algorithm

import org.spread.core.sequence.Sequence._
import scala.language.{existentials, implicitConversions}

//
// Generic sort, union, difference, intersection
//
// Improved version of Enchilada semantics
//
// Copyright 2017: Robbert van Dalen
//
object Combine {

  type OSEQ[@specialized X, S <: OrderingSeq[X,S]] = OrderingSeq[X,S]

  private def sort2[@specialized X,S <: OSEQ[X,S]](x: OSEQ[X,S]) = {
    Combiner[X,S]().sort(x.asInstanceOf[S])
  }
  private def union2[@specialized X,S <: OSEQ[X,S]](s1: OSEQ[X,S],s2: OSEQ[X,S]) = {
    Combiner[X,S]().union(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }
  private def difference2[@specialized X,S <: OSEQ[X,S]](s1: OSEQ[X,S],s2: OSEQ[X,S]) = {
    Combiner[X,S]().difference(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }
  private def intersect2[@specialized X,S <: OSEQ[X,S]](s1: OSEQ[X,S],s2: OSEQ[X,S]) = {
    Combiner[X,S]().difference(s1.asInstanceOf[S],s2.asInstanceOf[S])
  }

  implicit class CombineSyntax[@specialized X,S <: OSEQ[X,S]](val s: OrderingSeq[X,S]) {
    def sort = sort2(s)

    def union(o: OSEQ[X,S]) = union2(s,o)
    def difference(o: OSEQ[X,S]) = difference2(s,o)
    def intersect(o: OSEQ[X,S]) = intersect2(s,o)

    def :+:(o: OSEQ[X,S]) = union(o)
    def :*:(o: OSEQ[X,S]) = intersect(o)
    def :^:(o: OSEQ[X,S]) = difference(o)
  }

  private case class Combiner[@specialized X,S <: OSEQ[X,S]]() {
    type SQ = S

    def sort(r: SQ): SQ = {
      if (r.size <= 1) r
      else {
        val (left,right) = r.split(r.size / 2)
        combine_(sort(left),sort(right),Union)
      }
    }

    // TODO: do a range search, and than split (also for bigger and same)
    def smaller(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.lt(s.last,elem)) s
      else if (ord.gteq(s.first,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        smaller(l,elem).append(smaller(r,elem))
      }
    }

    def bigger(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.gt(s.first,elem)) s
      else if (ord.lteq(s.last,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        bigger(l,elem).append(bigger(r,elem))
      }
    }

    def same(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.equiv(s.first,elem) && ord.equiv(s.last,elem)) s
      else if (ord.gt(s.first,elem)) s.emptySeq
      else if (ord.lt(s.last,elem)) s.emptySeq
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
      if (s1.isEmpty) op.append(s1,s2)
      else if (s2.isEmpty) op.append(s1,s2)
      else if (s1.equalTo(s2)) op.equal(s1)
      else {
        val ord = s1.ordering
        if (ord.lt(s1.last,s2.first)) op.append(s1,s2)
        else if (ord.lt(s2.last,s1.first)) op.append(s2,s1)
        else if (s1.size == 1 && s2.size == 1) {
          val c = ord.compare(s1.first,s2.first)
          if (c == 0) op.equal(s1)
          if (c < 0) op.append(s1,s2)
          else op.append(s2,s1)
        }
        else {
          if (s1.size >= s2.size) {
            val elem = s1(s1.size/2)
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
