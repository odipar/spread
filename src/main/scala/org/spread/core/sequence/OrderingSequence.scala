package org.spread.core.sequence

import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.Sequence._

//
// Generic sort, union, difference, intersection
//
// Improved version of Enchilada semantics
//
// Copyright 2017: Robbert van Dalen
//
object OrderingSequence {
  
  trait OrderingSeq[@sp X,S <: OrderingSeq[X,S]] extends SeqImpl[X,S] {

    // We use Cat's Order because it is specialized
    def ordering: cats.Order[X]

    // Generic sort, must be overridden
    def sort: S

    // Default merge sort
    def defaultSort: S = {
      if (size <= 1) self
      else {
        val (left,right) = split(size / 2)
        combineSorted(left.sort,right.sort,Union)
      }
    }

    def union(o: S): S = combineSorted(this.sort,o.sort,Union)
    def difference(o: S): S = combineSorted(this.sort,o.sort,Difference)
    def intersect(o: S): S = combineSorted(this.sort,o.sort,Intersect)

    // TODO: Optimize with binary/galopping search
    def smaller(s: S,elem: X): S = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.lt(s.last,elem)) s
      else if (ord.gteqv(s.first,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        smaller(l,elem).append(smaller(r,elem))
      }
    }

    def bigger(s: S,elem: X): S = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.gt(s.first,elem)) s
      else if (ord.lteqv(s.last,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        bigger(l,elem).append(bigger(r,elem))
      }
    }

    def same(s: S,elem: X): S = {
      val ord = s.ordering
      if (s.size == 0) s
      else if (ord.eqv(s.first,elem) && ord.eqv(s.last,elem)) s
      else if (ord.gt(s.first,elem)) s.emptySeq
      else if (ord.lt(s.last,elem)) s.emptySeq
      else {
        val (l,r) = s.split(s.size / 2)
        same(l,elem).append(same(r,elem))
      }
    }

    def repeat(s1: S,m: Int): S = {
      if (m == 0) s1.emptySeq
      else if (m == 1) s1
      else {
        val m2 = m / 2
        val sr = repeat(s1,m2)
        sr.append(sr).append(repeat(s1,m - (m2 * 2)))
      }
    }

    def multiply(s1: S,m: Int): S = {
      if (s1.size == 0) s1
      else if (s1.size == 1) repeat(s1,m)
      else {
        val (l,r) = s1.split(s1.size / 2)
        multiply(l,m).append(multiply(r,m))
      }
    }

    def union(s1: S,s2: S): S = combine(s1,s2,Union)
    def difference(s1: S,s2: S): S = combine(s1,s2,Difference)
    def intersect(s1: S,s2: S): S = combine(s1,s2,Intersect)
    def combine(s1: S,s2: S,op: MergeOperator): S = combineSorted(s1.sort,s2.sort,op)

    def combineSorted(s1: S,s2: S,op: MergeOperator): S = {
      if (s1.isEmpty) op.append[X,S](s1,s2)
      else if (s2.isEmpty) op.append[X,S](s1,s2)
      else if (s1.equalTo(s2)) op.equal[X,S](s1)
      else {
        val ord = s1.ordering
        if (ord.lt(s1.last,s2.first)) op.append[X,S](s1,s2)
        else if (ord.lt(s2.last,s1.first)) op.append[X,S](s2,s1)
        else if (s1.size == 1 && s2.size == 1) {
          val c = ord.compare(s1.first,s2.first)
          if (c == 0) op.equal[X,S](s1)
          if (c < 0) op.append[X,S](s1,s2)
          else op.append[X,S](s2,s1)
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
            val ml = combineSorted(smaller1,smaller2,op)
            val mm = op.equalElems[X,S](same1,same2)
            val mr = combineSorted(bigger1,bigger2,op)
            ml.append(mm).append(mr)
          }
          else combineSorted(s2,s1,op)
        }
      }
    }
  }

  trait MergeOperator {
    def equal[@sp X,S <: OrderingSeq[X,S]](s1: S): S
    def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S): S
    def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S): S
  }

  case object Union extends MergeOperator {
    def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1.multiply(s1,2)
    def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
    def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
  }

  case object Difference extends MergeOperator {
    def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1.emptySeq
    def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = {
      val s = Math.abs(s1.size - s2.size)
      s1.split(s)._1
    }
    def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
  }

  case object Intersect extends MergeOperator {
    def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1
    def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.split(s1.size min s2.size)._1
    def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.emptySeq
  }
}
