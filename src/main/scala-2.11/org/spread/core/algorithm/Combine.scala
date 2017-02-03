package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.Sequence.{BSeq, OrderingContext, OrderingTreeContext, SSeq, STAT}
//import org.spread.core.relation.Relation.BinRel

//
// Efficient (multi-set) operations on sorted sequences
//
// Inspired by Enchilada (www.enchiladacode.nl)
//
// Copyright 2017: Robbert van Dalen
//
object Combine {

  var comb: Long = 0

  type BSEQ[X,S <: BSeq[X,Statistics[X],S,C],C <: CORD[X,S,C]] = BSeq[X,Statistics[X],S,C]
  type CORD[X,S <: BSEQ[X,S,C], C <: CORD[X,S,C]] = OrderingContext[X,Statistics[X],S,C]
  type SEQSTAT[X,SQ <: SSeq[X,Statistics[X],SQ,S,C], S <: BSEQ[X,S,C], C <: CORD[X,S,C]] = SSeq[X,Statistics[X],SQ,S,C]

  def sort[X,SQ <: SEQSTAT[X,SQ,S,C],S <: BSEQ[X,S,C], C <: CORD[X,S,C]](s: SEQSTAT[X,SQ,S,C]): SQ = {
    Combiner[X,SQ,S,C]().sort(s.asInstanceOf[SQ])
  }
  def union[X,SQ <: SEQSTAT[X,SQ,S,C],S <: BSEQ[X,S,C], C <: CORD[X,S,C]]
  (s1: SEQSTAT[X,SQ,S,C],s2: SEQSTAT[X,SQ,S,C]): SQ = {
    Combiner[X,SQ,S,C]().union(s1.asInstanceOf[SQ],s2.asInstanceOf[SQ])
  }
  def difference[X,SQ <: SEQSTAT[X,SQ,S,C],S <: BSEQ[X,S,C], C <: CORD[X,S,C]]
  (s1: SEQSTAT[X,SQ,S,C],s2: SEQSTAT[X,SQ,S,C]): SQ = {
    Combiner[X,SQ,S,C]().difference(s1.asInstanceOf[SQ],s2.asInstanceOf[SQ])
  }
  def intersect[X,SQ <: SEQSTAT[X,SQ,S,C],S <: BSEQ[X,S,C], C <: CORD[X,S,C]]
  (s1: SEQSTAT[X,SQ,S,C],s2: SEQSTAT[X,SQ,S,C]): SQ = {
    Combiner[X,SQ,S,C]().intersect(s1.asInstanceOf[SQ],s2.asInstanceOf[SQ])
  }



  case class Combiner[X,SQ <: SSeq[X,Statistics[X],SQ,S,C],S <: BSEQ[X,S,C], C <: CORD[X,S,C]]() {

    def sort(r: SQ): SQ = {
      if (r.size == 0) r
      else if (r.annotation.sorted) r
      else {
        val (left,right) = r.split(r.size / 2)
        combine_(sort(left),sort(right),Union)
      }
    }

    def pick(s: SQ): X = {
      {assert(s.size > 0)}
      // TODO: implement this more efficiently (BSeq internal implementation)
      val (left,right) = s.split(s.size / 2)
      left.annotation.last
    }

    def smaller(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.lt(ann.last,elem)) s
      else if (ord.gteq(ann.first,elem)) s.empty
      else {
        val (l,r) = s.split(s.size / 2)
        smaller(l,elem).append(smaller(r,elem))
      }
    }

    def bigger(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.gt(ann.first,elem)) s
      else if (ord.lteq(ann.last,elem)) s.empty
      else {
        val (l,r) = s.split(s.size / 2)
        bigger(l,elem).append(bigger(r,elem))
      }
    }

    def same(s: SQ,elem: X): SQ = {
      val ord = s.ordering
      val ann = s.annotation
      if (s.size == 0) s
      else if (ord.equiv(ann.first,elem) && ord.equiv(ann.last,elem)) s
      else if (ord.gt(ann.first,elem)) s.empty
      else if (ord.lt(ann.last,elem)) s.empty
      else {
        val (l,r) = s.split(s.size / 2)
        same(l,elem).append(same(r,elem))
      }
    }

    def repeat(s1: SQ,m: Int): SQ = {
      if (m == 0) s1.empty
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
      def equal(s1: SQ) = s1.empty
      def equalElems(s1: SQ,s2: SQ) = {
        val s = Math.abs(s1.size - s2.size)
        s1.split(s)._1
      }
      def append(s1: SQ,s2: SQ) = s1.append(s2)
    }

    case object Intersect extends MergeOperator {
      def equal(s1: SQ) = s1
      def equalElems(s1: SQ,s2: SQ) = s1.split(s1.size min s2.size)._1
      def append(s1: SQ,s2: SQ) = s1.empty
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
        val ord = s1.ordering
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
            val elem = pick(s1)
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
