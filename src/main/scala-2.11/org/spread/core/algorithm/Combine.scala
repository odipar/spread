package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.Sequence.{OrderingContext, OrderingTreeContext, SSeq}
import org.spread.core.relation.Relation.BinRel

//
// Efficient (multi-set) operations on sorted sequences
//
// Inspired by Enchilada (www.enchiladacode.nl)
//
// Copyright 2017: Robbert van Dalen
//
object Combine {
  type CORD[X,C <: CORD[X,C]] = OrderingContext[X,Statistics[X],C]
  type SStat[X, C <: CORD[X,C]] = SSeq[X,Statistics[X],C]
  //type SRel[X,Y, XC <: CORD[X,XC], YC <: CORD[X,XC]] = BinRel[X, Statistics[X],Y, Statistics[Y],XC,YC]

  def sort[X, C <: CORD[X,C]](r: SStat[X,C]): SStat[X,C] = {
    if (r.size == 0) r
    else if (r.annotation.sorted) r
    else {
      val (left,right) = r.split(r.size/2)
      union(sort(left),sort(right))
    }
  }

  /*def sort2[X,Y, XC <: CORD[X,XC], YC <: CORD[X,XC]](r: SRel[X,Y,XC,YC]): SRel[X,Y,XC,YC] = {
    if (r.size == 0) r
    else if (r.left.annotation.) r
    else {
      val (left,right) = r.split(r.size/2)
      union(sort(left),sort(right))
    }
  } */

  def pick[X, C <: CORD[X,C]](s: SStat[X,C]): X = {
    { assert(s.size > 0)}
    // TODO: implement this more efficiently (BSeq internal implementation)
    val (left,right) = s.split(s.size/2)
    left.annotation.last
  }


  def smaller[ X, C <: CORD[X,C]](s: SStat[X,C],elem: X): SStat[X,C] = {
    val ord = s.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.lt(ann.last,elem)) s
    else if (ord.gteq(ann.first,elem)) s.empty
    else {
      val (l,r) = s.split(s.size/2)
      smaller(l,elem).append(smaller(r,elem))
    }
  }

  def bigger[X, C <: CORD[X,C]](s: SStat[X,C],elem: X): SStat[X,C] = {
    val ord = s.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.gt(ann.first,elem)) s
    else if (ord.lteq(ann.last,elem)) s.empty
    else {
      val (l,r) = s.split(s.size/2)
      bigger(l,elem).append(bigger(r,elem))
    }
  }

  def same[X, C <: CORD[X,C]](s: SStat[X,C],elem: X): SStat[X,C] = {
    val ord = s.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.equiv(ann.first,elem) && ord.equiv(ann.last,elem)) s
    else if (ord.gt(ann.first,elem)) s.empty
    else if (ord.lt(ann.last,elem)) s.empty
    else {
      val (l,r) = s.split(s.size/2)
      same(l,elem).append(same(r,elem))
    }
  }

  def repeat[X, C <: CORD[X,C]](s1: SStat[X,C],m: Int): SStat[X,C] = {
    if (m == 0) s1.empty
    else if (m == 1) s1
    else {
      val m2 = m/2
      val sr = repeat(s1,m2)

      sr.append(sr).append(repeat(s1,m-(m2*2)))
    }
  }

  def multiply[X, C <: CORD[X,C]](s1: SStat[X,C],m: Int): SStat[X,C] = {
    if (s1.size == 0) s1
    else if (s1.size == 1) repeat(s1,m)
    else {
      val (l,r) = s1.split(s1.size/2)
      multiply(l,m).append(multiply(r,m))
    }
  }

  trait MergeOperator[X, C <: CORD[X,C]] {
    def equal(s1: SStat[X,C]): SStat[X,C]
    def equalElems(s1: SStat[X,C], s2: SStat[X,C]): SStat[X,C]
    def append(s1: SStat[X,C], s2: SStat[X,C]): SStat[X,C]
  }

  case class Union[X, C <: CORD[X,C]]() extends MergeOperator[X,C] {
    def equal(s1: SStat[X,C]) = multiply(s1,2)
    def equalElems(s1: SStat[X,C], s2: SStat[X,C]) = s1.append(s2)
    def append(s1: SStat[X,C], s2: SStat[X,C]) = s1.append(s2)
  }

  case class Difference[X, C <: CORD[X,C]]() extends MergeOperator[X,C] {
    def equal(s1: SStat[X,C]) =  s1.empty
    def equalElems(s1: SStat[X,C], s2: SStat[X,C]) = {
      val s = Math.abs(s1.size - s2.size)
      s1.split(s)._1
    }
    def append(s1: SStat[X,C], s2: SStat[X,C]) = s1.append(s2)
  }

  case class Intersect[X, C <: CORD[X,C]]() extends MergeOperator[X,C] {
    def equal(s1: SStat[X,C]) = s1
    def equalElems(s1: SStat[X,C], s2: SStat[X,C]) = s1.split(s1.size min s2.size)._1
    def append(s1: SStat[X,C], s2: SStat[X,C]) = s1.empty
  }

  def union[ X, C <: CORD[X,C]]
  (s1: SStat[X,C],s2: SStat[X,C]): SStat[X,C] = combine(s1,s2,Union())

  def difference[ X, C <: CORD[X,C]]
  (s1: SStat[X,C],s2: SStat[X,C]): SStat[X,C] = combine(s1,s2,Difference())

  def intersect[ X, C <: CORD[X,C]]
  (s1: SStat[X,C],s2: SStat[X,C]): SStat[X,C] = combine(s1,s2,Intersect())

  def combine[ X, C <: CORD[X,C]]
  (s1: SStat[X,C],s2: SStat[X,C],op: MergeOperator[X,C]): SStat[X,C] = {
    combine_(sort(s1),sort(s2),op)
  }

  def combine_[X, C <: CORD[X,C]]
  (s1: SStat[X,C],s2: SStat[X,C],op: MergeOperator[X,C]): SStat[X,C] = {
    if (s1.size == 0) op.append(s1,s2)
    else if (s2.size == 0) op.append(s1,s2)
    else if (s1.equalTo(s2)) { op.equal(s1) }
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
