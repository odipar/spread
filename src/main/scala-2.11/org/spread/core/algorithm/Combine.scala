package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.Sequence.BSeq
import org.spread.core.sequence.Sequence.ContextImpl

//
object Combine {
  type CStat[X] = ContextImpl[X,Statistics[X]]
  type SStat[X] = BSeq[X,Statistics[X],CStat[X]]

  def sort[@specialized(Int,Long,Double) X](r: SStat[X])(implicit cc: CStat[X]): SStat[X] = {
    if (r.size == 0) r
    else if (r.annotation.sorted) r
    else {
      val (left,right) = r.split(r.size/2)
      union(sort(left),sort(right))
    }
  }

  def splitMiddle[@specialized(Int,Long,Double) X](s: SStat[X])(implicit cc: CStat[X]): (SStat[X],SStat[X],SStat[X]) = {
    // TODO: implement this more efficiently (BSeq internal implementation)
    val (left,right) = s.split(s.size/2)
    val (middle,right2) = right.split(1)
    (left,middle,right2)
  }

  def smaller[@specialized(Int,Long,Double) X](s: SStat[X],elem: X)(implicit cc: CStat[X]): SStat[X] = {
    val ord = cc.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.lt(ann.last,elem)) s
    else if (ord.gteq(ann.first,elem)) cc.empty
    else {
      val (l,r) = s.split(s.size/2)
      smaller(l,elem).append(smaller(r,elem))
    }
  }

  def bigger[@specialized(Int,Long,Double) X](s: SStat[X],elem: X)(implicit cc: CStat[X]): SStat[X] = {
    val ord = cc.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.gt(ann.first,elem)) s
    else if (ord.lteq(ann.last,elem)) cc.empty
    else {
      val (l,r) = s.split(s.size/2)
      bigger(l,elem).append(bigger(r,elem))
    }
  }

  def same[@specialized(Int,Long,Double) X](s: SStat[X],elem: X)(implicit cc: CStat[X]): SStat[X] = {
    val ord = cc.ordering
    val ann = s.annotation

    if (s.size == 0) s
    else if (ord.equiv(ann.first,elem) && ord.equiv(ann.last,elem)) s
    else if (ord.gt(ann.first,elem)) cc.empty
    else if (ord.lt(ann.last,elem)) cc.empty
    else {
      val (l,r) = s.split(s.size/2)
      same(l,elem).append(same(r,elem))
    }
  }

  def repeat[@specialized(Int,Long,Double) X](s1: SStat[X],m: Int)(implicit cc: CStat[X] ): SStat[X] = {
    if (m == 0) cc.empty
    else if (m == 1) s1
    else {
      val m2 = m/2
      val sr = repeat(s1,m2)

      sr.append(sr).append(repeat(s1,m-(m2*2)))
    }
  }

  def multiply[@specialized(Int,Long,Double) X](s1: SStat[X],m: Int)(implicit cc: CStat[X] ): SStat[X] = {
    if (s1.size == 0) s1
    else if (s1.size == 1) repeat(s1,m)
    else {
      val (l,r) = s1.split(s1.size/2)
      multiply(l,m).append(multiply(r,m))
    }
  }

  trait MergeOperator[@specialized(Int,Long,Double) X] {
    def equal(s1: SStat[X])(implicit cc: CStat[X]): SStat[X]
    def equalElems(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]): SStat[X]
    def append(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]): SStat[X]
  }

  case class Union[@specialized(Int,Long,Double) X]() extends MergeOperator[X] {
    def equal(s1: SStat[X])(implicit cc: CStat[X]) = multiply(s1,2)
    def equalElems(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = s1.append(s2)
    def append(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = s1.append(s2)
  }

  case class Difference[@specialized(Int,Long,Double) X]() extends MergeOperator[X] {
    def equal(s1: SStat[X])(implicit cc: CStat[X]) = cc.empty
    def equalElems(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = {
      val s = Math.abs(s1.size - s2.size)
      s1.split(s)._1
    }
    def append(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = s1.append(s2)
  }

  case class Intersect[@specialized(Int,Long,Double) X]() extends MergeOperator[X] {
    def equal(s1: SStat[X])(implicit cc: CStat[X]) = s1
    def equalElems(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = {
      val s = s1.size min s2.size
      s1.split(s)._1
    }
    def append(s1: SStat[X], s2: SStat[X])(implicit cc: CStat[X]) = cc.empty
  }

  def union[@specialized(Int,Long,Double) X]
  (s1: SStat[X],s2: SStat[X])(implicit cc: CStat[X]): SStat[X] = merge(s1,s2,Union())

  def difference[@specialized(Int,Long,Double) X]
  (s1: SStat[X],s2: SStat[X])(implicit cc: CStat[X]): SStat[X] = merge(s1,s2,Difference())

  def intersect[@specialized(Int,Long,Double) X]
  (s1: SStat[X],s2: SStat[X])(implicit cc: CStat[X]): SStat[X] = merge(s1,s2,Intersect())

  def merge[@specialized(Int,Long,Double) X]
  (s1: SStat[X],s2: SStat[X],op: MergeOperator[X])(implicit cc: CStat[X]): SStat[X] = {
    merge2(sort(s1),sort(s2),op)
  }

  def merge2[@specialized(Int,Long,Double) X]
  (s1: SStat[X],s2: SStat[X],op: MergeOperator[X])(implicit cc: CStat[X]): SStat[X] = {
    if (s1.size == 0) op.append(s1,s2)
    else if (s2.size == 0) op.append(s1,s2)
    else if (s1.equalTo(s2)) op.equal(s1)
    else {
      val ann1 = s1.annotation
      val ann2 = s2.annotation

      val ord = cc.ordering
      if (ord.lt(ann1.last,ann2.first)) op.append(s1,s2)
      else if (ord.lt(ann2.last,ann1.first)) op.append(s2,s1)
      else {
        if (s1.size >= s2.size) {
          val (smaller1,same1,bigger1) = splitMiddle(s1)
          val elem = same1.annotation.first

          val smaller2 = smaller(s2,elem)
          val same2 = same(s2,elem)
          val bigger2 = bigger(s2,elem)

          val ml = merge2(smaller1,smaller2,op)
          val mm = op.equalElems(same1,same2)
          val mr = merge2(bigger1,bigger2,op)

          ml.append(mm).append(mr)
        }
        else merge2(s2,s1,op)
      }
    }
  }
}
