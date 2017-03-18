package org.spread.core.sequence

import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.Sequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// Generic sort, union, difference, intersection
//
// Improved version of Enchilada semantics
//
// Copyright 2017: Robbert van Dalen
//
object OrderingSequence {

  import spire.implicits._
  
  type Order[@sp X] = spire.algebra.Order[X]
  
  trait OrderingSeq[@sp X,S <: OrderingSeq[X,S]] extends SeqImpl[X,S] {

    // We use Spire's Order because it is specialized, and that we can re-use Spire's sort algorithms
    def ordering: spire.algebra.Order[X]

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

    def smallerIndex(elem: X): Long = {
      val ord = ordering
      var start: Long = 0
      var end = size

      while(start != end) {
        val mid = (start / 2) + (end / 2)
        if (ord.lt(this(mid),elem)) start = mid + 1
        else end = mid
      }
      end
    }

    def greaterIndex(elem: X): Long = {
      val ord = ordering
      var start: Long = 0
      var end = size

      while(start != end) {
        val mid = (start / 2) + (end / 2)
        if (ord.gt(this(mid),elem)) end = mid
        else start = mid + 1
      }
      end
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
      //else if (s1.equalTo(s2)) op.equal[X,S](s1)   // TODO: faster equality check
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
          if (((s1.size + s2.size) <= 65536)) op.smallMerge[X,S](s1,s2)(s1.tag)
          else if (s1.size >= s2.size) {
            val elem = s1(s1.size/2)

            val li1 = s1.smallerIndex(elem)
            val l1 = s1.split(li1)
            val gi1 = l1._2.greaterIndex(elem)
            val r1 = l1._2.split(gi1)

            val smaller1 = l1._1
            val same1 = r1._1
            val bigger1 = r1._2

            val li2 = s2.smallerIndex(elem)
            val l2 = s2.split(li2)
            val gi2 = l2._2.greaterIndex(elem)
            val r2 = l2._2.split(gi2)

            val smaller2 = l2._1
            val same2 = r2._1
            val bigger2 = r2._2
            
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
    def not_equal[@sp X](result: Array[X], value: X, k: Int): Int
    def equal[@sp X](result: Array[X], value: X, k: Int): Int

    def smallMerge[@sp X: ClassTag,S <: OrderingSeq[X,S]](s1: S,s2: S) = {
      val result = new Array[X](s1.size.toInt + s2.size.toInt)
      val ord = s1.ordering

      val a = s1.toArray
      val b = s2.toArray

      var i = 0
      var j = 0
      var k = 0

      while (i < a.length && j < b.length) {
        val c = ord.compare(a(i),b(j))
        if (c < 0) { k = not_equal(result,a(i),k) ; i = i + 1 }
        else if (c > 0) { k = not_equal(result,b(j),k) ; j = j + 1 }
        else { k = equal(result,a(i),k) ; k = equal(result,b(j),k) ; i = i + 1 ; j = j + 1 }
      }

      while (i < a.length) { k = not_equal(result,a(i),k) ; i = i + 1 }
      while (j < b.length) { k = not_equal(result,b(j),k) ; j = j + 1 }

      val rr = new Array[X](k)
      result.copyToArray(rr,0,k)
      s1.createSeq(rr)
    }
  }

  trait Union extends MergeOperator
  trait Difference extends MergeOperator
  trait Intersect extends MergeOperator
  
  case object Union extends Union {
    @inline final def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1.multiply(s1,2)
    @inline final def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
    @inline final def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
    @inline final def not_equal[@sp X](result: Array[X], value: X, k: Int): Int = { result(k) = value ; k + 1 }
    @inline final def equal[@sp X](result: Array[X], value: X, k: Int): Int = { result(k) = value ; k + 1 }
  }

  case object Difference extends Difference {
    @inline final def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1.emptySeq
    @inline final def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = {
      val s = Math.abs(s1.size - s2.size)
      s1.split(s)._1
    }
    @inline final def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.append(s2)
    @inline final def not_equal[@sp X](result: Array[X], value: X, k: Int): Int = { result(k) = value ; k + 1 }
    @inline final def equal[@sp X](result: Array[X], value: X, k: Int): Int = k
  }

  case object Intersect extends Intersect {
    @inline final def equal[@sp X,S <: OrderingSeq[X,S]](s1: S) = s1
    @inline final def equalElems[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.split(min(s1.size,s2.size))._1
    @inline final def append[@sp X,S <: OrderingSeq[X,S]](s1: S,s2: S) = s1.emptySeq
    @inline final def not_equal[@sp X](result: Array[X], value: X, k: Int): Int = k
    @inline final def equal[@sp X](result: Array[X], value: X, k: Int): Int = { result(k) = value ; k + 1 }
  }

  @inline final def min(l1: Long, l2: Long) = if (l1 < l2) l1 ; else l2
}
