package org.spread.core.annotation

import org.spread.core.constraint.Constraint.PropValue

import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence.Order

//
// Sequence Annotators, most notably StatisticsAnnotator
//
// Copyright 2017: Robbert van Dalen
//
object Annotation {

  trait Annotated[@sp A] {
    def annotation: A
  }

  trait Annotator[@sp X,@sp A] {
    def none: A
    def one(x: X): A
    def manyX(start: Int, end: Int, d: Array[X]): A
    def manyA(start: Int, end: Int, d: Array[Annotated[A]]): A
    def append(r1: A, r2: A): A

    def manyX(d: Array[X]): A = manyX(0,d.length,d)
    def manyA(d: Array[Annotated[A]]): A = manyA(0,d.length,d)
  }

  trait RangeAnnotator[@sp X, @sp A] extends Annotator[X,A] {
    def range(from: X, to: X): A
  }
  
  trait NoAnnotation

  object NoAnnotation extends NoAnnotation {
    def isValid = false
    override def toString = "'"
  }

  case class NoAnnotator[@sp X]() extends RangeAnnotator[X,NoAnnotation] with PropValue {
    def none: NoAnnotation = NoAnnotation
    def one(x: X): NoAnnotation = NoAnnotation
    def manyX(start: Int, end: Int, d: Array[X]) = NoAnnotation
    def manyA(start: Int, end: Int, d: Array[Annotated[NoAnnotation]]) = NoAnnotation
    def append(a1: NoAnnotation, a2: NoAnnotation): NoAnnotation = NoAnnotation
    def isNone(a: NoAnnotation) = true
    def range(start: X, end: X) = NoAnnotation
    def isValid = false
  }


  trait Statistics[@sp X] extends PropValue {
    def lowerBound: X
    def upperBound: X
    def first: X
    def last: X
    def sorted: Boolean
    override def toString: String = {
      "<" + first + "," + lowerBound + "," + "sorted=" +sorted + "," + upperBound + "," + last + ">"
    }
  }

  case class InvalidStatistics[@sp X]() extends Statistics[X] {
    def error = sys.error("invalid statistics")
    def lowerBound = error
    def upperBound = error
    def first = error
    def last = error
    def sorted = error
    def isValid = false
    override def toString = ".."
  }

  case class StatisticsImpl3[@sp X]
  (lowerBound: X) extends Statistics[X] {
    def upperBound = lowerBound
    def first = lowerBound
    def last = upperBound
    def sorted = true
    def isValid = true
  }

  case class StatisticsImpl2[@sp X]
  (lowerBound: X,upperBound: X) extends Statistics[X] {
    def first = lowerBound
    def last = upperBound
    def sorted = true
    def isValid = true
  }

  case class StatisticsImpl[@sp X]
  (lowerBound: X,upperBound: X, first: X, last: X, sorted: Boolean) extends Statistics[X] {
    def isValid = true
  }

  def createStats[@sp X]
  (lowerBound: X,upperBound: X, first: X, last: X, sorted: Boolean): Statistics[X] = {

    if ((lowerBound == first) && (upperBound == last) && sorted) {
      if (lowerBound == upperBound) StatisticsImpl3(lowerBound)
      else StatisticsImpl2(lowerBound,upperBound)
    }
    else StatisticsImpl(lowerBound: X,upperBound: X, first: X, last: X, sorted: Boolean)
  }
  StatisticsAnnotator
  case class StatisticsAnnotator[@sp X](implicit ord: Order[X])
    extends RangeAnnotator[X,Statistics[X]]{

    // explicit implementation to avoid boxing ??
    @inline final def min(v1: X, v2: X, o: Order[X]): X = {
      if (o.lt(v1,v2)) v1
      else v2
    }
    @inline final def max(v1: X, v2: X, o: Order[X]): X = {
      if (o.gt(v1,v2)) v1
      else v2
    }
    @inline final def gteqv(v1: X, v2: X, o: Order[X]): Boolean = {
      o.gteqv(v1,v2)
    }
    @inline final def lteqv(v1: X, v2: X, o: Order[X]): Boolean = {
      o.lteqv(v1,v2)
    }
    @inline final def lt(v1: X, v2: X, o: Order[X]): Boolean = {
      o.lt(v1,v2)
    }
    @inline final def gt(v1: X, v2: X, o: Order[X]): Boolean = {
      o.gt(v1,v2)
    }
    def range(start: X, end: X) = StatisticsImpl2(start,end)
    def ordering = ord
    def none: Statistics[X] = InvalidStatistics()
    def one(x: X) = StatisticsImpl2(x,x)
    def manyX(start: Int, end: Int, a: Array[X]): Statistics[X] ={
      var lower = a(start)
      var upper = a(start)
      var msorted = true
      var i = start+1
      while (i < end) {
        val x = a(i)
        lower = min(lower,x,ord)
        upper = max(upper,x,ord)
        msorted = msorted && ord.lteqv(a(i - 1),x)
        i = i + 1
      }
      createStats(lower,upper,a(start),a(end-1),msorted)
    }
    // Some duplication of code for performance reasons
    def manyA(start: Int, end: Int, a: Array[Annotated[Statistics[X]]]): Statistics[X] ={
      var s = a(start).annotation
      var lower = s.lowerBound
      var upper = s.upperBound
      var msorted = s.sorted
      var i = start+1
      while (i < end) {
        val x = a(i).annotation
        lower = min(lower,x.lowerBound,ord)
        upper = max(upper,x.upperBound,ord)
        msorted = msorted && x.sorted && ord.lteqv(a(i - 1).annotation.last,x.first)
        i = i + 1
      }
      createStats(lower,upper,s.first,a(end-1).annotation.last,msorted)
    }
    def append(s1: Statistics[X],s2: Statistics[X]): Statistics[X] ={
      StatisticsImpl(
        min(s1.lowerBound,s2.lowerBound,ord),
        max(s1.upperBound,s2.upperBound,ord),
        s1.first,s2.last,
        s1.sorted && s2.sorted && ord.lteqv(s1.last,s2.first)
      )
    }
  }

  implicit def stats[X](implicit ord: Order[X]) = StatisticsAnnotator[X]()
}
