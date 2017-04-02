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

  trait Annotator[@sp X,@sp A] {
    def none: A
    def one(x: X): A
    def manyX(d: Array[X]): A
    def manyA(d: Array[A]): A
    def append(r1: A, r2: A): A
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
    def manyX(d: Array[X]): NoAnnotation = NoAnnotation
    def manyA(d: Array[NoAnnotation]): NoAnnotation = NoAnnotation
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
    def range(start: X, end: X) = StatisticsImpl2(start,end)
    def ordering = ord
    def none: Statistics[X] = InvalidStatistics()
    def one(x: X) = StatisticsImpl2(x,x)
    def manyX(a: Array[X]): Statistics[X] ={
      var lower = a(0)
      var upper = a(0)
      var msorted = true
      for (i <- 1 until a.length) {
        val x = a(i)
        lower = min(lower,x,ord)
        upper = max(upper,x,ord)
        msorted = msorted && ord.lteqv(a(i - 1),x)
      }
      createStats(lower,upper,a(0),a(a.length-1),msorted)
    }
    // Some duplication of code for performance reasons
    def manyA(a: Array[Statistics[X]]): Statistics[X] ={
      var s = a(0)
      var lower = s.lowerBound
      var upper = s.upperBound
      var msorted = s.sorted
      for (i <- 1 until a.length) {
        val x = a(i)

        lower = min(lower,x.lowerBound,ord)
        upper = max(upper,x.upperBound,ord)

        msorted = msorted && x.sorted && ord.lteqv(a(i - 1).last,x.first)
      }
      createStats(lower,upper,s.first,a(a.length-1).last,msorted)
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
