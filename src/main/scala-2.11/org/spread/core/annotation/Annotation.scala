package org.spread.core.annotation

import org.spread.core.constraint.Constraint.{Domain, EqualP}

//
// Sequence Annotators, most notably StatisticsAnnotator
//
// Copyright 2017: Robbert van Dalen
//

object Annotation {

  trait Annotator[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A] {
    def none: A
    def one(x: X): A
    def manyX(d: Array[X]): A
    def manyA(d: Array[A]): A
    def append(r1: A,r2: A): A
  }

  trait NoAnnotation
  object NoAnnotation extends NoAnnotation { override def toString = "'" }

  case class NoAnnotator[@specialized(Int,Long,Double) X]() extends Annotator[X,NoAnnotation] {
    def none: NoAnnotation = NoAnnotation
    def one(x: X): NoAnnotation = NoAnnotation
    def manyX(d: Array[X]): NoAnnotation = NoAnnotation
    def manyA(d: Array[NoAnnotation]): NoAnnotation = NoAnnotation
    def append(a1: NoAnnotation, a2: NoAnnotation): NoAnnotation = NoAnnotation
  }

  case class Statistics[@specialized(Int,Long,Double) X]
  (lowerBound: X,upperBound: X, first: X, last: X, sorted: Boolean) extends Domain[X] {
    def isValid = true
    override def toString: String = "<" + first + "," + lowerBound + "," + sorted + "," + upperBound + "," + last + ">"
  }

  case class StatisticsAnnotator[@specialized(Int,Long,Double) X](implicit ord: Ordering[X]) extends Annotator[X,Statistics[X]]{
    def none: Statistics[X] = sys.error("no stats")
    def one(x: X) = Statistics(x,x,x,x,true)
    def manyX(a: Array[X]): Statistics[X] ={
      var lower = a(0)
      var upper = a(0)
      var msorted = true
      for (i <- 1 until a.length) {
        val x = a(i)
        lower = ord.min(lower,x)
        upper = ord.max(upper,x)
        msorted = msorted && ord.lteq(a(i - 1),x)
      }
      Statistics(lower,upper,a(0),a(a.length-1),msorted)
    }
    // Some duplication of code for performance reasons
    def manyA(a: Array[Statistics[X]]): Statistics[X] ={
      var s = a(0)
      var lower = s.lowerBound
      var upper = s.upperBound
      var msorted = true
      for (i <- 1 until a.length) {
        val x = a(i)
        lower = ord.min(lower,x.lowerBound)
        upper = ord.max(upper,x.upperBound)
        msorted = msorted && ord.lteq(a(i - 1).last,x.first)
      }
      Statistics(lower,upper,s.first,a(a.length-1).last,msorted)
    }
    def append(s1: Statistics[X],s2: Statistics[X]): Statistics[X] ={
      Statistics(
        ord.min(s1.lowerBound,s2.lowerBound),
        ord.max(s1.upperBound,s2.upperBound),
        s1.first,s2.last,
        s1.sorted && s2.sorted && ord.lteq(s1.last,s2.first)
      )
    }
  }
}
