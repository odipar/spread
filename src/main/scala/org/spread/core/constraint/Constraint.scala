package org.spread.core.constraint

import org.spread.core.annotation.Annotation.{NoAnnotation, Statistics, StatisticsAnnotator, createStats}
import org.spread.core.sequence.PairedSequence._

import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence.Order

//
// Standard constraint propagation objects
//
// Copyright 2017: Robbert van Dalen
//
object Constraint {

  trait PropValue {
    def isValid: Boolean
  }

  trait Prop[@sp X] {
    def isSolved(o1: X,o2: X): Boolean
    def isAnySolved(o1: Any,o2: Any) = isSolved(o1.asInstanceOf[X],o2.asInstanceOf[X])
    def isValid(o1: X, o2: X): Boolean
    def isAnyValid(o1: Any, o2: Any) = isValid(o1.asInstanceOf[X],o2.asInstanceOf[X])
    def propagate(o1: X, o2: X): (X,X)
    def propagateAny(o1: Any, o2: Any): (X,X) = propagate(o1.asInstanceOf[X],o2.asInstanceOf[X])
  }

  trait EqualProp[@sp X] extends Prop[X] {
    def notEqual: NotEqualProp[X]
  }
  trait NotEqualProp[@sp X] extends Prop[X] {
    def equal: EqualProp[X]
  }
  trait GreaterEqualProp[@sp X] extends Prop[X] {
    def notEqual: NotEqualProp[X]
    def lowerEqual: LowerEqualProp[X]
  }
  trait LowerEqualProp[@sp X] extends Prop[X] {
    def notEqual: NotEqualProp[X]
    def greaterEqual: GreaterEqualProp[X]
  }

  trait StatisticsProp[@sp X] extends Prop[Statistics[X]]

  case object EqualNoAnn extends EqualProp[NoAnnotation] with NotEqualProp[NoAnnotation] {
    val noAnn = (NoAnnotation,NoAnnotation)
    def notEqual = this
    def equal = this
    def not = this
    def isSolved(o1: NoAnnotation,o2: NoAnnotation) = false
    def isValid(o1: NoAnnotation,o2: NoAnnotation) = false
    def propagate(o1: NoAnnotation,o2: NoAnnotation): (NoAnnotation,NoAnnotation) = noAnn
  }

  case class EqualStatP[@sp X](implicit ann: StatisticsAnnotator[X])
    extends StatisticsProp[X] with EqualProp[Statistics[X]] {
    
    def ord = ann.ordering
    def notEqual = NotEqualStatP()
    
    def isSolved(o1: Statistics[X],o2: Statistics[X]) = {
      (o1.lowerBound == o2.upperBound) && (o1.upperBound == o2.lowerBound)
    }
    
    def isValid(o1: Statistics[X],o2: Statistics[X]) = {
      !(ann.gt(o1.lowerBound,o2.upperBound,ord) || ann.lt(o1.upperBound,o2.lowerBound,ord))
    }

    def propagate(o1: Statistics[X], o2: Statistics[X]): (Statistics[X],Statistics[X]) = {
      val left = propagateOne(o1,o2)
      val right = propagateOne(o2,o1)
      (left,right)
    }

    def propagateOne(o1: Statistics[X], o2: Statistics[X]): Statistics[X] = {
      if (ann.gt(o1.lowerBound,o2.upperBound,ord)) ann.none
      else if (ann.lt(o1.upperBound,o2.lowerBound,ord)) ann.none
      else {
        // TODO: more tight bounds on first and last, we now set first=lower and last=upper
        val lower = ann.max(o1.lowerBound,o2.lowerBound,ord)
        val upper = ann.min(o1.upperBound,o2.upperBound,ord)
        val sorted = o1.sorted && o2.sorted
        createStats(lower,upper,lower,upper,sorted)
      }
    }
    override def toString = "EqualStatP"
  }

  case class NotEqualStatP[@sp X](implicit ann: StatisticsAnnotator[X])
    extends StatisticsProp[X] with NotEqualProp[Statistics[X]] {

    def ord = ann.ordering
    def equal = EqualStatP()

    def isSolved(o1: Statistics[X],o2: Statistics[X]) = {
      (ann.gt(o1.lowerBound,o2.upperBound,ord) || ann.lt(o1.upperBound,o2.lowerBound,ord))
    }

    def isValid(o1: Statistics[X],o2: Statistics[X]) = {
      !((o1.lowerBound == o2.upperBound) && (o1.upperBound == o2.lowerBound))
    }

    def propagate(o1: Statistics[X], o2: Statistics[X]): (Statistics[X],Statistics[X]) = {
      val left = propagateOne(o1,o2)
      val right = propagateOne(o2,o1)
      (left,right)
    }

    def propagateOne(o1: Statistics[X], o2: Statistics[X]): Statistics[X] = {
      if(!isValid(o1,o2)) ann.none
      else {
        val lower = ann.min(o1.lowerBound,o2.lowerBound,ord)
        val upper = ann.max(o1.upperBound,o2.upperBound,ord)
        val sorted = o1.sorted && o2.sorted
        createStats(lower,upper,lower,upper,sorted)
      }
    }
    override def toString = "NotEqualStatP"
  }
  
  trait CompEqualStatP[@sp X] extends StatisticsProp[X] with EqualProp[Statistics[X]] {
    def ann: StatisticsAnnotator[X]
    def ord = ann.ordering

    def propagateGreaterEqual(o1: Statistics[X], o2: Statistics[X]): Statistics[X] = {
      if (!o1.isValid || !o1.isValid) ann.none
      else if (ann.gteqv(o1.lowerBound,o2.upperBound,ord)) o1       // (12,14) >= (6,10)
      else if (ann.lt(o1.upperBound,o2.lowerBound,ord)) ann.none    // (5,8)   >= (10,13)
      else {
        // TODO: more tight bounds on first and last, we now set first=lower and last=upper
        val lower = ann.max(o1.lowerBound,o2.lowerBound,ord)     // (2,10) >= (7,15) => (7,10)
        val upper = o1.upperBound
        val sorted = o1.sorted && o2.sorted
        createStats(lower,upper,lower,upper,sorted)
      }
    }
    def propagateLowerEqual(o1: Statistics[X], o2: Statistics[X]): Statistics[X] = {
      if (!o1.isValid || !o1.isValid) ann.none
      else if (ann.gt(o1.lowerBound,o2.upperBound,ord)) ann.none // (10,13) <= (5,8)   => none
      else if (ann.lteqv(o1.upperBound,o2.lowerBound,ord)) o1 // (6,10)  <= (12,14) => (6,10)
      else {
        // TODO: more tight bounds on first and last, we now set first=lower and last=upper
        val lower = o1.lowerBound
        val upper = ann.min(o1.upperBound,o2.upperBound,ord)
        val sorted = o1.sorted && o2.sorted
        createStats(lower,upper,lower,upper,sorted)
      }
    }
  }

  case class GreaterEqualStatP[@sp X](implicit a: StatisticsAnnotator[X])
    extends CompEqualStatP[X] with GreaterEqualProp[Statistics[X]] {

    def ann = a
    
    def lowerEqual = LowerEqualStatP[X]()
    def notEqual = NotEqualStatP[X]()

    def isSolved(o1: Statistics[X],o2: Statistics[X]) = ann.gteqv(o1.lowerBound,o2.upperBound,ord)
    def isValid(o1: Statistics[X],o2: Statistics[X]) = !ann.lt(o1.upperBound,o2.lowerBound,ord)

    def propagate(o1: Statistics[X], o2: Statistics[X]): (Statistics[X],Statistics[X]) = {
      val left = propagateGreaterEqual(o1,o2)
      val right = propagateLowerEqual(o2,o1)
      (left,right)
    }

    override def toString = "GreaterEqualStatP"
  }

  case class LowerEqualStatP[@sp X](implicit a: StatisticsAnnotator[X])
    extends CompEqualStatP[X] with LowerEqualProp[Statistics[X]] {

    def ann = a
    
    def greaterEqual = GreaterEqualStatP[X]()
    def notEqual = NotEqualStatP[X]()

    def isSolved(o1: Statistics[X],o2: Statistics[X]) = ann.lteqv(o1.upperBound,o2.lowerBound,ord)
    def isValid(o1: Statistics[X],o2: Statistics[X]) = !ann.gt(o1.lowerBound,o2.upperBound,ord)

    def propagate(o1: Statistics[X], o2: Statistics[X]): (Statistics[X],Statistics[X]) = {
      val left = propagateLowerEqual(o1,o2)
      val right = propagateGreaterEqual(o2,o1)
      (left,right)
    }

    override def toString = "LowerEqualStatP"
  }
  
  implicit def equalStatProp[@sp X](implicit o: Order[X]): EqualProp[Statistics[X]] = {
    EqualStatP[X]()(StatisticsAnnotator[X]())
  }

  implicit def notEqualStatProp[@sp X](implicit o: Order[X]): NotEqualProp[Statistics[X]] = {
    NotEqualStatP[X]()(StatisticsAnnotator[X]())
  }

  implicit def greatereEqualStatProp[@sp X](implicit o: Order[X]): GreaterEqualProp[Statistics[X]] = {
    GreaterEqualStatP[X]()(StatisticsAnnotator[X]())
  }

  implicit def lowerEqualStatProp[@sp X](implicit o: Order[X]): LowerEqualProp[Statistics[X]] = {
    LowerEqualStatP[X]()(StatisticsAnnotator[X]())
  }
}
