package org.spread.core.constraint

import org.spread.core.annotation.Annotation.{NoAnnotation, Statistics, StatisticsAnnotator, createStats}
import org.spread.core.sequence.PairedSequence._

import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp

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
    def propagate(o1: X, o2: X): (X,X)
    def propagateAny(o1: Any, o2: Any): (X,X) = propagate(o1.asInstanceOf[X],o2.asInstanceOf[X])
  }

  trait EqualProp[@sp X] extends Prop[X]

  trait StatisticsProp[@sp X] extends Prop[Statistics[X]]

  case object EqualNoAnn extends EqualProp[NoAnnotation] {
    val noAnn = (NoAnnotation,NoAnnotation)
    def propagate(o1: NoAnnotation,o2: NoAnnotation): (NoAnnotation,NoAnnotation) = noAnn
  }

  case class EqualStatP[@sp X](implicit ann: StatisticsAnnotator[X])
    extends StatisticsProp[X] with EqualProp[Statistics[X]] {
    
    def ord = ann.ordering
    def propagate(o1: Statistics[X], o2: Statistics[X]): (Statistics[X],Statistics[X]) = {
      val left = propagateOne(o1,o2)
      val right = propagateOne(o2,o1)
      (left,right)
    }
    def propagateOne(o1: Statistics[X], o2: Statistics[X]): Statistics[X] = {
      if (!o1.isValid || !o1.isValid) ann.none
      else if (ord.gt(o1.lowerBound,o2.upperBound)) ann.none
      else if (ord.lt(o1.upperBound,o2.lowerBound)) ann.none
      else {
        // TODO: more tight bounds on first and last, we now set first=lower and last=upper
        val lower = ord.max(o1.lowerBound,o2.lowerBound)
        val upper = ord.min(o1.upperBound,o2.upperBound)
        val sorted = o1.sorted && o2.sorted
        createStats(lower,upper,lower,upper,sorted)
      }
    }
    override def toString = "EqualStatP"
  }

  implicit def equalStatProp[@sp X](implicit s: StatisticsAnnotator[X]): EqualProp[Statistics[X]] = {
    EqualStatP[X]()
  }
  
  /*case class RelConstraint[@sp X, A <: PropValue]
    (r1: AnnSelector[_,_,_,_,_,X,A,_], r2: AnnSelector[_,_,_,_,_,X,A,_], prop: Prop[A]) {
    override def toString = "" + r1 + prop + r2
  } */
}
