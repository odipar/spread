package org.spread.core.constraint

import org.spread.core.annotation.Annotation.{Statistics, StatisticsAnnotator, createStats}
import org.spread.core.sequence.PairedSequence.RelCol
import scala.language.{existentials, implicitConversions}

//
// Standard constraint propagation objects
//
// Copyright 2017: Robbert van Dalen
//
object Constraint {

  trait PropValue {
    def isValid: Boolean
  }

  trait Prop[X] {
    def propagate(o1: X, o2: X): (X,X)
    def propagateAny(o1: Any, o2: Any): (X,X) = propagate(o1.asInstanceOf[X],o2.asInstanceOf[X])
  }

  trait EqualProp[X] extends Prop[X]

  trait StatisticsProp[X] extends Prop[Statistics[X]]

  case class EqualStatP[X](implicit ann: StatisticsAnnotator[X]) extends StatisticsProp[X] with EqualProp[Statistics[X]] {
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

  implicit def equalStatProp[X](implicit s: StatisticsAnnotator[X]): EqualProp[Statistics[X]] = EqualStatP[X]()

  case class RelConstraint[X,XA <: PropValue](r1: RelCol[X,XA],r2: RelCol[X,XA],prop: Prop[XA]){
    override def toString = "" + r1 + prop + r2
  }

}
