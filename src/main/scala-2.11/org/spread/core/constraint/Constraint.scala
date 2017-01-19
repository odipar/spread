package org.spread.core.constraint

import org.spread.core.relation.Relation._

//
// Standard constraint propagation objects
//
// Copyright 2017: Robbert van Dalen
//

object Constraint{

  trait Propagator[@specialized(Int,Long,Double) X] {
    def propagate[X](o1: Domain[X], o2: Domain[X])(implicit ord: Ordering[X]): (Domain[X],Domain[X])
    def propagateAny(o1: Domain[_], o2: Domain[_])(implicit ord: Ordering[_]): (Domain[X],Domain[X]) = {
      propagate(o1.asInstanceOf[Domain[X]],o2.asInstanceOf[Domain[X]])(ord.asInstanceOf[Ordering[X]])
    }
  }

  case class EqualP[@specialized(Int,Long,Double) X]() extends Propagator[X] {
    def propagate[X](o1: Domain[X], o2: Domain[X])(implicit ord: Ordering[X]): (Domain[X],Domain[X]) = {
      val one = propagateOne(o1,o2)
      (one,one)
    }
    def propagateOne[X](o1: Domain[X], o2: Domain[X])(implicit ord: Ordering[X]): Domain[X] = {
      if (ord.gt(o1.lowerBound,o2.upperBound)) EmptyDomain()
      else if (ord.lt(o1.upperBound,o2.lowerBound)) EmptyDomain()
      else DomainImpl(ord.max(o1.lowerBound,o2.lowerBound),ord.min(o1.upperBound,o2.upperBound))
    }
    override def toString = "EqualP"
  }

  trait Domain[@specialized(Int,Long,Double) X]{
    def isValid: Boolean
    def lowerBound: X
    def upperBound: X
  }

  def createDomain[@specialized(Int,Long,Double) X](l: X, u: X) = DomainImpl(l,u)

  case class DomainImpl[@specialized(Int,Long,Double) X](lowerBound: X,upperBound: X) extends Domain[X] {
    def isValid = true
    override def toString: String = lowerBound + "..." + upperBound
  }

  case class EmptyDomain[X]() extends Domain[X]{
    def isValid = false
    def error = sys.error("domain is empty")
    def ord = error
    def lowerBound: X = error
    def upperBound: X = error
    override def toString: String = ".."
  }

  case class RConstraint[X](r1: RCol[X], r2: RCol[X], prop: Propagator[X]) {

  }

  case class RelConstraint[X](r1: RelCol[X],r2: RelCol[X],prop: Propagator[X]){
    override def toString = "" + r1 + prop + r2
  }

}
