package org.spread.core.algorithm

import org.spread.core.constraint.Constraint._
import org.spread.core.relation.Relation._
import org.spread.core.annotation.Annotation._
import org.spread.core.sequence.Sequence.{LSSEQ, OrderingContext, createLongBSeq, emptyLongBSeq}

import scala.language.{existentials, implicitConversions}

//
// Constraint based relational join algorithm using binary annotated statistics
//
// Copyright 2017: Robbert van Dalen
//

object Solve {

  // existentially typed RelDomain, similar to RelDomain[_,_,_,_]
  type RELDOM = RelDomain[X,Y,XC,YC] forSome { type X ; type Y; type XC <: CORD[X,XC] ; type YC <: CORD[Y,YC] }

  case class RelDomain[X,Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
  (rel: BinRel[X,Y,XC,YC],from: Long,to: Long,leftDomain: Domain[X],rightDomain: Domain[Y]) {
    implicit def xc = rel.xc
    implicit def yc = rel.yc
    implicit def xord = xc.ordering
    implicit def yord = yc.ordering

    def isValid = leftDomain.isValid && rightDomain.isValid
    def size = to - from + 1
    def split: (RelDomain[X,Y,XC,YC],RelDomain[X,Y,XC,YC]) = {
      val r = (from + to + 1) / 2
      (RelDomain(rel,from,r-1,leftDomain,rightDomain).propagate,RelDomain(rel,r,to,leftDomain,rightDomain).propagate)
    }
    def getOrdering(column: ColumnPos): Ordering[_] = {
      if (column == LeftCol) xord
      else yord
    }
    def getDomain(column: ColumnPos): Domain[_] = {
      if (column == LeftCol) leftDomain
      else rightDomain
    }
    def setDomain(d: Domain[_],column: ColumnPos): RelDomain[X,Y,XC,YC] = {
      if (column == LeftCol) RelDomain(rel,from,to,d.asInstanceOf[Domain[X]],rightDomain)
      else RelDomain(rel,from,to,leftDomain,d.asInstanceOf[Domain[Y]])
    }
    def setAnyDomain(d: Domain[_],column: ColumnPos) = {
      setDomain(d,column)
    }
    def propagate: RelDomain[X,Y,XC,YC] = {
      if (isValid) {
        val l = rel.left.annotationRange(from,to)
        val r = rel.right.annotationRange(from,to)
        val d1 = xc.equalProp.propagate(leftDomain,l)._1
        val d2 = yc.equalProp.propagate(rightDomain,r)._1
        RelDomain(rel,from,to,d1,d2)
      }
      else this
    }
    def empty: EREL = rel.empty
    def applyRange: LSSEQ = createLongBSeq(from,to)
  }

  def createRelDomain[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
  (rel: BinRel[X,Y,XC,YC]) = {
    val l = rel.left.annotation
    val r = rel.right.annotation
    RelDomain(rel,0,rel.size-1,createDomain(l.lowerBound,l.upperBound),createDomain(r.lowerBound,r.upperBound))
  }

  type RELS = Map[Symbol,EREL]
  type RELSI = Map[EREL,Symbol]
  type CTRS = Set[RelConstraint[_]]
  type DOMS = Map[Symbol,RELDOM]

  def createModel: Model = Model(Map(),Map(),Set(),Map(),isValid = true)

  case class Model(rels: RELS,relsInv: RELSI,ctrs: CTRS,domains: DOMS,isValid: Boolean) {
    def addRelation[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
    (s: Symbol, rel: BinRel[X,Y,XC,YC]): Model = {
      Model(rels + (s->rel),relsInv + (rel->s),ctrs,domains + (s->createRelDomain(rel)),isValid)
    }
    def addConstraint[X](cc: Propagator[X], r1: RCol[X],r2: RCol[X]): Model = {
      val id1 = relsInv(r1.rel)
      val id2 = relsInv(r2.rel)
      val c = RelConstraint(r1.withID(id1),r2.withID(id2),cc)
      Model(rels,relsInv,ctrs + c,domains,isValid)
    }
    def setRelDomain(id: Symbol, rel: RELDOM): Model = {
      Model(rels,relsInv,ctrs,domains + (id -> rel),rel.isValid)
    }

    def propagateConstraints = propagateModelConstraints(this)
    def split = splitModel(this)
    def solve = solveModel(this)
    def isSolved = isModelSolved(this)
  }

  def propagateModelConstraints(m: Model): Model = {
    if (!m.isValid) m
    else {
      var fixpoint = false
      var isValid = true
      var domains = m.domains

      while(!fixpoint && isValid) { // Loop until fixpoint (no domains have changed)
        fixpoint = true

        val iter = m.ctrs.iterator
        while(iter.hasNext && isValid) {
          val c = iter.next

          val d1 = domains(c.r1.id)
          val d2 = domains(c.r2.id)

          val dd1 = d1.getDomain(c.r1.column)
          val dd2 = d2.getDomain(c.r2.column)

          val ord = d1.getOrdering(c.r1.column)
          val (rd1,rd2) = c.prop.propagateAny(dd1,dd2)(ord)

          isValid = rd1.isValid && rd2.isValid

          if ((dd1 != rd1) || (dd2 != rd2)) {
            fixpoint = false // Either one of the domains have been propagated to something different
            val dd1 = d1.setAnyDomain(rd1,c.r1.column)
            val dd2 = d2.setAnyDomain(rd2,c.r2.column)

            domains = domains + (c.r1.id -> dd1)
            domains = domains + (c.r2.id -> dd2)
          }
        }
      }
      Model(m.rels,m.relsInv,m.ctrs,domains,isValid)
    }
  }

  def selectBestSplitCandidate(m: Model): (Symbol,RELDOM) = {
    val doms = m.domains
    var bestCandidate = doms.last

    for (d <- doms) {
      if (d._2.size > bestCandidate._2.size) {
        // We found a RelDomain with a bigger size
        // TODO: select most restrictive left/right domain?
        bestCandidate = d
      }
    }
    bestCandidate
  }

  def isModelSolved(m: Model): Boolean = m.domains.values.foldLeft(true)((x,y) => x && (y.size == 1))

  def splitModel(m: Model): (Model,Model) = {
    val s = selectBestSplitCandidate(m)
    val (l: RELDOM,r: RELDOM) = s._2.split

    (m.setRelDomain(s._1,l),m.setRelDomain(s._1,r))
  }

  def solveModel(mm: Model): (Map[Symbol,LSSEQ]) = {
    val m = mm.propagateConstraints

    if (!m.isValid) m.domains.mapValues(x => emptyLongBSeq) // not valid - empty
    else if (m.isSolved && m.isValid) m.domains.mapValues(_.applyRange) // valid and solved, apply range
    else {
      val (m1,m2) = m.split

      val mm1 = solveModel(m1)
      val mm2 = solveModel(m2)

      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  case class ColSyntax[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
  (rel: BinRel[X,Y,XC,YC]){
    def L: RCol[X] = LeftRCol[X,Y,XC,YC](rel)
    def R: RCol[Y] = RightRCol[X,Y,XC,YC](rel)
  }

  implicit def toColSyntax[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
  (id: BinRel[X,Y,XC,YC]): ColSyntax[X,Y,XC,YC] = ColSyntax(id)

  final def main(args: Array[String]): Unit = {

    val a = createRel(
      (0.toLong until 100000).toArray,
      (0.toLong until 100000).toArray
    )

    val b = createRel(
      (500.toLong until 600).toArray,
      (500.toLong until 600).toArray
    )

    println("start")

    var m = createModel.
      addRelation('a, a).
      addRelation('b, b).
      addConstraint(EqualP(),b.R,a.L).
      addConstraint(EqualP(),a.R,b.L)

    val s = m.solve

    println("s: " + s)
  }

}
