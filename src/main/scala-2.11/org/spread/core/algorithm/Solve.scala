package org.spread.core.algorithm

import org.spread.core.constraint.Constraint._
import org.spread.core.relation.Relation._
import org.spread.core.annotation.Annotation._
import org.spread.core.sequence.Sequence.LongBSeq
import org.spread.core.sequence.Sequence.createLongBSeq
import org.spread.core.sequence.Sequence.emptyLongBSeq

import scala.language.{existentials, implicitConversions}

//
// Constraint based relational join algorithm using binary annotated statistics
//
// Copyright 2017: Robbert van Dalen
//

object Solve {

  case class RelDomain[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (rel: BinRel[X,Y],from: Long,to: Long,leftDomain: Domain[X],rightDomain: Domain[Y]) {
    implicit def xc = rel.xc
    implicit def yc = rel.yc
    implicit def xord = xc.ordering
    implicit def yord = yc.ordering

    def isValid = leftDomain.isValid && rightDomain.isValid
    def size = to - from + 1
    def split: (RelDomain[X,Y],RelDomain[X,Y]) = {
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
    def setDomain(d: Domain[_],column: ColumnPos): RelDomain[X,Y] = {
      if (column == LeftCol) RelDomain(rel,from,to,d.asInstanceOf[Domain[X]],rightDomain)
      else RelDomain(rel,from,to,leftDomain,d.asInstanceOf[Domain[Y]])
    }
    def propagate: RelDomain[X,Y] = {
      if (isValid) {
        val l = rel.left.annotationRange(from,to)
        val r = rel.right.annotationRange(from,to)
        val d1 = xc.equalProp.propagate(leftDomain,l)._1
        val d2 = yc.equalProp.propagate(rightDomain,r)._1
        RelDomain(rel,from,to,d1,d2)
      }
      else this
    }
    def empty: BinRel[_,_] = rel.empty
    def applyRange: LongBSeq = createLongBSeq(from,to)
  }

  def createRelDomain[X,Y](rel: BinRel[X,Y]) = {
    val l = rel.left.annotation
    val r = rel.right.annotation
    RelDomain(rel,0,rel.size-1,createDomain(l.lowerBound,l.upperBound),createDomain(r.lowerBound,r.upperBound))
  }

  type RELS = Map[Symbol,BinRel[_,_]]
  type RELSI = Map[BinRel[_,_],Symbol]
  type CTRS = Set[RelConstraint[_]]
  type DOMS = Map[Symbol,RelDomain[_,_]]

  def createModel: Model = Model(Map(),Map(),Set(),Map(),isValid = true)

  case class Model(rels: RELS,relsInv: RELSI,ctrs: CTRS,doms: DOMS,isValid: Boolean) {
    def addRelation[X,Y](s: Symbol, rel: BinRel[X,Y]): Model = {
      Model(rels + (s->rel),relsInv + (rel->s),ctrs,doms + (s->createRelDomain(rel)),isValid)
    }
    def addConstraint[X](cc: Propagator[X], r1: RCol[X],r2: RCol[X]): Model = {
      val id1 = relsInv(r1.rel)
      val id2 = relsInv(r2.rel)
      val c = RelConstraint(r1.withID(id1),r2.withID(id2),cc)
      Model(rels,relsInv,ctrs + c,doms,isValid)
    }
    def setRelDomain[X,Y](id: Symbol, rel: RelDomain[X,Y]) = Model(rels,relsInv,ctrs,doms + (id->rel),rel.isValid)
    def propagateConstraints = propagateModelConstraints(this)
    def split = splitModel(this)
    def solve = solveModel(this)
    def isSolved = isModelSolved(this)
  }

  def getDomainProperties(col: RelCol[_], doms: DOMS) = {
    val d = doms(col.id)
    val dd = d.getDomain(col.column)
    (col,d,dd)
  }

  def propagateModelConstraints(m: Model): Model = {
    if (!m.isValid) m
    else {
      var fixpoint = false
      var isValid = true
      var domains = m.doms

      while(!fixpoint && isValid) { // Loop until fixpoint (no domains have changed)
        fixpoint = true

        val iter = m.ctrs.iterator
        while(iter.hasNext && isValid) {
          val c = iter.next
          val (r1,d1,dd1) = getDomainProperties(c.r1,domains)
          val (r2,d2,dd2) = getDomainProperties(c.r2,domains)
          val (rd1: Domain[_],rd2: Domain[_]) = c.prop.propagateAny(dd1,dd2)(d1.getOrdering(r1.column))

          isValid = rd1.isValid && rd2.isValid

          if ((dd1 != rd1) || (dd2 != rd2)) {
            fixpoint = false // Either one of the domains have been propagated to something different
            domains = domains + (r1.id -> d1.setDomain(rd1,r1.column))
            domains = domains + (r2.id -> d2.setDomain(rd2,r2.column))
          }
        }
      }
      Model(m.rels,m.relsInv,m.ctrs,domains,isValid)
    }
  }

  def selectBestSplitCandidate(m: Model): (Symbol,RelDomain[_,_]) = {
    var bestCandidate = m.doms.last

    for (d <- m.doms) {
      if (d._2.size > bestCandidate._2.size) {
        // We found a RelDomain with a bigger size
        // TODO: select most restrictive left/right domain?
        bestCandidate = d
      }
    }
    bestCandidate
  }

  def isModelSolved(m: Model): Boolean = m.doms.values.foldLeft(true)((x,y) => x && (y.size == 1))

  def splitModel(m: Model): (Model,Model) = {
    val s = selectBestSplitCandidate(m)
    val (l: RelDomain[_,_],r: RelDomain[_,_]) = selectBestSplitCandidate(m)._2.split

    (m.setRelDomain(s._1,l),m.setRelDomain(s._1,r))
  }

  def solveModel(mm: Model): (Map[Symbol,LongBSeq]) = {
    val m = mm.propagateConstraints

    if (!m.isValid) m.doms.mapValues(x => emptyLongBSeq) // not valid - empty
    else if (m.isSolved && m.isValid) m.doms.mapValues(_.applyRange) // valid and solved, apply range
    else {
      val (m1,m2) = m.split

      val mm1 = solveModel(m1)
      val mm2 = solveModel(m2)

      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  case class ColSyntax[X,Y](rel: BinRel[X,Y]){
    def L: RCol[X] = LeftRCol[X](rel)
    def R: RCol[Y] = RightRCol[Y](rel)
  }

  implicit def toColSyntax[X,Y](id: BinRel[X,Y]): ColSyntax[X,Y] = ColSyntax(id)

  final def main(args: Array[String]): Unit = {

    val a = createRel(
      (0.toLong until 100000).toArray,
      (0.toLong until 100000).toArray
    )

    val b = createRel(
      (501.toLong until 601).toArray,
      (500.toLong until 600).toArray
    )

    println("start")

    var m = createModel.
      addRelation('a, a).
      addRelation('b, b).
      addConstraint(EqualP(),b.R,a.L).
      addConstraint(EqualP(),a.R,b.L)

    val s = m.solve

    //println("m: " + m.doms)
    println("s: " + s)
  }

}
