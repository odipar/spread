package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.StatisticsAnnotator
import org.spread.core.constraint.Constraint._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.RangedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// Constraint propagation relational join algorithm, based on user defined annotations
//
// Copyright 2017: Robbert van Dalen
//

object Solve {

  type CREL = ConstrainedRel[X1,X2,A1,A2,S1,S2,S] forSome {
    type X1
    type X2
    type A1 <: PropValue
    type A2 <: PropValue
    type S1 <: OSEQ[X1,A1,S1]
    type S2 <: OSEQ[X2,A2,S2]
    type S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  }

  case class ConstrainedRel[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (rel: BinRel[X1,X2,A1,A2,S1,S2,S],from: Long,to: Long,leftAnnotation: A1,rightAnnotation: A2) {
    type CR = ConstrainedRel[X1,X2,A1,A2,S1,S2,S]
    
    def isValid = leftAnnotation.isValid && rightAnnotation.isValid
    def size = to - from + 1
    def split: (CR,CR) = {
      val r = (from + to + 1) / 2
      (ConstrainedRel(rel,from,r-1,leftAnnotation,rightAnnotation).propagate,
        ConstrainedRel(rel,r,to,leftAnnotation,rightAnnotation).propagate)
    }
    def getAnnotation(column: ColumnPos): PropValue = {
      if (column == LeftCol) leftAnnotation
      else rightAnnotation
    }
    def setAnnotation(d: PropValue, column: ColumnPos): CR = {
      if (column == LeftCol) ConstrainedRel(rel,from,to,d.asInstanceOf[A1],rightAnnotation)
      else ConstrainedRel(rel,from,to,leftAnnotation,d.asInstanceOf[A2])
    }
    def setAnyAnnotation(d: PropValue,column: ColumnPos) = {
      setAnnotation(d,column)
    }
    def propagate: CR = {
      if (isValid) {
        val l = rel.left.annotationRange(from,to)
        val r = rel.right.annotationRange(from,to)

        val d1 = rel.left.equal.propagate(leftAnnotation,l)._1
        val d2 = rel.right.equal.propagate(rightAnnotation,r)._1
        ConstrainedRel(rel,from,to,d1,d2)
      }
      else this
    }
    def empty: EREL = rel.emptySeq
    def applyRange: LSEQ = createRange(from,to)
  }

  def createRelDomain[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (rel: BinRel[X1,X2,A1,A2,S1,S2,S]) = {
    val l = rel.left.annotation
    val r = rel.right.annotation
    ConstrainedRel(rel,0,rel.size-1,l,r)
  }

  type RCSTR = RelConstraint[X,A] forSome { type X ; type A <: PropValue }
  type RELS = Map[Symbol,EREL]
  type RELSI = Map[EREL,Symbol]
  type CTRS = Set[RCSTR]
  type DOMS = Map[Symbol,CREL]

  def createModel: Model = Model(Map(),Map(),Set(),Map(),isValid = true)

  var mm: Long = 0

  case class Model(rels: RELS,relsInv: RELSI,ctrs: CTRS,domains: DOMS,isValid: Boolean) {
    { mm = mm + 1 }
    def addSequence[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
    (s: Symbol, rel: BinRel[X1,X2,A1,A2,S1,S2,S]): Model = {
      Model(rels + (s->rel),relsInv + (rel->s),ctrs,domains + (s->createRelDomain(rel)),isValid)
    }
    def addConstraint[X,A <: PropValue](cc: Prop[A], r1: RCol[X,A], r2: RCol[X,A]): Model = {
      val id1 = relsInv(r1.rel)
      val id2 = relsInv(r2.rel)
      val c = RelConstraint(r1.withID(id1),r2.withID(id2),cc)
      Model(rels,relsInv,ctrs + c,domains,isValid)
    }
    def setRelDomain(id: Symbol, rel: CREL): Model = {
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

      while(!fixpoint && isValid) { // Loop until fixpoint (no annotations have changed after propagation)
        fixpoint = true

        val iter = m.ctrs.iterator
        while(iter.hasNext && isValid) {
          val c = iter.next

          val d1 = domains(c.r1.id)
          val d2 = domains(c.r2.id)

          val dd1 = d1.getAnnotation(c.r1.column)
          val dd2 = d2.getAnnotation(c.r2.column)

          val (rd1,rd2) = c.prop.propagateAny(dd1,dd2)

          isValid = rd1.isValid && rd2.isValid

          if ((dd1 != rd1) || (dd2 != rd2)) {
            fixpoint = false // Either one of the domains have been propagated to something different
            if (c.r1.id != c.r2.id) {
              domains = domains + (c.r1.id -> d1.setAnnotation(rd1,c.r1.column))
              domains = domains + (c.r2.id -> d2.setAnnotation(rd2,c.r2.column))
            }
            else {
              domains = domains + (c.r1.id -> d1.setAnnotation(rd1,c.r1.column).setAnnotation(rd2,c.r2.column))
            }
          }
        }
      }
      Model(m.rels,m.relsInv,m.ctrs,domains,isValid)
    }
  }

  def selectBestSplitCandidate(m: Model): (Symbol,CREL) = {
    val doms = m.domains
    var bestCandidate = doms.last

    for (d <- doms) {
      if (d._2.size > bestCandidate._2.size) {
        // We found a RelDomain with a bigger size
        // TODO: select most restrictive left/right domain? Should be pluggable
        bestCandidate = d
      }
    }
    bestCandidate
  }

  def isModelSolved(m: Model): Boolean = m.domains.values.foldLeft(true)((x,y) => x && (y.size == 1))

  def splitModel(m: Model): (Model,Model) = {
    val s = selectBestSplitCandidate(m)
    val (l: CREL,r: CREL) = s._2.split

    (m.setRelDomain(s._1,l),m.setRelDomain(s._1,r))
  }

  def solveModel(mm: Model): (Map[Symbol,LSEQ]) = {
    val m = mm.propagateConstraints

    if (!m.isValid) m.domains.mapValues(x => longSeqFactory) // not valid - empty
    else if (m.isSolved && m.isValid) m.domains.mapValues(_.applyRange) // valid and solved, apply range
    else {
      val (m1,m2) = m.split

      val mm1 = solveModel(m1)
      val mm2 = solveModel(m2)

      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  case class ColSyntax[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (rel: BinRel[X1,X2,A1,A2,S1,S2,S]) extends AnyVal {
    def L: RCol[X1,A1] = LeftRCol[X1,X2,A1,A2,S1,S2,S](rel)
    def R: RCol[X2,A2] = RightRCol[X1,X2,A1,A2,S1,S2,S](rel)
  }

  implicit def toColSyntax[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: OSEQ[X1,A1,S1],S2 <: OSEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (id: BinRel[X1,X2,A1,A2,S1,S2,S]): ColSyntax[X1,X2,A1,A2,S1,S2,S] = ColSyntax(id)

  def createPaired[X1: ClassTag,X2: ClassTag](x1: Array[X1], x2: Array[X2])(implicit o1: Ordering[X1], o2: Ordering[X2]) = {
    val xx1 = seqFactory[X1].createSeq(x1)
    val xx2 = seqFactory[X2].createSeq(x2)
    xx1 combineAnnOrd xx2
  }

  implicit def annotator[X](implicit ord: Ordering[X]): StatisticsAnnotator[X] = StatisticsAnnotator[X]()

  final def main(args: Array[String]): Unit = {

    val a = createPaired(
      (505.toLong until 100000).toArray,
      (505.toLong until 100000).toArray
    )

    val b = createPaired(
      (500.toLong until 6000).toArray,
      (500.toLong until 6000).toArray
    )

    val c = createPaired(
      (500.toLong until 510).toArray,
      (500.toLong until 510).toArray
    )

    var m = createModel.
      addSequence('a, a).
      addSequence('b, b).
      addSequence('c, c).
      addConstraint(EqualStatP[Long](),b.R,a.L).
      addConstraint(EqualStatP[Long](),b.R,b.L).
      addConstraint(EqualStatP[Long](),c.R,b.L)

    val s = m.solve

    println("s: "  + s)
  }
}
