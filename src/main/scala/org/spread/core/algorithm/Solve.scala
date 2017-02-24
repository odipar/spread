package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.StatisticsAnnotator
import org.spread.core.constraint.Constraint._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.RangedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.Sequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// Constraint propagation relational join algorithm, based on user defined annotations
//
// Copyright 2017: Robbert van Dalen
//

object Solve {

  /*type CREL = ConstrainedRel[X1,X2,A1,A2,S1,S2,S] forSome {
    type X1
    type X2
    type A1 <: PropValue
    type A2 <: PropValue
    type S1 <: ASEQ[X1,A1,S1]
    type S2 <: ASEQ[X2,A2,S2]
    type S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]
  }

  case class ConstrainedRel[X1,X2,A1 <: PropValue,A2 <: PropValue,S1 <: ASEQ[X1,A1,S1],S2 <: ASEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
  (rel: BinRel[X1,X2,A1,A2,S1,S2,S],from: Long,to: Long,leftAnnotation: A1,rightAnnotation: A2) {
    type CR = ConstrainedRel[X1,X2,A1,A2,S1,S2,S]
    
    def isValid = leftAnnotation.isValid && rightAnnotation.isValid
    def size = to - from + 1
    def split: (CR,CR) = {
      val r = (from + to + 1) / 2
      (ConstrainedRel(rel,from,r-1,leftAnnotation,rightAnnotation).propagate,
        ConstrainedRel(rel,r,to,leftAnnotation,rightAnnotation).propagate)
    }
    /*def getAnnotation(column: ColumnPos): PropValue = {
      if (column == LeftCol) leftAnnotation
      else rightAnnotation
    }
    def setAnnotation(d: PropValue, column: ColumnPos): CR = {
      if (column == LeftCol) ConstrainedRel(rel,from,to,d.asInstanceOf[A1],rightAnnotation)
      else ConstrainedRel(rel,from,to,leftAnnotation,d.asInstanceOf[A2])
    }
    def setAnyAnnotation(d: PropValue,column: ColumnPos) = {
      setAnnotation(d,column)
    } */
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

  def createRelDomain[@@sp X1,@@sp X2,A1 <: PropValue,A2 <: PropValue,S1 <: ASEQ[X1,A1,S1],S2 <: ASEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
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
    def addSequence[@@sp X1,@@sp X2,A1 <: PropValue,A2 <: PropValue,S1 <: ASEQ[X1,A1,S1],S2 <: ASEQ[X2,A2,S2], S <: AnnPairSeq[X1,X2,A1,A2,S1,S2,S]]
    (s: Symbol, rel: BinRel[X1,X2,A1,A2,S1,S2,S]): Model = {
      Model(rels + (s->rel),relsInv + (rel->s),ctrs,domains + (s->createRelDomain(rel)),isValid)
    }
    def addConstraint[X,A <: PropValue]
    (cc: Prop[A], r1: AnnSelector[_,_,_,_,_,X,A,_], r2: AnnSelector[_,_,_,_,_,X,A,_]): Model = {

      // Check if they have mathing IDs
      val id1 = relsInv(r1.s)
      val id2 = relsInv(r2.s)

      if ((id1 == null) || (id2 == null)) sys.error("No matching rel")
      
      val c = RelConstraint(r1,r2,cc)
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
      var relsInv = m.relsInv
      
      while(!fixpoint && isValid) { // Loop until fixpoint (no annotations have changed after propagation)
        fixpoint = true

        val iter = m.ctrs.iterator
        while(iter.hasNext && isValid) {
          val c = iter.next

          val c1: AnnSelector[_,_,_,_,EREL,_,_,_] = c.r1.asInstanceOf[AnnSelector[_,_,_,_,EREL,_,_,_]]
          val c2: AnnSelector[_,_,_,_,EREL,_,_,_] = c.r2.asInstanceOf[AnnSelector[_,_,_,_,EREL,_,_,_]]

          val id1: Symbol = relsInv(c1.s.asInstanceOf[EREL])
          val id2: Symbol = relsInv(c1.s.asInstanceOf[EREL])
          
          val d1: CREL = domains(id1)
          val d2: CREL = domains(id2)

          val dd1: PropValue = c1(d1.rel).asInstanceOf[AnnotatedSeq[_,_ <: PropValue,_]].annotation
          val dd2: PropValue = c2(d2.rel).asInstanceOf[AnnotatedSeq[_,_ <: PropValue,_]].annotation

          val (rd1,rd2) = c.prop.propagateAny(dd1,dd2)

          isValid = rd1.isValid && rd2.isValid

          if ((dd1 != rd1) || (dd2 != rd2)) {
            fixpoint = false // Either one of the domains have been propagated to something different
            if (id1 != id2) {
              domains = domains + (id1 -> d1.setAnnotation(rd1,dd1))
              domains = domains + (id2 -> d2.setAnnotation(rd2,dd2))
            } 
            else {
              domains = domains + (id1 -> d1.setAnnotation(rd1,dd1).setAnnotation(rd2,dd2))
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

  def createPaired[@@sp X1: ClassTag,@@sp X2: ClassTag]
    (x1: Array[X1], x2: Array[X2])(implicit o1: Ordering[X1], o2: Ordering[X2]) = {
      import Combiner._

      val xx1 = seqFactory[X1].createSeq(x1)
      val xx2 = seqFactory[X2].createSeq(x2)
      xx1 && xx2
  }

  implicit def annotator[@@sp X](implicit ord: Ordering[X]): StatisticsAnnotator[X] = StatisticsAnnotator[X]()

  final def main(args: Array[String]): Unit = {
    import Selector._

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

    val a1 = a.select(_.L,'a1)
    val a2 = a.select(_.R,'a2)

    val b1 = b.select(_.L,'b1)
    val b2 = b.select(_.R,'b2)

    val c1 = c.select(_.L,'c1)
    val c2 = c.select(_.R,'c2)
    
    /*var m = createModel.
      addSequence('a, a).
      addSequence('b, b).
      addSequence('c, c).
      addConstraint(EqualStatP[Long](),b.R,a.L).
      addConstraint(EqualStatP[Long](),b.R,b.L).
      addConstraint(EqualStatP[Long](),c.R,b.L)   */

   // val s = m.solve

    //println("s: "  + s)
  }  */
}
