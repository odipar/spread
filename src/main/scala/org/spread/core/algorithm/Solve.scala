package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.{NoAnnotation, StatisticsAnnotator}
import org.spread.core.constraint.Constraint.{EqualStatP, GreaterEqualStatP, Prop, PropValue}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.RangedSequence._
import org.spread.core.sequence.Sequence._
import spire.algebra.Order
import spire.implicits._

import scala.reflect.ClassTag
import scala.language.{existentials, implicitConversions}

//
// Constraint propagation relational join algorithm, based on user defined annotations
//
// Copyright 2017: Robbert van Dalen
//

object Solve {

  case class RelConstraint[X1,X2,X3,X4,A <: PropValue,S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2], S3 <: Seq[X3,S3],S4 <: AnnotatedSeq[X4,A,S4]]
  (s1: AnnSelector[X1,X2,A,S1,S2],s2: AnnSelector[X3,X4,A,S3,S4],prop: Prop[A]) {

    val leftSelector = LeftSelector[X1,X2,A,S1,S2](s1, prop)
    val rightSelector = RightSelector[X3,X4,A,S3,S4](s2, prop)
  }

  trait AnnSeqSelector[X1,X2,A <: PropValue,S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2]] {
    def selector: AnnSelector[X1,X2,A,S1,S2]
    def prop: Prop[A]
    def apply(): AnnotatedSeq[X2,A,S2] = selector()
    def applyAny(s: SSEQ): ASEQ = apply()
  }

  case class LeftSelector[X1,X2,A <: PropValue, S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2]]
  (selector: AnnSelector[X1,X2,A,S1,S2], prop: Prop[A])
    extends AnnSeqSelector[X1,X2,A,S1,S2] {
  }

  case class RightSelector[X1,X2,A <: PropValue, S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2]]
  (selector: AnnSelector[X1,X2,A,S1,S2], prop: Prop[A])
    extends AnnSeqSelector[X1,X2,A,S1,S2] {
  }

  type SSEQ = Seq[X,S] forSome { type X ; type S <: Seq[X,S] }
  type ASEQ = AnnotatedSeq[X,A,S] forSome { type X ; type A <: PropValue ; type S <: AnnotatedSeq[X,A,S] }
  type ANSEL = AnnSelector[X1,X2,A,S1,S2] forSome {
    type X1
    type X2
    type A <: PropValue
    type S1 <: Seq[X1,S1]
    type S2 <: AnnotatedSeq[X2,A,S2]
  }
  type ASEL[X1,A <: PropValue, S1 <: Seq[X1,S1]] = AnnSeqSelector[X1,X2,A,S1,S2] forSome {
    type X2 ;
    type S2 <: AnnotatedSeq[X2,A,S2]
  }

  type VMAP[X,S <: Seq[X,S]] = Map[ASEL[X,_ <: PropValue, S], _ <: PropValue]

  type PVAL = AnnSeqSelector[X1,X2,A,S1,S2] forSome {
    type X1
    type X2
    type A <: PropValue
    type S1 <: Seq[X1,S1]
    type S2 <: AnnotatedSeq[X2,A,S2]
  }

  case class ConstrainedRel[X, S <: Seq[X,S]](rel: Seq[X,S], from: Long, to: Long, vmap: VMAP[X,S]) {

    type CR = ConstrainedRel[X,S]

    def isValid = {
      val iter = vmap.valuesIterator
      var isValid = true
      while (iter.hasNext && isValid) {isValid = isValid && iter.next.isValid}
      isValid
    }
    def getAnnotation(sel: ASEL[X,_ <: PropValue, S]): PropValue = vmap(sel)
    def getAnyAnnotation(sel: PVAL): PropValue = vmap(sel.asInstanceOf[ASEL[X, _ <: PropValue,S]])

    def setAnnotation[V <: PropValue](d: V,sel: ASEL[X,V,S]): ConstrainedRel[X,S] = {
      val nmap = vmap + (sel -> d)
      ConstrainedRel(rel,from,to,nmap)
    }
    def setAnyAnnotation[V <: PropValue](d: V,sel: PVAL): ConstrainedRel[X,S] = {
      setAnnotation(d,sel.asInstanceOf[ASEL[X,V,S]])
    }
    def size = to - from + 1
    def split: (CR,CR) = {
      val r = (from + to + 1) / 2
      (ConstrainedRel(rel,from,r-1,vmap).propagate, ConstrainedRel(rel,r,to,vmap).propagate)
    }

    def propagate: CR = {
      if (isValid) {
        val k = vmap.keysIterator
        var newm: VMAP[X,S] = Map()

        while (k.hasNext) {
          val sel = k.next
          val annSeq = sel()
          val equalProp = annSeq.equal
          val ann0 = annSeq.annotationRange(from,to)
          val ann1 = vmap(sel)
          val propAnn = equalProp.propagateAny(ann0,ann1)._1
          newm = newm + (sel->propAnn)
        }
        ConstrainedRel(rel,from,to,newm)
      }
      else this
    }
    def applyRange: LSEQ[NoAnnotation] = createRange(from,to)
  }

  type RCSTR = RelConstraint[X1,X2,X3,X4,A,S1,S2,S3,S4] forSome {
    type X1
    type X2
    type X3
    type X4
    type A <: PropValue
    type S1 <: Seq[X1,S1]
    type S2 <: AnnotatedSeq[X2,A,S2]
    type S3 <: Seq[X3,S3]
    type S4 <: AnnotatedSeq[X4,A,S4]
  }

  type CREL = ConstrainedRel[X,S] forSome { type X ; type S <: Seq[X,S] }
  type CTRS = Set[RCSTR]
  type DOMS = Map[SSEQ,CREL]

  def createModel: Model = Model(Set(),Map(),isValid = true)

  def createRelDomain[@sp X, S <: Seq[X,S]](rel: S): ConstrainedRel[X,S] = {
    ConstrainedRel[X,S](rel,0,rel.size-1,Map())
  }

  var mm: Long = 0

  case class Model(ctrs: CTRS,domains: DOMS,isValid: Boolean) {
    { mm = mm + 1 }
    def addSequence[@sp X, S <: Seq[X,S]](seq: Seq[X,S]): Model = {
      Model(ctrs,domains + (seq->createRelDomain[X,S](seq.asInstanceOf[S])),isValid)
    }

    def addConstraint[X1,X2,X3,X4,A <: PropValue,S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2], S3 <: Seq[X3,S3], S4 <: AnnotatedSeq[X4,A,S4]]
    (r1: AnnSelector[X1,X2,A,S1,S2], r2: AnnSelector[X3,X4,A,S3,S4])(cc: Prop[A]): Model = {
      
      val c = RelConstraint(r1,r2,cc)

      var ndomains = domains

      val s1 = r1.seq
      val s2 = r2.seq
      
      if (!ndomains.contains(s1)) ndomains = ndomains + (s1->createRelDomain[X1,S1](s1.asInstanceOf[S1]))
      if (!ndomains.contains(s2)) ndomains = ndomains + (s2->createRelDomain[X3,S3](s2.asInstanceOf[S3]))

      ndomains = ndomains + (s1 -> ndomains(s1).setAnyAnnotation(r1().annotation,c.leftSelector))
      ndomains = ndomains + (s2 -> ndomains(s2).setAnyAnnotation(r2().annotation,c.rightSelector))
      
      Model(ctrs + c,ndomains,isValid)
    }
    def setRelDomain(seq: SSEQ, rel: CREL): Model = Model(ctrs,domains + (seq -> rel),rel.isValid)
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

          val s1 = c.s1.asSeq
          val s2 = c.s2.asSeq
          
          val d1 = domains(s1)
          val d2 = domains(s2)

          val r1 = d1.rel
          val r2 = d2.rel

          val left = c.leftSelector
          val right = c.rightSelector

          val dd1: PropValue = d1.getAnyAnnotation(left)
          val dd2: PropValue = d2.getAnyAnnotation(right)

          val (rd1,rd2) = c.prop.propagateAny(dd1,dd2)

          isValid = rd1.isValid && rd2.isValid

          if ((dd1 != rd1) || (dd2 != rd2)) {
            val k = d1.setAnyAnnotation(rd1,left)

            fixpoint = false // Either one of the domains have been propagated to something different

            if (s1 != s2) {
              domains = domains + (s1 -> d1.setAnyAnnotation(rd1,left))
              domains = domains + (s2 -> d2.setAnyAnnotation(rd2,right))
            }
            else {
              domains = domains + (s1 -> d1.setAnyAnnotation(rd1,left).setAnyAnnotation(rd2,right))
            }
          }
        }
      }
      Model(m.ctrs,domains,isValid)
    }
  }

  def selectBestSplitCandidate(m: Model): (SSEQ,CREL) = {
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

  def solveModel(mm: Model): (Map[SSEQ,LSEQ[NoAnnotation]]) = {
    val m = mm.propagateConstraints

    if (!m.isValid) m.domains.mapValues(x => defaultLongSeqFactory) // not valid - empty
    else if (m.isSolved && m.isValid) m.domains.mapValues(_.applyRange) // valid and solved, apply range
    else {
      val (m1,m2) = m.split
      val mm1 = solveModel(m1)
      val mm2 = solveModel(m2)
      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  def createSeq[@sp X: ClassTag](x: Array[X])(implicit o1: Order[X]) = { seqFactory[X].createSeq(x) }
  
  implicit def annotator[@sp X](implicit ord: Order[X]): StatisticsAnnotator[X] = StatisticsAnnotator[X]()

  def equalStatP[@sp X](implicit ann: StatisticsAnnotator[X]) = EqualStatP[X]()

  final def main(args: Array[String]): Unit = {
    import Selector._
    import Combiner._

    /*val t1 = (
      Array("a","b","c","d"),
      Array( 1 , 1,  2,  2 ),
      Array(0.1,0.2,0.3,0.4)
    )

    val t2 = (
      Array("a","c","c","e","a"),
      Array(0.1,0.2,0.3,0.4,0.1),
      Array( 2,  2,  3,  2,  1 )
    )

    val tt1 = createSeq(t1._1) && createSeq(t1._2) && createSeq(t1._3)
    val tt2 = createSeq(t2._1) && createSeq(t2._2) && createSeq(t2._3)
    val tt3 = createSeq(Array("c"))
    val tt4 = createSeq(Array(2))

    tt1.show
    tt2.show
    
    val s1_c1 = tt1.select(_.L.L)
    val s1_c2 = tt1.select(_.L.R)
    val s1_c3 = tt1.select(_.R)

    val s2_c1 = tt2.select(_.L.L)
    val s2_c2 = tt2.select(_.L.R)
    val s2_c3 = tt2.select(_.R)

    val s3_c1 = tt3.select(x => x)
    val s4_c1 = tt4.select(x => x)
                                       */
    /*var m = createModel.
      addConstraint(s1_c1,s2_c1)(equalStatP).
      addConstraint(s1_c2,s2_c3)(equalStatP).
      addConstraint(s1_c3,s2_c2)(equalStatP)
      */

   /* var m = createModel.
      addConstraint(s2_c1,s3_c1)(GreaterEqualStatP()).
      addConstraint(s4_c1,s2_c3)(GreaterEqualStatP())  */

    val s1 = createSeq((0 to 6).toArray)
    val s1_c1 = s1.select(x => x)

    val ls = createSeq(Array(0,2))
    val hs = createSeq(Array(4,6))

    val ls_c1 = ls.select(x => x)
    val hs_c1 = hs.select(x => x)

    var m = createModel.
      addConstraint(s1_c1,ls_c1)(GreaterEqualStatP()).
      addConstraint(hs_c1,s1_c1)(GreaterEqualStatP())

    println("start: " + mm)
    m.solve.map(x => x._2.sort.show)
    println("end: " + mm)
  }
}


