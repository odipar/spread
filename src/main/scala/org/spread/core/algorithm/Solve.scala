package org.spread.core.algorithm

import org.spread.core.annotation.Annotation.{NoAnnotation, StatisticsAnnotator}
import org.spread.core.constraint.Constraint.{EqualStatP, Prop, PropValue}
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
          val ann0: PropValue = annSeq.annotationRange(from,to)
          val ann1: PropValue = vmap(sel)
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
  type RELS = Map[Symbol,SSEQ]
  type RELSI = Map[SSEQ,Symbol]
  type CTRS = Set[RCSTR]
  type DOMS = Map[Symbol,CREL]

  def createModel: Model = Model(Map(),Map(),Set(),Map(),isValid = true)

  def createRelDomain[@sp X, S <: Seq[X,S]](rel: S): ConstrainedRel[X,S] = {
    ConstrainedRel[X,S](rel,0,rel.size-1,Map())
  }

  case class Model(rels: RELS,relsInv: RELSI,ctrs: CTRS,domains: DOMS,isValid: Boolean) {
    def addSequence[@sp X, S <: Seq[X,S]](s: Symbol, rel: Seq[X,S]): Model = {
      Model(rels + (s->rel),relsInv + (rel->s),ctrs,domains + (s->createRelDomain[X,S](rel.asInstanceOf[S])),isValid)
    }

    def addConstraint[X1,X2,X3,X4,A <: PropValue,S1 <: Seq[X1,S1],S2 <: AnnotatedSeq[X2,A,S2], S3 <: Seq[X3,S3], S4 <: AnnotatedSeq[X4,A,S4]]
    (cc: Prop[A], r1: AnnSelector[X1,X2,A,S1,S2], r2: AnnSelector[X3,X4,A,S3,S4]): Model = {

      // Check if they have matching IDs
      val id1 = relsInv(r1.seq)
      val id2 = relsInv(r2.seq)

      // TODO: if not found, add them with secret ids
      if ((id1 == null) || (id2 == null)) sys.error("No matching rel")

      val c = RelConstraint(r1,r2,cc)

      var ndomains = domains
      ndomains = ndomains + (id1 -> ndomains(id1).setAnyAnnotation(r1().annotation,c.leftSelector))
      ndomains = ndomains + (id2 -> ndomains(id2).setAnyAnnotation(r2().annotation,c.rightSelector))

      Model(rels,relsInv,ctrs + c,ndomains,isValid)
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

          val c1 = c.s1
          val c2 = c.s2

          // TODO: remove asInstanceOf[SSEQ]
          val id1: Symbol = relsInv(c1.seq.asInstanceOf[SSEQ])
          val id2: Symbol = relsInv(c2.seq.asInstanceOf[SSEQ])

          val d1 = domains(id1)
          val d2 = domains(id2)

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

            if (id1 != id2) {
              domains = domains + (id1 -> d1.setAnyAnnotation(rd1,left))
              domains = domains + (id2 -> d2.setAnyAnnotation(rd2,right))
            }
            else {
              domains = domains + (id1 -> d1.setAnyAnnotation(rd1,left).setAnyAnnotation(rd2,right))
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

  def solveModel(mm: Model): (Map[Symbol,LSEQ[NoAnnotation]]) = {
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

  def createPaired[@sp X1: ClassTag,@sp X2: ClassTag]
  (x1: Array[X1], x2: Array[X2])(implicit o1: Order[X1], o2: Order[X2]) = {
    import Combiner._

    val xx1 = seqFactory[X1].createSeq(x1)
    val xx2 = seqFactory[X2].createSeq(x2)

    xx1 && xx2
  }

  implicit def annotator[@sp X](implicit ord: Order[X]): StatisticsAnnotator[X] = StatisticsAnnotator[X]()


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

    val a1 = a.select('a1,_.L)
    val a2 = a.select('a2,_.R)

    val b1 = b.select('b1,_.L)
    val b2 = b.select('b2,_.R)

    val c1 = c.select('c1,_.L)
    val c2 = c.select('c2,_.R)

    var m = createModel.
      addSequence('a, a).
      addSequence('b, b).
      addSequence('c, c).
      addConstraint(EqualStatP[Long](),a2,b1).
      addConstraint(EqualStatP[Long](),b2,c1).
      addConstraint(EqualStatP[Long](),c2,a1)

    println("m: " + m.solve)
  }
}


