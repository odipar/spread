package org.spread.core.algorithm

import org.spread.core.algorithm.Solve.createSeq
import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint.{EqualProp, EqualStatP, GreaterEqualProp, GreaterEqualStatP, LowerEqualProp, LowerEqualStatP, NotEqualProp, Prop, PropValue}
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.AnnotatedSequence._
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.RangedSequence._
import org.spread.core.sequence.Sequence._
import org.spread.core.splithash.Hashing
import spire.algebra.Order
import spire.implicits._

import scala.reflect.ClassTag
import scala.language.{existentials, implicitConversions}


//
// Constraint propagation relational join algorithm, based on user defined annotations
// and a predicate DSL
//
// Copyright 2017: Robbert van Dalen
//

object Solve2 {
  type ASEQ[X,A,S <: AnnotatedSeq[X,A,S]] = AnnotatedSeq[X,A,S]
  type ASEL[X1,X2,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2]] = AnnSelector[X1,X2,A,S1,S2]

  type ANYSEL = AnnSelector[X1,X2,A,S1,S2] forSome {
    type X1
    type X2
    type A <: PropValue
    type S1 <: Seq[X1,S1]
    type S2 <: AnnotatedSeq[X2,A,S2]
  }
  type PVAL = PropValue
  type ANYSEQ = Seq[X,S] forSome { type X ; type S <: Seq[X,S] }

  trait Solver[S <: Solver[S]] {
    def asFixPoint: S

    def init[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): S

    def isValid[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): Boolean

    def isSolved[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): Boolean

    def propagate[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): S

    def split: (S,S)

    def solve[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]]
  }

  trait CExpr[E <: CExpr[E]] {
    def isValid[S <: Solver[S]](s: S): Boolean
    def isSolved[S <: Solver[S]](s: S): Boolean
    def init[S <: Solver[S]](s: S): S
    def propagate[S <: Solver[S]](s: S): S
  }

  case class CAndExpr[L <: CExpr[L],R <: CExpr[R]](left: L,right: R) extends CExpr[CAndExpr[L,R]] {
    def isValid[S <: Solver[S]](s: S) = left.isValid(s) && right.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = left.isSolved(s) && right.isSolved(s)
    def init[S <: Solver[S]](s: S) = right.init(left.init(s))
    def propagate[S <: Solver[S]](s: S) = right.propagate(left.propagate(s))
  }

  case class COrExpr[L <: CExpr[L],R <: CExpr[R]](left: L,right: R) extends CExpr[COrExpr[L,R]] {
    def isValid[S <: Solver[S]](s: S) = left.isValid(s) || right.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = left.isSolved(s) || right.isSolved(s)
    def init[S <: Solver[S]](s: S) = right.init(left.init(s))
    def propagate[S <: Solver[S]](s: S) = right.propagate(left.propagate(s))
  }

  case class CBinExpr[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]) extends CExpr[CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4]] {
    def isValid[S <: Solver[S]](s: S) = s.isValid(r1,r2,cc)
    def isSolved[S <: Solver[S]](s: S) = s.isSolved(r1,r2,cc)
    def init[S <: Solver[S]](s: S) = s.init(r1,r2,cc)
    def propagate[S <: Solver[S]](s: S) = s.propagate(r1,r2,cc)
  }
  
  def binExpr[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4])(cc: Prop[A]) = CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4](r1,r2,cc)

  implicit class CBinSyntax[X1,X2,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2]](r1: ASEL[X1,X2,A,S1,S2]) {
    def ===[X3,X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit eq: EqualProp[A]) = {
      binExpr(r1,r2)(eq)
    }
    def =!=[X3,X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit eq: NotEqualProp[A]) = {
      binExpr(r1,r2)(eq)
    }
    def >=[X3,X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit ge: GreaterEqualProp[A]) = {
      binExpr(r1,r2)(ge)
    }
    def <=[X3,X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit le: LowerEqualProp[A]) = {
      binExpr(r1,r2)(le)
    }
  }

  implicit class CExprSyntax[L <: CExpr[L]](left: L) {
    def and[R <: CExpr[R]](right: R): CAndExpr[L,R] = CAndExpr[L,R](left,right)
    def or[R <: CExpr[R]](right: R): COrExpr[L,R] = COrExpr[L,R](left,right)
  }

  def defaultSolver: DSolver = new DefaultSolver(Map(),Map(),true)
  
  case class SeqRange(start: Long, end: Long) {
    def size = (end-start+1)
    def applyRange = createRange(start,end)
  }

  trait DSolver extends Solver[DSolver] {
    def fixPoint: Boolean
    def seqs: Map[ANYSEQ,SeqRange]
    def doms: Map[ANYSEL,PropValue]
    def propagate: DSolver

    def solve2[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]]
    def solve3[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]]
  }
  
  case class DefaultSolver(seqs: Map[ANYSEQ,SeqRange], doms: Map[ANYSEL,PropValue], fixPoint: Boolean) extends DSolver {
    def self: DSolver = this
    
    def asFixPoint = DefaultSolver(seqs,doms,true)
    def isValid[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]) = {
      val a1 = doms(sel1)
      val a2 = doms(sel2)

      a1.isValid && a2.isValid && cc.isAnyValid(a1,a2)
    }

    def isSolved[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]) = {
      val a1 = doms(sel1)
      val a2 = doms(sel2)
      
      val r1 = seqs(sel1.asSeq)
      val r2 = seqs(sel2.asSeq)

      val a3 = sel1().annotationRange(r1.start,r1.end)
      val a4 = sel2().annotationRange(r2.start,r2.end)
      
      a1.isValid && a2.isValid && cc.isAnySolved(a1,a2) && cc.isAnySolved(a3,a4)
    }

    def propagate: DSolver = {
      var ndoms = doms
      for (dom <- doms.keys) {
        val seq = dom.asSeq
        val range = seqs(seq)
        val annSeq = dom()
        val pval = doms(dom)
        if (pval.isValid) {
          val ann = annSeq.annotationRange(range.start,range.end)
          val eqProp = annSeq.equal
          val annResult = eqProp.propagateAny(ann,pval)._1
          ndoms = ndoms + (dom -> annResult)
        }
      }
      DefaultSolver(seqs,ndoms,fixPoint)
    }

    def init[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): DSolver = {
      val s1 = sel1.asSeq
      val s2 = sel2.asSeq

      val nseqs = seqs + (s1 -> SeqRange(0,s1.size-1)) + (s2 -> SeqRange(0,s2.size-1))
      val ndoms = doms + (sel1 -> sel1().annotation) + (sel2 -> sel2().annotation)
      
      DefaultSolver(nseqs,ndoms,true)
    }
    
    def propagate[X1,X2,X3,X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): DSolver = {
      val s1 = sel1.asSeq
      val s2 = sel2.asSeq

      val r1 = seqs(s1)
      val r2 = seqs(s2)

      val a1 = doms(sel1)
      val a2 = doms(sel2)

      if (a1.isValid && a2.isValid) {
        val (aa1,aa2) = cc.propagateAny(a1,a2)
        if ((a1 == aa1) && (a2 == aa2)) this
        else DefaultSolver(seqs,doms + (sel1 -> aa1) + (sel2 -> aa2),false)
      }
      else this
    }

    def split: (DSolver,DSolver) = {
      val bestSeq = selectBestSplitCandidate
      val bestRange = seqs(bestSeq)
      val mid = (bestRange.start + bestRange.end + 1)/2

      val leftRange = SeqRange(bestRange.start,mid-1)
      val rightRange = SeqRange(mid,bestRange.end)

      val left = DefaultSolver(seqs + (bestSeq -> leftRange),doms,fixPoint)
      val right = DefaultSolver(seqs + (bestSeq -> rightRange),doms,fixPoint)

      (left.propagate,right.propagate)
    }

    def selectBestSplitCandidate: ANYSEQ = {
      // Select sequence that has the most valid annotations and least size
      // TODO: optimize with mutable maps?
      var candidates = seqs.map(x => (x._1,(0,x._2.size))).toMap[ANYSEQ,(Int,Long)]
      for (dom <- doms) {
        val annSeq = dom._1
        val ann = dom._2
        if (ann.isValid) {
          val seq = annSeq.asSeq
          val v = candidates(seq)
          candidates = candidates + (seq -> (v._1 - 1,v._2))
        }
      }
      // sort by -#valid annotations and least size
      val candidates2 = candidates.map(x => (x._2,x._1)).toSeq.sortBy(_._1).map(x => x._2)

      var bestCandidate = candidates2(0)
      for (seq <- candidates2) {
        val s = seqs(seq).size
        val cs = seqs(bestCandidate).size
        if ((cs <= 1) || ((s > 1) && (s < cs))) { bestCandidate = seq }
      }

      bestCandidate
    }

    def cartesianProduct = {
      var psize: Long = 1
      for (x <- seqs.values) { psize = psize * x.size }
      seqs.mapValues( x => repeat(x.applyRange,psize / x.size) )
    }

    def repeat(s1: LSEQ[NoAnnotation],m: Long): LSEQ[NoAnnotation] = {
      if (m == 0) s1.emptySeq
      else if (m == 1) s1
      else {
        val m2 = m / 2
        val sr = repeat(s1,m2)
        sr.append(sr).append(repeat(s1,m - (m2 * 2)))
      }
    }
    
    def solve[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]] = {
      // init
      val s2 = e.init(self)
      val s3 = s2.propagate
      s3.solve2(e)
    }

    def solve2[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]] = {
      if (!e.isValid(self)) seqs.mapValues(x => defaultLongSeqFactory)
      else if (e.isSolved(self)) cartesianProduct
      else {
        // propagate until fixpoint
        var s = e.propagate(self.asFixPoint)
        while (!s.fixPoint) {s = e.propagate(s.asFixPoint)}
        s.solve3(e)
      }
    }

    def solve3[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]] = {
      val (m1,m2) = split
      val mm1 = m1.solve2(e)
      val mm2 = m2.solve2(e)
      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  final def main(args: Array[String]): Unit = {
    import Selector._
    import Combiner._
    
    val s1 = createSeq[Long]((3 to 6).map(x => x.toLong).toArray)
    val s2 = createSeq[Long]((4 to 8).map(x => x.toLong).toArray)
    
    val t1_c1 = s1.selectSame
    val t2_c1 = s2.selectSame

    val c0 = (t1_c1 <= t2_c1)

    val r = defaultSolver.solve(c0)

    // 3,4 x
    // 4,4 x
    // 5,4
    // 6,4

    // 3,5 x
    // 4,5 x
    // 5,5 x
    // 6,5

    // 3,6 x
    // 4,6 x
    // 5,6 x
    // 6,6 x

    // 3,7 x
    // 4,7 x
    // 5,7 x
    // 6,7 x

    // 3,8 x
    // 4,8 x
    // 5,8 x
    // 6,8 x
    
    println("s1: " + r(s1))
    println("s2: " + r(s2))

  }
}