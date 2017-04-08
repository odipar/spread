package org.spread.core.algorithm

import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint._
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

object Solve {
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
  type ANYCEXPR = E forSome { type E <: CExpr[E] }

  trait Solver[S <: Solver[S]] {
    def asFixPoint: S

    def init[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): S

    def isValid[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): Boolean

    def isSolved[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): Boolean

    def propagate[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (r1: ASEL[X1,X2,A,S1,S2],r2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): S

    def split: (S,S)

    def solve[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]]
  }

  trait CExpr[E <: CExpr[E]] {
    def isValid[S <: Solver[S]](s: S): Boolean
    def isSolved[S <: Solver[S]](s: S): Boolean
    def init[S <: Solver[S]](s: S): S
    def propagate[S <: Solver[S]](s: S): S
    def not: ANYCEXPR

    def selectors: Set[ANYSEL]
    def substitute(s: Map[ANYSEL,ANYSEL]): E
  }

  case class CNotExpr[E <: CExpr[E]](e: E) extends CExpr[CNotExpr[E]] {
    val expr: ANYCEXPR = e.not
    def not = e.asInstanceOf[ANYCEXPR]
    
    def isValid[S <: Solver[S]](s: S) = expr.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = expr.isSolved(s)
    def init[S <: Solver[S]](s: S) = expr.init(s)
    def propagate[S <: Solver[S]](s: S) = expr.propagate(s)
    def selectors = expr.selectors
    def substitute(s: Map[ANYSEL,ANYSEL]) = CNotExpr(e.substitute(s))
  }
  
  case class CAndExpr[L <: CExpr[L],R <: CExpr[R]](left: L,right: R) extends CExpr[CAndExpr[L,R]] {
    def isValid[S <: Solver[S]](s: S) = left.isValid(s) && right.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = left.isSolved(s) && right.isSolved(s)
    def init[S <: Solver[S]](s: S) = right.init(left.init(s))
    def propagate[S <: Solver[S]](s: S) = right.propagate(left.propagate(s))
    def not: ANYCEXPR = COrExpr(left.not,right.not)
    def selectors = left.selectors.union(right.selectors)
    def substitute(s: Map[ANYSEL,ANYSEL]) = CAndExpr(left.substitute(s),right.substitute(s))
  }

  case class COrExpr[L <: CExpr[L],R <: CExpr[R]](left: L,right: R) extends CExpr[COrExpr[L,R]] {
    val rightSub = {
      // We need to substitute all common selectors by a copy so the (AND) propagation doesn't overlap
      val intersect = (left.selectors.intersect(right.selectors))
      val map: Map[ANYSEL,ANYSEL] = intersect.map(x => (x,x.copy)).toMap
      right.substitute(map)
    }
    def isValid[S <: Solver[S]](s: S) = left.isValid(s) || rightSub.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = left.isSolved(s) || rightSub.isSolved(s)
    def init[S <: Solver[S]](s: S) = rightSub.init(left.init(s))
    def propagate[S <: Solver[S]](s: S) = rightSub.propagate(left.propagate(s))
    def not: ANYCEXPR = CAndExpr(left.not,right.not)
    def selectors = left.selectors.union(right.selectors)
    def substitute(s: Map[ANYSEL,ANYSEL]) = COrExpr(left.substitute(s),right.substitute(s))
  }

  trait CBinExpr[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4], E <: CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4,E]]
   extends CExpr[E] {
    def r1: ASEL[X1,X2,A,S1,S2]
    def r2: ASEL[X3,X4,A,S3,S4]
    def cc: Prop[A]

    def selectors = Set[ANYSEL](r1,r2)
    def isValid[S <: Solver[S]](s: S) = s.isValid(r1,r2,cc)
    def isSolved[S <: Solver[S]](s: S) = s.isSolved(r1,r2,cc)
    def init[S <: Solver[S]](s: S) = s.init(r1,r2,cc)
    def propagate[S <: Solver[S]](s: S) = s.propagate(r1,r2,cc)
  }
  
  class CEqual[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val cc: EqualProp[A])
    extends CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4,CEqual[X1,X2,X3,X4,A,S1,S2,S3,S4]] {
    def not = CBinSyntax(r1).=!=(r2)(cc.notEqual).asInstanceOf[ANYCEXPR]
    
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CEqual(rr1,rr2,cc)
    }
  }

  class CNotEqual[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val cc: NotEqualProp[A])
    extends CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4,CNotEqual[X1,X2,X3,X4,A,S1,S2,S3,S4]] {
    def not = CBinSyntax(r1).===(r2)(cc.equal).asInstanceOf[ANYCEXPR]
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CNotEqual(rr1,rr2,cc)
    }
  }

  class CGreaterEqual[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val cc: GreaterEqualProp[A])
    extends CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4,CGreaterEqual[X1,X2,X3,X4,A,S1,S2,S3,S4]] {
    def not = CBinSyntax(r1).<(r2)(cc.lowerEqual,cc.notEqual).asInstanceOf[ANYCEXPR]
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CGreaterEqual(rr1,rr2,cc)
    }
  }

  class CGreater[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val c1: GreaterEqualProp[A], val c2: NotEqualProp[A])
   extends CExpr[CGreater[X1,X2,X3,X4,A,S1,S2,S3,S4]] {

    val expr = CAndExpr(new CGreaterEqual(r1,r2,c1).asInstanceOf[ANYCEXPR],new CNotEqual(r1.copy,r2.copy,c2).asInstanceOf[ANYCEXPR])

    def not = CBinSyntax(r1).<=(r2)(c1.lowerEqual)
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CGreater(rr1,rr2,c1,c2)
    }
    def selectors = Set[ANYSEL](r1,r2)
    def isValid[S <: Solver[S]](s: S) = expr.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = expr.isSolved(s)
    def init[S <: Solver[S]](s: S) = expr.init(s)
    def propagate[S <: Solver[S]](s: S) = expr.propagate(s)
  }

  class CLowerEqual[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val cc: LowerEqualProp[A])
    extends CBinExpr[X1,X2,X3,X4,A,S1,S2,S3,S4,CLowerEqual[X1,X2,X3,X4,A,S1,S2,S3,S4]] {

    def not: ANYCEXPR = CBinSyntax(r1).>(r2)(cc.greaterEqual,cc.notEqual).asInstanceOf[ANYCEXPR]
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CLowerEqual(rr1,rr2,cc)
    }
  }

  class CLower[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
  (val r1: ASEL[X1,X2,A,S1,S2], val r2: ASEL[X3,X4,A,S3,S4], val c1: LowerEqualProp[A], val c2: NotEqualProp[A]) extends
    CExpr[CLower[X1,X2,X3,X4,A,S1,S2,S3,S4]] {

    val expr = CAndExpr(new CLowerEqual(r1,r2,c1).asInstanceOf[ANYCEXPR],new CNotEqual(r1.copy,r2.copy,c2).asInstanceOf[ANYCEXPR])

    def not = CBinSyntax(r1).>=(r2)(c1.greaterEqual)
    def substitute(s: Map[ANYSEL,ANYSEL]) = {
      val rr1 = s.getOrElse(r1,r1).asInstanceOf[ASEL[X1,X2,A,S1,S2]]
      val rr2 = s.getOrElse(r2,r2).asInstanceOf[ASEL[X3,X4,A,S3,S4]]
      new CLower(rr1,rr2,c1,c2)
    }
    def selectors = Set[ANYSEL](r1,r2)
    def isValid[S <: Solver[S]](s: S) = expr.isValid(s)
    def isSolved[S <: Solver[S]](s: S) = expr.isSolved(s)
    def init[S <: Solver[S]](s: S) = expr.init(s)
    def propagate[S <: Solver[S]](s: S) = expr.propagate(s)
  }
  
  implicit class CBinSyntax[X1,@sp X2,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2]](r1: ASEL[X1,X2,A,S1,S2]) {
    def ===[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit cc: EqualProp[A]) = {
      new CEqual(r1,r2,cc)
    }
    def =!=[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit cc: NotEqualProp[A]) = {
      new CNotEqual(r1.copy,r2.copy,cc) // Non-equality needs to be constrained independently (no shared propagation)
    }
    def >=[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit cc: GreaterEqualProp[A])  = {
      new CGreaterEqual(r1,r2,cc)
    }
    def <=[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit cc: LowerEqualProp[A]) = {
      new CLowerEqual(r1,r2,cc)
    }
    def <[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit c1: LowerEqualProp[A], c2: NotEqualProp[A]) = {
      new CLower(r1,r2,c1,c2)
    }
    def >[X3,@sp X4,S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]](r2: ASEL[X3,X4,A,S3,S4])(implicit c1: GreaterEqualProp[A], c2: NotEqualProp[A]) = {
      new CGreater(r1,r2,c1,c2)
    }
  }

  def not[E <: CExpr[E]](e: E) = CNotExpr(e)
  def NOT[E <: CExpr[E]](e: E) = CNotExpr(e)
  def !![E <: CExpr[E]](e: E) = CNotExpr(e)

  implicit class CExprSyntax[L <: CExpr[L]](left: L) {
    def and[R <: CExpr[R]](right: R): CAndExpr[L,R] = CAndExpr[L,R](left,right)
    def or[R <: CExpr[R]](right: R): COrExpr[L,R] = COrExpr[L,R](left,right)

    def AND[R <: CExpr[R]](right: R): CAndExpr[L,R] = CAndExpr[L,R](left,right)
    def OR[R <: CExpr[R]](right: R): COrExpr[L,R] = COrExpr[L,R](left,right)

    def &&[R <: CExpr[R]](right: R): CAndExpr[L,R] = CAndExpr[L,R](left,right)
    def ||[R <: CExpr[R]](right: R): COrExpr[L,R] = COrExpr[L,R](left,right)
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

  var ss: Long = 0
  
  case class DefaultSolver(seqs: Map[ANYSEQ,SeqRange], doms: Map[ANYSEL,PropValue], fixPoint: Boolean) extends DSolver {
    def self: DSolver = this

    def asFixPoint = DefaultSolver(seqs,doms,true)
    def isValid[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]) = {
      val a1 = doms(sel1)
      val a2 = doms(sel2)

      a1.isValid && a2.isValid && cc.isAnyValid(a1,a2)
    }

    def isSolved[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]) = {
      val a1 = doms(sel1)
      val a2 = doms(sel2)

      if (a1.isValid && a2.isValid && cc.isAnySolved(a1,a2)) {
        val r1 = seqs(sel1.asSeq)
        val r2 = seqs(sel2.asSeq)

        val a3 = sel1().approxAnnotationRange(r1.start,r1.end)
        val a4 = sel2().approxAnnotationRange(r2.start,r2.end)

        cc.isAnySolved(a3,a4)
      }
      else false
    }

    def propagate: DSolver = {
      var ndoms = doms
      val iter = doms.iterator

      while(iter.hasNext) {
        val item = iter.next
        val dom = item._1
        val seq = dom.asSeq
        val range = seqs(seq)
        val annSeq = dom()
        val pval = item._2
        if (pval.isValid) {
          val ann = annSeq.approxAnnotationRange(range.start,range.end)
          val eqProp = annSeq.equal
          val annResult = eqProp.propagateAny(ann,pval)._1
          ndoms = ndoms + (dom -> annResult)
        }
      }
      DefaultSolver(seqs,ndoms,fixPoint)
    }

    def init[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): DSolver = {
      val s1 = sel1.asSeq
      val s2 = sel2.asSeq

      val nseqs = seqs + (s1 -> SeqRange(0,s1.size-1)) + (s2 -> SeqRange(0,s2.size-1))
      val ndoms = doms + (sel1 -> sel1().annotation) + (sel2 -> sel2().annotation)

      DefaultSolver(nseqs,ndoms,true)
    }

    def propagate[X1,@sp X2,X3,@sp X4,A <: PVAL,S1 <: Seq[X1,S1],S2 <: ASEQ[X2,A,S2],S3 <: Seq[X3,S3],S4 <: ASEQ[X4,A,S4]]
    (sel1: ASEL[X1,X2,A,S1,S2],sel2: ASEL[X3,X4,A,S3,S4],cc: Prop[A]): DSolver = {
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
      val iterator = seqs.iterator
      var bestCandidate = seqs.last
      
      while (iterator.hasNext) {
        val item = iterator.next
        if (bestCandidate._2.size < item._2.size) {
          bestCandidate = item
        }
      }
      bestCandidate._1
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
        while (!s.fixPoint) {
          s = e.propagate(s.asFixPoint)
        }
        s.solve3(e)
      }
    }

    def solve3[E <: CExpr[E]](e: E): Map[ANYSEQ,LSEQ[NoAnnotation]] = {
      ss = ss + 1

      val (m1,m2) = split
      val mm1 = m1.solve2(e)
      val mm2 = m2.solve2(e)
      mm1.transform((k,v) => v.append(mm2(k)))
    }
  }

  implicit def toConstantSel[X: ClassTag](x: X)(implicit o1: Order[X], ann: StatisticsAnnotator[X])= {
    import Selector._
    createSeq(Array(x)).selectSame
  }

  def createSeq[@sp X: ClassTag](x: Array[X])(implicit o1: Order[X]) = { seqFactory[X].createSeq(x) }
  
  final def main(args: Array[String]): Unit = {
    import Selector._
    import Combiner._

    val c1 = createSeq((0 until 100000).map(x => x.toLong).toArray)
    val c2 = createSeq((0 until 100000).map(x => x.toLong).reverse.toArray)

    
    // table with pairwise columns
    val t1 = c1
    val t2 = c2

    val T1_C1 = t1.selectSame
    val T2_C1 = t2.selectSame

    val p = (T1_C1 === T2_C1) AND (T2_C1 > 10000L) AND (T2_C1 <= 10010L)

    println("START: ")

    val solver = defaultSolver
    
    for (k <- 1 to 10) {
      ss = 0
      val solution = defaultSolver.solve(p)
      println("number of solvers: " + ss)
      println("solution(t1): " + solution(t1))
      println("solution(t2): " + solution(t2))
    }
    

  }
}