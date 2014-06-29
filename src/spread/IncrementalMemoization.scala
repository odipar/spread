package spread

import spread.Hashing._
import scala.collection.immutable.Map

/*
  SPREAD lib: Incremental Computational Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations and functions
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused memoized functions

  Copyright 2014: Robbert van Dalen
*/


object IncrementalMemoization {
  import scala.language.implicitConversions

  trait Expr[E] {
    def eval: E
    def reduce(s: Int, c: Context): (Context,Expr[E]) = (c,this)

    def stage: Int
    def minStage: Int = stage
    def maxStage: Int = stage
  }

  case class EExpr[E](eval: E) extends Expr[E] {
    def stage = 0
  }

  def ei[E](e: E): Expr[E] = EExpr(e)

  trait Context {
    def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E]) = (this,e)
    def merge(o: Context): Context = o
  }

  def fullReduce[E](s: Int, c: Context, e: Expr[E]): (Context, Expr[E]) = {
    val (cc,ee) = c.reduce(s,e) ; if  (ee == e) (cc,ee) else fullReduce(s,cc,ee)
  }

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = { F1(1,f,a) }
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = { F2(1,f,a,b) }
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = { F3(1,f,a,b,c) }

  def %%[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = { F1(2,f,a) }
  def %%[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = { F2(2,f,a,b) }
  def %%[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = { F3(2,f,a,b,c) }

  def combine(a: Int, b: Int): Int = { if (a == 0) b ; else if (b == 0) a ; else a min b }

  case class F1[A, R](stage: Int, f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage > 0") }
    override val maxStage = stage max a.maxStage
    override val minStage = { val sa = a.minStage ; if (sa == 0) stage ; else sa }
    override def reduce(s: Int, c: Context) = {
      val (ca,ea) = fullReduce(s,c,a)
      if (((a.stage == 0) && (ea.stage == 0))  && (s >= stage)) (ca,f(ea))
      else (ca,F1(stage,f,ea))
    }
    override val hashCode = jh(jh(jh(stage) ^ Hashing.jh(f)) + jh(a))
    override def toString = f + "(" + a + ")"
  }

  case class F2[A,B,R](stage: Int, f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage > 0") }
    // TODO: optimize by using 10 bits for minStage,stage and maxStage
    override val maxStage = stage max a.maxStage max b.maxStage
    override val minStage: Int = {
      val sa = a.minStage ; val sb = b.minStage
      val cc = combine(sa,sb) ; if (cc == 0) stage ; else cc
    }
    override def reduce(s: Int, c: Context) = {
      val (ca,ea) = fullReduce(s,c,a)
      val (cb,eb) = fullReduce(s,ca,b)

      if (((a.stage == 0) && (ea.stage == 0)) && ((b.stage == 0) && (eb.stage == 0)) && (s >= stage)) (cb,f(ea,eb))
      else (cb,F2(stage,f,ea,eb))
    }
    override val hashCode = jh(jh(jh(jh(stage) ^ Hashing.jh(f)) - jh(a)) ^ jh(b))
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class F3[A,B,C,R](stage: Int, f: (Expr[A],Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage > 0") }
    override val maxStage = stage max a.maxStage max b.maxStage max c.maxStage
    override val minStage: Int = {
      val sa = a.minStage ; val sb = b.minStage ; val sc = c.minStage
      val cc = combine(combine(sa,sb),sc) ; if (cc == 0) stage ; else cc
    }
    override def reduce(s: Int, mc: Context) = {
      val (ca,ea) = fullReduce(s,mc,a)
      val (cb,eb) = fullReduce(s,ca,b)
      val (cc,ec) = fullReduce(s,cb,c)

      if (((a.stage == 0) && (ea.stage == 0)) && ((b.stage == 0) &&
           (eb.stage == 0)) && ((c.stage == 0) && (ec.stage == 0)) && (s >= stage)) (cc,f(ea,eb,ec))
      else (cc,F3(stage,f,ea,eb,ec))
    }
    override val hashCode = jh(jh(jh(jh(jh(stage) ^ Hashing.jh(f)) - jh(a)) ^ jh(b)) + jh(c))
    override def toString = f + "(" + a + "," + c + "," + b + ")"
  }

  case class MemContext(m: Map[(Int,Expr[_]),Expr[_]]) extends Context
  {
    // TODO: optimize - one map per stage
    override def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E]) = {
      val se = (s,e)
      if (m.contains(se)) (this,m.get(se).get.asInstanceOf[Expr[E]])
      else {
        val (MemContext(mm),ee: Expr[E]) = e.reduce(s,this)
        (MemContext(mm.updated(se,ee)),ee)
      }
    }
  }

  // default memoization context
  def reduce[E](s: Int, e: Expr[E]) = ExprContext(MemContext(Map()),e).reduce(s)

  case class ExprContext[E](context: Context, expr: Expr[E]) {
    def reduce(s: Int): ExprContext[E] = { val (c,e) = context.reduce(s,expr) ; ExprContext(c,e) }
  }
}