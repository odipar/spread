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
  import scala.reflect.runtime.universe._

  type RE = Map[Expr[_], Expr[_]]

  trait Expr[E] {
    def eval: E
    
    def reduce(s: Int, c: Context): (Context, Expr[E]) = (c, this)
    def contains[X](x: X, c: Context): (Context, Boolean) = (c,false)
    def replace[X,EE](r: Replacement[X,EE], c: Context): (Context, Expr[E]) = (c,this)
    
    def children: Set[Expr[_]] = Set()
    def replaceable: Boolean = false
    def stage: Int
    def minStage: Int = stage
    def maxStage: Int = stage

    def apply[X,EE](x: X, to: Expr[EE])(implicit tag: TypeTag[EE]): Expr[E] = Replace(this,Replacement(x,to))
  }

  def asString(a: Any): String = a match {
    case s: String => "\"" + s + "\""
    case c: Char => "\'" + c + "\'"
    case _ => a.toString
  }
  case class Replacement[X,E](x: X, to: Expr[E])(implicit tag: TypeTag[E]) {
    val ttype = tag.tpe
    override def toString = "(" + asString(x) + "," + to + ")"
  }

  trait Context {
    def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E]) = e.reduce(s,this)
    def contains[E,X](e: Expr[E], x: X): (Context,Boolean) = e.contains(x,this)
    def replace[X,EE,E](e: Expr[E], r: Replacement[X,EE], c: Context): (Context, Expr[E]) = e.replace(r,this)
    def resetReplacements: Context
    def merge(o: Context): Context = o  // not yet
  }

  def fullReduce[E](s: Int, c: Context, e: Expr[E]): (Context, Expr[E]) = {
    if (e.stage == 0) (c,e)
    else if (s < e.minStage) (c,e)
    else { val (cc,ee) = c.reduce(s,e) ; fullReduce(s,cc,ee) }
  }

  def red[E](e: Expr[E]): Expr[E] = e
  def %[A,R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = F1(1,f,a)
  def %[A,B,R](f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = F2(1,f,a,b)
  def %[A,B,C,R](f: (Expr[A],Expr[B],Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = F3(1,f,a,b,c)

  def %%[A,R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = F1(2,f,a)
  def %%[A,B,R](f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = F2(2,f,a,b)
  def %%[A,B,C,R](f: (Expr[A],Expr[B],Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = F3(2,f,a,b,c)

  def combine(a: Int, b: Int): Int = { if (a == 0) b ; else if (b == 0) a ; else a min b }

  case class Reduce[E](e: Expr[E]) extends Expr[E] {
    def eval = sys.error("e")
    def stage = e.stage

    override def reduce(s: Int, c: Context): (Context, Expr[E]) = {
      e.reduce(s,c)
    }
  }

  case class Replace[X,E,EE](e: Expr[E],r: Replacement[X,EE]) extends Expr[E] {
    def eval = sys.error("e")
    def stage = 1

    override def reduce(s: Int, c: Context): (Context, Expr[E]) = {
      e.replace(r,c)
    }

    override def toString = e.toString + r
  }
  case class Var[X,E](x: X, e: Expr[E])(implicit tag: TypeTag[E]) extends Expr[E] {
    override def replaceable = true
    override def contains[X](xx: X, c: Context): (Context, Boolean) = {
      if (xx == x) (c,true)
      else c.contains(e,xx)
    }
    override def replace[X,EE](r: Replacement[X,EE], c: Context): (Context, Expr[E]) = {
      val eq =r.x == x

      if (eq && (r.ttype =:= tag.tpe)) {   // fast runtime eq type check
        (c,Var(r.x,r.to.asInstanceOf[Expr[E]]))
      }
      else if (eq && (r.ttype <:< tag.tpe)) {  // slow runtime subtype check
        (c,Var(r.x,r.to.asInstanceOf[Expr[E]]))
      }
      else {
        val (c1,ce: Expr[E],be) = contains_replace(r,e,c)
        if (be) (c1,Var(x, ce))
        else (c1,this)
      }
    }

    override def reduce(s: Int, c: Context) = { val (ca,ea) = fullReduce(s,c,e) ; (ca,Var(x,ea)) }
    override def children = Set(e)
    def eval = e.eval
    def stage = e.stage
    override def minStage = e.minStage
    override def maxStage = e.maxStage
    override val hashCode = jh(x)
    override def toString = "Var(" + asString(x) + "," + e +")"
  }

  def contains_replace[X,EE,E](r: Replacement[X,EE], e: Expr[E], c: Context): (Context,Expr[E],Boolean) = {
    val (c1,b) = c.contains(e,r.x)
    if (!b) (c1,e,false)
    else {
      val (c2,ee) = c1.replace(e,r,c)
      (c2,ee,true)
    }
  }
  case class F1[A, R](stage: Int, f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage == 0") }
    override val replaceable = a.replaceable
    override def children = Set(a)
    override val maxStage = stage max a.maxStage
    override val minStage = { val sa = a.minStage ; if (sa == 0) stage ; else sa }
    override def reduce(s: Int, c: Context) = {
      val (ca,ea) = fullReduce(s,c,a)
      if ((s >= stage) && ((a.stage == 0) && (ea.stage == 0))) (ca,f(ea))
      else (ca,F1(stage,f,ea))
    }
    override def replace[X,EE](r: Replacement[X,EE], c: Context): (Context, Expr[R]) = {
      val (c1,ca,ba) = contains_replace(r,a,c)
      if (ba) (c1,F1(stage,f,ca))
      else (c1,this)
    }
    override def contains[X](x: X, c: Context): (Context, Boolean) = {
      c.contains(a,x)
    }

    override val hashCode = jh(jh(jh(stage) ^ Hashing.jh(f)) + jh(a))
    override def toString = f + "(" + a + ")"
  }

  case class F2[A,B,R](stage: Int, f: (Expr[A],Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage == 0") }
    // TODO: optimize by using 10 bits for minStage,stage and maxStage
    override def children = Set(a,b)
    override val replaceable = a.replaceable || b.replaceable
    override val maxStage = stage max a.maxStage max b.maxStage
    override val minStage: Int = {
      val sa = a.minStage ; val sb = b.minStage
      val cc = combine(sa,sb) ; if (cc == 0) stage ; else cc
    }
    override def reduce(s: Int, c: Context) = {
      val (ca,ea) = fullReduce(s,c,a)
      val (cb,eb) = fullReduce(s,ca,b)

      if ((s >= stage) && ((a.stage == 0) && (ea.stage == 0)) && ((b.stage == 0) && (eb.stage == 0))) (cb,f(ea,eb))
      else (cb,F2(stage,f,ea,eb))
    }
    override def contains[X](x: X, c: Context): (Context, Boolean) = {
      val (c1,ac) = c.contains(a,x)
      val (c2,bc) = c1.contains(b,x)
      (c2,ac | bc)
    }
    override def replace[X,EE](r: Replacement[X,EE], c: Context): (Context, Expr[R]) = {
      val (c1,ca,ba) = contains_replace(r,a,c)
      val (c2,cb,bb) = contains_replace(r,b,c1)

      if (ba || bb) (c2,F2(stage,f,ca,cb))
      else (c2,this)
    }
    override val hashCode = jh(jh(jh(jh(stage) ^ Hashing.jh(f)) - jh(a)) ^ jh(b))
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class F3[A,B,C,R](stage: Int, f: (Expr[A],Expr[B],Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    def eval = { sys.error("eval should only be called on stage == 0") }
    override def children = Set(a,b,c)
    override val replaceable = a.replaceable || b.replaceable || c.replaceable
    override val maxStage = stage max a.maxStage max b.maxStage max c.maxStage
    override val minStage: Int = {
      val sa = a.minStage ; val sb = b.minStage ; val sc = c.minStage
      val cc = combine(combine(sa,sb),sc) ; if (cc == 0) stage ; else cc
    }
    override def reduce(s: Int, mc: Context) = {
      val (ca,ea) = fullReduce(s,mc,a)
      val (cb,eb) = fullReduce(s,ca,b)
      val (cc,ec) = fullReduce(s,cb,c)

      if ((s >= stage) && ((a.stage == 0) && (ea.stage == 0)) && ((b.stage == 0) && (eb.stage == 0))
      && ((c.stage == 0) && (ec.stage == 0)))  (cc,f(ea,eb,ec))
      else (cc,F3(stage,f,ea,eb,ec))
    }
    override def contains[X](x: X, ccc: Context): (Context, Boolean) = {
      val (c1,ac) = ccc.contains(a,x)
      val (c2,bc) = c1.contains(b,x)
      val (c3,cc) = c2.contains(c,x)
      (c3,ac | bc | cc)
    }
    override def replace[X,EE](r: Replacement[X,EE], ccc: Context): (Context, Expr[R]) = {
      val (c1,ca,ba) = contains_replace(r,a,ccc)
      val (c2,cb,bb) = contains_replace(r,b,c1)
      val (c3,cc,bc) = contains_replace(r,c,c2)

      if (ba || bb || bc) (c3,F3(stage,f,ca,cb,cc))
      else (c3,this)
    }
    override val hashCode = jh(jh(jh(jh(jh(stage) ^ Hashing.jh(f)) - jh(a)) ^ jh(b)) + jh(c))
    override def toString = f + "(" + a + "," + c + "," + b + ")"
  }

  case class EExpr[E](eval: E) extends Expr[E] { def stage = 0 ; override def toString = "`" + eval }

  implicit def ei[E](e: E): Expr[E] = EExpr(e)

  // default memoization & replacing context
  case class MemContext(m: Map[(Expr[_],Int),Expr[_]], cn: Map[(Expr[_],_),Boolean], r: Map[(Expr[_], Replacement[_,_]),Expr[_]]) extends Context
  {
    // TODO: optimize - one map per stage
    override def reduce[E](s: Int, e: Expr[E]): (Context,Expr[E]) = {
      val se = (e,s)
      if (m.contains(se)) {
        println("mem reduce: " + e)
        (this,m.get(se).get.asInstanceOf[Expr[E]])
      }
      else {
        val (MemContext(mm,cc,rr),ee: Expr[E]) = e.reduce(s,this)
        (MemContext(mm.updated(se,ee),cc,rr),ee)
      }
    }
    override def contains[E,X](e: Expr[E], x: X): (Context,Boolean) = {
      if (!e.replaceable) (this,false)
      else {
        val se = (e, x)
        if (cn.contains(se)) {
          println("mem contains: " + e)
          (this, cn.get(se).get)
        }
        else {
          val (MemContext(mm, cc, rr), b: Boolean) = e.contains(x, this)
          (MemContext(mm, cc.updated(se, b), rr), b)
        }
      }
    }
    override def replace[X,EE,E](e: Expr[E], rep: Replacement[X,EE], c: Context): (Context, Expr[E]) = {
      if (!e.replaceable) (this,e)
      else {
        val se = (e, rep)
        if (r.contains(se)) {
          println("mem replace: " + e)
          (this, r.get(se).get.asInstanceOf[Expr[E]])
        }
        else {
          val (MemContext(mm, cc, rr), ee: Expr[E]) = e.replace(rep, this)
          (MemContext(mm, cc, rr.updated(se, ee)), ee)
        }
      }
    }
    override def resetReplacements = MemContext(m,cn,Map())
  }

  val emptyContext = MemContext(Map(),Map(),Map())
  def spread[E](e: Expr[E]) = Spread(emptyContext,e)

  case class Spread[E](context: Context, expr: Expr[E]) {
    def reduce(s: Int): Spread[E] = { val (c,e) = context.reduce(s,expr) ; Spread(c,e) }
  }
}