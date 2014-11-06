package spread

import spread.Hashing._
import spread.IncrementalMemoization.FA1
import scala.collection.immutable.Map

/*
  SPREAD lib: Incremental Computational Reuse

  Features:

  1) spreadsheet-like incremental computation
  2) reuse of expensive (sub)computations and functions
  3) full traceability
  4) purely functional
  5) automatic garbage collection of unused hashed-consed functions

  - Requires scala 2.11 and higher (usage of TypeTags)

  Copyright 2014: Robbert van Dalen
*/

object IncrementalMemoization {

  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference

  import Hashing._
  import scala.reflect.runtime.universe.TypeTag

  trait Context {
    def mput(e: Trace[_]): Context
    def mget[X](e: Expr[X]): Option[Trace[X]]
    def mref(parent: Expr[_], child: Expr[_]): Context
  }

  trait Expr[V] {
    def containsQuote: Boolean
    def containsBinding: Boolean

    def unary_! : Expr[V] = hcons(Quote(this))
  }

  trait LazyExpr[V] extends Expr[V]

  trait Trace[V] extends Expr[V] {
    val containsQuote = to.containsQuote
    val containsBinding = to.containsBinding

    def from: Expr[V]
    def to: Expr[V]

    override def toString = {
      val (df,f) = getFrom(0,from)
      val (dt,t) = getTo(0,to)
      var r = "[" + f

      var s = df
      var i = 0
      while (i < s) {
        r = r + " . "
        i = i + 1
      }
      r = r + " ~> "
      s = dt
      i = 0
      while (i < s) {
        r = r + " . "
        i = i + 1
      }

      r = r  + t.toString + "]"
      r
    }

    override val hashCode = jh(jh(jh(from)) ^ jh(to))
  }

  case class Trace0[V](from: Expr[V], to: F0[V]) extends Trace[V] with F0[V] {
    def evalValue = to.evalValue
   // override def toString = "[" + from + " ~> " + getTo(to) + "]"
  }

  case class Trace1[V](from: Expr[V], to: Expr[V]) extends Trace[V] with LazyExpr[V] {
    //override def toString = ".. -> " + to
    //override def toString = "[" + from + " => " + getTo(to) + "]"
  }

  def getFrom[V](d: Int, e: Expr[V]): (Int,Expr[V]) = e match {
    case t: Trace[V] => getFrom(d+1,t.from)
    case _ => (d,e)
  }

  def getTo[V](d: Int, e: Expr[V]): (Int,Expr[V]) = e match {
    case t: Trace[V] => getTo(d+1,t.to)
    case _ => (d,e)
  }

  def trace[V](from: Expr[V], to: Expr[V]): Trace[V] = {
    to match {
      case f0: F0[V] => Trace0(from,f0)
      case _ => Trace1(from,to)
    }
  }

  case class Quote[V](expr: Expr[V]) extends Expr[V] {
    def containsQuote = true
    val containsBinding = expr.containsBinding

    override def toString = "!" + expr
  }

  case class Var[X](a: Any, e: Expr[X]) extends Expr[X] {
    def containsQuote = e.containsQuote
    def containsBinding = true

    override def toString = a + "~" + e
  }

  val vt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def hcons[V](c: Expr[V]): Expr[V] = {
    vt.get(c) match {
      case None => hcons2(c)
      case Some(x) => {
        val xg = x.get
        if (xg != null) {
          xg.asInstanceOf[Expr[V]]
        }
        else hcons2(c)
      }
    }
  }

  def hcons2[V](c: Expr[V]): Expr[V] = { vt.put(c, new WeakReference(c)) ; c }

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = hcons(FF1(f, a))
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = hcons(FF2(f, a, b))
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    hcons(FF3(f, a, b, c))
  }

  def evalArg[A](c: Context, e: Expr[A]): (Context,Expr[A]) = e match {
    case t0: Trace[A] => (c,getTo(0,t0)._2)
    case _ => fullRed(c,e)
  }

  def fullRed[A](c: Context, e: Expr[A]): (Context,Expr[A]) = {
    val (nc,ee) = eval(c,e)
    if (ee == e) (nc,ee)
    else fullRed(nc,ee)
  }

  case class ExprImpl[X](evalValue: X) extends F0[X] {
    def containsQuote = false
    def containsBinding = false
    override def toString = "`" + evalValue.toString
  }

  def ei[X](x: X): Expr[X] = ExprImpl(x)

  trait F0[R] extends Expr[R] {
    def evalValue: R
    def unary_~ : R = evalValue
  }

  trait F1[A, R] extends LazyExpr[R] {
    def f: Expr[A] => Expr[R]
    def a: Expr[A]

    override val hashCode = jh(jh(f) ^ jh(a))
  }

  trait F2[A, B, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]

    override val hashCode = jh(jh(jh(f) ^ jh(a)) + jh(b))
  }

  trait F3[A, B, C, R] extends LazyExpr[R] {
    def f: (Expr[A], Expr[B], Expr[C]) => Expr[R]
    def a: Expr[A]
    def b: Expr[B]
    def c: Expr[C]

    override val hashCode = jh(jh(jh(jh(f) ^ jh(a)) + jh(b)) - jh(c))
  }

  type F_ = F0[_]

  case class FF1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends F1[A, R] {
    val containsQuote = a.containsQuote
    val containsBinding = a.containsBinding

    override def toString = f + "(" + a + ")"
  }

  trait Infix

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends F2[A, B, R] {
    val containsQuote = a.containsQuote || b.containsQuote
    val containsBinding = a.containsBinding || b.containsBinding

    override def toString = {
      f match {
        case i: Infix => "(" + a + " " + f + " " + b + ")"
        case _ => f + "(" + a + "," + b + ")"
      }
    }
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends F3[A, B, C, R] {
    val containsQuote = a.containsQuote || b.containsQuote || c.containsQuote
    val containsBinding = a.containsBinding || b.containsBinding || c.containsBinding

    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }

  trait Func { override def toString = getClass.getSimpleName }

  trait FA1[A, X] extends (Expr[A] => Expr[X]) with Func {
    def apply(a: Expr[A]): Expr[X] = a match {
      case (af: F0[A]) => apply(af)
      case _ => %(this, a)
    }
    def apply(a: F0[A]): Expr[X]
  }

  trait FA2[A, B, X] extends ((Expr[A], Expr[B]) => Expr[X]) with Func {
    def apply(a: Expr[A], b: Expr[B]): Expr[X] = (a, b) match {
      case (af: F0[A], bf: F0[B]) => apply(af, bf)
      case _ => %(this, a, b)
    }
    def apply(a: F0[A], b: F0[B]): Expr[X]
  }

  trait FA3[A, B, C, X] extends ((Expr[A], Expr[B], Expr[C]) => Expr[X]) with Func {
    def apply(a: Expr[A], b: Expr[B], c: Expr[C]): Expr[X] = (a, b, c) match {
      case (af: F0[A], bf: F0[B], cf: F0[C]) => apply(af, bf, cf)
      case _ => %(this, a, b, c)
    }
    def apply(a: F0[A], b: F0[B], c: F0[C]): Expr[X]
  }


  def cc = CC(Map(),Map())
  def cm = CM(Map())

  object NC extends Context {
    def mref(parent: Expr[_], child: Expr[_]) = this
    def mput(e: Trace[_])= this
    def mget[X](e: Expr[X]) = None
  }

  case class CC(h: Map[Expr[_],Trace[_]], b: Map[Expr[_],Set[Expr[_]]]) extends Context {
    def mref(parent: Expr[_], child: Expr[_]) = {
      if (child.containsBinding) {
        if (b.contains(child)) {
          val s = b.get(child).get
          CC(h,b + (child -> (s + parent)))
        }
        else {
          val CC(_,b1) = backref(this,child)
          CC(h,b1 + (child -> Set(parent)))
        }
      }
      else this
    }

    def mput(e: Trace[_]) = {
      if (!h.contains(e.from)) CC(h + (e.from -> e) ,b)
      else this
    }

    def mget[X](e: Expr[X]) = {
     h.get(e).asInstanceOf[Option[Trace[X]]]
    }

    override def toString = {
      var s = b.size + "\n"
      for (i <- b) {
        s = s + i  + "\n"
      }
      s
    }
  }

  case class CM(b: Map[Expr[_],Set[Expr[_]]]) extends Context {
    val h = new WeakHashMap[Expr[_],WeakReference[Trace[_]]]
    var c = 0

    def mref(parent: Expr[_], child: Expr[_]) = {
      if (child.containsBinding) {
        if (b.contains(child)) {
          val s = b.get(child).get
          CM(b + (child -> (s + parent)))
        }
        else {
          val CM(b1) = backref(this,child)
          CM(b1 + (child -> Set(parent)))
        }
      }
      else this
    }

    def mput(e: Trace[_]) = {
      if (!h.contains(e.from)) h.put(e.from,new WeakReference(e))
      this
    }
    def contains(e: Expr[_]) = h.contains(e)
    def mget[X](e: Expr[X]) = {
      h.get(e) match {
        case None => None
        case Some(x) => {
          val wr: Trace[_] = x.get
          if (wr == null) None
          else Some(wr.asInstanceOf[Trace[X]])
        }
      }
    }


    /*override def toString = {
      var s = "size: " + h.size  +"\n"
      /*for (i <- h.keys) {
        s = s + i  + " => " + h.get(i).get.get + "\n"
      }
      s + "final size: " + h.size + "\n"
        */
      s
    } */
  }

  def backref[X](c: Context, e: Expr[X]): Context = {
    e match {
      case v: Var[X] => backref(c,v.e)
      case t: Trace[X] => backref(c,t.to)
      case f1: F1[_,X] => c.mref(e,f1.a)
      case f2: F2[_,_,X] => c.mref(e,f2.a).mref(e,f2.b)
      case f3: F3[_,_,_,X] => cc.mref(e,f3.a).mref(e,f3.b).mref(e,f3.c)
      case _ => c
    }
  }

  def removeQuotes[X](e: Expr[X]): Expr[X] = {
    if (e.containsQuote) %(removeQuotes1(),e)
    else e
  }

  case class removeQuotes1[X]() extends (Expr[X] => Expr[X]) with Func {
    def apply(e: Expr[X]) = removeQuotes2(e)

    override def toString = "removeQuotes"
  }

  def removeQuotes2[X](e: Expr[X]): Expr[X] = {
    if (!e.containsQuote) e
    else e match {
      case t: Trace[X] => removeQuotes2(t.to)
      case f1: F1[_,X] => %(f1.f,removeQuotes2(f1.a))
      case f2: F2[_,_,X] => %(f2.f,removeQuotes2(f2.a),removeQuotes2(f2.b))
      case f3: F3[_,_,_,X] => %(f3.f,removeQuotes2(f3.a),removeQuotes2(f3.b),removeQuotes2(f3.c))
      case q: Quote[X] => q.expr
      case _ => e
    }
  }

  def memoize[X](c: Context, e: Expr[X]): Context = {
    e match {
      case t: Trace[X] => {
        if (c.mget(t.from) != None) c
        else memoize(memoize(c.mput(t),t.from),t.to)
      }
      case f1: F1[_,X] =>  memoize(c,f1.a)
      case f2: F2[_,_,X] => memoize(memoize(c,f2.a),f2.b)
      case f3: F3[_,_,_,X] => memoize(memoize(memoize(c,f3.a),f3.b),f3.c)
      case _ => c
    }
  }

  def eval[X](c: Context, e: Expr[X]): (Context,Expr[X]) = {
    e match {
      case v: Var[X] => {
        (c,trace(v,v.e))
      }
      case le: LazyExpr[X] => {
        val m = c.mget(e)
        if (m == None) {
          val (cc,ee) = tracedEval(c,e)
          if (e == ee) (cc,e)
          else {
            val t = trace(e,ee)
            // memoize
            (cc.mput(t),t)
          }
        }
        else {
          //println("HIT: " + e)
          (c,m.get)
        }
      }
      case _ => (c,e)
    }
  }

  def tracedEval[X](c: Context, e: Expr[X]): (Context,Expr[X]) = {
    e match {
      case t1: Trace1[X] => {
        val (c1,te) = eval(c,t1.to)
        if (te == t1.to) (c1,t1)
        else (c1,te)
      }
      case f1: F1[_,X] => {
        val (c1: Context,aa) = evalArg(c,f1.a)

        if (f1.a == aa) (c1,f1.f(aa))
        else (c1,%(f1.f,aa))
      }
      case f2: F2[_,_,X] => {
        val (ca: Context,aa) = evalArg(c,f2.a)
        val (cb: Context,bb) = evalArg(ca,f2.b)

        if ((f2.a == aa) && (f2.b == bb)) (cb,f2.f(aa,bb))
        else (cb,%(f2.f,aa,bb))
      }
      case f3: F3[_,_,_,X] => {
        val (ca: Context,aa) = evalArg(c,f3.a)
        val (cb: Context,bb) = evalArg(ca,f3.b)
        val (ccc: Context,cc) = evalArg(cb,f3.c)

        if ((f3.a == aa) && (f3.b == bb) && (f3.c == cc)) (ccc,f3.f(aa,bb,cc))
        else (ccc,%(f3.f,aa,bb,cc))
      }
      case _ => (c,e)
    }
  }

  case class Temp(x: Any) {
    def ~ [X](e: Expr[X]) : Expr[X] = Var(x,e)
  }

  implicit def toTemp(x: Any): Temp = Temp(x)
}