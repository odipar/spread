package spread
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
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference

  trait Expr[V] {
    def eval: V
    def finish: Expr[V] = this
    def reduce: Expr[V] = this
    def trace: Expr[V] = Trace(this,fullRed(this))
    def replace[X](v: Expr[X], w: Expr[X]): Expr[V] = {
      if (this == v) w.asInstanceOf[Expr[V]]
      else this
    }
    def children: Vector[Expr[_]] = empty
    def hasDependencies: Boolean = false

    def $ : Expr[V] = labelExpr(this)
    def unary_! : V = eval
  }

  def labelExpr[V](e: Expr[V]): LabeledExpr[V] = e match {
    case LabeledExpr(l) => LabeledExpr(l)
    case _ => LabeledExpr(e)
  }

  case class LabeledExpr[V](expr: Expr[V]) extends Expr[V] {
    def eval = expr.eval
    override def finish = LabeledExpr(expr.finish)
    override def reduce = LabeledExpr(expr.reduce)
    override def children = expr.children
    override def hasDependencies = true
    override def toString = expr.toString + "'"
  }

  case class Trace[V](from: Expr[V], to: Expr[V]) extends Expr[V] {
    def eval = to.eval
    override def reduce = to
    override def finish = to
    override def children = Vector(from,to)
    override def hasDependencies = from.hasDependencies
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[V] = {
      val aa = replaceExpr(from, v, w)
      if (aa == from) this; else Trace(aa, fullRed(aa))
    }
    override def toString = from.toString + " => " + to.toString
  }

  val empty: Vector[Expr[_]] = Vector()

  val vt = WeakHashMap.empty[Expr[_], WeakReference[Expr[_]]]
  def mem[V](c: Expr[V]): Expr[V] = {
    if (!vt.contains(c)) vt.put(c, new WeakReference(c))
    vt.get(c).get.get.asInstanceOf[Expr[V]]
  }

  val wp = WeakHashMap.empty[Expr[_], WeakHashMap[Expr[_],WeakReference[Expr[_]]]]
  def addParent(child: Expr[_], parent: Expr[_]) = {
    if (child.hasDependencies) {
      val m = {
        if (!wp.contains(child)) WeakHashMap.empty[Expr[_], WeakReference[Expr[_]]]
        else wp.get(child).get
      }
      m.put(parent, new WeakReference(parent))
      wp.put(child, m)
    }
  }

  def getParents(child: Expr[_]): WeakHashMap[Expr[_],WeakReference[Expr[_]]] = {
    if(!wp.contains(child)) WeakHashMap.empty[Expr[_], WeakReference[Expr[_]]]
    else wp.get(child).get
  }

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = {
    val p = mem(F1(f, a)) ; addParent(a,p) ; p }
  def %%[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = {
    val p = mem(FF1(f, a)) ; addParent(a,p) ; p }
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = {
    val p = mem(F2(f, a, b)) ; addParent(a,p) ; addParent(b,p) ; p
  }
  def %%[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] =  {
    val p = mem(FF2(f, a, b)) ; addParent(a,p) ; addParent(b,p) ; p
  }
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    val p = mem(F3(f, a, b, c)) ; addParent(a,p) ; addParent(b,p) ; addParent(c,p) ; p
  }
  def %%[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    val p = mem(FF3(f, a, b, c)) ; ; addParent(a,p) ; addParent(b,p) ; addParent(c,p) ; p
  }

  def replaceExpr[V,X](e: Expr[V], v: Expr[X], w: Expr[X]): Expr[V] = if (e.hasDependencies) e.replace(v,w) ; else e

  def fullRed[A](e: Expr[A]): Expr[A] = {
    var ee = e
    while (ee.reduce != ee) { ee = ee.reduce }
    ee
  }

  case class ExprImpl[X](x: X) extends Expr[X] {
    def eval = x
    override def toString = "`" + x.toString
  }

  def ei[X](x: X): Expr[X] = mem(ExprImpl(x))

  case class F1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      if (aa == a) mem(f(a))
      else %(f, aa)
    }
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      if (aa == a) this ; else F1(f,aa)
    }
    override lazy val eval = reduce.eval
    override def children = Vector(a)
    override def toString = f + "(" + a + ")"
    override val hasDependencies = a.hasDependencies
  }

  case class FF1[A, R](f: Expr[A] => Expr[R], a: Expr[A]) extends Expr[R] {
    override lazy val finish = F1(f, a.finish)
    override lazy val eval = f(a).eval
    override def children = Vector(a)
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      if (aa == a) this ; else FF1(f,aa)
    }
    override def toString = f + "(" + a + ")"
    override val hasDependencies = a.hasDependencies
  }

  case class F2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)

      if ((aa == a) && (bb == b)) mem(f(a, b))
      else %(f, aa, bb)
    }
    override lazy val eval = reduce.eval
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      val bb = replaceExpr(b,v,w)
      if ((aa == a) && (bb == b)) this ; else F2(f,aa,bb)
    }
    override def children = Vector(a,b)
    override def toString = "(" + a + " " + f + " " + b + ")"
    override val hasDependencies = a.hasDependencies || b.hasDependencies
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends Expr[R] {
    override lazy val finish = F2(f, a.finish, b.finish)
    override lazy val eval = f(a, b).eval
    override def children = Vector(a,b)
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      val bb = replaceExpr(b,v,w)
      if ((aa == a) && (bb == b)) this ; else FF2(f,aa,bb)
    }
    override def toString = "(" + a + " " + f + " " + b + ")"
    override val hasDependencies = a.hasDependencies || b.hasDependencies
  }

  case class F3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    override lazy val reduce = {
      val aa = fullRed(a)
      val bb = fullRed(b)
      val cc = fullRed(c)

      if ((aa == a) && (bb == b) && (cc == c)) mem(f(a, b, c))
      else %(f, aa, bb, cc)
    }
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      val bb = replaceExpr(b,v,w)
      val cc = replaceExpr(c,v,w)
      if ((aa == a) && (bb == b) && (bb == b)) this ; else F3(f,aa,bb,cc)
    }
    override lazy val eval = reduce.eval
    override def children = Vector(a,b,c)
    override def toString = f + "[" + a + "," + b + "," + c + "]"
    override val hasDependencies = a.hasDependencies || b.hasDependencies || c.hasDependencies
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends Expr[R] {
    override lazy val finish = F3(f, a.finish, b.finish, c.finish)
    override lazy val eval = f(a, b, c).eval
    override def replace[X](v: Expr[X], w: Expr[X]): Expr[R] = {
      val aa = replaceExpr(a,v,w)
      val bb = replaceExpr(b,v,w)
      val cc = replaceExpr(c,v,w)
      if ((aa == a) && (bb == b) && (bb == b)) this ; else FF3(f,aa,bb,cc)
    }

    override def children = Vector(a,b,c)
    override def toString = f + "(" + a + "," + b + "," + c + ")"
    override val hasDependencies = a.hasDependencies || b.hasDependencies || c.hasDependencies
  }
}