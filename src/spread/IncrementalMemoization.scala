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
  5) automatic garbage collection of unused hashed-consed functions

  - Requires scala 2.11 and higher (usage of TypeTags)

  Copyright 2014: Robbert van Dalen
*/

/*
 Ideas:
  - (destructively) eraseTrail([((1 ++ 2) ** (3 ++ 4)) ~> [([(1 ++ 2) ~> 3] ** [(3 ++ 4) ~> 7]) ~> [(3 ** 7) ~> 21]]]) =>
          [((1 ++ 2) ** (3 ++ 4)) ~> 21]
  -

 */
object IncrementalMemoization {

  import scala.language.implicitConversions
  import scala.collection.mutable.WeakHashMap
  import java.lang.ref.WeakReference
  import Hashing._
  import scala.reflect.runtime.universe.TypeTag

  trait Expr[V] {
    def eval: Expr[V]
    def contains[X](x: X): Expr[Boolean]
    def set[X,O: TypeTag](x: X, o: Expr[O]): Expr[V]
    def unquote: Expr[V]
    def containsQuotes: Boolean

    def eraseTrail: Expr[V] = this

    def eraseQuotes: Expr[V] = Unquote(this,unquote)
    def replace[X,O: TypeTag](x: X, o: Expr[O]): Expr[V] = hcons(ContainsReplace(x,o,contains(x),this,set(x,o)))
    def \ : Expr[V] = hcons(Quote(this))

    def ^ : Expr[V] = eraseQuotes
    def @@[X,O: TypeTag](x: X, o: Expr[O]): Expr[V] = replace(x,o)
    def ?[X](x: X): Expr[Boolean] = contains(x)
  }

  trait LazyExpr[V] extends Expr[V] {
    def contains[X](x: X): Expr[Boolean] = fullRed(hcons(Contains(this, x)))
    def tracedContains[X](x: X): Expr[Boolean]

    def eval: Expr[V] = rt.get(this) match {
      case None => eval2
      case Some(x) => {
        val xg = x.get
        if (xg != null) xg.asInstanceOf[Expr[V]]
        else eval2
      }
    }
    def eval2: Expr[V] = {
      val r = tracedEval
      if (r == this) this
      else {
        val tr = trace(this, r)
        rt.put(this, new WeakReference(tr))
        return tr
      }
    }
    def tracedEval: Expr[V]
  }

  case class Unquote[V](from: Expr[V], to: Expr[V]) extends Expr[V] {
    def containsQuotes = to.containsQuotes
    def unquote = to.unquote

    def eval = this
    def contains[X](x: X) = to.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]): Expr[V] = to.set(x,e)
    override def toString = "(" + from + ".^ => " +  to + ")"
  }

  trait Trace[V] extends Expr[V] {
    def containsQuotes = to.containsQuotes
    def unquote = if (containsQuotes) to.unquote ; else this

    def from: Expr[V]
    def to: Expr[V]

    override def eraseTrail: Expr[V] = trace(getFrom(from),getTo(to))

    override val hashCode = jh(jh(jh(from)) ^ jh(to))
  }

  case class Trace0[V](from: Expr[V], to: F0[V]) extends Trace[V] with F0[V] {
    def value = to.value
    def set[X,O: TypeTag](x: X, e: Expr[O]): Expr[V] = to.set(x,e)
    def contains[X](x: X) = to.contains(x)
    override def toString = "[" + getFrom(from) + " ~> " + to + "]"
  }

  case class Trace1[V](from: Expr[V], to: Expr[V]) extends Trace[V] with LazyExpr[V] {
    def tracedEval = {
      val te = to.eval
      if (te == to) this
      else te
    }
    def tracedContains[X](x: X) = to.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]): Expr[V] = to.set(x,e)
    override def toString = "[" + getFrom(from) + " => " + to + "]"
  }

  def getFrom[V](e: Expr[V]): Expr[V] = e match {
    case t: Trace[V] => getFrom(t.from)
    case _ => e
  }

  def getTo[V](e: Expr[V]): Expr[V] = e match {
    case t: Trace[V] => getTo(t.to)
    case _ => e
  }

  def trace[V](from: Expr[V], to: Expr[V]): Expr[V] = {
    hcons(to match {
      case f0: F0[V] => Trace0(from,f0)
      case _ => Trace1(from,to)
    })
  }

  case class ContainsReplace[X,V,O](x: X, o: Expr[O], contains: Expr[Boolean], from: Expr[V], to: Expr[V]) extends LazyExpr[V] {
    def containsQuotes = to.containsQuotes
    def unquote = if (containsQuotes) to.unquote ; else this

    def tracedEval = to.eval
    def tracedContains[X](x: X): Expr[Boolean] = to.contains(x)
    def set[X,O: TypeTag](x: X, o: Expr[O]): Expr[V] = to.set(x,o)
    override def toString = from + " @@ (" + x + "," + o + ")  => " + to
  }

  case class Contains[V,X](expr: LazyExpr[V], x: X) extends LazyExpr[Boolean] {
    def containsQuotes = false
    def unquote = this

    def tracedEval = expr.tracedContains(x)
    def tracedContains[XX](x: XX) = this
    def set[X,O: TypeTag](x: X, e: Expr[O]) = this
    override def toString = expr + " ? " + x
  }

  case class Quote[V](expr: Expr[V]) extends Expr[V] {
    def containsQuotes = true
    def unquote = expr

    def eval = this
    def contains[X](x: X) = expr.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]): Expr[V] = Quote(expr.set(x,e))
    override def toString = expr +".\\"
  }

  case class Var[X,V](label: X, expr: Expr[V])(implicit vt: TypeTag[V]) extends LazyExpr[V] {
    def containsQuotes = expr.containsQuotes
    def unquote = if (containsQuotes) Var(label,expr.unquote) ; else this

    def tracedEval = expr
    def tracedContains[X](x: X) = {
      if (label == x) BTrue ; else expr.contains(x)
    }
    def set[X,O](x: X, e: Expr[O])(implicit ot: TypeTag[O]): Expr[V] = {
      def evt = vt.tpe
      def ovt = ot.tpe
      if ((label == x) && (evt <:< ovt)) Var(label,e).asInstanceOf[Expr[V]]
      else Var(label,expr.set(x,e))
    }
    override def toString = label + "~" + expr
  }

  val vt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def hcons[V](c: Expr[V]): Expr[V] = {
    vt.get(c) match {
      case None => hcons2(c)
      case Some(x) => {
        val xg = x.get
        if (xg != null) xg.asInstanceOf[Expr[V]]
        else hcons2(c)
      }
    }
  }

  def hcons2[V](c: Expr[V]): Expr[V] = { vt.put(c, new WeakReference(c)) ; c }

  val rt = new WeakHashMap[Expr[_], WeakReference[Expr[_]]]

  def %[A, R](f: Expr[A] => Expr[R], a: Expr[A]): Expr[R] = hcons(FF1(f, a))
  def %[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]): Expr[R] = hcons(FF2(f, a, b))
  def %[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[R] = {
    hcons(FF3(f, a, b, c))
  }

  def evalArg[A](e: Expr[A]): Expr[A] = e match {
    case t0: Trace[A] => getTo(t0)
    case _ => fullRed(e)
  }

  def fullRed[A](e: Expr[A]): Expr[A] = {
    val ee = e.eval
    if (ee == e) ee
    else fullRed(ee)
  }

  case class ExprImpl[X](value: X) extends F0[X] {
    def containsQuotes = false
    def unquote = this
    def tracedEval = this
    def set[X,O: TypeTag](x: X, e: Expr[O]) = this
    def contains[X](x: X) = BFalse
    def normalize = this
    override def toString = "`" + value.toString
  }

  def ei[X](x: X): Expr[X] = hcons(ExprImpl(x))

  trait F0[R] extends Expr[R] {
    def eval = this
    def value: R
    def unary_! : R = value
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
    val containsQuotes = a.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote) ; else this

    def tracedEval = {
      val aa = evalArg(a)

      if (a == aa) {
        aa match {
          case fa: F0[A] => f(aa)
          case _ => this
        }
      }
      else %(f,aa)
    }

    def tracedContains[X](x: X) = a.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      if ((aa == a)) this
      else %(f,aa)
    }
    override def toString = f + "(" + a + ")"
  }

  def containsReplace[V,X,O: TypeTag](e: Expr[V], x: X, o: Expr[O]): Expr[V] = {
    e.contains(x) match {
      case f: F0[Boolean] => {
        val b = f.value
        if (b) e.set(x,o)
        else e
      }
      case _ => sys.error("illegal system state")
    }
  }

  case class FF2[A, B, R](f: (Expr[A], Expr[B]) => Expr[R], a: Expr[A], b: Expr[B]) extends F2[A, B, R] {
    val containsQuotes = a.containsQuotes || b.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote,b.unquote) ; else this

    def tracedEval = {
      val aa = evalArg(a)
      val bb = evalArg(b)

      if ((a == aa) && (b == bb)) {
        (aa,bb) match {
          case (fa: F0[A], fb: F0[B]) => f(aa,bb)
          case _ => this
        }
      }
      else %(f,aa,bb)
    }

    def tracedContains[X](x: X) = a.contains(x) ||| b.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      val bb = containsReplace(b,x,e)

      if ((aa == a) && (bb == b)) this
      else %(f,aa,bb)
    }
    override def toString = "(" + a + " " + f + " " + b + ")"
  }

  case class FF3[A, B, C, R](f: (Expr[A], Expr[B], Expr[C]) => Expr[R], a: Expr[A], b: Expr[B], c: Expr[C]) extends F3[A, B, C, R] {
    val containsQuotes = a.containsQuotes || b.containsQuotes || c.containsQuotes
    def unquote = if (containsQuotes) %(f,a.unquote,b.unquote,c.unquote) ; else this

    def tracedEval = {
      val aa = evalArg(a)
      val bb = evalArg(b)
      val cc = evalArg(c)

      if ((a == aa) && (b == bb) && (c == cc)) {
        (aa,bb,cc) match {
          case (fa: F0[A], fb: F0[B], fc: F0[C]) => (f,aa,bb,cc)
          case _ => this
        }
      }
      %(f,aa,bb,cc)
    }

    def tracedContains[X](x: X) = a.contains(x) ||| b.contains(x) ||| c.contains(x)
    def set[X,O: TypeTag](x: X, e: Expr[O]) = {
      val aa = containsReplace(a,x,e)
      val bb = containsReplace(b,x,e)
      val cc = containsReplace(c,x,e)

      if ((aa == a) && (bb == b) && (cc == c)) this
      else %(f,aa,bb,cc)
    }
    override def toString = f + "(" + a + "," + b + "," + c + ")"
  }

  trait FA1[A, C] extends (Expr[A] => Expr[C]) {
    def apply(a: Expr[A]): Expr[C] = a match {
      case (af: F0[A]) => apply(af)
      case _ => %(this, a)
    }
    def apply(a: F0[A]): Expr[C]
  }

  trait FA2[A, B, C] extends ((Expr[A], Expr[B]) => Expr[C]) {
    def apply(a: Expr[A], b: Expr[B]): Expr[C] = (a, b) match {
      case (af: F0[A], bf: F0[B]) => apply(af, bf)
      case _ => %(this, a, b)
    }
    def apply(a: F0[A], b: F0[B]): Expr[C]
  }


  implicit def toL[X,V](x: X): L[X] = L(x)

  case class L[X](x: X) {
    def ~[V: TypeTag](e: Expr[V]): Expr[V] = Var(x,e)
  }

  type B = Expr[Boolean]

  trait BExpr extends Expr[Boolean] {
    def origin: B
    def |||(o: B): B = %(or, origin, o)
    def &&&(o: B): B = %(and, origin, o)
  }

  type F0B = F0[Boolean]

  trait BB extends BExpr with F0B {
    def containsQuotes = false
    def unquote = this

    def contains[X](x: X) = BFalse
    def set[X,O: TypeTag](x: X, e: Expr[O]) = this
    def origin = this
  }

  case object BTrue extends BB {
    def value = true
    override def toString = "true"
  }

  case object BFalse extends BB {
    def value = false
    override def toString = "false"
  }

  implicit def toB(b: Boolean) = if (b) BTrue ; else BFalse

  private case class BWrap(origin: B) extends BExpr {
    def containsQuotes = error

    def contains[X](x: X) = error
    def set[X,O: TypeTag](x: X, e: Expr[O]) = error
    def eval = error
    def unquote = error
    def error = sys.error("BWrap should not be used directly")
  }

  implicit def toBWrap(b: B): BExpr = b match {
    case bb: BExpr => bb
    case _ => BWrap(b)
  }

  object or extends FA2[Boolean,Boolean,Boolean] {
    def apply(a: F0B, b: F0B) = a.value || b.value
    override def toString = "|||"
  }

  object and extends FA2[Boolean,Boolean,Boolean] {
    def apply(a: F0B, b: F0B) = a.value && b.value
    override def toString = "&&&"
  }

  case class WB(i: Boolean) {
    def unary_! = toB(i)
  }

  implicit def toWB(i: Boolean): WB = WB(i)
}