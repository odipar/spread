package spread

object Test {
  import Types._
  //import OrderedSetImplementation._
  import OrderedTreapSet._
  import Hashing._

  //implicit def intToIExpr[B](i: Int): IntExpr[B] = IExpr[B](i)

  /*case class BindingOrdering[A,B](label: Ordering[B]) extends Ordering[Binding[A,B]] {
    def compare(m1: Binding[A,B], m2: Binding[A,B]): Int = label.compare(m1.label,m2.label)
  }

  case class BindingHasher[A,B](labeled: PriorityHasher[A], label: PriorityHasher[B]) extends Ordering[Binding[A,B]] {
    def hash(b: Binding[A,B]): Int = Hashing.jenkinsHash(labeled.hash(b.labeled) ^ label.hash(b.label))
  } */

  //implicit def mordering[A,B](implicit d: Ordering[B]): Ordering[Binding[A,B]] = BindingOrdering(d)
  //implicit def mhasher[A,B](implicit labeled: PriorityHasher[B], label: PriorityHasher[B]): PriorityHasher[Binding[A,B]] = BindingHasher(d)

  //type ST[X] = OrderedISetImpl[X, Any, STreap[X, Any], STreapContext[X, Any]]
  type BIN[B] = Binding[Int,B]
  //type BST[B] = ST[BIN[B]]

  //val dtc: DefaultSTreapContext[BIN[String]] = DefaultSTreapContext[BIN[String]]()
  //val e: BST[Any] = EmptyOrderedISet(dtc)

  // TODO: Memoization of bind
  // TODO: Memoization of evaluation
  // TODO: Memoization of bindings

  def main(args: Array[String]): Unit = {
    /*def i = (2:IntExpr[Any]) * 3
    println("i: " + i)
    println("i.e: " + i.evaluate)        */
  }

  /*trait IntExpr[B] extends Expr[Int,B] {
    def bind(s: SSet[Binding[Int,B]]) = sys.error("not yet")
    def bindings: SSet[Binding[Int,B]] = sys.error("not yet")
    def +(i: IntExpr[B]) = IAdd(this,i)
    def *(i: IntExpr[B]) = IMul(this,i)
  }

  case class IExpr[B](i: Int) extends IntExpr[B] {
    def evaluate = this
  }

  case class IAdd[B](first: IntExpr[B], second: IntExpr[B]) extends IntExpr[B] {
    lazy val evaluate: IntExpr[B] =  (first.evaluate,second.evaluate) match {
      case (IExpr(i1),IExpr(i2)) => IExpr(i1+i2)
      case _ => this
    }
  }

  case class IMul[B](first: IntExpr[B], second: IntExpr[B]) extends IntExpr[B] {
    lazy val evaluate: IntExpr[B] =  (first.evaluate,second.evaluate) match {
      case (IExpr(i1),IExpr(i2)) => IExpr(i1*i2)
      case _ => this
    }
  }

  case class IBinding[B](label: B, labeled: IntExpr[B]) extends IntExpr[B] with Binding[Int,B] {
    lazy val evaluate: IntExpr[B] = IBinding(label,labeled.evaluate.asInstanceOf[IntExpr[B]])
  } */
}