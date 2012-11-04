package spread

object Test {
  import Types._
  import OrderedSetImplementation._
  import OrderedTreapSet._

  implicit def intToIExpr[B](i: Int): IntExpr[B] = IExpr[B](i)

  /*type ST[X] = OrderedISetImpl[X, Any, STreap[X, Any], STreapContext[X, Any]]

  trait SSetImpl[A] extends OrderedISetImpl[A,Any,ST[A],DefaultSTreapContext[A],SSetImpl[A]]
    */

  // TODO: Memoization of bind
  // TODO: Memoization of evaluation
  // TODO: Memoization of bindings

  def main(args: Array[String]): Unit = {
    def i = (2:IntExpr[Any]) * 3 + 4
    println("i: " + i)
    println("i.e: " + i.evaluate)
  }

  trait IntExpr[B] extends Expr[Int,B] {
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
  }
}