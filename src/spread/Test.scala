package spread

object Test {
  import Types._

  // TODO: Memoization of bind
  // TODO: Memoization of evaluation

  trait IntExpr[B] extends Expr[Int,B] {
    def bind(s: SSet[Binding[Int,B]]) = sys.error("not yet")
    def bindings: SSet[Binding[Int,B]] = sys.error("not yet")
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
}
