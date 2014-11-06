package spread

object IncrementalLogic {
  import IncrementalMemoization._
  import scala.language.implicitConversions

  type B = Expr[Boolean]

  trait BExpr extends Expr[Boolean] {
    def origin: B
    def |||(o: B): B = %(or, origin, o)
    def &&&(o: B): B = %(and, origin, o)
  }

  type F0B = F0[Boolean]

  trait BB extends BExpr with F0B {
    def containsQuote = false
    def containsBinding = false

    def origin = this
  }

  case object BTrue extends BB {
    def evalValue = true
    override def toString = "true"
  }

  case object BFalse extends BB {
    def evalValue = false
    override def toString = "false"
  }

  implicit def toB(b: Boolean) = if (b) BTrue ; else BFalse

  private case class BWrap(origin: B) extends BExpr {
    def containsQuote = error
    def containsBinding = false

    def error = sys.error("BWrap should not be used directly")
  }

  implicit def toBWrap(b: B): BExpr = b match {
    case bb: BExpr => bb
    case _ => BWrap(b)
  }

  object or extends FA2[Boolean,Boolean,Boolean] with Infix {
    def apply(a: F0B, b: F0B) = a.evalValue || b.evalValue
    override def toString = "|||"
  }

  object and extends FA2[Boolean,Boolean,Boolean] with Infix {
    def apply(a: F0B, b: F0B) = a.evalValue && b.evalValue
    override def toString = "&&&"
  }

  case class WB(i: Boolean) {
    def unary_~ = toB(i)
  }

  implicit def toWB(i: Boolean): WB = WB(i)

  // lazy if-then-else
  case class ifte[X]() extends Function3[B,Expr[X],Expr[X],Expr[X]] {
    def apply(c: Expr[Boolean], a1: Expr[X], a2: Expr[X]): Expr[X] = {
      c match {
        case cc: F0[Boolean] => {
          if (~cc) a1
          else a2
        }
        case _ => %(this,c,a1,a2)
      }

    }
  }
}
