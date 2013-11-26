package spread

/*
  Copyright 2013: Robbert van Dalen
 */

object IncrementalArithmetic {
  import scala.language.implicitConversions
  import IncrementalMemoization._
  import java.math.BigInteger

  type I = FValue[Int]
  type BI = FValue[BigInteger]

  lazy val add = force21(fadd)
  lazy val sub = force21(fsub)
  lazy val mul = force21(fmul)

  val fadd: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() + i2()
    override def toString = "+"
  }

  val fsub: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() - i2()
    override def toString = "-"
  }

  val fmul: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() * i2()
    override def toString = "*"
  }

  trait IValue extends FValue[Int] {
    def origin: I
    def +(o: I): I = %(add,origin,o)
    def -(o: I): I = %(sub,origin,o)
    def *(o: I): I = %(mul,origin,o)
  }

  private case class IWrap(origin: I) extends IValue {
    def error = sys.error("IWrap should not be used directly")
    def apply() = error
    override def force = error
    override def eval = error
  }

  implicit def toI(i: I): IValue = i match {
    case ii: IValue => ii ; case _ => IWrap(i)
  }

  implicit def toI(i: Int): I = mem(TInt(i))

  case class TInt(i: Int) extends IValue {
    def origin = this
    def apply() = i
    override def toString = "`"+i.toString
  }
}
