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

  lazy val add = finish2(fadd)
  lazy val sub = finish2(fsub)
  lazy val mul = finish2(fmul)

  lazy val add2 = reduce2(fadd)
  lazy val sub2 = reduce2(fsub)
  lazy val mul2 = reduce2(fmul)

  lazy val fadd: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() + i2()
    override def toString = "+"
  }

  lazy val fsub: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() - i2()
    override def toString = "-"
  }

  lazy val fmul: Function2[I,I,I] = new Function2[I,I,I] {
    def apply(i1: I, i2: I) = i1() * i2()
    override def toString = "*"
  }

  trait IValue extends FValue[Int] {
    def origin: I
    def +(o: I): I = fadd(origin,o)
    def -(o: I): I = fsub(origin,o)
    def *(o: I): I = fmul(origin,o)
    def ++(o: I): I = %(fadd,origin,o)
    def --(o: I): I = %(fsub,origin,o)
    def **(o: I): I = %(fmul,origin,o)
    def +%(o: I): I = %(add2,origin,o)
    def -%(o: I): I = %(sub2,origin,o)
    def *%(o: I): I = %(mul2,origin,o)
    def +^(o: I): I = %(add,origin,o)
    def -^(o: I): I = %(sub,origin,o)
    def *^(o: I): I = %(mul,origin,o)
  }

  private case class IWrap(origin: I) extends IValue {
    def error = sys.error("IWrap should not be used directly")
    override def apply() = error
    override def finish = error
    override def iterate = error
  }

  implicit def toIWrap(i: I): IValue = i match {
    case ii: IValue => ii ; case _ => IWrap(i)
  }

  implicit def toInt(i: Int): I = mem(TInt(i))

  case class TInt(i: Int) extends IValue {
    def origin = this
    def apply() = i
    override def toString = "`"+i.toString
  }
}
