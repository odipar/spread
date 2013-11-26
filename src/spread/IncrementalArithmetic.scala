package spread

/*
  Copyright 2013: Robbert van Dalen
 */

object IncrementalArithmetic {
  import IncrementalMemoization._
  import scala.language.implicitConversions

  type I = FValue[Int]

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

  case class IWrap(origin: I) extends IValue {
    def apply() = sys.error("no")
    override def force = sys.error("no")
    override def eval = sys.error("no")
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
