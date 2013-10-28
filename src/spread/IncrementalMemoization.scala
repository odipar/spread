package spread

import java.lang.ref.WeakReference

/* A new incremental memoization algorithm */

object IncrementalMemoization {
  import language.implicitConversions

  import java.math.BigInteger

  var totalt: Long = 0

  trait Trace[R] {
    synchronized { totalt = totalt + 1 } // statistics

    def apply(): R
    def step: Trace[R] = this
  }

  type T[X] = Trace[X]
  type TInt = T[BigInteger]

  case class CInt(i: BigInteger) extends TInt {
    def apply = i

    def *(a: CInt) = cint(i multiply a.i)
    def +(a: CInt) = cint(i add a.i)
    def -(a: CInt) = cint(i subtract a.i)
    def <(a: CInt) = compare(a) < 0
    def >(a: CInt) = compare(a) > 0
    def compare(a: CInt) = i.compareTo(a.i)

    override def toString = i.toString
  }

  val mtable = scala.collection.mutable.WeakHashMap.empty[T[_], WeakReference[T[_]]]

  def memoize[R](t: T[R]): T[R] = {
    if (!mtable.contains(t)) mtable.put(t,new WeakReference(t))
    mtable.get(t).get.get.asInstanceOf[T[R]]
  }

  def cint(i: Long): CInt = cint(BigInteger.valueOf(i))
  def cint(i: BigInteger): CInt = CInt(i)

  def call1[A1,A2,R](f: T[A1] => T[R], a1: T[A1]): T[R] = memoize(UnaTrace(f,a1))
  def call2[A1,A2,R](f: (T[A1],T[A2]) => T[R], a1: T[A1], a2: T[A2]): T[R] = memoize(BinTrace(f,a1,a2))

  def t_call1[A1,A2,R](f: A1 => R, a1: T[A1]): T[R] = memoize(TUnaTrace(f,a1))
  def t_call2[A1,A2,R](f: (A1,A2) => R, a1: T[A1], a2: T[A2]): T[R] = memoize(TBinTrace(f,a1,a2))

  // sugar
  def $[A1,A2,R](f: T[A1] => T[R], a1: T[A1]) = call1(f,a1)
  def $[A1,A2,R](f: (T[A1],T[A2]) => T[R], a1: T[A1], a2: T[A2])= call2(f,a1,a2)
  implicit def toCInt(i: Int): CInt = cint(i)
  implicit def toCInt(i: BigInteger): CInt = cint(i)

  case class UnaTrace[A1,R](f: T[A1] => T[R],a1: T[A1]) extends T[R] {
    override lazy val apply = step.apply
    override lazy val step: T[R] = f(a1)
  }

  case class BinTrace[A1,A2,R](f: (T[A1],T[A2]) => T[R], a1: T[A1], a2: T[A2]) extends T[R] {
    override lazy val apply = step.apply
    override lazy val step: T[R] = f(a1,a2)
  }

  case class TUnaTrace[A1,R](f: A1 => R, a1: T[A1]) extends T[R] {
    override lazy val apply = f(a1())
  }

  case class TBinTrace[A1,A2,R](f: (A1,A2) => R,a1: T[A1], a2: T[A2]) extends T[R] {
    override lazy val apply = f(a1(),a2())
  }

  val add = (a1: TInt, a2: TInt) => t_call2((x: BigInteger,y: BigInteger) => {x add y},a1,a2)
  val mul = (a1: TInt, a2: TInt) => t_call2((x: BigInteger,y: BigInteger) => {x multiply y},a1,a2)

  // Examples

  val fib: TInt => TInt = a1 => {
    val a = a1()
    if (a < 1) 1
    else $(add,$(fib,a - 1),$(fib,a - 2))
  }

  val fac: TInt => TInt = a1 => {
    val a = a1()
    if (a < 1) 1
    else $(mul,a1,$(fac,a - 1))
  }
}
