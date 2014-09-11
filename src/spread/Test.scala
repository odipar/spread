package spread

import java.lang.ref.WeakReference
import scala.collection.mutable.WeakHashMap

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {

  import scala.language.implicitConversions
  import IncrementalMemoization._
  import IncrementalTreapMap._
  import IncrementalArithmetic._

  object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (!i < 2) (1~1)
      else ((!i)~i) ** %(fac,!i - 1)
    }
    override def toString = "fac"
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (!i < 2) i
      else (%(fib,!i-1)) ++ (%(fib,!i-2))
    }
    override def toString = "fib"
  }

  implicit val p = LPrioFactory[A32[Long]]

  final def main(args: Array[String]): Unit = {
    var a = longArray[Long]
    var ia = larray(Array[Long](0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000))
    var i = 0
    println("start0")
    while (i < 500) {
      var iaa = new A32[Long](0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000)
      a = a.put(i,iaa)
      i = i + 1
    }
    println("end0")
    var ii: Long = 0

    println("start")
    val c: Long = 50L * 5000L
    var r: Long = 0
    while (ii < c) {
      r = a.fold(0,suml)
      ii = ii + 1
    }
    println("end: " + xx)
  }


  val sumi: ((Int,Int) => Int) = new ((Int,Int) => Int) {
    def apply(a: Int, b: Int): Int = a + b
  }

  var xx: Long = 0
  var xxx: Long = 0

  val suml: ((Long,Long) => Long) = new ((Long,Long) => Long) {
    def apply(a: Long, b: Long): Long = {
      xx = xx + 1
      a + b
    }
  }

  trait Array32[@specialized(Int,Long,Double) V] {
    def value: Array[V]
    def fold(a: V, f: ((V,V) => V)): V = {
      val v = value
      var aa = a
      var i = 0
      while (i < 32) {
        aa = f(aa,v(i))
        i = i + 1
      }
      return aa
    }
    override val hashCode = {
      val v = value
      var i = 0
      var h = 0
      while (i < 32) {
        h = h + v(i).hashCode
        i = i + 1
      }
      h
    }
  }

  case class Array32Impl[V](value: Array[V]) extends Array32[V]
  case class IArray32Impl(value: Array[Int]) extends Array32[Int]
  case class LArray32Impl(value: Array[Long]) extends Array32[Long]
  case class DArray32Impl(value: Array[Double]) extends Array32[Double]

  def array[V](a: Array[V]): Array32[V] = Array32Impl(a)
  def iarray(a: Array[Int]): Array32[Int] = IArray32Impl(a)
  def larray(a: Array[Long]): Array32[Long] = LArray32Impl(a)
  def darray(a: Array[Double]): Array32[Double] = DArray32Impl(a)

  abstract class LongArray[@specialized(Int,Long,Double) V] {

    def first: Option[Long]
    def some: Option[Long]
    def last: Option[Long]

    //def split(i: Long): (LongArray[V],Option[A32[V]],LongArray[V])

    def get(i: Long): Option[A32[V]]
    def put(i: Long, a: A32[V]): LongArray[V]

    def fold(a: V, f: ((V,V) => V)): V
  }

  def longArray[V](implicit o: PO[Long,A32[V]]): LLArrayImpl = LLArrayImpl(empty)

  case class LArrayImpl[V](m: T[Long,A32[V]])(implicit o: PO[Long,A32[V]]) extends LongArray[V] {
    def first = { if (m.isEmpty) None ; else Some(m.first) }
    def some = { if (m.isEmpty) None ; else Some(m.key) }
    def last = { if (m.isEmpty) None ; else Some(m.last) }

    /*def split(i: Long) = {
      val (l: LongArray[V],mm: Option[A32[V]],r: LongArray[V]) = m.split(i)
      if (mm.isEmpty) (LArrayImpl(l),None,LArrayImpl(r))
      else (LArrayImpl(l),Some(mm.value),LArrayImpl(r))
    } */
    def get(i: Long) = m.get(i)
    def put(i: Long, a: A32[V]) = LArrayImpl(m.put(i,a))
    def fold(a: V, f: ((V,V) => V)): V = {
      fold(m,a,f)
    }
    final def fold(mm: T[Long,A32[V]],a: V, f: ((V,V) => V)): V = {
      mm.value.fold(a,f)
      /*if (mm.isEmpty) a
      else {
        val lf = fold(mm.left,a,f)
        val mf = f(lf,mm.value.fold(lf,f))
        val rf = fold(mm.right,mf,f)
        rf
      } */
    }
  }

  case class LLArrayImpl(m: T[Long,A32[Long]])(implicit o: PO[Long,A32[Long]]) extends LongArray[Long] {
    def first = { if (m.isEmpty) None ; else Some(m.first) }
    def some = { if (m.isEmpty) None ; else Some(m.key) }
    def last = { if (m.isEmpty) None ; else Some(m.last) }

    /*def split(i: Long) = {
      val (l: LongArray[V],mm: Option[A32[V]],r: LongArray[V]) = m.split(i)
      if (mm.isEmpty) (LArrayImpl(l),None,LArrayImpl(r))
      else (LArrayImpl(l),Some(mm.value),LArrayImpl(r))
    } */
    def get(i: Long) = m.get(i)
    def put(i: Long, a: A32[Long]) = LLArrayImpl(m.put(i,a))
    def fold(a: Long, f: ((Long,Long) => Long)): Long = {
      fold(m,a,f)
    }
    final def fold(mm: T[Long,A32[Long]],a: Long, f: ((Long,Long) => Long)): Long = {
      if (mm.isEmpty) a
      else {
        val lf = fold(mm.left,a,f)
        val mf = f(lf,mm.value.fold(lf,f))
        val rf = fold(mm.right,mf,f)
        rf
      }
    }
  }

   class A32[@specialized(Int,Long,Double) V]
  (a0: V, a1:V,a2:V,a3:V,a4:V,a5:V,a6:V,a7:V,a8:V,a9:V,a10:V,a11:V,a12:V,a13:V,a14:V,a15:V,a16:V,a17:V,a18:V,a19:V,a20:V,a21:V,a22:V,a23:V,a24:V,a25:V,a26:V,a27:V,a28:V,a29:V,a30:V,a31:V) {
    def fold(a: V,f: ((V,V) => V)): V = {
      f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(f(a,a0),a1),a2),a3),a4),a5),a6),a7),a8),a9),a10),a11),a12),a13),a14),a15),a16),a17),a18),a19),a20),a21),a22),a23),a24),a25),a26),a27),a28),a29),a30),a31)
    }
  }
}

