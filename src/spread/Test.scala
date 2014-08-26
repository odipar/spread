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

  /* object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (!i < 2) (1~1).\
      else ((!i)~i).\ ** %(fac,!i - 1)
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

  object sum extends FA1[Treap[Int,Int],Int] {
    def apply(f: F0[Treap[Int,Int]]): I = {
      val t: Treap[Int,Int] = f.evalValue
      val a = t.value.\
      if (t.left.isEmpty) {
        if (t.right.isEmpty) a
        else a ++ %(sum,ei(t.right))
      }
      else {
        if (t.right.isEmpty) %(sum,ei(t.left)) ++ a
        else (%(sum,ei(t.left)) ++ a) ++ %(sum,ei(t.right))
      }
    }
    override def toString = "sum"
  }
            */
  implicit val p = LPrioFactory[Array32[Long]]
  final def main(args: Array[String]): Unit = {
    var a = longArray[Long]
    var ia = larray(Array[Long](0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000))

    var i = 0
    while (i < 1) {
      a = a.put(i,ia)
      i = i + 1
    }
    var ii: Long = 0

    println("start")
    val c: Long = 50000L * 10000L
    var r: Long = 0
    while (ii < c) {
      r = a.fold(0,suml)
      ii = ii + 1
    }
    /*var iii: Long = 0
    var xx: Int = 0
    val ccc: Long = 50000L * 10000L * 32L

    val ar = Array[Long](0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000,13000,14000,15000,16000,17000,18000,19000,20000,21000,22000,23000,24000,25000,26000,27000,28000,29000,30000,31000)

    var s: Long = 0

    val sum = suml

    while (iii < ccc) {
      s = sum(s,ar(xx))
      xx = xx + 1
      if (xx >= 32) { xx = 0 }
      iii = iii + 1
    }      */
    println("end: " + r)
  }


  val sumi: ((Int,Int) => Int) = new ((Int,Int) => Int) {
    def apply(a: Int, b: Int): Int = a + b
  }

  val suml: ((Long,Long) => Long) = new ((Long,Long) => Long) {
    def apply(a: Long, b: Long): Long = a + b
  }

  trait Array32[V] {
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
  case class IArray32Impl[V](value: Array[Int]) extends Array32[Int]
  case class LArray32Impl[V](value: Array[Long]) extends Array32[Long]
  case class DArray32Impl[V](value: Array[Double]) extends Array32[Double]

  def array[V](a: Array[V]): Array32[V] = Array32Impl(a)
  def iarray(a: Array[Int]): Array32[Int] = IArray32Impl(a)
  def larray(a: Array[Long]): Array32[Long] = LArray32Impl(a)
  def darray(a: Array[Double]): Array32[Double] = DArray32Impl(a)

  trait LongArray[@specialized(Int,Long,Double) V] {

    def first: Option[Long]
    def some: Option[Long]
    def last: Option[Long]

    def split(i: Long): (LongArray[V],Option[Array32[V]],LongArray[V])

    def get(i: Long): Option[Array32[V]]
    def put(i: Long, a: Array32[V]): LongArray[V]

    def fold(a: V, f: ((V,V) => V)): V = {
      val s = some
      if (s == None) a
      else {
        val (l: LongArray[V],m: Option[Array32[V]],r: LongArray[V]) = split(s.get)
        val lf = l.fold(a,f)
        val mf = m.get.fold(lf,f)
        val rg = r.fold(mf,f)
        return rg
      }
    }
  }

  def longArray[V](implicit o: PO[Long,Array32[V]]): LongArray[V] = LArrayImpl(empty)

  case class LArrayImpl[V](m: T[Long,Array32[V]])(implicit o: PO[Long,Array32[V]]) extends LongArray[V] {
    def first = { if (m.isEmpty) None ; else Some(m.first) }
    def some = { if (m.isEmpty) None ; else Some(m.key) }
    def last = { if (m.isEmpty) None ; else Some(m.last) }

    def split(i: Long) = {
      val (l: T[Long,Array32[V]],mm: T[Long,Array32[V]],r: T[Long,Array32[V]]) = m.split(i)
      if (mm.isEmpty) (LArrayImpl(l),None,LArrayImpl(r))
      else (LArrayImpl(l),Some(mm.value),LArrayImpl(r))
    }
    def get(i: Long) = m.get(i)
    def put(i: Long, a: Array32[V]) = LArrayImpl(m.put(i,a))
  }
}

