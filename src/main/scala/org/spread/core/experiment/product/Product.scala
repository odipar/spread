package org.spread.core.experiment.product

import org.spread.core.language.Annotation.sp

object Product {
  final def main(args: Array[String]): Unit = {
   /* var x1: Array[Int] = (0 until 10000).toArray
    var x2: Array[Int] = (0 until 10000).toArray
    //var x3: Array[Int] = (0 until 50).toArray
    //var x4: Array[Int] = (0 until 50).toArray
    var xx: Array[Array[_]] = Array(x1, x2)
    val eq = Equal
    val ap = AndPredicate
    var i = 0
    var s = 100
    while (i < s) {
      val b1 = eq.product2(x1, x2, xx)
      println("i: " + i)
      i = i + 1
    }
    println("result: " + result.toList)
    println("result1: " + b1.toList)
    println("result2: " + b2.toList)
    println("result3: " + b3.toList) */
  }

  trait Predicate {
    @inline def eval[@sp A, @sp B](a: A, b: B): Boolean

    @inline final def product[@sp A, @sp B](a: Array[A], b: Array[B]): Array[Boolean] = {
      //throw new RuntimeException("this")
      val result = new Array[Boolean](a.length * b.length)
      var r = 0
      var s1 = a.length
      var s2 = b.length
      var i1 = 0
      while (i1 < s1) {
        var i2 = 0
        while (i2 < s2) {
          result(r) = eval(a(i1), b(i2))
          i2 = i2 + 1
          r = r + 1
        }
        i1 = i1 + 1
      }
      result
    }

    // assumes that Array[A] precedes Array[B] and that both are held by prod
    @inline final def product2[@sp A, @sp B](a: Array[A], b: Array[B], prod: Array[Array[_]]): Array[Boolean] = {
      val psize = prod.map(_.length).reduce((x, y) => x * y)
      val result = new Array[Boolean](psize)
      val prodMap = prod.map(
        x => ( {
          if (x == a) b.size
          else if (x == b) 1
          else 0
        }, x.size)
      )
      product3(product(a, b), result, prodMap, 0, 0, 0)
      result
    }

    @inline final def product3(src: Array[Boolean], dst: Array[Boolean], prodMap: Array[(Int, Int)], prdIdx: Int, srcIdx: Int, dstIdx: Int): Int = {
      if (prdIdx < (prodMap.length - 1)) {
        var sIdx = srcIdx
        var dIdx = dstIdx
        var adder = prodMap(prdIdx)._1
        var s = prodMap(prdIdx)._2
        var i = 0
        while (i < s) {
          dIdx = product3(src, dst, prodMap, prdIdx + 1, sIdx, dIdx)
          sIdx = sIdx + adder
          i = i + 1
        }
        dIdx
      }
      else product4(src, dst, prodMap, prdIdx, srcIdx, dstIdx)
    }

    @inline final def product4(src: Array[Boolean], dst: Array[Boolean], prodMap: Array[(Int, Int)], prdIdx: Int, srcIdx: Int, dstIdx: Int): Int = {
      var sIdx = srcIdx
      var dIdx = dstIdx
      var adder = prodMap(prdIdx)._1
      var s = prodMap(prdIdx)._2
      var i = 0
      while (i < s) {
        dst(sIdx) = src(sIdx)
        dIdx = dIdx + 1
        sIdx = sIdx + adder
        i = i + 1
      }
      dIdx
      dstIdx
    }
  }

  object Equal extends Predicate {
    @inline final def eval[@sp A, @sp B](a: A, b: B) = (a == b)
  }

  trait BinaryBoolPredicate {
    @inline def apply(a: Boolean, b: Boolean): Boolean

    @inline final def apply(a: Array[Boolean], b: Array[Boolean]): Array[Boolean] = {
      assert(a.length == b.length)
      var i = 0
      var s = a.length
      val result = new Array[Boolean](s)
      while (i < s) {
        result(i) = apply(a(i), b(i))
        i = i + 1
      }
      result
    }
  }

  object AndPredicate extends BinaryBoolPredicate {
    @inline final def apply(a: Boolean, b: Boolean) = a && b
  }

  object OrPredicate extends BinaryBoolPredicate {
    @inline final def apply(a: Boolean, b: Boolean) = a || b
  }

}
