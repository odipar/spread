package org.spread.core.experiment.product

import org.spread.core.language.Annotation.sp

object SampleApplication2 {

  final def main(args: Array[String]): Unit = {
    var x1 = Array('A','B','C','D')
    var x2 = Array('R','S','T')
    var x3 = Array('W','X','Y','Z')
    var x4 = Array('K','L')

    // TODO: combining different products?
    // val p2 = product2(x1,x2,Array(x1,x2,x3))
    // val p3 = product2(x3,x4,Array(x2,x3,x4))

    val a1 = product2(x1,x3,Array(x1,x2,x3,x4))
    val b1 = product2(x2,x4,Array(x1,x2,x3,x4))

    val a2 = product2(x1,x2,Array(x1,x2,x3,x4))
    val b2 = product2(x3,x4,Array(x1,x2,x3,x4))

    println(prettyString(combine(a1,b1)))
    println("")
    println(prettyString(combine(a2,b2)))
  }

  def product[@sp A,@sp B](a: Array[A], b: Array[B]): Array[(A,B)] = {
    val result = new Array[(A,B)](a.length*b.length)
    var r = 0

    for (i1 <- 0 until a.length) {
      for (i2 <- 0 until b.length) {
        result(r) = (a(i1),b(i2))
        r = r + 1
      }
    }

    result
  }

  // assumes that Array[A] precedes Array[B] and that both are held by prod
  def product2[@sp A,@sp B](a: Array[A], b: Array[B], prod: Array[Array[_]]): Array[(A,B)] = {
    val psize = prod.map(_.length).reduce((x,y) => x*y)
    val result = new Array[(A,B)](psize)

    val prodMap = prod.map(
      x => ({
        if (x == a) b.size
        else if (x == b) 1
        else 0
      },x.size)
    )

    product3(product(a,b),result,prodMap,0,0,0)

    result
  }

  def product3[@sp A,@sp B](src: Array[(A,B)], dst: Array[(A,B)], prodMap: Array[(Int,Int)], prdIdx: Int, srcIdx: Int, dstIdx: Int): Int = {
    if (prdIdx < (prodMap.size-1)) {
      var sIdx = srcIdx
      var dIdx = dstIdx

      var adder = prodMap(prdIdx)._1
      var s = prodMap(prdIdx)._2
      var i = 0
      while (i < s) {
        dIdx = product3(src,dst,prodMap,prdIdx+1,sIdx,dIdx)
        sIdx = sIdx + adder
        i = i + 1
      }
      dIdx
    }
    else product4(src,dst,prodMap,prdIdx,srcIdx,dstIdx)
  }

  def product4[@sp A,@sp B](src: Array[(A,B)], dst: Array[(A,B)], prodMap: Array[(Int,Int)], prdIdx: Int, srcIdx: Int, dstIdx: Int): Int = {
    var sIdx = srcIdx
    var dIdx = dstIdx

    var adder = prodMap(prdIdx)._1
    var s = prodMap(prdIdx)._2
    var i = 0
    while (i < s) {
      dst(dIdx) = src(sIdx)
      dIdx = dIdx + 1
      sIdx = sIdx + adder
      i = i + 1
    }
    dIdx
  }
  
  def prettyString[X](a: Array[X]): String = {
    var s = ""
    for (i <- 0 until a.length) {
      s = s + prettyString2(a(i)) + "\n"
    }
    s
  }

  def prettyString2[X](x: X): String = x match {
    case (x,y) => prettyString2(x) + " " + prettyString2(y)
    case x => "" + x
  }

  def combine[A,B](a: Array[A], b: Array[B]): Array[(A,B)] = {
    val result = new Array[(A,B)](a.length)
    var r = 0
    for (i1 <- 0 until a.length) {
      result(r) = (a(i1),b(i1))
      r = r + 1
    }

    result
  }
}