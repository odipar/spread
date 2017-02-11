package org.spread.core

/**
  * Created by rapido on 10/02/17.
  */
object Experiments {
  def s1: Array[Number] = Array(1,2,3)
  def s2: Array[Number] = Array(1.2,3.4,5.4)
  final def main(args: Array[String]): Unit = {
    val a = s1 ++ s2
    println("a: " + a)
  }
}