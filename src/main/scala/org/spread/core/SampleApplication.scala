package org.spread.core

import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.MappedSequence._
import org.spread.core.sequence.Sequence._
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.ArraySequence.ArraySeq
import org.spread.core.sequence.VectorSequence.VectorSeq
import org.spread.core.sequence.RangedSequence._

import scala.{specialized => sp2}
import scala.language.{existentials, implicitConversions}
import scala.language.experimental.macros
import cats.Order
import org.spread.core.annotation.Annotation.Annotator

//
// Exposing all the nice stuff
//

object SampleApplication {

  final def main(args: Array[String]): Unit = {
    import Selector._
    import cats.instances.all._
    import Combiner._
    
   /* val f1 = seqFactory[Int]
    val f2 = seqFactory[Double]

    val a = f1.createSeq(Array(3,2,2))
    val b = f2.createSeq(Array(4.0,6.0,5.0))

    val aa = VectorSeq(Vector(3,2,2))
    val bb = VectorSeq(Vector(4.0,6.0,5.0))

    val aaa = ArraySeq(Array(3,2,2))
    val bbb = ArraySeq(Array(4.0,6.0,5.0))

    val e = (a && b).sort
    val c = e ++ e

    val ee = (aa && bb).sort
    val cc = ee ++ ee

    val eee = aaa && bbb
    val ccc = eee ++ eee

    val c1 = e && e
    val c2 = e && ee
    val c3 = e && eee

    val c4 = ee && e
    val c5 = ee && ee
    val c6 = ee && eee

    val c7 = eee && e
    val c8 = eee && ee
    val c9 = eee && eee
    
    val col1 = c1.select(_.L.L,'c1)
    val col2 = c1.select(_.L.R,'c2)
    val col3 = c1.select(_.R.L,'c3)
    val col4 = c1.select(_.R.R,'c4)

    println("c1: " + c1)
    println("c2: " + c2)
    println("c3: " + c3)
    println("c4: " + c4)
    println("c5: " + c5)
    println("c6: " + c6)
    println("c7: " + c7)
    println("c8: " + c8)
    println("c9: " + c9)*/

    //val a = VectorSeq((0 to 10000000).toVector)
    /*println("start")
    val aa = k.toArray
    println("end")
      */
    val a = seqFactory[Long]
    var v = a.createSeq(Array(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))

    for (i <- 1 to 30) {
      v = v ++ v
    }

    /*val m = a.lmap(x => x*2)
    val ms = m.order.sort
    val i = ms.source.source
    println("start")
    val s = i.sort
    println("s: " + s.split(10)._1) */

    println("v.size: " + v.size)
    println("v: " + v.annotate(LongSum))
  }
  
  object LongSum extends Annotator[Long,Long] {
    def none = 0
    def one(x: Long) = x
    def manyX(x: Array[Long]) = {
      var y = x(0)
      var s = x.length
      var i = 1
      while (i < s) {
        y = y + x(i)
        i = i + 1
      }
      y
    }
    def manyA(a: Array[Long]): Long = {
      var y = a(0)
      var s = a.length
      var i = 0
      while (i < s) {
        y = y + a(i)
        i = i + 1
      }
      y
    }
    def append(a1: Long, a2: Long): Long = a1+a2
  }

}