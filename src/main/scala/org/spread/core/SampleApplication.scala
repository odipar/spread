package org.spread.core

import java.util.Comparator
import java.util.concurrent.TimeUnit

import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.MappedSequence._
import org.spread.core.sequence.Sequence._
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.ArraySequence.ArraySeq
import org.spread.core.sequence.VectorSequence.VectorSeq
import org.spread.core.sequence.RangedSequence._
import org.spread.core.sequence.OrderingSequence._

import scala.{specialized => sp2}
import scala.language.{existentials, implicitConversions}
import scala.language.experimental.macros
import spire.implicits._
import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.ArraySequence

import scala.collection.immutable.HashSet
import scala.reflect.ClassTag

//
// Exposing all the nice stuff
//

object SampleApplication {

  final def main(args: Array[String]): Unit = {
    import Selector._
    import Combiner._
   
    val f1 = seqFactory[Int]
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
    
    val col1 = c1.select(_.L.L)
    val col2 = c1.select(_.L.R)
    val col3 = c1.select(_.R.L)
    val col4 = c1.select(_.R.R)

    println("c1: " + c1)
    println("c2: " + c2)
    println("c3: " + c3)
    println("c4: " + c4)
    println("c5: " + c5)
    println("c6: " + c6)
    println("c7: " + c7)
    println("c8: " + c8)
    println("c9: " + c9)

    println("col1: " + col1(c1.split(1)._1))
  }
}