package org.spread.core

import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.sequence.MappedSequence._
import org.spread.core.algorithm.Combine._
import org.spread.core.sequence.ArraySequence.ArraySeq
import org.spread.core.sequence.VectorSequence.VectorSeq

import scala.language.{existentials, implicitConversions}

//
// Exposing all the nice stuff
//

object SampleApplication {
  
  final def main(args: Array[String]): Unit = {
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


    println("c1: " + c1)
    println("c2: " + c2)
    println("c3: " + c3)
    println("c4: " + c4)
    println("c5: " + c5)
    println("c6: " + c6)
    println("c7: " + c7)
    println("c8: " + c8)
    println("c9: " + c9)

  }
}