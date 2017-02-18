package org.spread.core

import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.PairedSequence._
import org.spread.core.algorithm.Combine._

//
// Exposing all the nice stuff
//

object SampleApplication {
  
  final def main(args: Array[String]): Unit = {
    val f1 = seqFactory[Int]
    val f2 = seqFactory[Double]

    val b = f1.createSeq(Array(3,2,2))
    val c = f2.createSeq(Array(4.0,6.0,5.0))

    val e = b combineAnnOrd c
    
    val s = e.sort

    println("sorted: " + s)

  }
}