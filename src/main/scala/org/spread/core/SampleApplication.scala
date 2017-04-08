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
import org.spread.core.constraint.Constraint._
import org.spread.core.algorithm.Solve._

import scala.{specialized => sp2}
import scala.language.{existentials, implicitConversions}
import scala.language.experimental.macros
import spire.implicits._
import org.spread.core.algorithm.Solve._
import org.spread.core.annotation.Annotation.{Annotator, Statistics, StatisticsAnnotator}
import org.spread.core.constraint.Constraint.EqualStatP
import org.spread.core.sequence.ArraySequence
import org.spread.core.splithash.Hashing

import scala.collection.immutable.HashSet
import scala.reflect.ClassTag

//
// Exposing all the nice stuff
//

object SampleApplication {
  
  final def main(args: Array[String]): Unit = {
    import Selector._
    import Combiner._

    val c1 = createSeq((0 until 100000).map(x => x).toArray)
    val c2 = createSeq((0 until 100000).map(x => Hashing.siphash24(x,-x)).toArray)
    val c3 = createSeq((0 until 100000).map(x => Hashing.siphash24(-x,x)).toArray)

    // table with pairwise columns
    val t1 = (c2 && c1).sort
    val t2 = (c3 && c1).sort      

    val T1_C1 = t1.select(_.L)
    val T1_C2 = t1.select(_.R)

    val T2_C1 = t2.select(_.L)
    val T2_C2 = t2.select(_.R)

    val p = (T1_C2 === T2_C2) AND (T1_C1 === c2(10000) OR T1_C1 === c2(20000)) AND (T2_C1 === c3(10000) OR T2_C1 === c3(20000))


    val solver = defaultSolver

    for (k <- 1 to 10) {
      ss = 0
      val solution = defaultSolver.solve(p)
      println("number of solvers: " + ss)
      println("solution(t1): " + solution(t1).size)
      println("solution(t2): " + solution(t2).size)

    }                                        
  }
}