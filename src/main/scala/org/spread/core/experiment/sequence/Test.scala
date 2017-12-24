package org.spread.core.experiment.sequence

object Test {
  import TreeSequence._
  import PairedSequence._

  final def main(args: Array[String]): Unit = {
    val factory = emptyTree[Int]

    val s1 = factory.createSeq(Array(1, 2, 3))
    val s2 = factory.createSeq((1 to 5).toArray)

    var s3 = s1


    for (i <- 1 to 100000) {
      s3 = s3 ++ s2

      if ((i % 10000) == 0) { println("i: " + i)}
    }

    s3 = s3.split(100)._1
    
    val k = pair(s3,s3)

    println("nodes: " + nodes)
  }
}
