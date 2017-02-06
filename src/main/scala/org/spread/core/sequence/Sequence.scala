package org.spread.core.sequence

import org.spread.core.annotation.Annotation._

import scala.reflect.ClassTag

object Sequence {
  
  trait Context[@specialized(Int,Long,Double) +X]

  trait Seq[@specialized(Int,Long,Double) +X,S <: Seq[X,S]] {
    type TC <: Context[X]

    def self: S
    def context: TC
    
    def size: Long

    def emptySeq: S
    def append[SS <: S](o: SS): S
    def split(o: Long): (S,S)
    def equalTo[SS <: S](o: SS): Boolean
  }

  trait SeqImpl[@specialized(Int,Long,Double) X,S <: SeqImpl[X,S]] extends Seq[X,S] {
    def combine[@specialized(Int,Long,Double) Y,SY <: Seq[Y,SY]](o: Seq[Y,SY]) = {
      PairedSequence.BinSeqImpl[X,Y,S,SY](self,o.asInstanceOf[SY])
    }
  }

  trait NoContext extends Context[Nothing]
  object NoContext extends NoContext
  trait OrderingContext[@specialized(Int,Long,Double) X] extends Context[X] { def ord: Ordering[X] }

}