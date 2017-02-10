package org.spread.core.sequence

import org.spread.core.annotation.Annotation._

import scala.reflect.ClassTag

object Sequence {
  
  trait Context[@specialized(Int,Long,Double) +X]
  
  trait ValueOption[@specialized(Int,Long,Double) +X] {
    def isEmpty: Boolean
    def some: X
  }

  trait EmptyValue extends ValueOption[Nothing] { def isEmpty = false }
  trait SomeValue[X] extends ValueOption[X] { def isEmpty = true }

  trait Seq[@specialized(Int,Long,Double) +X,S <: Seq[X,S]] extends ValueOption[X] {
    type TC <: Context[X]

    def self: S
    def context: TC
    
    def size: Long
    def height: Int
    
    def emptySeq: S
    def append[SS <: S](o: SS): S
    def split(o: Long): (S,S)
    def equalTo[SS <: S](o: SS): Boolean

    final def isEmpty = (size == 0)
    final def ++[SS <: S](o: SS): S = append(o)
  }

  trait SeqImpl[@specialized(Int,Long,Double) X,S <: SeqImpl[X,S]] extends Seq[X,S] {
    def combine[@specialized(Int,Long,Double) Y,SY <: Seq[Y,SY]](o: Seq[Y,SY]) = {
      PairedSequence.BinSeqImpl[X,Y,S,SY](self,o.asInstanceOf[SY])
    }
    def &&[@specialized(Int,Long,Double) Y,SY <: Seq[Y,SY]](o: Seq[Y,SY]) = combine(o)
  }

  trait NoContext extends Context[Nothing]
  object NoContext extends NoContext
  trait OrderingContext[@specialized(Int,Long,Double) X] extends Context[X] { def ord: Ordering[X] }

}