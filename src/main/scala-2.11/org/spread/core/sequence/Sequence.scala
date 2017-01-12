package org.spread.core.sequence

import scala.language.{existentials, implicitConversions}

import scala.reflect.ClassTag

object Sequence{

  trait Seq[X,A,S <: Seq[X,A,S,AN], AN <: Annotator[X,A]] {
    def append(s: S): S
    def split(x: Long): (S,S)
    def annotation: A
    def someIndex: Long
    def size: Long
  }
  
  trait SeqImpl[X,A,S <: SeqImpl[X,A,S,C],C <: Context[X,A,S,C]]{
    def self: S

    def append(o: S)(implicit c: C): S = c.append(self,o)
    def split(x: Long)(implicit c: C): (S,S) = c.split(self,x)
    def annotation: A
    def someIndex: Long
    def size: Long
  }

  trait Annotator[X,A]{
    def none: A
    def one(x: X): A
    def many(d: Array[X]): A
    def append(r1: A,r2: A): A
  }

  trait Context[X,A,S <: SeqImpl[X,A,S,C],C <: Context[X,A,S,C]]{
    def annotator: Annotator[X,A]
    def create(x: X): S
    def append(s1: S,s2: S): S
    def split(s: S,x: Long): (S,S)
  }

  case class ArraySeq[X,A,C <: ArraySeqContext[X,A,C]](seq: Array[X],annotation: A)
    extends SeqImpl[X,A,ArraySeq[X,A,C],C]{
    type S = ArraySeq[X,A,C]
    def self: S = this
    def someIndex: Long = seq.length / 2
    def size: Long = seq.length
    override def toString: String = seq.toList + " : " + annotation
  }

  trait ArraySeqContext[X,A,C <: ArraySeqContext[X,A,C]]
    extends Context[X,A,ArraySeq[X,A,C],C]{

    type S = ArraySeq[X,A,C]
    implicit def classTag: ClassTag[X]

    def create(x: X) = ArraySeq(Array(x),annotator.one(x))
    def append(s1: S,s2: S): S = ArraySeq(s1.seq ++ s2.seq,annotator.append(s1.annotation,s2.annotation))
    def split(s: S,o: Long) ={
      val (s1,s2) = s.seq.splitAt(o.toInt)
      (ArraySeq(s1,annotator.many(s1)),ArraySeq(s2,annotator.many(s2)))
    }
  }

  case class Statistics[X](lowerBound: X,upperBound: X, first: X, last: X, sorted: Boolean) {
    override def toString: String = "<" + first + "," + lowerBound + "," + sorted + "," + upperBound + "," + last + ">"
  }

  case class StatisticsAnnotator[X](implicit ord: Ordering[X]) extends Annotator[X,Statistics[X]]{
    def none = sys.error("no stats")
    def one(x: X) = Statistics(x,x,x,x,true)
    def many(a: Array[X]) ={
      var mmin = a(0)
      var mmax = a(0)
      var msorted = true
      for (i <- 1 until a.length) {
        val x = a(i)
        mmin = ord.min(mmin,x)
        mmax = ord.max(mmax,x)
        msorted = msorted && ord.lteq(a(i - 1),x)
      }
      Statistics(mmin,mmax,a(0),a(a.length-1),msorted)
    }
    def append(s1: Statistics[X],s2: Statistics[X]) ={
      Statistics(
        ord.min(s1.lowerBound,s2.lowerBound),
        ord.max(s1.upperBound,s2.upperBound),
        s1.first,s2.last,
        s1.sorted && s2.sorted && ord.lteq(s1.last,s2.first)
      )
    }
  }

  case class ArraySequenceWithStatistics[X]
  (seq: ArraySeq[X, Statistics[X],ArraySequenceWithStatistics[X]],annotator: StatisticsAnnotator[X],cTag: ClassTag[X])
    extends ArraySeqContext[X,Statistics[X],ArraySequenceWithStatistics[X]] with Seq[X,Statistics[X],ArraySequenceWithStatistics[X],StatisticsAnnotator[X]] {
    implicit def classTag = cTag
    implicit def context = this

    type SS = ArraySequenceWithStatistics[X]

    def create(a: Array[X]): SS = ArraySequenceWithStatistics(ArraySeq(a,annotator.many(a)),annotator,cTag)
    def append(o: SS): SS = ArraySequenceWithStatistics(seq.append(o.seq),annotator,cTag)
    def split(x: Long): (SS,SS) ={
      val (l,r) = seq.split(x)(this)
      (ArraySequenceWithStatistics(l,annotator,cTag),ArraySequenceWithStatistics(r,annotator,cTag))
    }
    def annotation = seq.annotation
    def someIndex = seq.someIndex
    def size = seq.size
    override def toString: String = seq.toString
  }

  implicit def createStats[X](a: Array[X])(implicit ord: Ordering[X],cTag: ClassTag[X]) ={
    ArraySequenceWithStatistics(null,StatisticsAnnotator(),cTag).create(a)
  }

  final def main(args: Array[String]): Unit ={
    val a = createStats(Array(1,2,3,4))
    val b = createStats(Array(6,5,4,3))

    println("a: " + a)
    println("b: " + b)
    println("c: " + a.append(b))
    println("a: " + a.append(b).split(2))
  }
}