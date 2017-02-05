package org.spread.core.sequence

import scala.reflect.ClassTag

object Sequence {

  trait Context[@specialized(Int,Long,Double) +X]
  trait OrderingContext[@specialized(Int,Long,Double) X] extends Context[X] { def ord: Ordering[X] }
  
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
    def combine[Y,SY <: Seq[Y,SY]](o: Seq[Y,SY]): BinSeq[X,Y,S,SY] = BinSeq[X,Y,S,SY](self,o.asInstanceOf[SY])
  }

  trait NoContext extends Context[Nothing]
  object NoContext extends NoContext

  case class VectorSeq[@specialized(Int,Long,Double) X: ClassTag](x: Vector[X]) extends SeqImpl[X,VectorSeq[X]] {
    type S = VectorSeq[X]
    type TC = NoContext

    def context = NoContext
    def self: S = this
    def size: Long = x.length

    def emptySeq = VectorSeq(Vector())
    def append[SS <: S](o: SS): S = VectorSeq(x ++ o.x)
    def split(o: Long) = { val (l,r) = x.splitAt(o.toInt) ; (VectorSeq(l),VectorSeq(r)) }
    def equalTo[SS <: S](o: SS): Boolean = x.equals(o.x)
  }

  trait AnnotatedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeq[X,A,S]]
    extends SeqImpl[X,S] {

    type AS <: ASeq[X,A,S]

    def sequence: S#AS
    def create(s: S#AS): S

    def append[S2 <: S](o: S2): S = create(sequence.append(o.sequence)(self))
    def equalTo[S2 <: S](o: S2): Boolean = sequence.equalToTree(o.sequence)(self)
    def split(o: Long) = { val (l,r) = sequence.split(o)(self) ; (create(l),create(r)) }

    def annotation: A = sequence.annotation(self)
    def annotationRange(start: Long, end: Long)(implicit c: S) = sequence.annotationRange(start,end)(self)
    def size = sequence.size
  }

  trait OrderedAnnotatedSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: OrderedAnnotatedSeq[X,A,S]]
  extends AnnotatedSeq[X,A,S] {
    type TC <: OrderingContext[X]
    def ordering = context.ord
  }

  trait ASeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) A,S <: AnnotatedSeq[X,A,S]] {
    def size: Long
    def append[AS <: S#AS](o: AS)(implicit s: S): S#AS
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def split(o: Long)(implicit s: S): (S#AS,S#AS)
    def annotation(implicit s: S): A
    def annotationRange(start: Long, end: Long)(implicit c: S): A
  }

  case class BinSeq[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y,S1 <: Seq[X,S1],S2 <: Seq[Y,S2]]
  (left: S1, right: S2) extends SeqImpl[(X,Y),BinSeq[X,Y,S1,S2]] {
    type S = BinSeq[X,Y,S1,S2]

    { assert (left.size ==right.size) }

    type TC = NoContext
    def context = NoContext
    
    def self: S = this
    def create(l: S1, r: S2): S = BinSeq(left,right)
    def emptySeq: S = create(left.emptySeq,right.emptySeq)
    def size: Long = left.size
    def append[SS <: S](o: SS): S = create(left.append(o.left),right.append(o.right))
    def split(o: Long): (S,S) = {
      val (ll,lr) = left.split(o)
      val (rl,rr) = right.split(o)
      (BinSeq(ll,rl),BinSeq(lr,rr))
    }
    def equalTo[SS <: S](o: SS): Boolean = left.equals(o.left) && right.equals(o.right)
  }

  final def main(args: Array[String]): Unit = {
  }
}