package org.spread.core.sequence

import org.spread.core.constraint.Constraint.EqualProp
import scala.language.{existentials, implicitConversions}

object Sequence {
  trait Seq[X,S <: Seq[X,S]] {

    def self: S
    def emptySeq: S
    
    def append[S2 <: S](o: S2): S
    def split(o: Long): (S,S)
    def equalTo[SS <: S](o: SS): Boolean

    def size: Long
    def height: Int

    def apply(i: Long): X
    def first: X
    def last: X

    def isEmpty: Boolean = (size == 0)
    def ++[S2 <: S](o: S2): S = append(o)
  }

  trait OrderingSeq[X,S <: OrderingSeq[X,S]] extends Seq[X,S] {
    def ordering: Ordering[X]
  }

  trait AnnotatedSeq[X,A,S <: AnnotatedSeq[X,A,S]] extends Seq[X,S] {
    def annotation: A
    def annotationRange(start: Long, end: Long): A
    def equal: EqualProp[A]
  }

  trait AnnOrdSeq[X,A,S <: AnnOrdSeq[X,A,S]] extends AnnotatedSeq[X,A,S] with OrderingSeq[X,S]

  trait PairedSeq[X1,X2,S1 <: Seq[X1,S1], S2 <: Seq[X2,S2], S <: PairedSeq[X1,X2,S1,S2,S]] extends Seq[(X1,X2),S] {
    def left: S1
    def right: S2

    def emptySeq = create(left.emptySeq,right.emptySeq)
    def create(l: S1, r: S2): S
    def append[SS <: S](o: SS): S = create(left append o.left, right append o.right)
    def split(o: Long) = {
      val (ll,lr) = left.split(o)
      val (rl,rr) = right.split(o)
      (create(ll,rl),create(lr,rr))
    }
    def equalTo[SS <: S](o: SS): Boolean = left.equals(o.left) && right.equals(o.right)
    def size = left.size
    def height = (left.height max right.height) + 1
    def first = (left.first,right.first)
    def last = (left.last,right.last)
    def apply(i: Long) = (left(i),right(i))
  }

  // Annotated Seq with representation
  trait AnnSeqWithRepr[X,A,S <: AnnSeqWithRepr[X,A,S]] extends AnnotatedSeq[X,A,S] {
    type AS <: AnnSeqRepr[X,A,S]

    def repr: S#AS

    def create(s: S#AS): S
    def append[S2 <: S](o: S2): S = create(repr.append(o.repr)(self))
    def split(o: Long): (S,S) = { val (l,r) = repr.split(o)(self); (create(l),create(r)) }
    def equalTo[SS <: S](o: SS): Boolean = repr.equalToTree(o.repr)(self)
    def annotation = repr.annotation(self)
    def annotationRange(start: Long, end: Long) = repr.annotationRange(start,end)(self)
    def size = repr.size
    def height = repr.height+1
    def first = repr.first(self)
    def last = repr.last(self)
    def apply(i: Long) = repr.apply(i)(self)
  }

  trait AnnOrdSeqWithRepr[X,A,S <: AnnOrdSeqWithRepr[X,A,S]] extends AnnSeqWithRepr[X,A,S] with AnnOrdSeq[X,A,S]
  
  trait AnnSeqRepr[X,A,S <: AnnSeqWithRepr[X,A,S]] {
    def size: Long
    def height: Int
    def append[AS <: S#AS](o: AS)(implicit c: S): S#AS
    def split(o: Long)(implicit c: S): (S#AS,S#AS)
    def equalToTree[AS <: S#AS](o: AS)(implicit s: S): Boolean
    def annotation(implicit c: S): A
    def annotationRange(start: Long, end: Long)(implicit c: S): A
    def first(implicit c: S): X
    def last(implicit c: S): X
    def apply(i: Long)(implicit c: S): X
  }
}
