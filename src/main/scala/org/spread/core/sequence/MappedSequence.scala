package org.spread.core.sequence

import cats.Order
import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.sequence.Sequence._
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence._
import org.spread.core.sequence.RangedSequence._

import scala.reflect.ClassTag

//
// Lazy map Seq (wrapping another Seq)
//
// Copyright 2017: Robbert van Dalen
//
object MappedSequence {
  case class MapSeq[@sp X, @sp Y, S <: Seq[X,S]](source: S)(implicit f: X=>Y) extends SeqImpl[Y,MapSeq[X,Y,S]] {
    type SS = MapSeq[X,Y,S]
    
    def self: SS = this
    def size: Long = source.size
    def height = source.height+1

    def create(c: S): SS = MapSeq[X,Y,S](c)(f)
    def emptySeq = create(source.emptySeq)
    def append[S2 <: SS](o: S2): SS = create(source ++ o.source)
    def split(o: Long) = { val (l,r) = source.split(o.toInt) ; (create(l),create(r)) }
    def equalTo[S2 <: SS](o: S2): Boolean = source.equals(o.s)
    def first = f(source.first)
    def last = f(source.last)
    def apply(i: Long) = f(source(i))
    def annotate[@sp A: ClassTag](annotator: Annotator[Y,A]): A = ???
  }

  case class OrdSeq[@sp X, S <: Seq[X,S]](source: S)(implicit ord: Order[X]) extends OrderingSeq[X,OrdSeq[X,S]] {
    type SS = OrdSeq[X,S]
    def self: SS = this
    def size: Long = source.size
    def height = source.height + 1
    def sort = defaultSort
    def ordering = ord
    def create(c: S): SS = OrdSeq[X,S](c)(ord)
    def emptySeq = create(source.emptySeq)
    def append[S2 <: SS](o: S2): SS = create(source ++ o.source)
    def split(o: Long) = {val (l,r) = source.split(o.toInt); (create(l),create(r))}
    def equalTo[S2 <: SS](o: S2): Boolean = source.equals(o.s)
    def first = source.first
    def last = source.last
    def apply(i: Long) = source(i)
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A = source.annotate(annotator)
  }

  case class IndexedOrdSeq[@sp X, S <: OrderingSeq[X,S]](index: LSEQ, source: S)
    extends OrderingSeq[X,IndexedOrdSeq[X,S]] {
    type SS = IndexedOrdSeq[X,S]

    def self: SS = this
    def size: Long = index.size
    def height = index.height+1

    def sort = defaultSort
    def ordering = source.ordering
    def emptySeq: SS = IndexedOrdSeq[X,S](index.emptySeq,source.emptySeq)
    def append[S2 <: SS](o: S2): SS = {
      if (source == o.source) {
        IndexedOrdSeq[X,S](index append o.index,source)
      }
      else {
        // TODO, use the same factory with path dependent types to prevent this
        sys.error("Can only append same underlying sources")
      }
    }
    def split(o: Long) = {
      val (l,r) = index.split(o)
      (IndexedOrdSeq(l,source),IndexedOrdSeq(r,source))
    }
    def equalTo[S2 <: SS](o: S2): Boolean = ((source == o.source) && index.equalTo(o.index))
    def first = source(index.first)
    def last = source(index.last)
    def apply(i: Long) = source(index(i))
    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A = ???
  }


  implicit class Mapper[@sp X, S <: Seq[X,S]](s: Seq[X,S]) {
    def lmap[@sp Y](f: X=>Y) = MapSeq[X,Y,S](s.asInstanceOf[S])(f)
    def order(implicit o: Order[X]) = OrdSeq[X,S](s.asInstanceOf[S])(o)
  }

  implicit class OrdMapper[@sp X, S <: OrderingSeq[X,S]](s: OrderingSeq[X,S]) {
    def index = IndexedOrdSeq[X,S](createRange(0,s.size-1),s.asInstanceOf[S])
  }
}