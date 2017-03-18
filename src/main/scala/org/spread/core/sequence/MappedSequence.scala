package org.spread.core.sequence

import org.spread.core.annotation.Annotation.{Annotator, NoAnnotation}
import org.spread.core.sequence.Sequence._
import org.spread.core.language.Annotation.sp
import org.spread.core.sequence.OrderingSequence._
import org.spread.core.sequence.RangedSequence._

import scala.language.{existentials, implicitConversions}
import scala.reflect.ClassTag

//
// Lazy map Seq (wrapping another Seq)
//
// Copyright 2017: Robbert van Dalen
//
object MappedSequence {
  case class MapSeq[@sp X, @sp Y, S <: Seq[X,S]](source: S)(implicit f: X=>Y, yt: ClassTag[Y]) extends SeqImpl[Y,MapSeq[X,Y,S]] {
    type SS = MapSeq[X,Y,S]
    
    def self: SS = this
    def size: Long = source.size
    def height = source.height+1

    def tag = yt
    def create(c: S): SS = MapSeq[X,Y,S](c)(f,yt)
    def createSeq(a: Array[Y]) = ???
    def toArray = source.toArray.map(f)
    def emptySeq = create(source.emptySeq)
    def append[S2 <: SS](o: S2): SS = create(source ++ o.source)
    def split(o: Long) = { val (l,r) = source.split(o.toInt) ; (create(l),create(r)) }
    def equalTo[S2 <: SS](o: S2): Boolean = source.equals(o.s)
    def first = f(source.first)
    def last = f(source.last)
    def apply(i: Long) = f(source(i))
  }

  case class OrdSeq[@sp X, S <: Seq[X,S]](source: S)(implicit ord: Order[X], xt: ClassTag[X]) extends OrderingSeq[X,OrdSeq[X,S]] {
    type SS = OrdSeq[X,S]
    def self: SS = this
    def size: Long = source.size
    def height = source.height + 1
    def sort = defaultSort
    def ordering = ord
    def tag = xt
    def create(c: S): SS = OrdSeq[X,S](c)(ord,xt)
    def createSeq(a: Array[X]) = OrdSeq(source.createSeq(a))
    def toArray: Array[X] = source.toArray
    def emptySeq = create(source.emptySeq)
    def append[S2 <: SS](o: S2): SS = create(source ++ o.source)
    def split(o: Long) = {val (l,r) = source.split(o.toInt); (create(l),create(r))}
    def equalTo[S2 <: SS](o: S2): Boolean = source.equals(o.s)
    def first = source.first
    def last = source.last
    def apply(i: Long) = source(i)
  }

  implicit class Mapper[@sp X, S <: Seq[X,S]](s: Seq[X,S]) {
    def lmap[@sp Y](f: X=>Y)(implicit ct: ClassTag[Y]) = MapSeq[X,Y,S](s.asInstanceOf[S])(f,ct)
    def order(implicit o: Order[X], ct: ClassTag[X]) = OrdSeq[X,S](s.asInstanceOf[S])(o,ct)
  }
}