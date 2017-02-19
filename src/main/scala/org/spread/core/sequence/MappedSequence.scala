package org.spread.core.sequence

import org.spread.core.sequence.Sequence._

//
// Lazy map Seq (wrapping another Seq)
//
// Copyright 2017: Robbert van Dalen
//
object MappedSequence {
  case class MapSeq[@specialized X, @specialized Y, S <: Seq[X,S]](s: S)(implicit f: X=>Y) extends Seq[Y,MapSeq[X,Y,S]] {
    type SS = MapSeq[X,Y,S]
    
    def self: SS = this
    def size: Long = s.size
    val height = s.height+1 

    def create(c: S): SS = MapSeq[X,Y,S](c)(f)
    def emptySeq = create(s.emptySeq)
    def append[S2 <: SS](o: S2): SS = create(s ++ o.s)
    def split(o: Long) = { val (l,r) = s.split(o.toInt) ; (create(l),create(r)) }
    def equalTo[S2 <: SS](o: S2): Boolean = s.equals(o.s)
    def first = f(s.first)
    def last = f(s.last)
    def apply(i: Long) = f(s(i))
  }

  implicit class Mapper[@specialized X, S <: Seq[X,S]](s: Seq[X,S]) {
    def lmap[@specialized Y](f: X=>Y) = MapSeq[X,Y,S](s.asInstanceOf[S])(f)
  }
}