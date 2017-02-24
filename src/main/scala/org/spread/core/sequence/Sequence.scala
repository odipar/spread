package org.spread.core.sequence

import org.spread.core.annotation.Annotation.Annotator
import org.spread.core.constraint.Constraint.EqualProp

import scala.language.{existentials, implicitConversions}
import org.spread.core.language.Annotation.sp

import scala.reflect.ClassTag

object Sequence {
  trait Seq[X,S <: Seq[X,S]] {

    def self: S
    def emptySeq: S
    
    def append[S2 <: S](o: S2): S
    def split(o: Long): (S,S)
    def equalTo[S2 <: S](o: S2): Boolean

    def size: Long
    def height: Int

    def apply(i: Long): X
    def first: X
    def last: X

    def annotate[@sp A: ClassTag](annotator: Annotator[X,A]): A
    
    def isEmpty: Boolean = (size == 0)
    def ++[S2 <: S](o: S2): S = append(o)
  }
  
  // Yet to be defined extra methods
  trait SeqImpl[X, S <: SeqImpl[X,S]] extends Seq[X,S] { }
  
  implicit class Show[X, S <: Seq[X,S]](val s: Seq[X,S]) extends AnyVal {
    def show: Unit = {
      println(s.getClass.getSimpleName + ":")
      val size = { if (s.size > 20) 20 ; else s.size.toInt }
      val part = s.split(size)._1
      for (i <- (0 until size)) {
        println(" " + part(i))
      }
    }
  }
}
