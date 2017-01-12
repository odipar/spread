package org.spread.core

import org.spread.core.sequence.Sequence._

import scala.reflect.ClassTag

object Relation{

  case class Relation[X,Y,XA,YA,XAN <: Annotator[X,XA],YAN <: Annotator[Y,YA],L <: Seq[X,XA,L,XAN],R <: Seq[Y,YA,R,YAN]]
  (left: L,right: R){
    { assert(left.size == right.size) }
    type Rel = Relation[X,Y,XA,YA,XAN,YAN,L,R]
    def split(x: Long): (Rel,Rel) ={
      val (ll,lr) = left.split(x)
      val (rl,rr) = right.split(x)
      (Relation(ll,rl),Relation(lr,rr))
    }
    def append(o: Rel): Rel = Relation(left.append(o.left),right.append(o.right))
    def annotation: (XA,YA) = (left.annotation,right.annotation)
    def size: Long = left.size
  }

  def createRel[X: ClassTag,Y: ClassTag](x: Array[X], y: Array[Y])(implicit ordx: Ordering[X], ordy: Ordering[Y]):
  Relation[X,Y,Statistics[X],Statistics[Y],StatisticsAnnotator[X],StatisticsAnnotator[Y],ArraySequenceWithStatistics[X],ArraySequenceWithStatistics[Y]] = {
    Relation(createStats(x),createStats(y))
  }
  final def main(args: Array[String]): Unit ={
    val c = createRel(Array(1,2,3),Array("a","b","c"))
    println("c: " + c.annotation)
  }
}