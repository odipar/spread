package org.spread.core.relation

import org.spread.core.sequence.Sequence.{Context, _}
import org.spread.core.annotation.Annotation._

import scala.reflect.ClassTag
import scala.language.{existentials, implicitConversions}

//
// A Column oriented Binary Relation based on two efficient 16/64-way BTrees
//
// Copyright 2017: Robbert van Dalen
//

object Relation {

  trait BRel[@specialized(Int,Long,Double) X,
  @specialized(Int,Long,Double) XA,
  @specialized(Int,Long,Double) Y,
  @specialized(Int,Long,Double) YA,
  XC <: OrderingContext[X,XA,XC],
  YC <: OrderingContext[Y,YA,YC]] {
    type R <: BRel[X,XA,Y,YA,XC,YC]

    def left: SSeq[X,XA,XC]
    def right: SSeq[Y,YA,YC]

    def create(left: SSeq[X,XA,XC],right: SSeq[Y,YA,YC]): R
    def size = left.size
    def append(r: R): R = create(left.append(r.left),right.append(r.right))
    def split(i: Long): (R,R) = {
      val (ll,lr) = left.split(i)
      val (rl,rr) = right.split(i)
      (create(ll,rl),create(lr,rr))
    }
    def empty = create(left.empty,right.empty)
    def annotationRange(s: Long, e: Long): (XA,YA) = (left.annotationRange(s,e),right.annotationRange(s,e))

    /*def toSeq: SSeq[(X,Y),(XA,YA),CombinedContext[X,XA,Y,YA,XC,YC]] = {
     null
    } */
    override def toString = left.toString + "_" + right.toString
  }

  trait CombinedContext[@specialized(Int,Long,Double) X,
  @specialized(Int,Long,Double) XA,
  @specialized(Int,Long,Double) Y,
  @specialized(Int,Long,Double) YA,
  XC <: OrderingContext[X,XA,XC],
  YC <: OrderingContext[Y,YA,YC]] extends Context[(X,Y),(XA,YA),CombinedContext[X,XA,Y,YA,XC,YC]]

/*  case class BSeqRel[@specialized(Int,Long,Double) X,
  @specialized(Int,Long,Double) XA,
  @specialized(Int,Long,Double) Y,
  @specialized(Int,Long,Double) YA,
  XC <: Context[X,XA,XC],
  YC <: Context[Y,YA,YC]](rel: BRel[X,XA,Y,YA,XC,YC]) extends SSeq[(X,Y),(XA,YA),(XC,YC)] {
    def seq = sys.error("no")
  }  */

  type ST[@specialized(Int,Long,Double) X] = Statistics[X]
  type DC[@specialized(Int,Long,Double) X] = OrderingTreeContext[X,ST[X]]
  type RR[@specialized(Int,Long,Double) X] = SSeq[X,Statistics[X],DC[X]]
  type ORD[@specialized(Int,Long,Double) X] = Ordering[X]


  case class BinRel[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y]
  (left: RR[X],right: RR[Y],xc: DC[X],yc: DC[Y])
    extends BRel[X,ST[X],Y,ST[Y],DC[X],DC[Y]] {
    type R = BinRel[X,Y]

    { assert(left.size == right.size) }

    implicit def xcontext = xc
    implicit def ycontext = yc
    def create(left: RR[X],right: RR[Y]): R = BinRel(left,right,xc,yc)
    def appendAny(r: BinRel[_,_]) = append(r.asInstanceOf[BinRel[X,Y]])
  }

  def createRel[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) Y: ClassTag]
  (x: Array[X],y: Array[Y])(implicit ordx: ORD[X],ordy: ORD[Y],xc: DC[X],yc: DC[Y]) = {
    val xx = seqArray2[X,Statistics[X],DC[X]](x)
    val yy = seqArray2[Y,Statistics[Y],DC[Y]](y)
    BinRel(xx,yy,xc,yc)
  }

  def createRel[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) Y: ClassTag]
  (x: Seq[(X,Y)])(implicit ordx: ORD[X],ordy: ORD[Y],xc: DC[X],yc: DC[Y]): BinRel[X,Y] = {
    createRel(x.map(_._1).toArray,x.map(_._2).toArray)
  }

  sealed trait ColumnPos
  object LeftCol extends ColumnPos
  object RightCol extends ColumnPos

  sealed trait RelCol[X] {
    def id: Symbol
    def column: ColumnPos
  }

  case class LeftCol[Y](id: Symbol) extends RelCol[Y] {
    def column = LeftCol
    override def toString: String = id + ".L"
  }

  case class RightCol[X](id: Symbol) extends RelCol[X] {
    def column = RightCol
    override def toString: String = id + ".R"
  }

  sealed trait RCol[X] {
    def rel: BinRel[_,_]
    def column: ColumnPos
    def withID(s: Symbol): RelCol[X]
  }

  case class RightRCol[Y](r: BinRel[_,Y]) extends RCol[Y] {
    def rel: BinRel[_,_] = r
    def column = RightCol
    def withID(s: Symbol): RelCol[Y] = RightCol(s)
  }

  case class LeftRCol[X](r: BinRel[X,_]) extends RCol[X] {
    def rel: BinRel[_,_] = r
    def column = LeftCol
    def withID(s: Symbol): RelCol[X] = LeftCol(s)
  }

  final def main(args: Array[String]): Unit = {
    val a = createRel(Seq(
      (2,7),
      (3,6),
      (4,5),
      (5,4),
      (6,3),
      (7,2),
      (8,1),
      (9,0)
    ))

    val b = createRel(Seq(
      (2,7),
      (3,6),
      (4,5),
      (5,4),
      (6,3),
      (7,2),
      (8,1),
      (9,0)
    ))

    val c = a.append(b)

    println("c: " + c)
  }
}