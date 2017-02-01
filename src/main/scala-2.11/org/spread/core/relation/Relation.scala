package org.spread.core.relation

import org.spread.core.sequence.Sequence.{Context, OrderingContext, _}
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
  type DC[@specialized(Int,Long,Double) X, C <: OrderingContext[X,ST[X],C]] = OrderingContext[X,ST[X],C]
  type DC2[@specialized(Int,Long,Double) X] = OrderingTreeContext[X,ST[X]]
  type RR[@specialized(Int,Long,Double) X, C <: OrderingContext[X,ST[X],C]] = SSeq[X,ST[X],C]
  type ORD[@specialized(Int,Long,Double) X] = Ordering[X]
  type CORD[X,C <: CORD[X,C]] = OrderingContext[X,Statistics[X],C]

  /* WRAP BRel */

  case class BinRel[X, Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]]
  (left: RR[X,XC],right: RR[Y,YC],xc: DC[X,XC],yc: DC[Y,YC])
    extends BRel[X,ST[X],Y,ST[Y],XC,YC] {
    type R = BinRel[X,Y,XC,YC]

    { assert(left.size == right.size) }

    implicit def xcontext = xc
    implicit def ycontext = yc
    def create(left: RR[X,XC],right: RR[Y,YC]): R = BinRel(left,right,xc,yc)
    def appendAny(r: BinRel[_,_,_,_]) = append(r.asInstanceOf[BinRel[X,Y,XC,YC]])
  }

  def createRel[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) Y: ClassTag]
  (x: Array[X],y: Array[Y])(implicit ordx: ORD[X],ordy: ORD[Y],xc: DC2[X],yc: DC2[Y]): BinRel[X,Y,DC2[X],DC2[Y]]  = {
    val xx = seqArray2[X,Statistics[X],DC2[X]](x)
    val yy = seqArray2[Y,Statistics[Y],DC2[Y]](y)
    BinRel(xx,yy,xc,yc)
  }

  def createRel[@specialized(Int,Long,Double) X: ClassTag,@specialized(Int,Long,Double) Y: ClassTag]
  (x: Seq[(X,Y)])(implicit ordx: ORD[X],ordy: ORD[Y],xc: DC2[X],yc: DC2[Y]): BinRel[X,Y,DC2[X],DC2[Y]] = {
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
    def rel: BinRel[_,_,_,_]
    def column: ColumnPos
    def withID(s: Symbol): RelCol[X]
  }

  case class RightRCol[X, Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]](r: BinRel[X,Y,XC,YC]) extends RCol[Y] {
    def rel: BinRel[_,_,_,_] = r
    def column = RightCol
    def withID(s: Symbol): RelCol[Y] = RightCol(s)
  }

  case class LeftRCol[X, Y, XC <: CORD[X,XC], YC <: CORD[Y,YC]](r: BinRel[X,Y,XC,YC]) extends RCol[X] {
    def rel: BinRel[_,_,_,_] = r
    def column = LeftCol
    def withID(s: Symbol): RelCol[X] = LeftCol(s)
  }

  type AnyRel = BinRel[_,_,_,_]

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