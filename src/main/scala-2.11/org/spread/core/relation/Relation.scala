package org.spread.core.relation

import org.spread.core.sequence.Sequence._
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
  XC <: Context[X,XA,XC],
  YC <: Context[Y,YA,YC]] {
    type R <: BRel[X,XA,Y,YA,XC,YC]

    implicit def xcontext: XC
    implicit def ycontext: YC
    def create(left: BSeq[X,XA,XC],right: BSeq[Y,YA,YC]): R

    def left: BSeq[X,XA,XC]
    def right: BSeq[Y,YA,YC]

    def size = left.size
    def append(r: R): R = create(left.append(r.left),right.append(r.right))
    def split(i: Long): (R,R) = {
      val (ll,lr) = left.split(i)
      val (rl,rr) = right.split(i)
      (create(ll,rl),create(lr,rr))
    }
    def empty = create(xcontext.empty,ycontext.empty)
    def annotationRange(s: Long, e: Long): (XA,YA) = (left.annotationRange(s,e),right.annotationRange(s,e))
    override def toString = left.toString + "||" + right.toString
  }

  type ST[@specialized(Int,Long,Double) X] = Statistics[X]
  type DC[@specialized(Int,Long,Double) X] = ContextImpl[X,ST[X]]
  type RR[@specialized(Int,Long,Double) X] = BSeq[X,Statistics[X],DC[X]]
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
    val xx = seqArray[X,Statistics[X],DC[X]](x)
    val yy = seqArray[Y,Statistics[Y],DC[Y]](y)
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