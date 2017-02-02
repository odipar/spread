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

  trait BRel[X,XA,Y,YA,XC <: OrderingContext[X,XA,XC],YC <: OrderingContext[Y,YA,YC]] {
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
    def annotation: (XA,YA) = (left.annotation,right.annotation)
    override def toString = left.toString + "_" + right.toString
  }

  type ST[ X] = Statistics[X]
  type OTC[ X] = OrderingTreeContext[X,ST[X]]
  type SSEQ[ X, C <: OrderingContext[X,ST[X],C]] = SSeq[X,ST[X],C]
  type ORD[ X] = Ordering[X]
  type CORD[ X,C <: CORD[X,C]] = OrderingContext[X,Statistics[X],C]

  // type wizardry: correct existentially typed BinRel, similar to BinRel[_,_,_,_]

  type EREL = BinRel[X,XA,Y,YA,XC,YC] forSome {
    type X ; type XA; type Y; type YA; type XC <: OrderingContext[X,XA,XC] ; type YC <: OrderingContext[Y,YA,YC]
  }

  case class BinRel[X,XA,Y,YA,XC <: OrderingContext[X,XA,XC],YC <: OrderingContext[Y,YA,YC]]
  (left: SSeq[X,XA,XC],right: SSeq[Y,YA,YC],xc: XC,yc: YC)
  extends BRel[X,XA,Y,YA,XC,YC] {
    type R = BinRel[X,XA,Y,YA,XC,YC]

    { assert(left.size == right.size) }

    implicit def xcontext = xc
    implicit def ycontext = yc
    implicit def xord = xc.ordering
    implicit def yord = yc.ordering
    def create(left: SSeq[X,XA,XC],right: SSeq[Y,YA,YC]): R = BinRel(left,right,xc,yc)
  }

  def createRelArray[ X: ClassTag, Y: ClassTag]
  (x: Array[X],y: Array[Y])(implicit ordx: ORD[X],ordy: ORD[Y],xc: OTC[X],yc: OTC[Y])  = {
    val xx = seqArray2[X,Statistics[X],OTC[X]](x)
    val yy = seqArray2[Y,Statistics[Y],OTC[Y]](y)
    BinRel(xx,yy,xc,yc)
  }

  def createRel[ X: ClassTag, Y: ClassTag]
  (x: Seq[(X,Y)])(implicit ordx: ORD[X],ordy: ORD[Y],xc: OTC[X],yc: OTC[Y]) = {
    createRelArray(x.map(_._1).toArray,x.map(_._2).toArray)
  }

  sealed trait ColumnPos
  object LeftCol extends ColumnPos
  object RightCol extends ColumnPos

  sealed trait RelCol[X,XA] {
    def id: Symbol
    def column: ColumnPos
  }

  case class LeftCol[Y,YA](id: Symbol) extends RelCol[Y,YA] {
    def column = LeftCol
    override def toString: String = id + ".L"
  }

  case class RightCol[X,XA](id: Symbol) extends RelCol[X,XA] {
    def column = RightCol
    override def toString: String = id + ".R"
  }

  sealed trait RCol[X,XA] {
    def rel: EREL
    def column: ColumnPos
    def withID(s: Symbol): RelCol[X,XA]
  }

  case class RightRCol[X,XA,Y,YA,XC <: OrderingContext[X,XA,XC],YC <: OrderingContext[Y,YA,YC]]
  (r: BinRel[X,XA,Y,YA,XC,YC]) extends RCol[Y,YA] {
    def rel: EREL = r
    def column = RightCol
    def withID(s: Symbol): RelCol[Y,YA] = RightCol(s)
  }

  case class LeftRCol[X,XA,Y,YA,XC <: OrderingContext[X,XA,XC],YC <: OrderingContext[Y,YA,YC]]
  (r: BinRel[X,XA,Y,YA,XC,YC]) extends RCol[X,XA] {
    def rel: EREL = r
    def column = LeftCol
    def withID(s: Symbol): RelCol[X,XA] = LeftCol(s)
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