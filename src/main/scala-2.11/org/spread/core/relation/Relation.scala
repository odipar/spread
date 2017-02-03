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



  /*def createRelArray[ X: ClassTag, Y: ClassTag]
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
  }    */
}