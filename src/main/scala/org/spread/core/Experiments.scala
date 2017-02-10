package org.spread.core

/**
  * Created by rapido on 10/02/17.
  */
object Experiments {

  trait MyContext[+X]
  trait MyContextImpl[+X] extends MyContext[X]
  
  trait MySeq[+X,+S <: MySeq[X,S]] {
    type CT <: MyContext[X]
    def append[X2 >: X, S2 >: S <: MySeq[X2,S2]](o: MySeq[X2,S2]): MySeq[X2,S2] = MySeqPair(this,o)
  }
  
  case class MySeqImpl[X](x: X) extends MySeq[X,MySeqImpl[X]] {
    type CT <: MyContextImpl[X]
  }
  case class MySeqPair[X1,S1 <: MySeq[X1,S1], X2 >: X1, S2 >: S1 <: MySeq[X2,S2]](s1: MySeq[X1,S1], s2: MySeq[X2,S2])
    extends MySeq[X2,S2] {
    type CT = MyContextImpl[X2]
  }

  type MS[+X, +S <: MySeq[X,S], +C] = MySeq[X,S] { type CT <: C }
  type MSEQ[+X, +S <: MySeq[X,S]] = MS[X,S,C forSome { type C <: MyContextImpl[X] } ]

  def union[X1,S1 <: MSEQ[X1,S1], X2 >: X1, S2 >: S1 <: MSEQ[X2,S2]](s1: MSEQ[X1,S1], s2: MSEQ[X2,S2]) = {
    s2.append(s1)
  }

  final def main(args: Array[String]): Unit = {
    val a = MySeqImpl(1)
    val b = MySeqImpl(2.0)
    val c = a.append(b)
    val d = b.append(a)

    //val u1 = union(a,b)
    val u2 = union(a,b)

  } 
}