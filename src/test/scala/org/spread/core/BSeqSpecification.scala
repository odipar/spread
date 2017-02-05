package org.spread.core

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll
import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.Sequence._
import org.scalacheck.Prop.BooleanOperators
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.RangedSequence._

object BSeqSpecification extends Properties("BSeq") {
  type SSEQ = AnnTreeSeq[Long,Statistics[Long]]

  val factory = EmptyLongTreeSeq()
  implicit def arbitraryIntSeq: Arbitrary[SSEQ] = Arbitrary(longSeq)

  def longSeq: Gen[SSEQ] = for {
    l <- Gen.choose(0, 1000)
    a <- Gen.listOfN(l,arbitrary[Int])
  } yield factory.createSeq(a.toArray.map(_.toLong))

  property("appendSize") = forAll { (p1: SSEQ,p2: SSEQ) =>
    p1.append(p2).size == p2.append(p1).size
  }

  property("split") = forAll { (p: SSEQ,index: Long) =>
    val i = (index % (p.size+1))/2
    val (l1,r1) = p.split(i)  // random split
    l1.append(r1).equalTo(p)
  }

  property("statsBounds") = forAll { (p1: SSEQ,p2: SSEQ) =>
    val c = p1.append(p2)
    val ca = c.annotation
    val a1 = c.annotation

      ((p1.size > 0) ==> (a1.first == ca.first)) &&
        ((p1.size > 0) ==>  (a1.last == ca.last))
  }

  property("statsMinMax") = forAll { (p1: SSEQ,p2: SSEQ) =>
    if ((p1.size > 0) && (p2.size > 0)) {
      val c = p1.append(p2)

      val ca = c.annotation
      val a1 = p1.annotation
      val a2 = p2.annotation

      val l = a1.lowerBound min a2.lowerBound
      val h = a1.upperBound max a2.upperBound
      (l == ca.lowerBound) && (h == ca.upperBound)
    }
    else true
  }

  property("multiSet") = forAll { (p: SSEQ) =>
    import org.spread.core.algorithm.Combine._

    val s = sort(p)
    val u = union(s,s)
    val d = difference(s,u)

    s.equalTo(d)
  }
}