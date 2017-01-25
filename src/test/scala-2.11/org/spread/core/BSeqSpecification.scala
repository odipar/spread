package org.spread.core

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{sized, listOfN}
import org.scalacheck.Prop.forAll
import org.spread.core.annotation.Annotation.Statistics
import org.spread.core.sequence.Sequence._
import org.scalacheck.Prop.BooleanOperators

object BSeqSpecification extends Properties("BSeq") {
  type BSEQ[X] = BSeq[X,Statistics[X],ContextImpl[X,Statistics[X]]]

  implicit def arbitraryIntSeq: Arbitrary[BSEQ[Long]] = Arbitrary(longSeq)

  def longSeq: Gen[BSEQ[Long]] = for {
    l <- Gen.choose(0, 1000)
    a <- Gen.listOfN(l,arbitrary[Int])
  } yield seqArray(a.toArray.map(_.toLong))

  property("appendSize") = forAll { (p1: BSEQ[Long], p2: BSEQ[Long]) =>
    p1.append(p2).size == p2.append(p1).size
  }

  property("split") = forAll { (p: BSEQ[Long], index: Long) =>
    val i = (index % (p.size+1))/2
    val (l1,r1) = p.split(i)  // random split
    l1.append(r1).equalTo(p)
  }

  property("statsBounds") = forAll { (p1: BSEQ[Long], p2: BSEQ[Long]) =>
    val c = p1.append(p2)

      ((p1.size > 0) ==> (p1.annotation.first == c.annotation.first)) &&
        ((p2.size > 0) ==>  (p2.annotation.last == c.annotation.last))
  }

  property("statsMinMax") = forAll { (p1: BSEQ[Long], p2: BSEQ[Long]) =>
    val c = p1.append(p2)

    if ((p1.size > 0) && (p2.size > 0)) {
      val l = p1.annotation.lowerBound min p2.annotation.lowerBound
      val h = p1.annotation.upperBound max p2.annotation.upperBound
      (l == c.annotation.lowerBound) && (h == c.annotation.upperBound)
    }
    else true
  }
}