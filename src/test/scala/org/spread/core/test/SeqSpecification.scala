package org.spread.core.test

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.spread.core.annotation.Annotation._
import org.spread.core.constraint.Constraint._
import org.spread.core.sequence.AnnotatedTreeSequence._
import org.spread.core.sequence.RangedSequence._
import scala.language.{existentials, implicitConversions, postfixOps}
import spire.implicits._

object SeqSpecification extends Properties("Seq") {
  type SSEQ = AnnTreeSeq[Long,Statistics[Long]]

  val factory = seqFactory[Long]
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

      val l = min(a1.lowerBound, a2.lowerBound)
      val h = max(a1.upperBound, a2.upperBound)
      (l == ca.lowerBound) && (h == ca.upperBound)
    }
    else true
  }

  def min(l1: Long, l2: Long) = if (l1 < l2) l1 ; else l2
  def max(l1: Long, l2: Long) = if (l1 > l2) l1 ; else l2

  property("multiSet") = forAll { (p: SSEQ) =>

    val s = p.sort
    val u = s union s
    val d = s difference u

    s.equalTo(d)
  }
}