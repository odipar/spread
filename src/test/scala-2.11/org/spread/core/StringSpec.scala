package org.spread.core

/**
  * Created by rapido on 10/01/17.
  */
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

case class SPair(a: String, b: String)

object StringSpecification extends Properties("SPair") {

  val spairGen: Gen[SPair] = for {
    ag <- Gen.alphaStr
    bg <- Gen.alphaStr
  } yield SPair(
    a = ag,
    b = bg
  )
  implicit  val  arbitraryFoo:  Arbitrary[SPair]  =  Arbitrary(spairGen)

  property("startsWith") = forAll { p: SPair =>
    (p.a+p.b).startsWith(p.a)
  }

  property("concatenate") = forAll { p: SPair =>
    (p.a+p.b).length > p.a.length && (p.a+p.b).length > p.b.length
  }

  property("substring") = forAll { (p: SPair, c: String) =>
    (p.a+p.b+c).substring(p.a.length, p.a.length+p.b.length) == p.b
  }

  property("t1") = forAll { l: List[Int] =>
    l.distinct.distinct == l.distinct
  }
}
