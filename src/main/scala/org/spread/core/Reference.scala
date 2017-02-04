package org.spread.core

/**
  * Created by rapido on 31/01/16.
  */
object Reference {

  trait Ref[X]

  trait RefResolver {
    def resolve[X](r: Ref[X]): Ref[X]
  }
}
