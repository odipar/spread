package org.spread.core.experiment.sequence

import org.spread.core.experiment.sequence.Storage.Storable

object Container {

  import org.spread.core.language.Annotation.sp

  trait Container[@sp X, C <: Container[X, C]] extends Storable[C] {
    type SS <: ArrayContainer[X, SS]

    def size: Long
    def height: Int
    def parts: Array[C]
    def whole: SS

    def store: C
  }

  trait ArrayContainer[@sp X, C <: ArrayContainer[X, C]] {
    def size: Int
    def apply(x: Int): X
    def toArray: Array[X]
  }
}
