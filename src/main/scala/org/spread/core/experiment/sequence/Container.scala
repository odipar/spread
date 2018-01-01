package org.spread.core.experiment.sequence

object Container {

  import org.spread.core.language.Annotation.sp

  trait Container[@sp X, C <: Container[X, C]] {
    type SS <: ArrayContainer[X, SS]
    
    def size: Long
    def height: Int
    def parts: Array[C]
    def whole: SS
  }

  trait ArrayContainer[@sp X, C <: ArrayContainer[X, C]] {
    def size: Int
    def apply(x: Int): X
    def toArray: Array[X]
  }
}
