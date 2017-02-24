package org.spread.core.language
import scala.Specializable._

// Here we can turn on/off scala specialization for all classes

object Annotation {
  class sp() extends scala.specialized()
  //class sp() extends scala.annotation.StaticAnnotation
}
