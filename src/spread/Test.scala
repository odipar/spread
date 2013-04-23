package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

object Test {

  def main(args: Array[String]): Unit = {
    import Engine_v2._

    val i1 = EInt(1,1)
    val i2 = EInt(2,1)
    val a12: MultiSetExpr = createAlt(i1) combine createAlt(i2)

    val i3 = EInt(9,1)
    val i4 = EPair(ESymbol("a",1),a12,1)
    val a34 = createAlt(i3) combine createAlt(i4)

    val add = EAdd(a12,a34,1)

    println(add.labels.asString)
    //println(add.reduce.asString)

   /* val i3 = EInt(1,1)
    val i4 = EInt(2,1)
    val a34 = createAlt(i3) combine createAlt(i4)

    val add = EAdd(a12,a34,1)
    println(add.asString)
    println(add.reduce.asString)       */
  }
}