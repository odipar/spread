package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

object Test {

  def main(args: Array[String]): Unit = {
    import Engine_v2._

    val e = createMap(EPair(ESymbol("y",1),EAdd(EPair(ESymbol("x",1),EExpr,1),EInt(3,1),1),1))
    val m1: MultiMapExpr = createMap(EPair(ESymbol("x",1),EInt(1,1),1))
    val m2: MultiMapExpr = createMap(EPair(ESymbol("x",1),EInt(2,2),1))

    val e0 = EBind(EEMap(e,1),EEMap(m1,1),1)
    val e1 = ERed(EBind(EEMap(e,1),EEMap(m1,1),1),1)
    val e2 = ERed(EBind(EEMap(e,1),EEMap(m2,1),1),1)
    val e3 = ERed(EBind(EEMap(e,1),EEMap(m1 combine m2,1),1),1)

    println(e1.reduce.asString)
    println(e2.reduce.asString)
    println(e3.reduce.asString)

  }
}