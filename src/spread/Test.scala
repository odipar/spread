package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

object Test {

  def main(args: Array[String]): Unit = {
    import Engine_v2._
    import Parser._

    val i1 = "[c=((a'2 1 +)'d)'. 1 +] [(a'2 1 +)'d=2] ! $ [a=3] !"

    val e1 = SpreadParser.doParse(i1).get.toSpread

    println(e1.asString)
    println(e1.reduce.asString)
  }
}

  /*
[b'=b'] [b=1,b=2] ^ => [b'{1,2}=b'{1,2}] => [b'1=b'1,b'1=b'2,b'2=b'2]
[b'=b'] [b=1,b=2] ! => [b'1=b'1,b'2=b'2]

[b=a,d=a] => [a={b,d}]

[y=x' 2 +] [x={0,1,2,3,4,5}] */