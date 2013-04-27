package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

object Test {

  def main(args: Array[String]): Unit = {
    import Engine_v2._
    import Parser._

    val i1 = "{1,2} {3,4} +"
    val i2 = "[a`={b`:c`}] [a`={d`:c`}] + [b=1,d=2] ! $"

    val e1 = SpreadParser.doParse(i1).get.toSpread

    //val e2 = SpreadParser.doParse(i2).get.toSpread

    println(e1.asString)
    println(e1.reduce.asString)
    /*println(e2.asString)
    println(e2.reduce.asString) */
  }
}

/*

 add:      [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] + => [a={5:1,2:2},b={2:2,3},c=1]
 multiply: [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] * => [a={6:1,2},b=2,c=.]
 maximum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] | => [a={3:1,2},b=2,c=1]
 minimum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] & => [a={2:1,2},b=2,c=.]

 bind: [a=b'3,c=d'4] [b={1,2},d=3] ^^  => [a=b'{1,2},c=d'3]
 bind-add: [a=b'2,c=d'3] [b={1,2},d=3] ^*  =>

 desolve:  [a=1,b=2,c=d'3] >               => {a'1,b'2,c'(d'3)}
 capture:  {a'1,a'2,1 b'3 +,c'(d'3)} <     => [a={1,2},b=3]


 */