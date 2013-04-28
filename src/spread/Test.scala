package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

/*

GOAL SET OF OPERATORS

UNARY:
 wipe:     [x'a=b'2] #                     => [a=2]
 turn:     [a=b,c=d] \                     => [b=a,d=b]
 pack:     {a'1,a'2,1 b'3 +,c'(d'3)} <     => [a={1,2},b=3]
 unpack:   [a=1,b=2,c=d'3] >               => {a'1,b'2,c'(d'3)}
 solve:    [a=x`,x=b` c` +,b=1,c=1] @      => [a=b'1]
 unsolve:  [a=x'(b'1 c'2 +)] ^             => [a=x`,x=b` c` +,b=1,c=1]

BINARY:
 add:      [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] + => [a={5:1,2:2},b={2:2,3},c=1]
 multiply: [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] * => [a={6:1,2},b=2,c=.]
 maximum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] | => [a={3:1,2},b=2,c=1]
 minimum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] & => [a={2:1,2},b=2,c=.]
 negate:   [a={2:1,2},b=2] -                         => [a={_2:1,_1:2}
 bind:     [a=b'3,c=d'4] [b={1,2},d=3] !             => [a=b'{1,2},c=d'3]
 iterate:  [a=x` x`] [x=1,x=2] ~                     => [a={x'1 x'2,x'2 x'2}]
 match:    2010.x`.x` 2010.10.10 ?                   => [[x=10]] <-- VERY HARD TO IMPLEMENT!

 */

object Test {

  def main(args: Array[String]): Unit = {
    import Engine_v2._
    import Parser._

    val i1 = "[a=x` y` +] [x=a`,x=b,y=1,y=2] ~ # [a=2] !"
    val i2 = "[a`={b`:c`}] [a`={d`:c`}] + [b=1,d=2] ! $"

    val e1 = SpreadParser.doParse(i1).get.toSpread

    println(e1.asString)
    println(e1.reduce.asString)
  }
}
