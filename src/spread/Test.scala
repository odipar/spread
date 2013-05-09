package spread

import scala.language.implicitConversions
import spread.Engine_v2._
import spread.Engine_v2.EInt

/*

a'      unbound label
a'b     bound label
"h"     char/string

sequence and concat
[a=b,b=c];[d=e,f=g] == b.c;e.g

[0=a,1=b,2=c]       == a.b.c
"h"."e"."l"."l"."o" == "hello"
a.b;c.d             == a.b.c.d
[a];c.d             == a.c.d


GOAL SET OF OPERATORS

UNARY:
 reduce:    [1 2 +] $                       => [(1 2 +,3)]
 wipe:      [(x'a,a)] #                     => [a=2]
 turn:      [a=b,c=d] %                     => [b=a,d=b]
 pack:      {1,2,3,4} <                     => 1.2.3.4
 unpack:    [a=1,b=2,c=d'3] >               => {a'1,b'2,c'(d'3)}
 foreach:   [1,2,3,4] 1 +@                  => [2,3,4,5]
 fold:      [1,2,3,4] . +                   => 10
 rfold      . [1.2.3.4] +                   => 1.3.6.10

REFLECTION:
 backquery: [(1 2 +,3)] /t                   => [(1 2 +).3]
 bindings:  [a=x'(b'1 c'2 +)] /b             => [a=x`,x=b` c` +,b=1,c=1]
 labels:

 // [1,2,3] <\~\ => [[0=1],[0=2],[0=3]]

x' unbound
x'1 bound

BINARY:
 add:      [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] + => [a={5:1,2:2},b={2:2,3},c=1]
 subtract: [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] - => ?
 multiply: [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] * => [a={6:1,2},b=2,c=.]
 maximum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] | => [a={3:1,2},b=2,c=1]
 minimum:  [a={2:1,2},b=2] [a={3:1,2},b={2,3},c=1] & => [a={2:1,2},b=2,c=.]
 bind:     [a=b'3,c=d'4] [b={1,2},d=3] !             => [a=b'{1,2},c=d'3]
 match:    2010.x`.x` 2010.10.10 ?                   => [[x=10]] <-- VERY HARD TO IMPLEMENT!

  ?+ (best match)
  ?* (all matches)
  ?& (first minimum match)
  ?| (first maximum match)
     etc. etc

 concat:   1.2;3.4                                   =>  1.2.3.4
           "hello";"world"                           =>   "hello world"

           "hello ";{"world","moon"}                 => {"hello world","hello moon"}

           ["hello world"] [x`;"world"] ?

 [[a=1,b=2,c=3] x';[c=3] ?] = [[x=0],[x=[a=1]],[x=[a=1,a=2]],x=]   etc
 [0={6:0}];[0={7:0}] == [0={6:0},1={7:0}] == {6:0}.{7:0}

 DESIGN TO BE DECIDED:
  Implement matching with leading and trailing?

  a.b.x.d.e.f x;[2=x];y ? == [[x=a.b,y=[3=d,4=e,5=f]]]

 */

object Test {
  import java.io._
  import javax.swing._
  import javax.swing.text._
  import java.awt._
  import java.awt.event._

  final def main(args: Array[String]): Unit = {}
  {
    var f = new EnchiladaFrame
    f.setVisible(true)
  }

  class EnchiladaFrame extends JFrame
  {
    var textPane = SpreadREPL
    var scrollPane = new JScrollPane(textPane)

    {
      setLayout(new BorderLayout)
      setSize(640, 400);
      getContentPane().add(scrollPane,BorderLayout.CENTER)
    }
  }
}
