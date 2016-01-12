package spread

import java.io.OutputStream
import java.lang.ref.WeakReference
import spread.IncrementalMemoization.Expr
import scala.collection.mutable.WeakHashMap

/*
 Copyright 2014: Robbert van Dalen
 */

object Test {

  import scala.language.implicitConversions
  import IncrementalMemoization._
  import IncrementalArithmetic._
  import scala.xml._

  object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (~i < 2) !('a~1)
      else fac(~i - 1) * !i
    }
    override def toString = "fac"
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I) = {
      if (~i < 2) i
      else %(fib,i - 1) + %(fib,i - 2)
    }
    override def toString = "fib"
  }

  object g extends (I => I) with Func {
    def apply(x: I) = 2 ** x
  }

  object h extends (I => I) with Func {
    def apply(x: I) = x ** x
  }

  object f extends ((I,I) => I) with Func {
    def apply(x: I,y: I) = %(g,x) + %(h,y)
  }

  final def main(args: Array[String]): Unit = {

    //val e = (1 ++ (2 ++ (3 ++ (4 ++ (5 ++ (6 ++ (7 ++ (8 ++ (9 ++ (1 ++ (2 ++ (3 ++ (4 ++ (5 ++ (6 ++ (7 ++ (8 ++ 9)))))))))))))))))
    //val e1 = (((1 ++ 2) ++ (3 ++ 4)) ++ ((6 ++ 7) ++ (8 ++ 9))) ++ (((1 ++ 2) ++ (3 ++ 4)) ++ ((6 ++ 7) ++ (8 ++ 9)))
    val e1 = (1 ++ 2) ** (3 ++ 'a~4)
    val e = %(fac,5)
    //    println("o5: " + TT.o5)

    val (c,r) = fullRed(CM(Map()),e)
    //val (c2,r2) = fullRed(cm,removeQuotes(r))
    println("r: " + r)
    println("r.depth: " + r.depth)
    println("c: " + c)


    val (c1,r1) = fullRed(cm,e1)
    val c2 = backref(cm,e1)
    println("e1: " + e1.depth)
    println("r1: " + r1)
    println("r1.depth: " + r1.depth)
    println("c2: " + c2)

    /*

     (1 ++ 2) ** (3 ++ 4)
       1 ++ 2
       => 3
       3 ++ 4
       => 7
     3 * 7
     => 21
     */

    val s = toIds(r1)
    val xml = toNodes(s)

    val xmls: String = xml.toString

    var o = new java.io.FileWriter("spread.xml")
    o.write(xmls)
    o.close


    import javax.swing.tree._
    import javax.swing._
    import java.awt._
    import IncrementalTreeView._

    def createTView(t: MyTreeNode): JComponent = {
      val model1 = new DefaultTreeModel(t)

      var tree1 = new JTree(model1)
      tree1.setShowsRootHandles(true)
      tree1.setPreferredSize(new Dimension(600, 600))
      tree1.setMinimumSize(new Dimension(10, 10))
      tree1
    }

    /*val tree1 = createTView(GNode(null,e,cm))

    var f = new JFrame()
    f.setLayout(new BorderLayout)
    f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

    f.add(tree1, BorderLayout.CENTER)
    f.pack()
    f.setVisible(true)     */

    //println("r: " + r)
  }



  case class IDMap(id: Long, depth: Map[Any,Int], map: Map[Any,Long]) {
    def contains(v: Any) = map.contains(v)
    def put(e: Any, d: Int): IDMap = {
      depth.get(e) match {
        case None => IDMap(id + 1, depth + (e -> d), map + (e -> id))
        case Some(dd) => {
          if (dd > d) IDMap(id + 1, depth + (e -> d), map + (e -> id))
          else  IDMap(id + 1, depth, map + (e -> id))
        }
      }

    }
  }

  def ids(mm: IDMap, e: Any, depth: Int): IDMap = {
    val d = depth + 1
    if (mm.contains(e)) mm
    else {
      val m = mm.put(e,d)
      e match {
        case t: Trace[_] => {
          ids(ids(m,t.from,d),getTo(0,t)._2,d)
        }
        case f1: F1[_,_] => {
          ids(m,f1.a,d)
        }
        case f2: F2[_,_,_] => {
          ids(ids(m,f2.a,d),f2.b,d)
        }
        case f3: F3[_,_,_,_] => {
          ids(ids(ids(m,f3.a,d),f3.b,d),f3.c,d)
        }
        case _ => m
      }
    }
  }

  def toIds(v: Any): IDMap = {
    ids(IDMap(1,Map(),Map()),v,0)
  }

  def toID(i: Any)(implicit m: IDMap): String = {
    m.map(i).toString
  }

  def toNodes(implicit m: IDMap): Node = {
    val mm = m.map
    val dd = m.depth
    var groups: scala.collection.immutable.SortedSet[Int] =  scala.collection.immutable.SortedSet()

    var ss: NodeSeq = <start></start>
    for (i <- mm) {
      var d: Int = -dd(i._1)
      groups = groups + d
      val n = {
        <node id={i._2.toString} group={d.toString}>
          {
          i._1 match {
            case t: Trace[_] => {
              <trace>
                <from id={toID(t.from)}/>
                <to id={toID(getTo(0,t)._2)}/>
              </trace>
            }
            case f1: F1[_,_] => {
              <lazy1>
                <function name={f1.f.toString}/>
                <arg1 id={toID(f1.a)}/>
              </lazy1>
            }
            case f2: F2[_,_,_] => {
              <lazy2>
                <function name={f2.f.toString}/>
                <arg1 id={toID(f2.a)}/>
                <arg2 id={toID(f2.b)}/>
              </lazy2>            }
            case f3: F3[_,_,_,_] => {
              <lazy3>
                <function name={f3.f.toString}/>
                <arg1 id={toID(f3.a)}/>
                <arg2 id={toID(f3.b)}/>
                <arg3 id={toID(f3.c)}/>
              </lazy3>
            }
            case x => {
                <content value={x.toString}/>
            }
          }
          }
        </node>
      }
      ss = ss :+ n
    }
    for (i <- groups) {
      val n = { <group id={i.toString} gid={(i+1).toString} /> }
      ss = ss :+ n
    }
    <result>{ss}</result>
  }


  object IncrementalTreeView {

    import javax.swing.tree.TreeNode
    import spread.IncrementalMemoization._
    import scala.language.existentials

    trait MyTreeNode extends TreeNode {
      def context: Context
      def parent: TreeNode
      def childs: Vector[TreeNode]
      def children = scala.collection.JavaConversions.asJavaEnumeration(childs.iterator)
      def getAllowsChildren = true
      def getChildAt(childIndex: Int): TreeNode = childs(childIndex)
      def getChildCount: Int = childs.size
      def getIndex(node: TreeNode): Int = sys.error("no")
      def getParent: TreeNode = parent
    }

    def expand(t: TreeNode): Unit = {
      var c = t.getChildCount
      var i = 0
      while (i < c) {
        expand(t.getChildAt(i)); i = i + 1
      }
    }

    case class GNode(parent: TreeNode,t: Any, context: Context) extends MyTreeNode {
      lazy val childs: Vector[TreeNode] = {
        val c = context

        t match {
          case i: Var[_] => {
            Vector[TreeNode](INode(this,i,c))
          }
          case i: Trace[_] => {
            Vector[TreeNode](GNode(this,i.to,c), GNode(this,i.from,c),INode(this,i,c))
          }
          case v: F1[_,_] => Vector[TreeNode](GNode(this,v.f,c),GNode(this,v.a,c),INode(this,v,c))
          case v: F2[_,_,_] => Vector[TreeNode](GNode(this,v.f,c),GNode(this,v.a,c),GNode(this,v.b,c),INode(this,v,c))
          case v: F3[_,_,_,_] => Vector[TreeNode](GNode(this,v.f,c),GNode(this,v.a,c),GNode(this,v.b,c),GNode(this,v.c,c),INode(this,v,c))
          case _ => {
            Vector[TreeNode]()
          }
        }
      }
      def isLeaf: Boolean = childs.size == 0
      override def toString = t.toString
    }

    case class INode(parent: TreeNode,t: Expr[_], context: Context) extends MyTreeNode {
      lazy val childs: Vector[TreeNode] = {
        val (cc,e: Expr[_]) = eval(context,t)

        if (e == t) Vector()
        else Vector[TreeNode](GNode(this,eval(context,t)._2,context))
      }
      def isLeaf: Boolean = childs.size == 0
      override def toString = {
        if (isLeaf) getTo(0,t)._2.toString
        else "EVAL"
      }
    }

  }

}
