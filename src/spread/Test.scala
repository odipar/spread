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
  import IncrementalTreapRelation._
  import IncrementalArithmetic._
  import scala.xml._

  object fac extends FA1[Int,Int] {
    def apply(i: F0I): I = {
      if (~i < 2) 1
      else %(fac,~i - 1) * i
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

    var e1: TreapRelation[Int,Int] = empty
    var e2 = e1

    var i1 = 0
    var i2 = 0

    while (i1 < 10) {
      i2 = 0
      while (i2 < 10) {

        e1 = e1.put(RTuple(i1+5,i2+5))
        e2 = e2.put(RTuple(i1,i2))
        i2 = i2 + 1
      }
      i1 = i1 + 1
    }

    val op:ICombinator[Int,Int] = ICombinator(Intersect())

    println(op(e1,e2))

    val e = %(fib,40)
    //val e = ('a~1 ++ 2) ** 'c~(3 ++ 4)
    //    println("o5: " + TT.o5)

    val (c,r) = fullRed(cm,e)
    println("r: " + r)
    val s = toIds(r)
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

    spread.Relation.Test.main

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

object Relation {
  import scala.language.existentials
  import scala.language.higherKinds
  import scala.language.implicitConversions

  trait RelationalAlgebra {
    import IncrementalTreapRelation.Tuple
    import IncrementalTreapRelation.NoneTuple
    import IncrementalTreapRelation.SomeTuple
    import IncrementalTreapRelation.RelOrdering

    type Rel[A,B] <: Relation[A,B]
    type RelOrd[A,B] <: RelOrdering[A,B]

    trait Relation[A,B] extends Split[A,B] {
      def self: Rel[A,B]

      def min: Tuple[A,B]
      def some: Tuple[A,B]
      def max: Tuple[A,B]

      def union(r: Rel[A,B])(implicit p: RelOrd[A,B]): Rel[A,B]
      def split(a: SomeTuple[A,B])(implicit p: RelOrd[A,B]): Split[A,B]

      def getMin(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B]
      def getMax(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B]

    }

    case class BinOps[A,B](o: Rel[A,B]) {
      def swap(implicit o1: RelOrd[B,A]): Rel[B,A] = swapImpl(o)
      def extend[C](f: B => C)(implicit o1: RelOrd[A,Rel[B,C]], o2: RelOrd[B,C]): Rel[A,Rel[B,C]] = extendImpl(o,f)
      def group[C](implicit p1: RelOrd[A,B], p2: RelOrd[A,Rel[A,B]]): Rel[A,Rel[A,B]] = groupImpl(o)
      def map[C,D](f: SomeTuple[A,B] => Rel[C,D])(implicit o1: RelOrd[A,B], o2: RelOrd[C,D]): Rel[C,D] = mapImpl(o,f)
      def count: Long = countImpl(o)

      //def ==(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) == 0
      def >(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) > 0
      def <(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) < 0
    }

    case class TerOps[A,B,C](o: Rel[A,Rel[B,C]]) {
      def drop(implicit o0: RelOrd[B,C],o1: RelOrd[A,C]): Rel[A,C] = dropImpl(o)
    }

    implicit def binOps[A,B](o: Rel[A,B]): BinOps[A,B] = BinOps(o)
    implicit def terOps[A,B,C](o: Rel[A,Rel[B,C]]): TerOps[A,B,C] = TerOps(o)

    trait Split[A,B] {
      def left: Rel[A,B]
      def middle: Rel[A,B]
      def right: Rel[A,B]
    }

    case class SplitImpl[A,B](left: Rel[A,B], middle: Rel[A,B], right: Rel[A,B]) extends Split[A,B]

    def emptyRel[A,B]: Rel[A,B]
    def create[A,B](a: A, b: B)(implicit p: RelOrd[A,B]): Rel[A,B]

    /*def difference[A,B](r1: Rel[A,B], r2: Rel[A,B]): Rel[A,B] = r2
    def intersect[A,B] (r1: Rel[A,B], r2: Rel[A,B]): Rel[A,B] = r1
      */

    def groupImpl[A,B]      (r: Rel[A,B])(implicit p1: RelOrd[A,B], p2: RelOrd[A,Rel[A,B]]): Rel[A,Rel[A,B]] = r.some match {
      case n: NoneTuple => emptyRel[A,Rel[A,B]]
      case s: SomeTuple[A,B] => {
        def min = r.getMin(s.first).asSome
        def max = r.getMax(s.first).asSome

        val smin = r.split(min)
        val smax = smin.right.split(max)
        val smiddle = smin.middle union smax.left union smax.middle

        val left = groupImpl(smin.left)
        val middle = create(s.first,smiddle)
        val right = groupImpl(smax.right)

        left union middle union right
      }
    }

    def extendImpl[A,B,C] (r: Rel[A,B],f: B => C)(implicit o1: RelOrd[A,Rel[B,C]], o2: RelOrd[B,C]): Rel[A,Rel[B,C]] = {
      object Extend extends (SomeTuple[A,B] => Rel[A,Rel[B,C]]) {
        def apply(t: SomeTuple[A,B]) =  create(t.first,create(t.second,f(t.second)))
      }
      mapImpl(r,Extend)
    }

    def mapImpl[A,B,C,D] (r: Rel[A,B],f: SomeTuple[A,B] => Rel[C,D])(implicit o2: RelOrd[C,D]): Rel[C,D] = {
      object RMonoid extends RelMonoid[C,D]

      reduceImpl(r,f,RMonoid)
    }

    /*def equal_join[A,B,C](r1: Rel[A,B], r2: Rel[A,C]): Rel[A,Rel[B,C]] = (r1.some,r2.some) match {
      case (n1: NoneTuple, _) => emptyRel[A,Rel[B,C]]
      case (_, n2: NoneTuple) => emptyRel[A,Rel[B,C]]
      case (s1: SomeTuple[A,B], s2: SomeTuple[A,C]) => {
        //val (l,m,r) = split(r2)
        val md = {
          val c = r1.ordering.orderingA.compare(s1.first,s2.first)

          if (c == 0) {
          }
          else emptyRel[A,Rel[B,C]]
        }
      }
    } */


    def swapImpl[A,B](r: Rel[A,B])(implicit o1: RelOrd[B,A]): Rel[B,A] = reduceImpl(r,SwapTuple()(o1),RelMonoid()(o1))

    def dropImpl[A,B,C](r: Rel[A,Rel[B,C]])(implicit o0: RelOrd[B,C],o1: RelOrd[A,C]): Rel[A,C] = {
      object RMonoid extends RelMonoid[A,C]

      object Drop extends (SomeTuple[A,Rel[B,C]] => Rel[A,C]) {
        def apply(t: SomeTuple[A,Rel[B,C]]): Rel[A,C] = {
          reduceImpl(t.second,{ x: SomeTuple[B,C] => create(t.first,x.second) },RMonoid)
        }
      }

      reduceImpl(r,Drop,RMonoid)
    }



    /*def second[A,B,C]  (r: Rel[A,Rel[B,C]]): Rel[A,C]
      */

    /*
          (c1.some, c2.some) match {
        case (None,None) => 0
        case (None,_) => -1
        case (_,None) => 1
        case (Some(s),_) => {
          val f1 = c1.first.get
          val f2 = c2.first.get
          val c = compare(f1.second,f2.second)
          if (c != 0)  c
          else compareCompoundExpr(c1.split(f1)._3,c2.split(f2)._3)
        }
      }
     */

    // TODO: optimize via common prefix determination
    def compareRel[A,B](r1: Rel[A,B], r2: Rel[A,B])(implicit p: RelOrd[A,B]): Int = (r1.some,r2.some) match {
      case (n1: NoneTuple, n2: NoneTuple) => 0
      case (n1: NoneTuple, _) => -1
      case (_, n2: NoneTuple) => 1
      case (s: SomeTuple[A,B],_) => {
        val m1 = r1.min.asSome
        val m2 = r2.min.asSome

        val c = p.compare(m1,m2)
        if (c != 0) c
        else compareRel(r1.split(m1).right,r2.split(m2).right)
      }
    }

    case class RelOrdering2[A,B](implicit p2: RelOrd[A,B]) extends Ordering[Rel[A,B]] {
      def compare(r1: Rel[A,B], r2: Rel[A,B]): Int = compareRel(r1,r2)
    }

    implicit def relOrd[A,B](implicit p2: RelOrd[A,B]): Ordering[Rel[A,B]] = RelOrdering2()

    def reduceImpl[A,B,M](r: Rel[A,B], f: SomeTuple[A,B] => M, m: Monoid[M]): M = r.some match {
      case n: NoneTuple => m.zero
      case s: SomeTuple[A,B] => {
        val ls = reduceImpl(r.left,f,m)
        val ms = m.append(m.zero,f(s))
        val rs = reduceImpl(r.right,f,m)
        m.append(m.append(ls,ms),rs)
      }
    }

    def countImpl[A,B](r: Rel[A,B]): Long = {
      object CountTuple extends (SomeTuple[Any,Any] => Long) { def apply(t: SomeTuple[Any,Any]) = 1 }
      object AddLong extends Monoid[Long] { def zero: Long = 0 ; def append(a1: Long, a2: Long): Long = a1 + a2 }

      reduceImpl(r,CountTuple,AddLong)
    }

    case class SwapTuple[A,B](implicit p: RelOrd[B,A]) extends (SomeTuple[A,B] => Rel[B,A]) {
      def apply(t: SomeTuple[A,B]): Rel[B,A] = create(t.second,t.first)
    }

    case class RelMonoid[A,B](implicit p: RelOrd[A,B]) extends Monoid[Rel[A,B]] {
      def zero: Rel[A,B] = emptyRel
      def append(a1: Rel[A,B], a2: Rel[A,B]): Rel[A,B] = a1 union a2
    }
  }

  // Rel[A,Rel[Long,Option[B]]]  - Rel[A,Rel[Long,Option[C]]] => Rel[A,Rel[Long,Option[Rel[B,C]]]

  (0,Some("a"))
  (10,None)
  (15,Some("b"))

  trait Monoid[A] {
    def zero: A
    def append(a1: A, a2: A): A
  }

  object SimpleAlgebra extends RelationalAlgebra {
    import IncrementalTreapRelation._

    trait TreapRel[A,B] extends Relation[A,B] {
      def self = this

      def treap: TreapRelation[A,B]

      def left = createRel(treap.left)
      def middle = createRel(treap.middle)
      def right = createRel(treap.right)

      def some = treap

      def getMin(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B] = treap.getMin(a)
      def getMax(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B] = treap.getMax(a)

      def min = treap.min
      def max = treap.max

      override def toString = "[" + treap + "]"
    }

    case object TreapRel0 extends TreapRel[Nothing,Nothing] {
      def treap = empty
      def split(a: SomeTuple[Nothing,Nothing])(implicit p: RelOrd[Nothing,Nothing]): Split[Nothing,Nothing] = SplitImpl[Nothing,Nothing](this,this,this)
      def union(r: Rel[Nothing,Nothing])(implicit p: RelOrd[Nothing,Nothing]): Rel[Nothing,Nothing] = r
    }

    case class TreapRel1[A,B](treap: TreapRelation[A,B]) extends TreapRel[A,B] {
      def split(a: SomeTuple[A,B])(implicit p: RelOrd[A,B]): Split[A,B] = {
        val (l,m,r) = treap.split(a)
        SplitImpl(createRel(l),createRel(m),createRel(r))
      }
      def union(r: Rel[A,B])(implicit p: RelOrd[A,B]): Rel[A,B] = createRel(p.union(treap,r.treap))
    }

    def createRel[A,B](t: TreapRelation[A,B]): TreapRel[A,B] = {
      if (t.isEmpty) TreapRel0.asInstanceOf[Rel[A,B]]
      else TreapRel1(t)
    }

   // implicit def relOrd2[A,B,C](implicit a: Ordering[A], r: Ordering[Rel[B,C]]): PrioFactory[A,Rel[B,C]] = DefaultPrioFact()(a,r)
    //implicit def relOrd3[A,B,C](implicit a: Ordering[Rel[A,B]], r: Ordering[C]): PrioFactory[Rel[A,B],C] = DefaultPrioFact()(a,r)

    type Rel[A,B] = TreapRel[A,B]
    type RelOrd[A,B] = PrioFactory[A,B]

    def emptyRel[A,B] = createRel(empty)
    def create[A,B](a: A, b: B)(implicit p: RelOrd[A,B]): Rel[A,B] = createRel(empty.put(RTuple(a,b)))
  }

  object Test{

    import SimpleAlgebra._

    final def main(): Unit ={
      val a = create(1,-1)
      val b = create(1,2)
      val c = create(2,3)
      val d = create(2,4)
      val e = create(0,1)
      val f = create(3,1)
      val g = create(-1,-1)
      val h = create(-1,1)

      val f0 = a union b union c union f union g union h
      val f1 = c union d union e union f union g union h
      val f2 = f0 union f1

      val aa = create(0,f0)
      val bb = create(0,f1)
      val cc = aa union bb

      println("f0: " + f0)
      println("f1: " + f1)
      println("f2: " + f2)
      //println("f2.swap: " + swap(f2))
      println("f2.count: " + f2.count)
      println("f2.group" + f2.group)
      //println("f2.swap.group" + group(swap(f2)))

      println(f2.extend(x=>x+2).drop.extend(x=>x-2).drop == f2)
    }
  }

}
