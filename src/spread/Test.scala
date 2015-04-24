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

        e1 = e1.put(FullT(i1+5,i2+5))
        e2 = e2.put(FullT(i1,i2))
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
    import IncrementalTreapRelation.LeftTuple
    import IncrementalTreapRelation.RightTuple
    import IncrementalTreapRelation.OptionTuple
    import IncrementalTreapRelation.FullTuple
    import IncrementalTreapRelation.RelOrdering
    import IncrementalTreapRelation.NoneT
    import IncrementalTreapRelation.FullT
    import IncrementalTreapRelation.LeftT
    import IncrementalTreapRelation.RightT


    type Rel[A,B] <: Relation[A,B]
    type RelOrd[A,B] <: RelOrdering[A,B]

    trait Relation[A,B] extends Split[A,B] {
      def self: Rel[A,B]

      def isEmpty: Boolean
      def isLeaf: Boolean

      def min: Tuple[A,B]
      def some: Tuple[A,B]
      def max: Tuple[A,B]

      def union(r: Rel[A,B])(implicit p: RelOrd[A,B]): Rel[A,B]
      def split(a: FullTuple[A,B])(implicit p: RelOrd[A,B]): Split[A,B]
      def splitA(a: A)(implicit p: RelOrd[A,B]): Split[A,B]

      def getMin(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B]
      def getMax(a: A)(implicit p: RelOrd[A,B]): Tuple[A,B]

    }

    case class BinOps[A,B](o: Rel[A,B]) {
      def swap(implicit o1: RelOrd[B,A]): Rel[B,A] = swapImpl(o)
      def extend[E](f: A => E)(implicit o1: RelOrd[A,FullTuple[E,B]], o2: RelOrd[E,B]): Rel[A,FullTuple[E,B]] = extendImpl(o,f)
      def group[C](implicit p1: RelOrd[A,B], p2: RelOrd[A,Rel[A,B]]): Rel[A,Rel[A,B]] = groupImpl(o)
      def map[C,D](f: FullTuple[A,B] => Rel[C,D])(implicit o1: RelOrd[A,B], o2: RelOrd[C,D]): Rel[C,D] = mapImpl(o,f)
      def count: Long = countImpl(o)
      def join[C](ot: Rel[A,C])(implicit o0: RelOrd[A,B], o1: RelOrd[A,C], o2: RelOrd[B,C], o3: RelOrd[A,FullTuple[B,C]]): Rel[A,FullTuple[B,C]] = joinImpl(o,ot)
      def joinOuter[C](ot: Rel[A,C])(implicit p0: RelOrd[A,B], p1: RelOrd[A,C], p3: RelOrd[A,SomeTuple[B,C]], p4: RelOrd[A,FullTuple[B,C]]): Rel[A,SomeTuple[B,C]] = joinOuterImpl(o,ot)

      def compose[C](r2: Rel[B,C])(implicit o0: RelOrd[A,B], o1: RelOrd[B,A], o2: RelOrd[B,C], o3: RelOrd[A,C], o4: RelOrd[B,SomeTuple[A,C]], o5: RelOrd[C,SomeTuple[B,A]], o6: RelOrd[C,A], o7: RelOrd[B,FullTuple[A,C]]): Rel[A,C] = composeImpl(o,r2)

        //def compose[C](r2: Rel[B,C]): Rel[A,C] = o.swap.joinOuter(r2).rol.drop.swap

      //def ==(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) == 0
      def >(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) > 0
      def <(o2: Rel[A,B])(implicit p1: RelOrd[A,B]) = compareRel(o,o2) < 0
    }

    case class TerOps[A,B,C](o: Rel[A,SomeTuple[B,C]]) {
      def drop(implicit o0: RelOrd[B,C],o1: RelOrd[A,C]): Rel[A,C] = dropImpl(o)
      def rol(implicit o0: RelOrd[C,SomeTuple[A,B]]): Rel[C,SomeTuple[A,B]] = rolImpl(o)
      def ror(implicit o0: RelOrd[B,SomeTuple[C,A]]): Rel[B,SomeTuple[C,A]] = rorImpl(o)
    }


    implicit def binOps[A,B](o: Rel[A,B]): BinOps[A,B] = BinOps(o)
    implicit def terOps[A,B,C](o: Rel[A,SomeTuple[B,C]]): TerOps[A,B,C] = TerOps(o)

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

    def groupImpl[A,B](r: Rel[A,B])(implicit p1: RelOrd[A,B], p2: RelOrd[A,Rel[A,B]]): Rel[A,Rel[A,B]] = r.some match {
      case n: NoneTuple => emptyRel[A,Rel[A,B]]
      case s: FullTuple[A,B] => {
        def min = r.getMin(s.first).asFull
        def max = r.getMax(s.first).asFull

        val smin = r.split(min)
        val smax = smin.right.split(max)
        val smiddle = smin.middle union smax.left union smax.middle

        val left = groupImpl(smin.left)
        val middle = create(s.first,smiddle)
        val right = groupImpl(smax.right)

        left union middle union right
      }
    }

    def extendImpl[A,B,E] (r: Rel[A,B],f: A => E)(implicit o1: RelOrd[A,FullTuple[E,B]]): Rel[A,FullTuple[E,B]] = {
      object Extend extends (FullTuple[A,B] => Rel[A,FullTuple[E,B]]) {
        def apply(t: FullTuple[A,B]) =  create(t.first,FullT(f(t.first),t.second))
      }
      mapImpl(r,Extend)
    }

    def mapImpl[A,B,C,D] (r: Rel[A,B],f: FullTuple[A,B] => Rel[C,D])(implicit o2: RelOrd[C,D]): Rel[C,D] = {
      object RMonoid extends RelMonoid[C,D]
      reduceImpl(r,f,RMonoid)
    }

    def joinImpl[A,B,C](r1: Rel[A,B], r2: Rel[A,C])(implicit p0: RelOrd[A,B], p1: RelOrd[A,C], p2: RelOrd[B,C], p3: RelOrd[A,FullTuple[B,C]]): Rel[A,FullTuple[B,C]] = {
      if (r1.isEmpty || r2.isEmpty) emptyRel
      else if (p0.orderingA.compare(r1.max.asFull.first,r2.min.asFull.first) < 0) emptyRel
      else if (p0.orderingA.compare(r1.min.asFull.first,r2.max.asFull.first) > 0) emptyRel
      else {
        val a: A = r2.some.asFull.first

        val s1 = r1.splitA(a)
        val s2 = r2.splitA(a)

        val left = joinImpl(s1.left,s2.left)
        val middle = joinEqualImpl(a,s1.middle,s2.middle)
        val right  = joinImpl(s1.right,s2.right)

        left union middle union right
      }
    }
    def leftOuter[A,B,C](r1: Rel[A,B])(implicit p0: RelOrd[A,B], p3: RelOrd[A,SomeTuple[B,C]]): Rel[A,SomeTuple[B,C]] = {
      object Left extends (FullTuple[A,B] => Rel[A,SomeTuple[B,C]]) {
        def apply(t: FullTuple[A,B]) = create(t.first,LeftT(t.second))
      }

      reduceImpl(r1,Left,RelMonoid[A,SomeTuple[B,C]]())
    }

    def rightOuter[A,B,C](r1: Rel[A,C])(implicit p0: RelOrd[A,B], p1: RelOrd[A,C], p3: RelOrd[A,SomeTuple[B,C]]): Rel[A,SomeTuple[B,C]] = {
      object Right extends (FullTuple[A,C] => Rel[A,SomeTuple[B,C]]) {
        def apply(t: FullTuple[A,C]) = create(t.first,RightT(t.second))
      }

      reduceImpl(r1,Right,RelMonoid[A,SomeTuple[B,C]]())
    }

    // TODO: UNIFY with joinImpl
    def joinOuterImpl[A,B,C](r1: Rel[A,B], r2: Rel[A,C])(implicit p0: RelOrd[A,B], p1: RelOrd[A,C], p3: RelOrd[A,SomeTuple[B,C]], p4: RelOrd[A,FullTuple[B,C]]): Rel[A,SomeTuple[B,C]] = {
      if (r1.isEmpty) {
        if (r2.isEmpty) emptyRel
        else rightOuter(r2)
      }
      else if (r2.isEmpty) leftOuter(r1)
      else if (p0.orderingA.compare(r1.max.asFull.first,r2.min.asFull.first) < 0) leftOuter(r1) union rightOuter(r2)
      else if (p0.orderingA.compare(r1.min.asFull.first,r2.max.asFull.first) > 0) leftOuter(r1) union rightOuter(r2)
      else {
        val a: A = r2.some.asFull.first

        val s1 = r1.splitA(a)
        val s2 = r2.splitA(a)

        val left = joinOuterImpl(s1.left,s2.left)
        val middle = {
          if (!s1.middle.isEmpty && !s2.middle.isEmpty) joinEqualImpl(a,s1.middle,s2.middle).asInstanceOf[Rel[A,SomeTuple[B,C]]]
          else joinOuterImpl(s1.middle,s2.middle)
        }
        val right  = joinOuterImpl(s1.right,s2.right)
        left union middle union right
      }
    }

    def joinEqualImpl[A,B,C](a: A, r1: Rel[A,B], r2: Rel[A,C])(implicit o0: RelOrd[A,FullTuple[B,C]]): Rel[A,FullTuple[B,C]] ={
      if (r1.isEmpty || r2.isEmpty) emptyRel
      else if (r1.isLeaf) {
        if (r2.isLeaf) {
          val lt = r1.some.asFull
          val rt = r2.some.asFull

          create(a,FullT(lt.second,rt.second))
        }
        else {
          val left = joinEqualImpl(a,r1,r2.left)
          val middle = joinEqualImpl(a,r1,r2.middle)
          val right = joinEqualImpl(a,r1,r2.right)
          left union middle union right
        }
      }
      else {
        val left = joinEqualImpl(a,r1.left,r2)
        val middle = joinEqualImpl(a,r1.middle,r2)
        val right = joinEqualImpl(a,r1.right,r2)
        left union middle union right
      }
    }

    def swapImpl[A,B](r: Rel[A,B])(implicit o1: RelOrd[B,A]): Rel[B,A] = reduceImpl(r,SwapTuple()(o1),RelMonoid()(o1))

    def dropImpl[A,B,C](r: Rel[A,SomeTuple[B,C]])(implicit o0: RelOrd[B,C],o1: RelOrd[A,C]): Rel[A,C] = {
      object RMonoid extends RelMonoid[A,C]

      object Drop extends (FullTuple[A,SomeTuple[B,C]] => Rel[A,C]) {
        def apply(t: FullTuple[A,SomeTuple[B,C]]): Rel[A,C] = t.second match {
          case l: LeftTuple[B] => emptyRel
          case r: RightTuple[C] => create(t.first,r.second)
          case f: FullTuple[B,C] => create(t.first,f.second)
        }
      }

      reduceImpl(r,Drop,RMonoid)
    }

    def createTuple[A,B](a: A, b: Option[B]): SomeTuple[A,B] = b match {
      case None => LeftT(a)
      case Some(x) => FullT(a,x)
    }

    def createTuple[C,A](c: Option[C], a: A): SomeTuple[C,A] = c match {
      case None => RightT(a)
      case Some(x) => FullT(x,a)
    }

    def composeImpl[A,B,C](r1: Rel[A,B], r2: Rel[B,C])(implicit o0: RelOrd[A,B], o1: RelOrd[B,A], o2: RelOrd[B,C], o3: RelOrd[A,C], o4: RelOrd[B,SomeTuple[A,C]], o5: RelOrd[C,SomeTuple[B,A]], o6: RelOrd[C,A], o7: RelOrd[B,FullTuple[A,C]]): Rel[A,C] ={
      r1.swap.joinOuter(r2).rol.drop.swap
    }

    def rolImpl[A,B,C](r: Rel[A,SomeTuple[B,C]])(implicit o0: RelOrd[C,SomeTuple[A,B]]): Rel[C,SomeTuple[A,B]] = {
      object RMonoid extends RelMonoid[C,SomeTuple[A,B]]

      object Rol extends (FullTuple[A,SomeTuple[B,C]] => Rel[C,SomeTuple[A,B]]) {
        def apply(t: FullTuple[A,SomeTuple[B,C]]): Rel[C,SomeTuple[A,B]] = t.second.secondOption match {
          case None => emptyRel
          case Some(c) => create(c,createTuple(t.first,t.second.firstOption))
        }
      }

      reduceImpl(r,Rol,RMonoid)
    }

    def rorImpl[A,B,C](r: Rel[A,SomeTuple[B,C]])(implicit o0: RelOrd[B,SomeTuple[C,A]]): Rel[B,SomeTuple[C,A]] = {
      object RMonoid extends RelMonoid[B,SomeTuple[C,A]]

      object Ror extends (FullTuple[A,SomeTuple[B,C]] => Rel[B,SomeTuple[C,A]]) {
        def apply(t: FullTuple[A,SomeTuple[B,C]]): Rel[B,SomeTuple[C,A]] = t.second.firstOption match {
          case None => emptyRel
          case Some(b) => create(b,createTuple(t.second.secondOption,t.first))
        }
      }

      reduceImpl(r,Ror,RMonoid)
    }


    // TODO: optimize via common prefix determination
    def compareRel[A,B](r1: Rel[A,B], r2: Rel[A,B])(implicit p: RelOrd[A,B]): Int = (r1.some,r2.some) match {
      case (n1: NoneTuple, n2: NoneTuple) => 0
      case (n1: NoneTuple, _) => -1
      case (_, n2: NoneTuple) => 1
      case (s: FullTuple[A,B],_) => {
        val m1 = r1.min.asFull
        val m2 = r2.min.asFull

        val c = p.compare(m1,m2)
        if (c != 0) c
        else compareRel(r1.split(m1).right,r2.split(m2).right)
      }
    }

    case class RelOrdering2[A,B](implicit p2: RelOrd[A,B]) extends Ordering[Rel[A,B]] {
      def compare(r1: Rel[A,B], r2: Rel[A,B]): Int = compareRel(r1,r2)
    }

    implicit def relOrd[A,B](implicit p2: RelOrd[A,B]): Ordering[Rel[A,B]] = RelOrdering2()

    def reduceImpl[A,B,M](r: Rel[A,B], f: FullTuple[A,B] => M, m: Monoid[M]): M = r.some match {
      case n: NoneTuple => m.zero
      case s: FullTuple[A,B] => {
        val ls = reduceImpl(r.left,f,m)
        val ms = m.append(m.zero,f(s))
        val rs = reduceImpl(r.right,f,m)
        m.append(m.append(ls,ms),rs)
      }
    }

    def countImpl[A,B](r: Rel[A,B]): Long = {
      object CountTuple extends (FullTuple[Any,Any] => Long) { def apply(t: FullTuple[Any,Any]) = 1 }
      object AddLong extends Monoid[Long] { def zero: Long = 0 ; def append(a1: Long, a2: Long): Long = a1 + a2 }

      reduceImpl(r,CountTuple,AddLong)
    }

    case class SwapTuple[A,B](implicit p: RelOrd[B,A]) extends (FullTuple[A,B] => Rel[B,A]) {
      def apply(t: FullTuple[A,B]): Rel[B,A] = create(t.second,t.first)
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

      def isEmpty: Boolean = treap.isEmpty
      def isLeaf: Boolean = treap.isLeaf

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
      def split(a: FullTuple[Nothing,Nothing])(implicit p: RelOrd[Nothing,Nothing]): Split[Nothing,Nothing] = SplitImpl[Nothing,Nothing](this,this,this)
      def splitA(a: Nothing)(implicit p: RelOrd[Nothing,Nothing]): Split[Nothing,Nothing] = SplitImpl[Nothing,Nothing](this,this,this)

      def union(r: Rel[Nothing,Nothing])(implicit p: RelOrd[Nothing,Nothing]): Rel[Nothing,Nothing] = r
    }

    case class TreapRel1[A,B](treap: TreapRelation[A,B]) extends TreapRel[A,B] {
      def split(a: FullTuple[A,B])(implicit p: RelOrd[A,B]): Split[A,B] = {
        val (l,m,r) = treap.split(a)
        SplitImpl(createRel(l),createRel(m),createRel(r))
      }
      def splitA(a: A)(implicit p: RelOrd[A,B]): Split[A,B] = {
        val (l,m,r) = treap.splitA(a)
        SplitImpl(createRel(l),createRel(m),createRel(r))
      }

        def union(r: Rel[A,B])(implicit p: RelOrd[A,B]): Rel[A,B] = createRel(p.union(treap,r.treap))
    }

    def createRel[A,B](t: TreapRelation[A,B]): TreapRel[A,B] = {
      if (t.isEmpty) TreapRel0.asInstanceOf[Rel[A,B]]
      else TreapRel1(t)
    }

    type Rel[A,B] = TreapRel[A,B]
    type RelOrd[A,B] = PrioFactory[A,B]

    def emptyRel[A,B] = createRel(empty)
    def create[A,B](a: A, b: B)(implicit p: RelOrd[A,B]): Rel[A,B] = createRel(empty.put(FullT(a,b)))
  }

  object Test{

    import SimpleAlgebra._
    import IncrementalTreapRelation._

    final def main(): Unit ={
      val ado = create("a",0) union create("b",1) union create("c",2) union create("d",3) union create("e",4) union create("f",5)

      val s_ado = ado.swap
      val parent = create(4,1) union create(4,2) union create(5,1)
      val s_parent = parent.swap

      val last: Rel[Int,Float] = create(0,1.2f) union create(2,3.3f)
      val currency = create(0,0) union create(1,2) union create(2,1) union create(3,0)

      val currencyEnum = create(0,"USD") union create(1,"EUR") union create(2,"GBP")

      val s_currency = currency.compose(currencyEnum)

      val adoJoin = s_ado.joinOuter(last).joinOuter(s_currency).joinOuter(s_parent)

      println(adoJoin)
    }
  }



}
