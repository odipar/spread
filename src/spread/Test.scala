package spread

import scala.language.implicitConversions

object Test {
  import Types._
  import OrderedSetImplementation._
  import OrderedTreapSet._
  import WeightBalancedSequence._
  import SequenceImplementation._
  import Hashing._
  import scala.collection.immutable.SortedSet

  //implicit def intToIExpr[B](i: Int): IntExpr[B] = IExpr[B](i)

  case class BindingOrdering[A,B](label: Ordering[B]) extends Ordering[Binding[A,B]] {
    def compare(m1: Binding[A,B], m2: Binding[A,B]): Int = label.compare(m1.label,m2.label)
  }

  case class BindingHasher[A,B](labelHasher: PriorityHasher[B]) extends PriorityHasher[Binding[A,B]] {
    def hash(b: Binding[A,B]): Int = labelHasher.hash(b.label)
  }

  case object StringHasher extends PriorityHasher[String] {
    def hash(b: String) = Hashing.jenkinsHash(b.hashCode)
  }

  implicit def mordering[A,B](implicit d: Ordering[B]): Ordering[Binding[A,B]] = BindingOrdering(d)
  implicit def mhasher[A,B](implicit lh: PriorityHasher[B]): PriorityHasher[Binding[A,B]] = BindingHasher(lh)
  implicit def sHasher: PriorityHasher[String] = StringHasher

  type SQT[N,X,M] = SeqImpl[N,X,M,IWBTree[N,X,M],IWBTreeContext[N,X,M]]
  val s: SQT[Int,Int,Int] = EmptySeqImpl(DepthMeasuringContext()(DefaultIWBTreeContext[Int]()))

  type ST[X,M,P] = OrderedISetImpl[X, M, STreap[X,M,P], STreapContext[X,M,P]]
  val e: ST[Int,Any,Int] = EmptyOrderedISet(DefaultSTreapContext[Int]()) // no measure


  type SST[X] = ST[X,Any,Int]
  type BIN = Binding[Int,String]
  type BST = SST[BIN]

  val b = DefaultSTreapContext[BIN]()

  val ee: BST = EmptyOrderedISet(b)

  //type ST[X] = OrderedISetImpl[X, Any, STreap[X, Any], STreapContext[X, Any]]
  //type BIN[B] = Binding[Int,B]
  //type BST[B] = ST[BIN[B]]

  //val dtc: DefaultSTreapContext[BIN[String]] = DefaultSTreapContext[BIN[String]]()
  //val e: BST[Any] = EmptyOrderedISet(dtc)
      //
  // TODO: Memoization of bind
  // TODO: Memoization of evaluation
  // TODO: Memoization of bindings


  def main(args: Array[String]): Unit = {
    import Engine._
    /*val i1 = IExpr(1)
    val i2 = IExpr(2)
    val a = IAdd(i1,i2)
    val i3 = IExpr(3)
    val i4 = BoundExpr(LocalLabel(Symbol("a")),IExpr(4))
    val m = IMul(i3,i4)
    val a2 = IAdd(a,m)
    val b:Map[Expr,Expr] = Map(Symbol("a")->UnboundExpr(LocalLabel(Symbol("b"))))
    val r = a2.reduce
    val rb = r.bind(b)
    val m1 = MExpr(Map(Symbol("x")->a2,Symbol("y")->m))
    val m2 = MExpr(b)
    val m3 = Red(m2)
    val bb = Bind(m1,m3)
    val rr = Red(bb)
    val r2 = Red(m1)
    val b2 = Bind(r2,m3)
    println(rr.reduce.asString)
    println(b2.reduce.asString)  */

    /*val i1 = BoundExpr(LocalLabel(Symbol("a")),IExpr(1))
    val i2 = IExpr(2)
    val a = IAdd(i1,i2)
    val m = Red(MExpr(Map(Symbol("b")->a)))
    val m1 = MExpr(Map(Symbol("a")->IExpr(4)))
    val b0 = Bind(m,m1)
    val m2 = Red(MExpr(Map(Symbol("c")->b0)))
    val m3 = MExpr(Map(Symbol("a")->UnboundExpr(LocalLabel(Symbol("d")))))
    val bb = ((Bind(m2,m3)))
    val m4 = MExpr(Map(Symbol("d")->IExpr(5)))
    val bb2 = Bind(bb,m4)
    println(bb.asString)
    println(bb.reduce.asString)  */

    /*val i1 = UnboundExpr(LocalLabel(Symbol("a")))
    val i2 = IExpr(2)
    val a = IAdd(i1,i2)
    val m = Red(MExpr(Map(Symbol("b")->a)))
    val m1 = MExpr(Map(Symbol("a")->IExpr(4)))
    val b = Bind(m,m1)
    println(b.asString)
    println(b.reduce.asString)*/

     val a1 = IExpr(1)
    val a2 = IExpr(2)
    val aa2 = Alternatives(SortedSet(a1,a2))
    val a5 = IExpr(5)
    val a6 = BoundExpr(LocalLabel(Symbol("a")),IExpr(4))
    val aa6 = Alternatives(SortedSet(a5,a6))
    val add2 = IMul(a1,aa6)
    val m1 = Red(MExpr(Map(Symbol("c")->add2)))
    val m2 = MExpr(Map(Symbol("a")->IExpr(9)))
    val bb = Bind(m1,m2)
    println(bb.asString)
    println(bb.reduce.asString)
    /*val a1 = IExpr(1)
    val a2 = IExpr(2)
    val aa2 = Alternatives(SortedSet(a1,a2))
    val a3 = IExpr(3)
    val a4 = IExpr(4)
    val aa4 = Alternatives(SortedSet(a3,a4))
    val aa24 = Alternatives(SortedSet(aa2,aa4))
    val aa2424 = Alternatives(SortedSet(aa24,aa2))
    println(aa24.reduce.asString) */
    }
}