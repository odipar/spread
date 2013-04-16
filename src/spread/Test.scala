package spread

import scala.language.implicitConversions

object Test {
  import Types._
  import OrderedSetImplementation._
  import OrderedTreapSet._
  import WeightBalancedSequence._
  import SequenceImplementation._
  import Hashing._

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
    val i1 = IExpr(1)
    val i2 = IExpr(2)
    val a = IAdd(i1,i2)
    val i3 = IExpr(3)
    val i4 = BoundExpr(LocalLabel(Symbol("a")),IExpr(4))
    val m = IMul(i3,i4)
    val a2 = IAdd(a,m)
    val b:Map[Expr,Expr] = Map(Symbol("a")->IExpr(5))
    val r = a2.reduce
    val rb = r.bind(b)
    val m1 = MExpr(Map(Symbol("x1")->a2,Symbol("x2")->m))
    val m2 = MExpr(b)
    val bb = Bind(m1,m2)
    val rr = Red(bb)
    println("bb: " + rr.reduce.asString)
  }
}