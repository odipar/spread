package spread

import javax.swing.DefaultComboBoxModel

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

  // TODO: Memoization of bind
  // TODO: Memoization of evaluation
  // TODO: Memoization of bindings


  def main(args: Array[String]): Unit = {
    /*import Natural._
    import Integer._
    import Rational._


    println("s")

    val one = rOne
    val two = one + one
    val three = two + one
    val four = two + two
    val five = four + one
    val fifteen = (three * five)
    val sixteen = four * four
    val eight = four + four
    val twelve = eight + four
    val thirteen = twelve + one
    val ten = (five * two)
    val minus_ten = -ten
    val six = sixteen + minus_ten
    val thirty_six = six * six

    val r1 = ten.simplify
    val r2 = (one + -(one)).simplify
    val r3 = r1 * r2
    val r4 = ~r2
    val r5 = r3 * r4

    println("r1: " + r1)
    println("r2: " + r2)
    println("r3: " + r3)
    println("r4: " + r4)
    println("r5: " + r5.simplify)
       */



    val one = "01" :- 1:IntS
    val two = "02" :- one + one
    val four = "04" :- two * two
    val six = "06" :- four + two
    val thirtysix = "36" :- six * six

    println(thirtysix)
    println(thirtysix.bindings.prettyString)
    println(thirtysix.evaluate)

  }

  type IntS = IntExpr

  implicit def intIntS(i: Int): IntS = IExpr(i)
  implicit def stringLabel(s: String): ILabel = ILabel(s)

  trait IntExpr extends Expr[Int,String] {
    def bind(s: Binding[Int,String]) = this
    def bindings: BST
    def +(i: IntExpr) = IAdd(this,i)
    def *(i: IntExpr) = IMul(this,i)
  }

  case object IEmpty extends IntExpr {
    def bindings = ee
    def evaluate = this
  }

  case class IExpr[B](i: Int) extends IntExpr {
    def bindings = ee
    def evaluate = this
    override def toString = i.toString
  }

  case class IAdd[B](first: IntExpr, second: IntExpr) extends IntExpr {
    override def bind(s: Binding[Int,String]) = bindings.get(s) match {
      case None => this
      case Some(x) => IAdd(first.bind(s),second.bind(s))
    }
    lazy val bindings = first.bindings.union(second.bindings)
    lazy val evaluate: IntExpr = (first.evaluate,second.evaluate) match {
      case (IExpr(i1),IExpr(i2)) => IExpr(i1+i2)
      case (i: IBinding, e: IntExpr) => IAdd(i.labeled,e).evaluate
      case (e: IntExpr, i: IBinding) => IAdd(e,i.labeled).evaluate
      case _ => this
    }
    override def toString = "("+first.toString + " + " + second.toString+")"
  }

  case class IMul[B](first: IntExpr, second: IntExpr) extends IntExpr {
    override def bind(s: Binding[Int,String]) = bindings.get(s) match {
      case None => this
      case Some(x) => IMul(first.bind(s),second.bind(s))
    }
    lazy val bindings = first.bindings union second.bindings
    lazy val evaluate: IntExpr =  (first.evaluate,second.evaluate) match {
      case (IExpr(i1),IExpr(i2)) => IExpr(i1*i2)
      case (i: IBinding, e: IntExpr) => IMul(i.labeled,e).evaluate
      case (e: IntExpr, i: IBinding) => IMul(e,i.labeled).evaluate
      case _ => this
    }
    override def toString = "("+first.toString + " * " + second.toString+")"
  }

  case class ILabel(label: String) extends IntExpr with Binding[Int,String] {
    def labeled = IEmpty
    lazy val bindings = ee.put(label)
    lazy val evaluate: IntExpr = this
    def :-(e: IntExpr) = IBinding(label,e)
    override def toString = "(\"" + label +")"
  }
  case class IBinding(label: String, labeled: IntExpr) extends IntExpr with Binding[Int,String] {
    lazy val bindings = labeled.bindings.put(this)
    lazy val evaluate: IntExpr = labeled.evaluate.asInstanceOf[IntExpr]
    override def toString = "(\"" + label + "\":-" + labeled +")"
  }
}