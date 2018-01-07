package org.spread.core.experiment.sequence

import com.esotericsoftware.kryo.io.Output
import org.spread.core.experiment.eav.EAV
import org.spread.core.experiment.expression.Spread._
import org.spread.core.experiment.sequence.Sequence._
import org.spread.core.language.Annotation.sp
import spire.algebra.Order

import scala.collection.immutable.HashMap
import org.spread.core.experiment.eav.EAV._
import org.spread.core.experiment.sequence.Container.Container
import org.spread.core.experiment.sequence.Storage._
import org.spread.core.splithash.Hashing

import scala.language.{existentials, implicitConversions}

object Test {

  import TreeSequence._

  case class AA2() extends LongAttribute {
    override def toString = "aa2"
  }

  case class AA1() extends LongAttribute {
    override def toString = "aa1"
  }

  val a1 = AA1()
  val a2 = AA2()

  final def main(args: Array[String]): Unit = {

    val eavFactory = emptyTree[EAV]

    val l1 = (0L until 1000000L).map(x => LongEAV(Entity(Array(x+1)), a1, LongValueImpl(x)))
    val l2 = (-6000L until -5000L).map(x => LongEAV(Entity(Array(x+1)), a1, LongValueImpl(x)))
    val l3 = (-5000L until 100L).map(x => LongEAV(Entity(Array(x+1)), a1, LongValueImpl(x)))

    println("START 0")
    
    val s1 = eavFactory.createSeq(l1.toArray)
    val s2 = eavFactory.createSeq(l2.toArray)
    val s3 = eavFactory.createSeq(l3.toArray)
    val s4 = s2 ++ s3

    for (i <- 1 to 1) {
      val mem = InMemoryStorage()

      Storage.withStorage(mem) {

        val ss1 = s1.store
        val ss2 = s2.store
        val ss3 = s3.store
        val ss4 = s4.store

        println("START 1")
        withContext(StrongStoringMemoizationContext()) {

          val r1 = %(rangeOfSeq, ss1).eval
          println("ranges: " + ranges)
          val r2 = %(rangeOfSeq, ss2).eval
          println("ranges: " + ranges)
          val r3 = %(rangeOfSeq, ss3).eval
          println("ranges: " + ranges)
          val r4 = %(rangeOfSeq, ss4).eval
          println("ranges: " + ranges)
          
          val t1 = Table("t2", ss1)
          val t2_1 = Table("t1", ss3)
          val t2_2 = Table("t1", ss4)

          /*withStorage(StackedStorage(Storage.storage, NullStorage()))*/
          {
            /*withContext(StackedContext(Spread.context, StrongStoringMemoizationContext()))*/
            {
              val cc1: Constraint = (t1(a1) === t2_1(a1))

              val tt1 = cc1.tableMap

              val c1 = cartesianProduct(tt1, cc1.store)
              val e1 = c1.eval
              println("e1: " + e1.head)
              println("cp: " + cp)
              println("prds: " + prds)

              val cc2: Constraint = (t1(a1) === t2_2(a1))
              val tt2 = cc2.tableMap
              val c2 = cartesianProduct(tt2, cc2.store)
              val e2 = c2.eval

              println("e2: " + e2.head)
              println("cp: " + cp)
              println("prds: " + prds)

              val st = mem
              val vals = st.m.toSeq
              val max = vals.map(x => x._2.size).max
              val sum = vals.map(x => x._2.size).sum
              val maxval = vals.filter(x => x._2.size == max)
              
              println("ranges: " + ranges)

              println("max: " + max)
              println("sum: " + sum)
              println("max_node: " + st.get(maxval(0)._1))
            }
          }
        }
      }
    }
  }

  type CEAV = Container[EAV, S] forSome {type S <: Container[EAV, S]}

  var reav: Long = 0

  case class REAV(index: Long, eav: CEAV, range: EAVRange) extends EAVRange {
    {
      reav += 1
    }
    def start: EAV = range.start
    def end: EAV = range.end
    override val hashCode: Int = {
      Hashing.siphash24(
        Hashing.siphash24(index.toInt, (index >>> 32).toInt),
        Hashing.siphash24(range.hashCode, eav.hashCode))
    }
  }

  trait Propagator[@sp X] {
    def ord: Order[X]
    def isValid(s1: X, e1: X, s2: X, e2: X): Boolean
    def isSolution(s1: X, e1: X, s2: X, e2: X): Boolean
  }

  var valid: Long = 0

  trait EqualPropagator[@sp X] extends Propagator[X] {
    def isValid(s1: X, e1: X, s2: X, e2: X): Boolean = {
      valid += 1
      val s3 = ord.max(s1, s2)
      val e3 = ord.min(e1, e2)
      ord.lteqv(s3, e3)
    }
    def isSolution(s1: X, e1: X, s2: X, e2: X): Boolean = {
      (ord.compare(s1, e1) == 0) && (ord.compare(s2, e2) == 0) && (ord.compare(s1, s2) == 0)
    }
  }

  object ConstantString extends StringAttribute {
    override def toString: String = "constantString"
  }

  object ConstantLong extends LongAttribute {
    override def toString: String = "constantLong"
  }

  implicit def toStringAttr(s: String): TableAttribute[TreeSeq[EAV, ArraySeqImpl[EAV]]] = {
    val eavFactory = emptyTree[EAV]
    val eav = AttributeValueEAV(ConstantString, StringValueImpl(s))
    val tb = Table("_string_." + s, eavFactory.createSeq(Array(eav)))
    TableAttribute(tb, ConstantString)
  }

  implicit def toLongAttr(l: Long): TableAttribute[TreeSeq[EAV, ArraySeqImpl[EAV]]] = {
    val eavFactory = emptyTree[EAV]
    val eav = AttributeValueEAV(ConstantLong, LongValueImpl(l))
    val tb = Table("_long_." + l, eavFactory.createSeq(Array(eav)))
    TableAttribute(tb, ConstantLong)
  }

  case class EqualEntityPropagator() extends EqualPropagator[Entity] {
    def ord: Order[Entity] = EntityOrdering
  }

  case class EqualAttributePropagator() extends EqualPropagator[Attribute] {
    def ord: Order[Attribute] = AttributeOrdering
  }

  case class EqualValuePropagator() extends EqualPropagator[Value] {
    def ord: Order[Value] = ValueOrdering
  }

  case class Table[S <: Container[EAV, S]](name: String, s: S) {
    def apply(a: Attribute): TableAttribute[S] = TableAttribute(this, a)
    def ===[S2 <: Container[EAV, S2]](o: Table[S2]): Constraint = {
      EntityConstraint(this, o, EqualEntityPropagator())
    }
    override def hashCode: Int = name.hashCode
    override def equals(o: Any): Boolean = o match {
      case oo: Table[_] => name == oo.name
      case _ => false
    }
    override def toString: String = name
  }

  case class TableAttribute[S <: Container[EAV, S]](s: Table[S], a: Attribute) {
    val eavFactory = emptyTree[EAV]
    def attrTable = Table(s.toString + "(" + a.toString + ")", eavFactory.createSeq(Array(AttributeEAV(a))))
    def attrConstraint: Constraint = AttributeConstraint(s, attrTable, EqualAttributePropagator())
    def ===[S2 <: Container[EAV, S2]](o: TableAttribute[S2]): Constraint = {
      attrConstraint AND o.attrConstraint AND ValueConstraint(s, o.s, EqualValuePropagator())
    }
  }

  trait Constraint extends Storable[Constraint] {
    def tableMap: Map[String, (Long, CEAV)]
    def AND(o: Constraint): Constraint = AndConstraint(this, o)
    def OR(o: Constraint): Constraint = OrConstraint(this, o)
    def isValid(m: Map[String, EAVRange]): Boolean
    def isSolution(m: Map[String, EAVRange]): Boolean
    def store: Constraint = this
  }

  trait LeafConstraint[@sp X, S1 <: Container[EAV, S1], S2 <: Container[EAV, S2]] extends Constraint {
    def tableMap: Map[String, (Long, CEAV)] = Map[String, (Long, CEAV)](left -> (0L, t1.s), right -> (0L, t2.s))
    def propagator: Propagator[X]

    def t1: Table[S1]
    def t2: Table[S2]

    def left: String = t1.name
    def right: String = t2.name

    def select(eav: EAV): X

    def isValid(m: Map[String, EAVRange]): Boolean = {
      val r1 = m(left)
      val r2 = m(right)
      propagator.isValid(select(r1.start), select(r1.end), select(r2.start), select(r2.end))
    }

    def isSolution(m: Map[String, EAVRange]): Boolean = {
      val r1 = m(left);
      val r2 = m(right)
      propagator.isSolution(select(r1.start), select(r1.end), select(r2.start), select(r2.end))
    }
  }

  case class AndConstraint(l: Constraint, r: Constraint) extends Constraint {
    def tableMap: Map[String, (Long, CEAV)] = l.tableMap ++ r.tableMap
    def isValid(m: Map[String, EAVRange]): Boolean = l.isValid(m) && r.isValid(m)
    def isSolution(m: Map[String, EAVRange]): Boolean = l.isSolution(m) && r.isSolution(m)

    def ref1(ref: Constraint): Constraint = AndConstraint(l.store, r.store)
    def ref2(r: Ref): Constraint = RefConstraint(r)
    override def store: Constraint = storage.put(this, ref1, ref2)

    override val hashCode: Int = Hashing.siphash24(l.hashCode + 100, r.hashCode - 100)
  }

  case class RefConstraint(ref: Ref) extends Constraint with RefObject[Constraint] {
    def tableMap = resolve.tableMap
    def isValid(m: Map[String, EAVRange]): Boolean = resolve.isValid(m)
    def isSolution(m: Map[String, EAVRange]): Boolean = resolve.isSolution(m)
  }

  case class OrConstraint(l: Constraint, r: Constraint) extends Constraint {
    def tableMap: Map[String, (Long, CEAV)] = l.tableMap ++ r.tableMap
    def isValid(m: Map[String, EAVRange]): Boolean = l.isValid(m) || r.isValid(m)
    def isSolution(m: Map[String, EAVRange]): Boolean = l.isSolution(m) || r.isSolution(m)
    override val hashCode: Int = Hashing.siphash24(l.hashCode - 100, r.hashCode + 100)
  }

  case class EntityConstraint[S1 <: Container[EAV, S1], S2 <: Container[EAV, S2]]
  (t1: Table[S1], t2: Table[S2], propagator: Propagator[Entity]) extends LeafConstraint[Entity, S1, S2] {
    def select(eav: EAV): Entity = eav.e
  }

  case class AttributeConstraint[S1 <: Container[EAV, S1], S2 <: Container[EAV, S2]]
  (t1: Table[S1], t2: Table[S2], propagator: Propagator[Attribute]) extends LeafConstraint[Attribute, S1, S2] {
    def select(eav: EAV): Attribute = eav.a
  }

  case class ValueConstraint[S1 <: Container[EAV, S1], S2 <: Container[EAV, S2]]
  (t1: Table[S1], t2: Table[S2], propagator: Propagator[Value]) extends LeafConstraint[Value, S1, S2] {
    def select(eav: EAV): Value = eav.v
  }

  trait Index extends Storable[Index] {
    def size: Long
    def parts: Array[Index]
    def store: Index = this
  }

  val emptyIndex = EmptyIndex()

  case class EmptyIndex() extends Index {
    def size: Long = 0
    def parts: Array[Index] = Array()
    override def toString: String = "~"
  }

  def createLeafIndex(indices: Array[Long]): Index = {
    if (indices.length == 0) EmptyIndex()
    else LeafIndex(indices)
  }

  case class LeafIndex(indices: Array[Long]) extends Index {
    {
      require(indices.length > 0)
    }
    def size: Long = indices.length
    def parts: Array[Index] = Array(this)
    override def toString: String = indices.foldLeft("<")((x, y) => x + " " + y) + ">"
    override lazy val hashCode: Int = {
      var hash: Int = Hashing.siphash24(indices.length, indices.length)
      var s = indices.length
      var i = 0
      while (i < s) {
        val v = indices(i)
        val v1 = v.toInt
        val v2 = (v >>> 32).toInt
        hash = Hashing.siphash24(Hashing.siphash24(hash, v1), v2)
        i += 1
      }
      hash
    }
    def ref1(r: Index): Index = this
    def ref2(r: Ref): Index = RefIndex(r, size)
    override def store: Index = storage.put(this, ref1, ref2)
  }

  case class RangeIndex(from: Long, to: Long) extends Index {
    def size: Long = to - from
    def parts: Array[Index] = Array(this)

    def ref1(r: Index): Index = this
    def ref2(r: Ref): Index = RefIndex(r, size)
    override def store: Index = storage.put(this, ref1, ref2)
  }

  case class RefIndex(ref: Ref, size: Long) extends Index with RefObject[Index] {
    def parts = resolve.parts
  }

  def branchIndex(parts: List[Index]): Index = {
    var result: List[Index] = List()
    for (p <- parts) p match {
      case e: EmptyIndex =>
      case _ => result = p.store +: result
    }

    val s = result.size

    if (s == 0) emptyIndex
    else if (s == 1) result.head
    else BranchIndex(result.reverse.toArray).store
  }

  case class BranchIndex(parts: Array[Index]) extends Index {
    val size: Long = parts.map(x => x.size).sum
    override def toString: String = parts.foldLeft("|")((x, y) => x + " " + y) + "|"

    def ref1(r: Index): Index = BranchIndex(parts.map(x => x.store))
    def ref2(r: Ref): Index = RefIndex(r, size)
    override def store: Index = storage.put(this, ref1, ref2)
  }

  var product: Long = 0

  val cartesianProduct = cartesianProduct2()

  case class cartesianProduct2() extends FA2[Map[String, (Long, CEAV)], Constraint, Map[String, Index]] {
    def apply(t: Map[String, (Long, CEAV)], c: Constraint): Expr[Map[String, Index]] = {
      product += 1
      val m = t.map(x => (x._1, %(rangeOfSeq, x._2._2)))
      val m2 = flattenValues()(m)
      val tt = combine(t, m2)
      cartesianProduct_2(tt, c)
    }
  }

  val combine = combine2()

  case class combine2() extends FA2[Map[String, (Long, CEAV)], Map[String, EAVRange], Map[String, REAV]] {
    def apply(l: Map[String, (Long, CEAV)], r: Map[String, EAVRange]): Expr[Map[String, REAV]] = {
      l.map(x => (x._1, REAV(x._2._1, x._2._2, r(x._1))))
    }
  }

  var cp: Long = 0
  var prds: Long = 0

  val cartesianProduct_2 = cartesianProduct2_2()

  case class cartesianProduct2_2() extends FA2[Map[String, REAV], Constraint, Map[String, Index]] {
    def apply(t: Map[String, REAV], c: Constraint): Expr[Map[String, Index]] = {

      if (c.isSolution(t)) {
        println("t: " + t)
        ???
      }
      else if (c.isValid(t)) {
        val m = t.map(x => (x._1, x._2.eav.parts.length)).toSeq.sorted.reverse
        val f = m.filter(x => x._2 > 1)
        if (f.isEmpty) {
          val seq = t.toSeq

          val values: Map[String, Array[EAV]] = t.map(x => (x._1, x._2.eav.whole.toArray))
          val tables: Array[String] = seq.map(x => x._1).toArray
          val sizes: Array[Int] = seq.map(x => x._2.eav.size.toInt).toArray
          val indices: Array[Long] = tables.map(key => t(key).index)

          // TODO: mutableMap
          var map: Map[String, EAVRange] = HashMap()
          var result: Map[String, List[Long]] = tables.map(x => (x, List[Long]())).toMap

          val prod: Int = sizes.product

          var i = 0

          while (i < prod) {
            cp += 1
            var ss = tables.length
            var ii = 0
            var iii = i
            while (ii < ss) {
              val table = tables(ii)
              val index = iii % sizes(ii)
              val value = values(table)(index)
              map = map.updated(table, value)
              iii /= sizes(ii)
              ii += 1
            }
            if (c.isSolution(map)) {
              ii = 0
              var iii = i
              while (ii < ss) {
                val table = tables(ii)
                val index: Long = iii % sizes(ii)
                result = result.updated(table, (index + indices(ii)) +: result(table))
                iii /= sizes(ii)
                ii += 1
              }
            }
            i += 1
          }

          result.map(x => (x._1, createLeafIndex(x._2.toArray.reverse).store))
        }
        else {
          val key: String = f.head._1
          val next: REAV = t(key)
          var offset: Long = next.index

          var rm = t.map(x => (x._1, (x._2.index, x._2.eav))).toMap[String, (Long, CEAV)]

          val products = next.eav.parts.toList.map(
            x => {
              /*println("x.first: " + x.first)
              println("x.last: " + x.last)
              println("x.size: " + x.size)
              println("x.hashCode: " + x.hashCode)
              println   */
              prds += 1
              rm = rm + (key -> (offset, x.asInstanceOf[CEAV]))
              val prod = cartesianProduct(rm, c)
              offset += x.size
              prod
            }
          )

          branchIndexMap(flatten()(products))
        }
      }
      else t.map(x => (x._1, emptyIndex))
    }
  }

  val branchIndexMap = branchIndexMap2()

  case class branchIndexMap2() extends FA1[List[Map[String, Index]], Map[String, Index]] {
    def apply(a: List[Map[String, Index]]): Expr[Map[String, Index]] = {
      var s: Map[String, List[Index]] = Map()

      for (x <- a) {
        for (kv <- x) {
          val k = kv._1
          if (s.contains(k)) {
            s = s + (k -> (x(k) +: s(k)))
          }
          else {
            s = s + (k -> List(x(k)))
          }
        }
      }

      s.map(x => (x._1, branchIndex(x._2.reverse)))
    }
  }

  case class flatten[@sp X]() extends FA1[List[Expr[X]], List[X]] {
    val el: element[X] = element[X]()
    def apply(a: List[Expr[X]]): Expr[List[X]] = {
      if (a.isEmpty) List()
      else concat()(el(a.head), this (a.tail))
    }
  }

  var flt: Long = 0

  case class flattenValues[X, Y]() extends FA1[Map[X, Expr[Y]], Map[X, Y]] {
    val el: elementValue[X, Y] = elementValue[X, Y]()
    def apply(a: Map[X, Expr[Y]]): Expr[Map[X, Y]] = {
      if (a.isEmpty) Map[X, Y]()
      else concatMap()(el(a.head._1, a.head._2), this (a.tail))
    }
  }

  case class concat[@sp X]() extends FA2[List[X], List[X], List[X]] {
    def apply(x1: List[X], x2: List[X]): Expr[List[X]] = x1 ++ x2
  }

  case class concatMap[X, Y]() extends FA2[Map[X, Y], Map[X, Y], Map[X, Y]] {
    def apply(x1: Map[X, Y], x2: Map[X, Y]): Expr[Map[X, Y]] = x1 ++ x2
  }


  case class element[@sp X]() extends FA1[X, List[X]] {
    def apply(x: X): Expr[List[X]] = List(x)
  }

  case class elementValue[X, Y]() extends FA2[X, Y, Map[X, Y]] {
    def apply(x: X, y: Y): Expr[Map[X, Y]] = Map(x -> y)
  }

  var ranges: Long = 0

  val rangeOfSeq = rangeOfSeq2()

  case class rangeOfSeq2() extends FA1[CEAV, EAVRange] {
    def ord: Order[EAV] = Order[EAV]

    def apply(x: CEAV): Expr[EAVRange] = {
      ranges += 1
      if (x.parts.length == 1) {
        val values = x.whole.toArray
        val s = values.length
        var i = 1
        var min = values(0)
        var max = values(0)

        while (i < s) {
          min = ord.min(min, values(i))
          max = ord.max(max, values(i))
          i += 1
        }
        EAVRangeImpl(min, max)
      }
      else {
        if (x.parts.length == 0) sys.error("0")
        rangeOfRanges(x.parts.map(p => %(this, p.asInstanceOf[CEAV])).toList)
      }
    }
  }

  val rangeOfRanges = rangeOfRanges2()

  case class rangeOfRanges2() extends FA1[List[Expr[EAVRange]], EAVRange] {
    def apply(x: List[Expr[EAVRange]]): Expr[EAVRange] = {
      if (x.lengthCompare(0) == 0) sys.error("no")
      else if (x.lengthCompare(1) == 0) x.head
      else mergeRanges(x.head, rangeOfRanges(x.tail))
    }
  }

  val mergeRanges = mergeRanges2()

  case class mergeRanges2() extends FA2[EAVRange, EAVRange, EAVRange] {
    def apply(r1: EAVRange, r2: EAVRange): Expr[EAVRange] = r1.union(r2)
  }
}
