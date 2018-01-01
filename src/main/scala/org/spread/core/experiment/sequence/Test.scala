package org.spread.core.experiment.sequence

import java.util.Base64
import javax.management.StringValueExp

import org.spread.core.experiment.expression.Spread.FA1
import org.spread.core.experiment.sequence.Sequence.{ArraySeqImpl, Seq}
import org.spread.core.experiment.expression.Spread._
import org.spread.core.language.Annotation.sp
import spire.algebra.Order

import scala.collection.immutable.HashMap
import scala.collection.mutable
import org.spread.core.experiment.eav.EAV.{EAVRange, _}
import org.spread.core.experiment.sequence.Container.Container
import org.spread.core.splithash.Hashing

import scala.language.{existentials, implicitConversions}


object Test {
  import TreeSequence._

  object A1 extends LongAttribute {
    override def toString = "a1"
  }
  object A2 extends LongAttribute {
    override def toString = "a2"
  }

  final def main(args: Array[String]): Unit = {

    val eavFactory = emptyTree[EAV]

    val l1 = (0 until 50000).map(x => LongEAV(Entity(Array(x.toLong)), A1, LongValueImpl(x.toLong)))
    val l2 = (-50 until 50).map(x => LongEAV(Entity(Array(x.toLong)), A1, LongValueImpl(x.toLong)))
    val l3 = (10000000 until 10000005).map(x => LongEAV(Entity(Array(x.toLong)), A1, LongValueImpl(x.toLong)))

    val s1 = eavFactory.createSeq(l1.toArray)
    println("nodes: " + nodes)
    val s2 = eavFactory.createSeq(l2.toArray)
    println("nodes: " + nodes)
    val s3 = eavFactory.createSeq(l3.toArray)
    println("nodes: " + nodes)
    val s4 = s1 ++ s3
    println("nodes: " + nodes)


    val ct = new StrongMemoizationContext(mutable.HashMap())

    println("start 0")

    val r1 = %(rangeOfSeq, s1).eval(ct)
    println("ranges: " + ranges)
    val r2 = %(rangeOfSeq, s2).eval(ct)
    println("ranges: " + ranges)
    val r3 = %(rangeOfSeq, s3).eval(ct)
    println("ranges: " + ranges)
    val r4 = %(rangeOfSeq, s4).eval(ct)
    println("ranges: " + ranges)

    println("start 1")

    println("start 2")
    val t1 = Table("t1", s1)
    val t2 = Table("t2", s1)
    val t3 = Table("t3", s3)
    val t4 = Table("t1", s4)
    val t5 = Table("t2", s4)

    val cc1: Constraint = (t1(A1) === t2(A1))

    val tt1 = cc1.tableMap
    val c1 = %(cartesianProduct, tt1, cc1)
    val e1 = c1.eval(ct)

    println("e1: " + e1.head)

    println("start 3")
    val cc2: Constraint = (t4(A1) === t5(A1))

    println("prds: " + prds)
    println("ranges: " + ranges)
    println("flt: " + flt)
    println("reav: " + reav)
    println("valid: " + valid)
    println("cp: " + cp)
    println("product: " + product)
    val tt2 = cc2.tableMap
    
    val c2 = %(cartesianProduct, tt2, cc2)
    val e2 = c2.eval(ct)

    println("prds: " + prds)
    println("ranges: " + ranges)
    println("flt: " + flt)
    println("reav: " + reav)
    println("valid: " + valid)
    println("cp: " + cp)
    println("product: " + product)

    println("e2: " + e2.head)
  }
  
  type CEAV = Container[EAV, S] forSome { type S <: Container[EAV, S] }

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

  case class EqualEntityPropagator(implicit o: Order[Entity]) extends EqualPropagator[Entity] {
    def ord: Order[Entity] = o
  }

  case class EqualAttributePropagator(implicit o: Order[Attribute]) extends EqualPropagator[Attribute] {
    def ord: Order[Attribute] = o
  }

  case class EqualValuePropagator(implicit o: Order[Value]) extends EqualPropagator[Value] {
    def ord: Order[Value] = o
  }

  case class Table[S <: Container[EAV, S]](name: String, s: S) {
    def apply(a: Attribute): TableAttribute[S] = TableAttribute(this, a)
    def ===[S2 <: Container[EAV, S2]](o: Table[S2]): Constraint ={
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
    def ===[S2 <: Container[EAV, S2]](o: TableAttribute[S2]): Constraint ={
      attrConstraint AND o.attrConstraint AND ValueConstraint(s, o.s, EqualValuePropagator())
    }
  }

  trait Constraint {
    def tableMap: Map[String, (Long, CEAV)]
    def AND(o: Constraint): Constraint = AndConstraint(this, o)
    def OR(o: Constraint): Constraint = OrConstraint(this, o)
    def isValid(m: Map[String, EAVRange]): Boolean
    def isSolution(m: Map[String, EAVRange]): Boolean
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
      val r1 = m(left) ; val r2 = m(right)
      propagator.isValid(select(r1.start), select(r1.end), select(r2.start), select(r2.end))
    }

    def isSolution(m: Map[String, EAVRange]): Boolean = {
      val r1 = m(left) ; val r2 = m(right)
      propagator.isSolution(select(r1.start), select(r1.end), select(r2.start), select(r2.end))
    }
  }

  case class AndConstraint(l: Constraint, r: Constraint) extends Constraint {
    def tableMap: Map[String, (Long, CEAV)] = l.tableMap ++ r.tableMap
    def isValid(m: Map[String, EAVRange]): Boolean = l.isValid(m) && r.isValid(m)
    def isSolution(m: Map[String, EAVRange]): Boolean = l.isSolution(m) && r.isSolution(m)
    override val hashCode: Int = Hashing.siphash24(l.hashCode + 100, r.hashCode - 100)
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

  trait Index {
    def size: Long
    def parts: Array[Index]
  }

  object EmptyIndex extends Index {
    def size: Long = 0
    def parts: Array[Index] = Array()
    override def toString: String = "~"
  }

  case class LeafIndex(indices: Array[Long]) extends Index {
    def size: Long = indices.length
    def parts: Array[Index] = Array(this)
    override def toString: String = indices.foldLeft("<")((x, y) => x + " " + y) + ">"
    //override val hashCode = indices.hashCode
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
  }

  case class RangeIndex(from: Long, to: Long) extends Index {
    def size: Long = to - from
    def parts: Array[Index] = Array(this)
  }

  def branchIndex(parts: List[Index]): Index = {
    var result: List[Index] = List()
    for (p <- parts) p match {
      case EmptyIndex =>
      case _ => result = p +: result
    }

    val s = result.size

    if (s == 0) EmptyIndex
    else if (s == 1) result.head
    else BranchIndex(result.reverse.toArray)
  }

  case class BranchIndex(parts: Array[Index]) extends Index {
    val size: Long = parts.map(x => x.size).sum
    override def toString: String = parts.foldLeft("|")((x, y) => x + " " + y) + "|"
  }

  var product: Long = 0

  object cartesianProduct extends FA2[Map[String, (Long, CEAV)], Constraint, Map[String, Index]] {
    def apply(t: Map[String, (Long, CEAV)], c: Constraint): Expr[Map[String, Index]] = {
      product += 1
      val m = t.map(x => (x._1, %(rangeOfSeq, x._2._2)))
      val m2 = flattenValues()(m)
      val tt = combine(t, m2)
      %(cartesianProduct2, tt, c)
    }
  }

  object combine extends FA2[Map[String, (Long, CEAV)], Map[String, EAVRange], Map[String, REAV]] {
    def apply(l: Map[String, (Long, CEAV)], r: Map[String, EAVRange]): Expr[Map[String, REAV]] = {
      l.map(x => (x._1, REAV(x._2._1, x._2._2, r(x._1))))
    }
  }

  var cp: Long = 0
  var prds: Long = 0

  object cartesianProduct2 extends FA2[Map[String, REAV], Constraint, Map[String, Index]] {
    def apply(t: Map[String, REAV], c: Constraint): Expr[Map[String, Index]] = {

      if (c.isSolution(t)) ???
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

          /*while (i < prod) {
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
          }    */

          result.map(x => (x._1, LeafIndex(x._2.toArray.reverse)))
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
              val prod = %(cartesianProduct, rm, c)
              offset += x.size
              prod
            }
          )

          %(branchIndexMap, flatten()(products))
        }
      }
      else t.map(x => (x._1, EmptyIndex))
    }
  }

  object branchIndexMap extends FA1[List[Map[String, Index]], Map[String, Index]] {
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
      else concat()(el(a.head), this(a.tail))
    }
  }

  var flt: Long = 0

  case class flattenValues[X, Y]() extends FA1[Map[X, Expr[Y]], Map[X, Y]] {
    val el: elementValue[X, Y] = elementValue[X, Y]()
    def apply(a: Map[X, Expr[Y]]): Expr[Map[X, Y]] = {
      if (a.isEmpty) Map[X, Y]()
      else concatMap()(el(a.head._1, a.head._2), this(a.tail))
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

  object rangeOfSeq extends FA1[CEAV, EAVRange] {
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
      else %(rangeOfRanges, x.parts.map(p => %(this, p.asInstanceOf[CEAV])).toList)
    }
  }

  object rangeOfRanges extends FA1[List[Expr[EAVRange]], EAVRange] {
    def apply(x: List[Expr[EAVRange]]): Expr[EAVRange] = {
      if (x.lengthCompare(0) == 0) sys.error("no")
      else if (x.lengthCompare(1) == 0) x.head
      else %(mergeRanges, x.head, %(rangeOfRanges, x.tail))
    }
  }

  object mergeRanges extends FA2[EAVRange, EAVRange, EAVRange] {
    def apply(r1: EAVRange, r2: EAVRange): Expr[EAVRange] = r1.union(r2)
  }
}
