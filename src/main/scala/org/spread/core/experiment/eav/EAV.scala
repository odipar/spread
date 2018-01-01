package org.spread.core.experiment.eav

import spire.algebra.Order

object EAV {
  case class Entity(id: Array[Long]) {
    override def toString: String = id.tail.foldLeft(id(0).toString)((x, y) => x + "." + y)
  }

  trait Attribute {
    val name: String = getClass.getName
    override def toString: String = name
    def compare(a1: Value, a2: Value): Int
  }
  
  trait StringAttribute extends Attribute {
    def compare(a1: Value, a2: Value): Int = {
      a1.asInstanceOf[StringValue].compareTo(a2.asInstanceOf[StringValue])
    }
  }
  trait DoubleAttribute extends Attribute {
    def compare(a1: Value, a2: Value): Int = {
      a1.asInstanceOf[DoubleValue].compareTo(a2.asInstanceOf[DoubleValue])
    }
  }
  trait LongAttribute extends Attribute {
    def compare(a1: Value, a2: Value): Int = {
      a1.asInstanceOf[LongValue].compareTo(a2.asInstanceOf[LongValue])
    }
  }

  trait Value {
    def asDouble: DoubleValue = DoubleErrorValue
    def asString: StringValue = StringErrorValue
    def asLong: LongValue = LongErrorValue
  }

  trait NullValue extends Value {
    override def toString: String = "NULL"
  }
  
  trait ErrorValue extends Value {
    override def toString: String = "ERROR"
  }

  def compareSpecialValues(v1: Value, v2: Value): Int = {
    v1 match {
      case n1: NullValue => v2 match {
        case n2: NullValue => 0
        case _ => -1
      }
      case n1: ErrorValue => v2 match {
        case n2: ErrorValue => 0
        case _ => -1
      }
      case _ => v2 match {
        case n2: NullValue => 1
        case n2: ErrorValue => -1
        case _ => 0
      }
    }
  }

  trait StringValue extends Value {
    def compareTo(o: StringValue): Int = {
      val c = compareSpecialValues(this, o)
      if (c == 0) this.asInstanceOf[StringValueImpl].s.compareTo(o.asInstanceOf[StringValueImpl].s)
      else c
    }
    override def asString: StringValue = this
  }
  case class StringValueImpl(s: String) extends StringValue {
    override def toString: String = "\"" + s + "\""
  }
  object StringNullValue extends StringValue with NullValue
  object StringErrorValue extends StringValue with ErrorValue

  trait LongValue extends Value {
    override def asLong: LongValue = this
    def compareTo(o: LongValue): Int = {
      val c = compareSpecialValues(this, o)
      if (c == 0) this.asInstanceOf[LongValueImpl].l.compareTo(o.asInstanceOf[LongValueImpl].l)
      else c
    }
  }
  case class LongValueImpl(l: Long) extends LongValue {
    override def toString: String = l.toString
  }
  object LongNullValue extends LongValue with NullValue
  object LongErrorValue extends LongValue with ErrorValue

  trait DoubleValue extends Value {
    override def asDouble: DoubleValue = this
    def compareTo(o: DoubleValue): Int = {
      val c = compareSpecialValues(this, o)
      if (c == 0) this.asInstanceOf[DoubleValueImpl].d.compareTo(o.asInstanceOf[DoubleValueImpl].d)
      else c
    }
  }
  case class DoubleValueImpl(d: Double) extends DoubleValue {
    override def toString: String = d.toString
  }
  object DoubleNullValue extends DoubleValue with NullValue
  object DoubleErrorValue extends DoubleValue with ErrorValue

  trait EAV extends EAVRange {
    def e: Entity
    def a: Attribute
    def v: Value

    def start: EAV = this
    def end: EAV = this
    override def toString: String = e.toString + "." + a.toString + " = " + v.toString
  }

  trait EAVRange {
    def start: EAV
    def end: EAV

    def union(o: EAVRange): EAVRange = EAVRangeImpl(min(start, o.start), max(end, o.end))
    def intersect(o: EAVRange): EAVRange = EAVRangeImpl(max(start, o.start), min(end, o.end))
    def isEmpty: Boolean = ord.compare(start, end) > 0

    @inline def ord: Order[EAV] = EAVOrdering
    @inline def min(r1: EAV, r2: EAV): EAV = if (ord.compare(r1, r2) < 0) r1; else r2
    @inline def max(r1: EAV, r2: EAV): EAV = if (ord.compare(r1, r2) > 0) r1; else r2
  }

  case class EAVRangeImpl(start: EAV, end: EAV) extends EAVRange
  
  case class StringEAV(e: Entity, a: StringAttribute, v: StringValue) extends EAV
  case class LongEAV(e: Entity, a: LongAttribute, v: LongValue) extends EAV
  case class DoubleEAV(e: Entity, a: DoubleAttribute, v: DoubleValue) extends EAV

  case class AttributeEAV(a: Attribute) extends EAV {
    def e: Entity = sys.error("No entity")
    def v: Value = sys.error("No value")
    override def toString: String =  "?" + a.toString + "?"
  }

  case class AttributeValueEAV(a: Attribute, v: Value) extends EAV {
    def e: Entity = sys.error("No entity")
    override def toString: String =  "?" + a.toString + " = " + v
  }

  implicit def eavOrdering: Order[EAV] = EAVOrdering

  def rank(a: Value): Int = a match {
    case l: LongValue => 0
    case d: DoubleValue => 1
    case i: StringValue => 2
  }

  def compareEntityIds(i1: Array[Long], i2: Array[Long]): Int = {
    val s = i1.length min i2.length
    var i = 0
    var c = 0

    while ((i < s) && (c == 0)) {
      c = i1(i).compareTo(i2(i))
      i += 1
    }
    if (c == 0) i1.length.compareTo(i2.length)
    else c
  }

  object EntityOrdering extends Order[Entity] {
    def compare(e1: Entity, e2: Entity): Int = compareEntityIds(e1.id, e2.id)
  }

  object AttributeOrdering extends Order[Attribute] {
    def compare(a1: Attribute, a2: Attribute): Int = a1.name.compareTo(a2.name)
  }

  object ValueOrdering extends Order[Value] {
    def compare(v1: Value, v2: Value): Int = {
      val r1 = rank(v1)
      val r2 = rank(v2)
      val c = r1 - r2
      if (c == 0) v1 match {
        case s: StringValue => s.compareTo(v2.asInstanceOf[StringValue])
        case l: LongValue => l.compareTo(v2.asInstanceOf[LongValue])
        case d: DoubleValue => d.compareTo(v2.asInstanceOf[DoubleValue])
      }
      else c
    }
  }

  implicit val entityOrdering: Order[Entity] = EntityOrdering
  implicit val attributeOrdering: Order[Attribute] = AttributeOrdering
  implicit val valueOrdering: Order[Value] = ValueOrdering

  object EAVOrdering extends Order[EAV] {
    val eOrd: Order[Entity] = EntityOrdering
    val aOrd: Order[Attribute] = AttributeOrdering
    val vOrd: Order[Value] = ValueOrdering
    
    def compare(r1: EAV, r2: EAV): Int = {
      val c = eOrd.compare(r1.e, r2.e)
      if (c == 0) {
        val c = aOrd.compare(r1.a, r2.a)
        if (c == 0) vOrd.compare(r1.v, r2.v)
        else c
      }
      else c
    }
  }
}
