package org.spread.core.experiment.sequence

import java.io.ByteArrayOutputStream
import java.lang.ref.WeakReference
import java.util

import com.esotericsoftware.kryo.io.{Input, Output}
import org.spread.core.splithash.Hashing
import com.twitter.chill.{KryoBase, ScalaKryoInstantiator}
import org.spread.core.experiment.eav.EAV._
import org.spread.core.experiment.expression.Spread
import org.spread.core.experiment.expression.Spread.Cons
import org.spread.core.experiment.sequence.Sequence.ArraySeqImpl

import scala.collection.mutable
import scala.util.DynamicVariable

object Storage {

  val storage: DynamicVariable[Storage] = new DynamicVariable(InMemoryStorage())

  final def main(args: Array[String]): Unit = {

    for (i <- 1 to 1) {
      Storage.storage.withValue(InMemoryStorage()) {

        var tree: Tree[Long] = Leaf(ArraySeqImpl((0L until 32L).toArray))

        for (i <- 1 to 5) {
          tree = Branch(ArraySeqImpl((0 until 32).map(x => tree).toArray))
        }

        val s = tree.store
        
        println("sizes: " + Storage.storage.value.asInstanceOf[InMemoryStorage].m.values.map(x => x.size))
      }
    }
  }

  trait Ref {
    def size: Int
    def apply(i: Int): Int
  }

  case class LongRef(l: Long) extends Ref {
    def size: Int = 2
    def apply(i: Int): Int = {
      if (i == 0) l.toInt
      else if (i == 1) (l >>> 32).toInt
      else sys.error("index out of bounds")
    }
  }

  trait Storage {
    def get[X](id: Ref): X
    def put[X](t: X, s: X => X, r: Ref => X): X
  }

  case class ByteArrayWrapper(b: Array[Byte]) {
    def size: Int = b.length
    override def equals(o: Any): Boolean = o match {
      case ba: ByteArrayWrapper => util.Arrays.equals(b, ba.b)
      case _ => false
    }

    override def hashCode: Int = util.Arrays.hashCode(b)
  }
  
  case class InMemoryStorage() extends Storage() {
    val c: mutable.HashMap[Any, Any] = mutable.HashMap()
    val m: mutable.HashMap[Ref, ByteArrayWrapper] = mutable.HashMap()
    val m2: mutable.HashMap[ByteArrayWrapper, Ref] = mutable.HashMap()
    val buffer = new Array[Byte](16384)
    var i: Long = 0L

    lazy val instantiator: ScalaKryoInstantiator = {
      val i = new ScalaKryoInstantiator
      i.setRegistrationRequired(false)
      i
    }


    lazy val kryo: KryoBase = {
      val kryo = instantiator.newKryo()
      
      kryo.register(classOf[Tree[_]], 1000)
      kryo.register(classOf[Leaf[_]], 1001)
      kryo.register(classOf[Branch[_]], 1002)
      kryo.register(classOf[RefTree[_]], 1003)
      kryo.register(classOf[LongRef], 1004)
      kryo.register(classOf[Array[Tree[_]]], 1005)

      kryo.register(classOf[Array[TreeSequence.TreeNode[_,_ ]]], 1006)
      kryo.register(classOf[TreeSequence.TreeNode[_,_ ]], 1007)
      kryo.register(classOf[TreeSequence.Empty[_,_ ]], 1008)
      kryo.register(classOf[TreeSequence.Leaf[_,_ ]], 1009)
      kryo.register(classOf[TreeSequence.Branch[_,_ ]], 1010)
      kryo.register(classOf[TreeSequence.RefNode[_,_ ]], 1011)

      kryo.register(classOf[EAV], 1012)
      kryo.register(classOf[LongEAV], 1013)
      kryo.register(classOf[AttributeEAV], 1014)
      kryo.register(classOf[AttributeValueEAV], 1015)
      kryo.register(classOf[LongAttribute], 1016)
      kryo.register(classOf[StringAttribute], 1017)
      kryo.register(classOf[DoubleAttribute], 1018)
      kryo.register(classOf[Entity], 1019)
      kryo.register(classOf[LongValueImpl], 1020)


      kryo.register(classOf[Spread.Cons[_]], 1022)
      kryo.register(classOf[Spread.RefExpr[_]], 1023)
      kryo.register(classOf[Spread.FA0[_]], 1024)
      kryo.register(classOf[Spread.FA1[_,_]], 1025)
      kryo.register(classOf[Spread.FA2[_,_,_]], 1026)
      kryo.register(classOf[Spread.FA3[_,_,_,_]], 1027)
      kryo.register(classOf[Spread.add2], 1028)
      kryo.register(classOf[Spread.mul2], 1029)
      kryo.register(classOf[Spread.AnExpr[_]], 1030)
      kryo.register(classOf[Spread.IExpr], 1031)
      kryo.register(classOf[Spread.F1[_,_]], 1032)
      kryo.register(classOf[Spread.F2[_,_,_]], 1033)
      kryo.register(classOf[Spread.F3[_,_,_,_]], 1034)
      kryo.register(classOf[Spread.Empty[_]], 1035)
      kryo.register(classOf[Spread.Trace[_]], 1036)
      kryo.register(classOf[Spread.fib2], 1037)
      kryo.register(classOf[Spread.fac2], 1038)
      kryo.register(classOf[Spread.RefHashList[_]], 1039)
      kryo.register(classOf[ArraySeqImpl[_]], 1040)

      kryo.setReferences(false)
      kryo
    }

    def get[X](id: Ref): X = kryo.readClassAndObject(new Input(m(id).b)).asInstanceOf[X]

    def put[X](t: X, s: X => X, r: Ref => X): X = {
      if (c.contains(t)) c(t).asInstanceOf[X]
      else {
        val st = s(t)
        
        val rOutput = new Output(buffer)
        kryo.writeClassAndObject(rOutput, st)
        val ba = ByteArrayWrapper(rOutput.toBytes)

        if (ba.size > 64) {
          if (m2.contains(ba)) r(m2(ba))
          else {
            val ref = LongRef(i)

            c.update(t, r(ref))
            m.update(ref, ba)
            m2.update(ba, ref)

            i += 1
            r(ref)
          }
        }
        else {
          c.update(t, st)
          st
        }
      }
    }
  }

  trait Storable[+S <: Storable[S]] {
    def storage: Storage = Storage.storage.value
    def store: S
  }

  trait Tree[X] extends Storable[Tree[X]] {
    def size: Int
    def store: Tree[X] = this
    def first: X
  }

  case class EmptyTree[X]() extends Tree[X] {
    def size: Int = 0
    def first: X = sys.error("no")
  }

  case class Leaf[X](x: ArraySeqImpl[X]) extends Tree[X] {
    def size: Int = x.size
    def first = x(0)
    def ref1(x: Tree[X]): Tree[X] = this
    def ref2(x: Ref): Tree[X] = RefTree(x, size)
    override def store: Tree[X] = storage.put(this, ref1, ref2)
  }

  case class Branch[X](children: ArraySeqImpl[Tree[X]]) extends Tree[X]
  {
    val size: Int = {
      var s = children.size
      var i = 0
      var sum = 0
      while (i < s) {
        sum = sum + children(i).size
        i += 1
      }
      sum
    }

    def ref1(x: Tree[X]): Tree[X] = Branch(children.map(x => x.store))
    def ref2(x: Ref): Tree[X] = RefTree(x, size)
    override def store: Tree[X] = storage.put(this, ref1, ref2)

    def first: X = children(0).first
  }

  trait RefObject[S <: Storable[S]] extends Storable[S] {
    @transient var cache: WeakReference[S] = _

    def ref: Ref

    def resolve: S = {
      if (cache == null) {
        cache = new WeakReference[S](storage.get(ref))
        resolve
      }
      val n = cache.get
      if (n == null) {
        val z: AnyRef = storage.get(ref)
        cache = new WeakReference(z.asInstanceOf[S])
        z.asInstanceOf[S]
      }
      else n
    }

  }
  case class RefTree[X](ref: Ref, size: Int) extends Tree[X] with RefObject[Tree[X]] {
    def first: X = resolve.first
  }
}
