package org.spread.core.experiment.sequence

import java.io.ByteArrayOutputStream
import java.lang.ref.WeakReference
import java.util

import com.esotericsoftware.kryo.io.{Input, Output}
import org.spread.core.splithash.Hashing
import com.twitter.chill.{KryoBase, ScalaKryoInstantiator}

import scala.collection.mutable

object Storage {

  final def main(args: Array[String]): Unit = {

    for (i <- 1 to 1000) {
      implicit val s = InMemoryStorage()

      val leaf: Leaf[Long] = Leaf((12345667904395542L until (12345667904395542L + 32)).toArray)
      val branch1: Branch[Long] = Branch((0 until 32).map(x => leaf).toArray)
      val branch2: Branch[Long] = Branch((0 until 32).map(x => branch1).toArray)
      val branch3: Branch[Long] = Branch((0 until 32).map(x => branch2).toArray)
      val branch4: Branch[Long] = Branch((0 until 16).map(x => branch3).toArray)

      println("size: " + branch4.size)

      val stored = branch4.store(s)

      println("stored: " + s.m.values.map(x => x.b.length).sum)

      println("i: " + i)
    }
  }

  trait Ref {
    def size: Int
    def apply(i: Int): Int

    override def equals(o: Any): Boolean = o match {
      case r: Ref => {
        val s = size
        var i = 0
        var eq = true
        
        while ((i < s) && eq) {
          eq = this(i) == r(i)
          i += 1
        }
        eq
      }
      case _ => false
    }

    override def hashCode: Int = {
      var hash = 1234567
      var s = size
      var i = 0
      while (i < s) {
        hash = Hashing.siphash24(hash, hash | this(i))
        i += 1
      }
      hash
    }
  }

  case class LongRef(l: Long) extends Ref {
    def size: Int = 2
    def apply(i: Int): Int = {
      if (i == 0) l.toInt
      else if (i == 1) (l >>> 32).toInt
      else sys.error("index out of bounds")
    }
    override def hashCode: Int = {
      Hashing.siphash24(l.toInt, (l >>> 32).toInt)
    }
  }

  trait Storage {
    def get[S <: Storable[S]](id: Ref): S
    def put[S <: Storable[S]](s: S): Ref
  }

  case class ByteArrayWrapper(b: Array[Byte]) {
    override def equals(o: Any) = o match {
      case ba: ByteArrayWrapper => {
        util.Arrays.equals(b, ba.b)
      }
      case _ => false
    }

    override val hashCode: Int = util.Arrays.hashCode(b)
  }

  case class InMemoryStorage() extends Storage() {
    val m: mutable.HashMap[Ref, ByteArrayWrapper] = mutable.HashMap()
    val m2: mutable.HashMap[ByteArrayWrapper, Ref] = mutable.HashMap()
    var i: Long = 12345667904395542L
    val oBuffer = new ByteArrayOutputStream(2048)
    val rOutput = new Output(oBuffer)

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
      kryo.setReferences(false)
      kryo
    }

    def get[S <: Storable[S]](id: Ref): S = {
      kryo.readClassAndObject(new Input(m(id).b)).asInstanceOf[S]
    }
    def put[S <: Storable[S]](s: S): Ref = {
      rOutput.clear
      kryo.writeClassAndObject(rOutput, s)
      val ba = ByteArrayWrapper(rOutput.toBytes)

      if (m2.contains(ba)) m2(ba)
      else {
        val ref = LongRef(i)

        m.update(ref, ba)
        m2.update(ba, ref)

        i += 1
        ref
      }
    }
  }

  trait Storable[+S <: Storable[S]] {
    def store(s: Storage): S
  }

  trait Tree[X] extends Storable[Tree[X]] {
    def size: Int
    def store(s: Storage): Tree[X] = this
    def first(implicit s: Storage): X
  }

  case class EmptyTree[X]() extends Tree[X] {
    def size: Int = 0
    def first(implicit s: Storage): X = sys.error("no")
  }

  case class Leaf[X](x: Array[X]) extends Tree[X]     {
    def size: Int = x.length
    def first(implicit s: Storage) = x(0)
  }

  def storeTree[X](t: Tree[X], s: Storage): Ref = s.put(t.store(s))

  case class Branch[X](children: Array[Tree[X]]) extends Tree[X]
  {
    val size: Int = {
      var s = children.length
      var i = 0
      var sum = 0
      while (i < s) {
        sum = sum + children(i).size
        i += 1
      }
      sum
    }

    override def store(s: Storage): Tree[X] = {
      var cc = children.clone()

      var sz = children.size
      var i = 0

      while (i < sz) {
        var child = children(i)

        if (child.isInstanceOf[RefTree[X]]) {
          cc(i) = child
        }
        else {
          var ref = storeTree(children(i), s)
          cc(i) = RefTree(ref, children(i).size)
        }

        i += 1
      }

      val n: Tree[X] = Branch(cc)
      RefTree(s.put(n), size)
    }
    def first(implicit s: Storage): X = children(0).first
  }

  case class RefTree[X](ref: Ref, size: Int) extends Tree[X] {
    @transient var cache: WeakReference[Tree[X]] = new WeakReference(null)

    def resolve(implicit s: Storage): Tree[X] = {
      val t: Tree[X] = cache.get
      if (t == null) {
        val t: Tree[X] = s.get(ref)
        cache = new WeakReference[Tree[X]](t)
        t
      }
      else t
    }
    def first(implicit s: Storage): X = resolve.first(s)
  }
}
