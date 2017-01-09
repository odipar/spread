package spread

import scala.reflect._
import scala.language.implicitConversions
import scala.language.existentials

object BinRel{

  trait BRel[X,Y,M,R <: BRel[X,Y,M,R,C,A],C <: BContext[X,Y,M,R,C,A],A <: BAnnotator[X,Y,M,R,C,A]]{
    def self: R
    def split(x: Long,c: C): (R,R)
    def concat(o: R,c: C): R
    def firstDomain: X
    def lastDomain: X
    def firstRange: Y
    def lastRange: Y
    def size: Long
    def annotation: M
  }

  trait BContext[X,Y,M,R <: BRel[X,Y,M,R,C,A],C <: BContext[X,Y,M,R,C,A],A <: BAnnotator[X,Y,M,R,C,A]]{
    def empty: R
    def create(x: X,y: Y): R
    def split(r: R,x: Long): (R,R)
    def concat(r1: R,r2: R): R
    def annotator: A
  }

  trait BAnnotator[X,Y,M,R <: BRel[X,Y,M,R,C,A],C <: BContext[X,Y,M,R,C,A],A <: BAnnotator[X,Y,M,R,C,A]]{
    def none: M
    def one(x: X,y: Y): M
    def many(d: Array[X],r: Array[Y]): M
    def append(r1: R,r2: R): M
  }

  case class BArrayRel[@specialized(Int,Long) X,@specialized(Int,Long) Y,M](domain: Array[X],range: Array[Y],annotation: M) extends BRel[X,Y,M,BArrayRel[X,Y,M],BArrayContext[X,Y,M],BArrayAnnotator[X,Y,M]]{
    {
      if (domain.length != range.length) sys.error("domain.length != range.length")
    }
    def self: BArrayRel[X,Y,M] = this
    def firstDomain: X = domain(0)
    def lastDomain: X = domain(domain.length - 1)
    def firstRange: Y = range(0)
    def lastRange: Y = range(range.length - 1)
    def size: Long = domain.length
    def split(x: Long,c: BArrayContext[X,Y,M]) = c.split(self,x)
    def concat(o: BArrayRel[X,Y,M],c: BArrayContext[X,Y,M]) = c.concat(self,o)
    override def toString: String ={
      var s = ""
      for (i <- 0 until domain.length) {
        s = s + domain(i) + "|" + range(i) + "\n"
      }
      s
    }
  }

  trait BArrayAnnotator[X,Y,M] extends BAnnotator[X,Y,M,BArrayRel[X,Y,M],BArrayContext[X,Y,M],BArrayAnnotator[X,Y,M]]

  case class BArrayContext[@specialized(Int,Long) X: ClassTag,@specialized(Int,Long) Y: ClassTag,M](a: BArrayAnnotator[X,Y,M]) extends BContext[X,Y,M,BArrayRel[X,Y,M],BArrayContext[X,Y,M],BArrayAnnotator[X,Y,M]]{
    type AR = BArrayRel[X,Y,M]
    type AA = BArrayAnnotator[X,Y,M]
    val annotator: AA = a
    def empty = BArrayRel(Array[X](),Array[Y](),annotator.none)
    def create(x: X,y: Y): BArrayRel[X,Y,M] = BArrayRel(Array(x),Array(y),annotator.one(x,y))
    def createBin(x: Array[X],y: Array[Y]) = BArrayRel(x,y,annotator.many(x,y))
    def split(r: BArrayRel[X,Y,M],x: Long): (AR,AR) ={
      val (d1,d2) = r.domain.splitAt(x.toInt)
      val (r1,r2) = r.range.splitAt(x.toInt)
      (BArrayRel(d1,r1,annotator.many(d1,r1)),BArrayRel(d2,r2,annotator.many(d2,r2)))
    }
    def concat(r1: BArrayRel[X,Y,M],r2: BArrayRel[X,Y,M]): AR ={
      BArrayRel(r1.domain ++ r2.domain,r1.range ++ r2.range,annotator.append(r1,r2))
    }
  }

  case class BStatistics[@specialized(Int,Long) X,@specialized(Int,Long) Y](minx: X,maxx: X,miny: Y,maxy: Y,sortedx: Boolean,sortedy: Boolean){
    override def toString: String ={
      "Stats[" +
        "\n minx: " + minx +
        "\n maxx: " + maxx +
        "\n miny: " + miny +
        "\n maxy: " + maxy +
        "\n sortedx: " + sortedx +
        "\n sortedy: " + sortedy + "\n]\n"
    }
    def domainAndRange(implicit xord: Ordering[X],yord: Ordering[Y]): (Domain[X],Domain[Y]) ={
      (DomainImpl(minx,maxx),DomainImpl(miny,maxy))
    }
    def domainAndRangeAny(xord: Ordering[_],yord: Ordering[_]): (Domain[_],Domain[_]) ={
      domainAndRange(xord.asInstanceOf[Ordering[X]],yord.asInstanceOf[Ordering[Y]])
    }
  }

  class StatisticsAnnotator[@specialized(Int,Long) X,@specialized(Int,Long) Y]
  (xord: Ordering[X],yord: Ordering[Y])
    extends BArrayAnnotator[X,Y,BStatistics[X,Y]]{
    type S = BStatistics[X,Y]
    def none: S = sys.error("no stats")
    def one(x: X,y: Y): S = BStatistics(minx = x,maxx = x,miny = y,maxy = y,sortedx = true,sortedy = true)
    def many(x: Array[X],y: Array[Y]): S ={
      val (minx,maxx,sortedx) = mmsorted(x,xord)
      val (miny,maxy,sortedy) = mmsorted(y,yord)
      BStatistics(minx,maxx,miny,maxy,sortedx,sortedy)
    }
    def append(r1: BArrayRel[X,Y,BStatistics[X,Y]],r2: BArrayRel[X,Y,BStatistics[X,Y]]): S ={
      val a1 = r1.annotation
      val a2 = r2.annotation
      BStatistics(
        xord.min(a1.minx,a2.minx),
        xord.max(a1.maxx,a2.maxx),
        yord.min(a1.miny,a2.miny),
        yord.max(a1.maxy,a2.maxy),
        a1.sortedx && a2.sortedx && xord.lteq(r1.lastDomain,r2.firstDomain),
        a1.sortedy && a2.sortedy && yord.lteq(r1.lastRange,r2.firstRange)
      )
    }
    def mmsorted[@specialized(Int,Long) X](a: Array[X],s: Ordering[X]): (X,X,Boolean) ={
      var mmin = a(0)
      var mmax = a(0)
      var msorted = true
      for (i <- 1 until a.length) {
        val x = a(i)
        mmin = s.min(mmin,x)
        mmax = s.max(mmax,x)
        msorted = msorted && s.lteq(a(i - 1),x)
      }
      (mmin,mmax,msorted)
    }
  }

  def ann[@specialized(Int,Long) X,@specialized(Int,Long) Y](xord: Ordering[X],yord: Ordering[Y]): StatisticsAnnotator[X,Y] = new StatisticsAnnotator[X,Y](xord,yord)
  implicit def cc[@specialized(Int,Long) X: ClassTag,@specialized(Int,Long) Y: ClassTag](implicit xord: Ordering[X],yord: Ordering[Y]): BArrayContext[X,Y,BStatistics[X,Y]] = BArrayContext[X,Y,BStatistics[X,Y]](ann(xord,yord))
  def createRel[@specialized(Int,Long) X: ClassTag,@specialized(Int,Long) Y: ClassTag](x: Array[X],y: Array[Y])(implicit xord: Ordering[X],yord: Ordering[Y]): BinRel[X,Y] ={
    val c = cc[X,Y]
    val r = c.createBin(x,y)
    BinRel(r,c)
  }

  case class BinRel[@specialized(Int,Long) X,@specialized(Int,Long) Y](r: BArrayRel[X,Y,BStatistics[X,Y]],c: BArrayContext[X,Y,BStatistics[X,Y]]){
    implicit def context: BArrayContext[X,Y,BStatistics[X,Y]] = c
    def statistics: BStatistics[X,Y] = r.annotation
    def split(x: Long): (BinRel[X,Y],BinRel[X,Y]) ={
      val (left,right) = r.split(x,c)
      (BinRel(left,c),BinRel(right,c))
    }
    def concat(o: BinRel[X,Y]): BinRel[X,Y] = BinRel(r.concat(o.r,c),c)
    def concatAny(o: BinRel[_,_]): BinRel[X,Y] = concat(o.asInstanceOf[BinRel[X,Y]])
    def firstDomain: X = r.firstDomain
    def lastDomain: X = r.lastDomain
    def firstRange: Y = r.firstRange
    def lastRange: Y = r.lastRange
    def size: Long = r.size
    override def toString: String = r.toString
  }

  trait Propagator

  object === extends Propagator {
    override def toString = "==="
  }

  sealed trait RelCol{
    def id: Symbol
    def left: Boolean
    def right: Boolean = !left
  }

  case class LeftCol(id: Symbol) extends RelCol{
    def left = true
    override def toString: String = id + ".L"
  }

  case class RightCol(id: Symbol) extends RelCol{
    def left = false
    override def toString: String = id + ".R"
  }

  case class RelConstraint(r1: RelCol,r2: RelCol,c: Propagator){
    override def toString = "" + r1 + c + r2
  }

  def model: Model = Model(Map(),Set(),Map(),true)

  trait Domain[X]{
    def ord: Ordering[X]
    def isValid: Boolean
    def lowerBound: X
    def upperBound: X
    def propagate(c: Propagator,o: Domain[X]): Domain[X]
    def propagateAny(c: Propagator,o: Domain[_]): Domain[_] = propagate(c,o.asInstanceOf[Domain[X]])
    def ==(o: Domain[X]): Domain[X] = propagate(===,o)
    override def toString: String = lowerBound + "..." + upperBound
  }

  case class DomainImpl[X](lowerBound: X,upperBound: X)(implicit o: Ordering[X]) extends Domain[X]{
    def ord = o
    def isValid = true
    def propagate(c: Propagator,o: Domain[X]): Domain[X] = c match {
      case === => {
        if (ord.gt(lowerBound,o.upperBound)) EmptyDomain()
        else if (ord.lt(upperBound,o.lowerBound)) EmptyDomain()
        else DomainImpl(ord.max(lowerBound,o.lowerBound),ord.min(upperBound,o.upperBound))(ord)
      }
      case _ => sys.error("cannot propagate")
    }
  }

  case class EmptyDomain[X]() extends Domain[X]{
    def isValid = false
    def ord = sys.error("domain is empty")
    def lowerBound: X = sys.error("domain is empty")
    def upperBound: X = lowerBound
    def propagate(c: Propagator,o: Domain[X]): Domain[X] = this
    override def toString: String = ".."
  }

  var solves: Long = 0

  case class Model(rels: Map[Symbol,BinRel[_,_]],
                   constraints: Set[RelConstraint],
                   domains: Map[Symbol,(Domain[_],Domain[_])],isValid: Boolean){
    def :+[X,Y](id: Symbol,rel: BinRel[X,Y])(implicit xord: Ordering[X],yord: Ordering[Y]): Model ={
      Model(rels + (id -> rel),constraints,domains + (id -> rel.statistics.domainAndRange),isValid)
    }
    def :!(r1: RelCol,r2: RelCol,cc: Propagator): Model = Model(rels,constraints + RelConstraint(r1,r2,cc),domains,isValid)
    def :=(r1: RelCol,r2: RelCol): Model = :!(r1,r2,===)

    def propagateConstraints: Model ={
      if (!isValid) this
      else {
        var fixpoint = false
        var newDomains = domains
        var newIsValid = isValid
        while (!fixpoint && newIsValid) {
          fixpoint = true
          val iter = constraints.iterator
          while(iter.hasNext && newIsValid) {
            val c = iter.next
            val d1 = newDomains(c.r1.id)
            val d2 = newDomains(c.r2.id)
            val dn = {
              if (c.r1.left) {
                if (c.r2.left) (d1._1.propagateAny(c.c,d2._1),d1._2)
                else (d1._1.propagateAny(c.c,d2._2),d1._2)
              }
              else {
                if (c.r2.left) (d1._1,d1._2.propagateAny(c.c,d2._1))
                else (d1._1,d1._2.propagateAny(c.c,d2._2))
              }
            }
            newIsValid = newIsValid && (dn._1.isValid) && (dn._2.isValid)

            newDomains = newDomains + (c.r1.id -> dn)
            if (dn != d1) {
              fixpoint = false
            }
          }
        }
        Model(rels,constraints,newDomains,newIsValid)
      }
    }
    def propagateRels: Model ={
      if (!isValid) this
      else {
        var newDomains = domains
        var newIsValid = isValid
        var iter = rels.keys.iterator

        while(iter.hasNext && newIsValid) {
          val r = iter.next
          val dm = domains(r)
          val ordx = dm._1.ord
          val ordy = dm._2.ord
          val rd = rels(r).statistics.domainAndRangeAny(ordx,ordy)
          val p1 = dm._1.propagateAny(===,rd._1)
          val p2 = dm._2.propagateAny(===,rd._2)
          val dn = (p1,p2)
          newIsValid = newIsValid && (dn._1.isValid) && (dn._2.isValid)
          newDomains = newDomains + (r -> dn)
        }
        Model(rels,constraints,newDomains,newIsValid)
      }
    }
    def split: (Model,Model,Symbol) ={
      if (!isValid) sys.error("cannot split. Model has no solutions")
      else {
        var rr: Symbol = null
        var i = rels.keys.iterator
        while (i.hasNext) {
          var r = i.next
          val dm = domains(r)
          val ordx = dm._1.ord
          val ordy = dm._2.ord
          val rd = rels(r).statistics.domainAndRangeAny(ordx,ordy)

          val p1 = dm._1.propagateAny(===,rd._1)
          val p2 = dm._2.propagateAny(===,rd._2)
          val dn = (p1,p2)

          if (dm != rd) rr = r
        }
        if (rr == null) { rr = rels.keysIterator.next }

        val rel: BinRel[_,_] = rels(rr)
        val (r1,r2) = rel.split(rel.size / 2)
        val m1 = Model(rels + (rr -> r1),constraints,domains,isValid)
        val m2 = Model(rels + (rr -> r2),constraints,domains,isValid)
        val c1 = m1.propagateRels.propagateConstraints
        val c2 = m2.propagateRels.propagateConstraints
        (c1,c2,rr)
      }
    }

    def merge(m: Model): Model ={
      val r1 = rels
      val r2 = m.rels
      var r3: Map[Symbol,BinRel[_,_]] = Map()
      for (r <- r1.keys) {
        val br1: BinRel[_,_] = r1(r)
        val br2: BinRel[_,_] = r2(r)
        val c: BinRel[_,_] = br1.concatAny(br2)
        r3 = r3 + (r -> c)
      }

      var doms: Map[Symbol,(Domain[_],Domain[_])] = Map()
      for (r <- r3.keys) {
        val rr = r3(r)
        val ordx = domains(r)._1.ord
        val ordy = domains(r)._2.ord
        doms = doms + (r -> rr.statistics.domainAndRangeAny(ordx,ordy))
      }
      Model(r3,constraints,doms,true).propagateConstraints
    }

    def isSolution: Boolean = {
      var isSolution = true
      for (r <- rels.keys) {
        if (rels(r).size != 1) isSolution = false
      }
      isSolution && isValid
    }

    def solve: Model = {
      if (isSolution || !isValid) this
      else {
        solves = solves + 1
        val (m1,m2,s) = split

        val mm1 = m1.solve
        val mm2 = m2.solve

        if (mm1.isValid) {
          if (mm2.isValid) mm1.merge(mm2)
          else mm1
        }
        else {
          if (mm2.isValid) mm2
          else Model(rels,constraints,domains,false)
        }
      }
    }

    override def toString: String = {
      var s = ""
      for (i <- rels.keys) {
        s = s + i + " - " + domains(i) + "\n"
        s = s + rels(i)
      }
      s
    }
  }

  case class ColSyntax(id: Symbol){
    def L = LeftCol(id)
    def R = RightCol(id)
  }

  implicit def toColSyntax(id: Symbol): ColSyntax = ColSyntax(id)
  final def main(args: Array[String]): Unit ={
    // a(X,Y),b(Y,X) => a'(X,Y),b'(Y,X)
   /* var a = createRel(Array(0,1,3,4,5),Array(2,3,6,1,2))
    var b = createRel(Array(1,2,3,5,6),Array(3,5,6,2,3))
    val p = model :+
      ('a,a) :+
      ('b,b) :=
      ('a.L,'b.R) :=
      ('b.R,'a.L) :=
      ('b.L,'a.R) :=
      ('a.R,'b.L)

    println(p)
    val pp = p.propagateConstraints
    println(pp.solve) */

    /*var a = createRel((0 to 10).toArray,(15 to 25).toArray)
    var b = createRel((10 to 20).toArray,(15 to 25).toArray)
    var c = createRel((15 to 25).toArray,(5 to 15).toArray)

    var a = createRel((0 to 10).toArray,(15 to 25).toArray)
    var b = createRel((15 to 25).toArray,(15 to 25).toArray)
    var c = createRel((15 to 25).toArray,(3 to 13).toArray)


    var p = model :+
      ('a,a) :+
      ('b,b) :+
      ('c,c) :=
      ('a.R,'b.L) :=
      ('b.L,'a.R) :=
      ('b.R,'c.L) :=
      ('c.L,'b.R) :=
      ('c.R,'a.L) :=
      ('a.L,'c.R)

    var pp = p.propagateConstraints

    pp = pp.solve
    println("pp: " + pp)
    println("pp: " + pp.isValid)  */

    var a = createRel((0 to 100).toArray,(200 to 300).toArray)
    var b = createRel((-50 to 50).toArray,(151 to 251).toArray)

    val p = model :+
      ('a,a) :+
      ('b,b) :=
      ('a.L,'b.L) :=
      ('b.L,'a.L) :=
      ('b.R,'a.R) :=
      ('a.R,'b.R)

    val pp = p.propagateConstraints.solve
    println("solves: " +solves)

    //println(pp)
    println(pp.isValid)
    println(pp)

  }
}