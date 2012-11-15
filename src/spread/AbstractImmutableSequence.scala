package spread

object AbstractImmutableSequence {

  // Abstract Immutable Ordered Sequence with a measurement.
  // Example measurements are size, depth, min, max, etc.
  // Finger trees, AVL trees, Radix trees (and other implementations), need at least implement ISeq and ISeqContext

  trait ISeq[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]] {
    def self: SS

    def measure(implicit c: CC): Option[M]
    def size(implicit c: CC): N

    def first(implicit c: CC): Option[X]
    def last(implicit c: CC): Option[X]

    def left(implicit c: CC): SS
    def right(implicit c: CC): SS

    def append(s: SS)(implicit c: CC): (SS,CC)
    def compare(s: SS)(implicit c: CC): Int
  }

  // ISeqContext drives the construction of new ISeqs with empty, create and join (building).
  // It must provide a (partial) order on ISeq elements with compareOrder (ordering).
  // The accompanying ISeq may be optionally measured via the measure method. (measuring).
  // New immutable ISeqContexts can be created and threaded while creating ISeqs (memoization)

  trait ISeqContext[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]] {
    def self: CC
    implicit def c: CC = self

    def empty: (SS,CC)
    def create(x: X): (SS,CC)
    def create(l: SS, r: SS): (SS,CC)

    def append(s1: SS, s2: SS): (SS,CC)

    def measure(x: Option[X]): Option[M]
    def measure(l: Option[M],r: Option[M]): Option[M]

    def compareOrder(x1: X, x2: X): Int

    def sizing: Num[N]
  }

  trait Num[N] {
    def zero: N
    def one: N

    def add(n1: N, n2: N): N
    def sub(n1: N, n2: N): N
    def mul(n1: N, n2: N): N

    def equals(n1: N, n2: N): Boolean
    def compare(n1: N, n2: N): Int
  }

  trait ISeqImpl[N,X,M,SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]] extends ISeq[N,X,M,SS,CC] {
    def split(n: N)(implicit c: CC): (SS,SS,CC) = {
      val s = c.sizing
      if (s.compare(n,s.zero) <= 0) (c.empty._1,self,c)
      else if (s.compare(n,size) >= 0) (self,c.empty._1,c)
      else {
        val cc = s.compare(left.size,n)
        if (cc == 0) (left,right,c)
        else if (cc < 0) {
          val (l,r,c1) = c.split(right,s.sub(n,left.size))
          val (l2,c2) = c1.append(left,l)
          (l2,r,c2)
        }
        else {
          val (l,r,c1) = c.split(left,n)
          val (r2,c2) = c1.append(r,right)
          (l,r2,c2)
        }
      }
    }

    def compare(o: SS)(implicit c: CC): Int = {
      if (self eq o) 0 // fast equality
      else {
        val ss = c.sizing
        val ts = size
        val os = o.size
        val cc = ss.compare(ts,os)

        if (cc == 0) {
          if (ss.equals(ts,ss.zero)) 0
          else if (ss.equals(ts,ss.one)) c.compareOrder(first.get,o.first.get)
          else {
            val ls = left.size
            val (l,r,c2) = o.split(ls)
            val c1 = c2.compare(left,l)
            if (c1 == 0) c2.compare(right,r)
            else c1
          }
        }
        else if (cc < 0) {
          val (l,r,c2) = o.split(ts)
          val c1 = c2.compare(self,l)
          if (c1 == 0) -1
          else c1
        }
        else -o.compare(self)
      }
    }
  }

  trait ISeqContextImpl[N,X,M,SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]] extends ISeqContext[N,X,M,SS,CC] {
    def measure(x: Option[X]): Option[M] = None
    def measure(l: Option[M], r: Option[M]): Option[M] = None
    def append(s1: SS, s2: SS): (SS,CC) = s1.append(s2)
    def split(s: SS, n: N): (SS,SS,CC) = s.split(n)
    def compare(s1: SS, s2: SS): Int = s1.compare(s2)
  }

  case class ISeqOrdering[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]](implicit c: CC) extends Ordering[SS] {
    def compare(s1: SS, s2: SS): Int = s1.compare(s2)(c)
  }

  implicit def seqOrdering[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]](implicit c: CC): Ordering[SS] = {
    ISeqOrdering[N,X,M,SS,CC]()
  }
}