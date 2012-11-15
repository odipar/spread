package spread

object AbstractImmutableSequence {

  trait ISeq[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]] {
    def self: SS

    def measure(implicit c: CC): Option[M]
    def size(implicit c: CC): N

    def first(implicit c: CC): Option[X]
    def last(implicit c: CC): Option[X]

    def left(implicit c: CC): SS
    def right(implicit c: CC): SS

    def append(s: SS)(implicit c: CC): (SS,CC)

    def depth: Int
  }

  trait ISeqContext[N,X,M, SS <: ISeq[N,X,M,SS,CC], CC <: ISeqContext[N,X,M,SS,CC]] {
    def self: CC
    implicit def c: CC = self

    def empty: (SS,CC)
    def create(x: X): (SS,CC)
    def create(l: SS, r: SS): (SS,CC)

    def append(s1: SS, s2: SS): (SS,CC)

    def measure(x: Option[X]): Option[M]
    def measure(l: Option[M],r: Option[M]): Option[M]

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
  }

  trait ISeqContextImpl[N,X,M,SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]] extends ISeqContext[N,X,M,SS,CC] {
    def measure(x: Option[X]): Option[M] = None
    def measure(l: Option[M], r: Option[M]): Option[M] = None
    def append(s1: SS, s2: SS): (SS,CC) = s1.append(s2)
    def split(s: SS, n: N): (SS,SS,CC) = s.split(n)
  }
}