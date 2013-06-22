package spread

import spread.SequenceImplementation.EmptySeqImpl

object SequenceImplementation {
  import AbstractImmutableSequence._

  trait SeqI[N,X,M,SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC], SIS <: SeqI[N,X,M,SS,CC,SIS]]
  {
    def s: SS // returns the concrete Seq
    def c: CC // returns the concrete SeqContext

    def construct(ss: (SS,CC)): SIS
    def create(x: X): SIS = construct(c.create(x))
    def add(x: X): SIS = this append create(x)
    def append(o: SIS): SIS = construct(s.append(o.s)(c))
    def compare(o: SIS): Int = s.compare(o.s)(c)
    def measure: Option[M] = s.measure(c)
    def size: N = s.size(c)
    def first: Option[X] = s.first(c)
    def last: Option[X] = s.last(c)
    def first2: Option[X] = s.first2(c)
    def last2: Option[X] = s.last2(c)
    def left: SIS = construct((s.left(c),c))
    def right: SIS = construct((s.right(c),c))
    def split(n: N): (SIS,SIS) = {
      val (l,r,cc) = s.split(n)(c)
      (construct((l,cc)),construct((r,cc)))
    }

    def prettyString2: String = {
      val ss = c.sizing
      if (ss.equals(size,ss.zero)) ""
      else if (ss.equals(size,ss.one)) first.get.toString
      else left.prettyString2 + "." + right.prettyString2
    }
    def prettyString = "<" + prettyString2 + ">"
  }

  trait SeqImpl[N,X,M, SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]]
   extends SeqI[N,X,M,SS,CC,SeqImpl[N,X,M,SS,CC]] {
    def construct(s: (SS,CC)) = {
      val (ss,cc) = s
      val sss = cc.sizing
      if (ss.equals(ss.size(c),sss.zero)) EmptySeqImpl(cc)
      else LSeqImpl(ss,cc)
    }
  }

  case class EmptySeqImpl[N,X,M, SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]](c: CC)
   extends SeqImpl[N,X,M,SS,CC] {
    def s = c.empty._1
  }

  case class LSeqImpl[N,X,M, SS <: ISeqImpl[N,X,M,SS,CC], CC <: ISeqContextImpl[N,X,M,SS,CC]](s: SS, c: CC)
    extends SeqImpl[N,X,M,SS,CC]
}
