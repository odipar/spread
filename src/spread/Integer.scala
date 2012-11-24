package spread

// Debit\Credit Numbers (Nat\Nat) - NO ZEROs!

object Integer {
  import Natural._

  def iOne[O]: IntPImpl[O,NatImpl[O]] = Integer.create(nOne+nOne,nOne)

  def create[O,N <: Nat[O,N]](d: N, c: N): IntPImpl[O,N] = {
    NatPair(d,c)
  }

  trait IntP[O,N <: Nat[O,N],I <: IntP[O,N,I]] extends Nat[O,I] {
    def debit: N
    def credit: N
    def negate: I

    def unary_-() = negate

    override def toString = debit+"\\"+credit
  }

  trait IntPImpl[O,N <: Nat[O,N]] extends IntP[O,N,IntPImpl[O,N]] {
    def self = this
    type IP = IntPImpl[O,N]

    def half = {
      if (debit.even && !credit.even) create(debit.half.inc,credit.half)
      else create(debit.half,credit.half)
    }
    def div(o: IP) = this
    def add(o: IP) = create(debit + o.debit,credit + o.credit)
    def mul(o: IP) = create((debit * o.debit) + (credit * o.credit),(debit * o.credit) + (credit * o.debit))
    def diff1(o: IP) = create(debit.diff1(o.debit),credit.diff1(o.credit))
    def compare(o: IP) = (debit + o.credit).compare(credit + o.debit)
    def simplify = {
      if (debit > credit) create(debit.diff1(credit),debit.one)
      else create(debit.one,debit.diff1(credit))
    }
    def negate = create(credit,debit)

  }

  case class NatPair[O,N <: Nat[O,N]](debit: N, credit: N) extends IntPImpl[O,N]
}