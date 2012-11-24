package spread

// Debit\Credit Numbers (Nat\Nat) - NO ZEROs!

object Integer {
  import Natural._

  def iOne: IntPImpl[NatImpl] = Integer.create(nOne+nOne,nOne)

  def create[N <: Nat[N]](d: N, c: N): IntPImpl[N] = {
    NatPair(d,c)
  }

  trait IntP[N <: Nat[N],I <: IntP[N,I]] extends Nat[I] {
    def debit: N
    def credit: N
    def negate: I

    def unary_-() = negate

    override def toString = debit+"\\"+credit
  }

  trait IntPImpl[N <: Nat[N]] extends IntP[N,IntPImpl[N]] {
    def self = this
    type IP = IntPImpl[N]

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

  case class NatPair[N <: Nat[N]](debit: N, credit: N) extends IntPImpl[N]
}