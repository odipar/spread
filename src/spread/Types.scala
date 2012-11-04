package spread

object Types {
  // very abstract types that cover all use cases //

  def main(args: Array[String]): Unit = {}

  trait SortedSet[A,S <: SortedSet[A,S]] {
    def create(a: A): S                 // must be O(1)
    def equals(o: S): Boolean           // must be O(1)

    def apply(a: A): Option[A]          // maximally log(size)
    def size: Long                      // maximally log(size)
    def split(a: A): (S, S)             // maximally log(size)

    def union(o: S): S                  // if a < b - maximally log(min(a.size,b.size))
    def order(a1: A, a2: A): Int        // orders two elements a < b that are the SortedSet
  }

  trait SSet[A] extends SortedSet[A,SSet[A]]

  trait MultiPair[A,B] {
    def domain: A
    def range: SSet[B]
  }

  trait Relation[A,B] extends SortedSet[MultiPair[A,B],Relation[A,B]] {
    def flip: Relation[B,A]
  }

  trait Name {
    def compare(o: Name): Int
  }

  trait Expr[A] {
    def evaluate: Expr[A]
    def variables: SSet[Var[A]]
    def bind(n: Name, e: Expr[A]): Expr[A]
  }

  trait Var[A] extends Expr[A] {
    def name: Name
    def expression: Expr[A]
  }

  trait UnaryOperator[A] extends Expr[A] {
    def first: Expr[A]
  }

  trait BinaryOperator[A] extends UnaryOperator[A] {
    def second: Expr[A]
  }
}