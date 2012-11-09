package spread



// very abstract types that cover all use cases //
object Types {

  // immutable sorted set
  trait SortedSet[A,S <: SortedSet[A,S]] {
    def create(a: A): S                 // must be O(1)
    def equalsTo(o: S): Boolean         // must be O(1)

    def get(a: A): Option[A]            // maximally log(size)
    def split(a: A): (S,Option[A],S)    // maximally log(size)

    def union(b: S): S                  // if this < b - maximally log(min(a.size,b.size))
    def order(a1: A, a2: A): Int        // orders two elements a < b that are the SortedSet
  }

  trait SSet[A] extends SortedSet[A,SSet[A]]

  trait MultiPair[A,B] {
    def domain: A
    def range: SSet[B]
  }

  // immutable binary relation
  trait Relation[A,B] extends SortedSet[MultiPair[A,B],Relation[A,B]] {
    def flip: Relation[B,A]
  }

  // immutable expressions
  trait Expr[A,B] {
    def evaluate: Expr[A,B]
    def bind(b: SSet[Binding[A,B]]): Expr[A,B]
    def bindings: SSet[Binding[A,B]]
  }

  trait Binding[A,B] extends Expr[A,B] {
    def labeled: Expr[A,B]
    def label: B
  }

  trait UnaryOperator[A,B] extends Expr[A,B] {
    def first: Expr[A,B]
  }

  trait BinaryOperator[A,B] extends UnaryOperator[A,B] {
    def second: Expr[A,B]
  }
}