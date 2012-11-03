package spread

object Types {

  // very abstract interface that covers all use cases //
  trait Relation[A,B] {
    def create(a: A, b: Set[B]): Relation[A,B]
    def apply(a: A): Relation[B,B]

    def size: Long
    def flip: Relation[B,A]

    def split(a: A): (Relation[A,B],Relation[A,B])
    def union(o: Relation[A,B]): Relation[A,B]
  }
}