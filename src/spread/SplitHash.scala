package spread

/**
  * Created by rapido on 20/01/16.
  */
object SplitHash {

  trait SplitHash[X] {
    def concat(s1: SplitHash[X], s2: SplitHash[X])
    def split(i: Long): (SplitHash[X], SplitHash[X])
    def size(s: SplitHash[X]): Long
    def iterator: Iterator[X]
    def explode: Seq[SplitHash[X]]
    def compress: SplitHash[X]
  }
}
