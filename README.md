###SPREAD
I want to know *exactly* what my software is doing. Better still, I want *cryptographic proof* that my software executes each and every computation step correctly.

Wouldn't you?

Currently, I don't know what Windows 10 is doing (or it is *very* hard to find out) and I hate that.

That's because most of Windows 10:

* is compiled to machine code that bears no resemblance to the original source code,
* hides intricate networks of mutable objects after abstract interfaces,
* and destroys precious machine state after machine state.

Welcome to SPREAD!

In SPREAD, all data, machines states, code and computations are cryptographically authenticated.

To put it mildly, some practical issues had to be resolved to make SPREAD a reality. First and foremost, SPREAD almost completely eradicates mutable state.

Obviously, keeping *all* the state for authentication purposes would require enormous amounts of storage. SPREAD solves that issue by cryptographically 'signing' states *incrementally*, while still allowing full user-control over which ones need to be signed, and at what level of granularity.

Alternatively, full machine states can also be incrementally stored by SPREAD. In turn, this allows the pervasive re-use of state that was calculated earlier.

So SPREAD kinda acts like a spreadsheet. That's because spreadsheets also re-use the previous states of a workbook to optimally recalculate the next.

Unlike SPREAD however, spreadsheets are incapable of keeping all their versions around. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets. In contrast, SPREAD only permits referentially transparent functions as primitives.

So can SPREAD be used as a drop-in Excel replacement?

Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly the next-generation of software in general.

####Warning
SPREAD is currently in extreme ALPHA stage. For the impatient, there is already some novelty to be found in the repository:

####[SplitHash](https://github.com/odipar/spread/blob/master/src/spread/SplitHash.scala): an immutable, uniquely represented Sequence Authenticated Data Structure.

### Implementation
SPREAD is currently implemented as a Scala DSL. This is not an absolute requirement for SPREAD to work, but Scala does bring some nice benefits to the table:

* It runs on the Java Virtual Machine (JVM)
* Purely functional (where appropriate)
* Strongly typed at compile time
* And it supports the creation of Domain Specific Languages (DSLs)

(to be honest, I would have rather liked to implement SPREAD in the Avail Programming Language, but Avail's toolchain isn't just there yet).

###The anotomy of a simple SPREAD expression

Now let's take a look at this standard Scala code:
```scala
val e = (1 + 2) * (3 + 4)
```
A Scala compiler would compile the above source code into bytecode that would produce a single *Int * result when executed on the JVM.

And this is the SPREAD equivalent of the same Scala code:
```scala
val e = (1 !+ 2) !* (3 !+ 4)
```
Notice the exclamation `!` marks. In SPREAD, each operation that is prefixed by an exclamation mark *constructs* an expression, and is not directly evaluated. Consequently,  to get to the canonical value of an expression, we need to explicitly request its *evaluation*.

Now here is where the SPREAD magic comes in: the final result won't just be a single `Int`, but a full trace of all (sub)computations that lead up to that `Int`, including all the ancestor expressions!

So what happens if we run this?

```scala
println(e.fullEval)
```
we get the following result:
```
((1 !+ 2) !* (3 !+ 4)) =>
	(1 !+ 2) => 3
	(3 !+ 4) => 7
	(3 !* 7)
(3 !* 7) => 21
```

Notice that the full trace holds the final value 21. Alternatively, we may choose to destroy the trace by requesting its head.

Indeed, destroying data in SPREAD is certainly possible, but directly goes against the grain of SPREAD. That said, keeping all traces is not always an option (due to memory constraints). So in that case, it is better to cryptographically sign a trace, rather than to destroy it. With signing, it remains possible to fully authenticate every computation.

Authenticating a trace can be achieved by prefixing a tilde `~` to any expression. When such expression is evaluated, it will yield a default 128 bit cryptographic hash of its trace, together with its final value. In similar fashion, more cryptographic bits can be acquired by prefixing more consecutive tildes.

### Fibonacci revisited
To show how well this works in practice we first define the infamous fibonacci function in plain Scala:

```scala
object fib extends Function1[Int,Int] {
    def apply(i: Int) = {
        if (i < 2) 1
        else fib(i-1) + fib(i-2)
    }
}
```
Of course, real Scala wizards would abbreviate the above to:

```scala
val fib: Int=>Int = i => { if (i < 2) 1 ; else fib(i-1) + fib(i-2) }
```
.. but that's slightly different from the first definition: the first definition dynamically creates an anynomous function object, while the latter creates an immutable top-level object. Currently, SPREAD only permits user-defined functions to be immutable top-level objects (this restriction will eventually be lifted).

Now here is the same `fib` function in SPREAD format:
```scala
object fib extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)
    }
}
```
.. which I actually believe is not too bad: apart from the extra syntactic noise (due to limitations of the DSL) the SPREAD version pretty much matches the idiomatic Scala version.

now if we evaluate fib(4):

```scala
println(%(fib,4).fullEval)
```
we get the following result:
```
fib(4) => (fib(3) !+ fib(2))
(fib(3) !+ fib(2)) =>
	fib(3) => (fib(2) !+ fib(1))
	(fib(2) !+ fib(1)) =>
		fib(2) => (fib(1) !+ fib(0))
		(fib(1) !+ fib(0)) =>
			fib(1) => 1
			fib(0) => 1
			(1 !+ 1)
		(1 !+ 1) => 2
		fib(1) => 1
		(2 !+ 1)
	(2 !+ 1) => 3
	fib(2) => (fib(1) !+ fib(0))
	(fib(1) !+ fib(0)) =>
		fib(1) => 1
		fib(0) => 1
		(1 !+ 1)
	(1 !+ 1) => 2
	(3 !+ 2)
(3 !+ 2) => 5
```
Just calculating `fib(4)` already generates a medium sized trace. Although in this case, keeping the trace of `fib(4)` would be relatively cheap, the naïve storage of `fib(25)` would incur 971138 items - which is just too much overhead.

Fortunately, the size of a trace can be reduced by applying various evaluation strategies. Of course, which strategy to choose from depends on certain trade-offs that have to be made.
###Pruning traces
Our first strategy is to prune a trace at certain levels. We can encode pruning directly in another version of the fib function:
```scala
object fib2 extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else if (!i < 5) ~%(fib,!i)
      else %(fib2,!i-1) + %(fib2,!i-2)
    }
}
```
Notice that, when `fib2(i)` is called with `i < 5`, it applies the tilde(`~`) operator on `fib(i)`.

So,
```scala
println(%(fib2,6).fullEval)
```
..  gives us the following trace:
```
fib2(6) => (fib2(5) !+ fib2(4))
(fib2(5) !+ fib2(4)) =>
	fib2(5) => (fib2(4) !+ fib2(3))
	(fib2(4) !+ fib2(3)) =>
		fib2(4) => ~fib(4)
		~fib(4) =>
			#B415F7B40A7F9A0A3C57E23CD6EC2ED6 => 5
		fib2(3) => ~fib(3)
		~fib(3) =>
			#C87382C86910AD69BB6DC1BB60577160 => 3
		(5 !+ 3)
	(5 !+ 3) => 8
	fib2(4) => ~fib(4)
	~fib(4) =>
		#B415F7B40A7F9A0A3C57E23CD6EC2ED6 => 5
	(8 !+ 5)
(8 !+ 5) => 13
```

Notice that the fulll (sub)trace of fib(4) is now replaced by its cryptographic hash (indicated by the `#` prefix).
###Memoizing traces
A possibly more advantageous strategy is to reduce the memory footprint via (dynamic) memoization. Memoization is nothing more than a special kind of index that maps function calls to traces, during evaluation.

Memoization yields 2 major benefits:

- the resulting trace of a function call can be re-used
- (sub)traces can be structurally shared by reference

Now if we take a closer look at `fib2(6)` it is easy to spot a potential re-use of `fib(4)`. Via memoization, we don't need to calculate fib(4) twice, but only once.

Memoization works pretty good in practice for most algorithms. But it is not a silver bullet: memoization is only practical when past (sub)computations share a lot of structure with future (sub)computations.

Memoization is easy to set up in SPREAD. We just need to associate an evaluation with a so-called MemoizationContext. During evaluation, SPREAD memoizes and re-uses all function calls via the associated MemoizationContext.

There are currently three default Memoization strategies to choose from:

- EmptyContext : this context doesn't store anything
- StrongMemoizationContext: this context strongly references its items
- WeakMemoizationContext: this context weakly references its items

Here is an example that shows each strategy:
```scala
val empty  = EmptyMemoizationContext
val strong = StrongMemoizationContext(HashMap())
val weak   = WeakMemoizationContext(WeakHashMap())

var (result1,new_empty)  = %(fib,25).fullEval(empty)
var (result2,new_strong) = %(fib,25).fullEval(strong)
var (result3,new_weak)   = %(fib,25).fullEval(weak)

result1 = null
result2 = null
result3 = null

System.gc()
```

Although all three computations *must* return the same result, their computational and memory bounds are completely different.

- The first computation is very inefficient as it cannot re-use previous `fib` calls. As a consequence, both the time complexity and the memory complexity are bound by `O(exp(n))`. However, memory will be garbage collected (GC'ed) and reclaimed as soon as the result becomes unreachable.

- The second computation is much more very efficient, as it does re-use the previous `fib` calls. Through memoization, both time- and memory complexity becomes `O(log(n)*n)`. However, traces will **never** be GC'ed, as long as the context still holds **strong** references to them.

- The third computation is as efficient as the second. But in this case, only *weak* references were held to every trace, so memory can be fully reclaimed when the result becomes unreachable.

But remember that the trace of  `fib(n)` grows exponentially with `n`. So how is it possible to get such a low memory bound of `O(n*log(n))`?
### Exponential Data Structures
To get an idea of how to reach that bound we need to first consider the following simple example:
```scala
  def concat(x: List[Any], y: List[Any]): List[Any] = List(x,y)
  def size(x: List[Any]): Int= x match {
    case List(x: List[Any],y: List[Any]) => size(x) + size(y)
    case l: List[_] => l.size
  }

  var l: List[Any] = List(1,2,3,4)
  var i = 0
  while (i < 20) {
    l = concat(l,l)
    i = i + 1
  }
  println("size: " + size(l)) // 20 steps -> 4194304
```
With simple Lists we can already create `O(exp(n))` sized structures in `O(n)` time. But alas, to get to a specific element at a certain index also takes `O(n)`.

For SPREAD, a more advanced Sequence Abstract Data Type (ADT) has been implemented:

```scala
trait Sequence[X] {
	def size: Int
	def concat(o: Sequence[X]): Sequence[X]
    def split(i: Int): (Sequence[X], Sequence[X])
    def first: X
    def last: X
}
```

SPREAD works on top of this ADT, where all operations **must** be within time complexity bound `O(log(n))` (with the exception of `size` which **must** be `O(1)`). The Sequence ADT is used by SPREAD's evaluation algorithm that combines and concatenate (sub)traces into bigger traces.

There are numerous known data structures that can be used to implement this ADT. Examples are: AVL trees, Skip Lists, Treaps, Finger Trees, etc. However, none of them can meet another very important constraint:

*A certain ordered list of objects must always be uniquely and canonically be represented by the Sequence ADT.*

This constraint **must** also be satisfied, otherwise SPREAD wouldn't be able to *incrementally* and cryptographically sign computations, data, states, etc.

Now what, in essence, is the big deal breaker? The issue with standard cryptographic hashes is that they are not *incremental*.

Here is an example. Let's say that you've just calculated the SHA256 hash of a big multi-terabyte file, which took you about 5 minutes. After that you change 1 byte in the middle of the file.

The question is: how fast can you recalculate the SHA256 hash of that slightly modified file?

###SplitHash
The first known Sequence ADT that is able to incrementally re-hash slightly modified data in `O(log(n))` is called SplitHash.

What's interesting about SplitHash is that it isn't build on top of a single cryptographic hash type (like GIT's SHA1), but on top of 'infinite' hashes with the following API.

```scala
trait Hash {
	def hashAt(i: Int): Int
	override def hashCode = hashAt(0)
}
```

Pending crypthographic scrutiny, this API makes hash collisions very unlikely. Only objects that implement the Hash trait may be put into a SplitHash, so it is no surprise that all SPREAD primitives are Hashes, including SplitHash itself!

###Building them
It's very easy to create and split SplitHashes. See if you can follow what is going on:
```scala
val s1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8 // construct
val s2 = s1 ! s1
val (s3,s4) = s2.split(12)
val (s5,s6) = s2.split(2)
val s7 = s4 ! s5
val s8 = 5 ! 6 ! 7 ! 8 ! 1 ! 2

println(s7 == s8) // true
println(s7.hashAt(0) == s8.hashAt(0)) // true

```

So SplitHashes are used by SPREAD as the basic building block for traces. But they are also extremely useful to implement incremental algorithms for Sequences, such as fold, map, etc.
###Incremental Sum
The implementation of a sum algorithm in SPREAD showcases everything what makes SPREAD so special. With this implementation we get:

- An authenticated, proven sum
- Scalable to bigger sums
- Optionally incremental (spreadsheet like)

Here is the implementation:
```scala
object sum extends FA1[_SplitHash[Int],Int] {
    def apply(s: $SplitHash[Int]) = {
          val ss = !s
          if (ss.size == 1) ss.last
          else {
                val parts = ss.splitParts
                var ssum = %(sum,expr(parts(0)))
                var i = 1
                while (i < parts.length) {
                      ssum = ssum !+ %(sum,expr(parts(i)))
                      i = i + 1
                }
                ssum
          }
    }
}
```
Yes I know, not the prettiest code. But don't bother too much about the syntactic sugar for now, let's put it to use:

```scala
val context = WeakMemoizationContext(WeakHashMap())
val seq1 = 1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8
val seq2 = 1 ! 2 ! 3 ! 9 ! 5 ! 6 ! 7 ! 8

val (sum1,context2) = %(sum,expr(seq1)).eval(context)
val (sum2,context3) = %(sum,expr(seq2)).eval(context2)

println(sum1)
println(sum2)
```
What now follows is the output of the two sums:

###**WE FIRST LOOK AT THE TRACE OF THE FIRST SUM1. KEEP SCROLLING**
```
sum($(1 ! 2 ! 3 ! 4 ! 5 ! 6 ! 7 ! 8)) => (sum($(1 ! 2 ! 3 ! 4)) !+ sum($(5 ! 6 ! 7 ! 8)))
(sum($(1 ! 2 ! 3 ! 4)) !+ sum($(5 ! 6 ! 7 ! 8))) =>
	sum($(1 ! 2 ! 3 ! 4)) => (sum($(1 ! 2)) !+ sum($(3 ! 4)))
	(sum($(1 ! 2)) !+ sum($(3 ! 4))) =>
		sum($(1 ! 2)) => (sum($(1)) !+ sum($(2)))
		(sum($(1)) !+ sum($(2))) =>
			sum($(1)) => 1
			sum($(2)) => 2
			(1 !+ 2)
		(1 !+ 2) => 3
		sum($(3 ! 4)) => (sum($(3)) !+ sum($(4)))
		(sum($(3)) !+ sum($(4))) =>
			sum($(3)) => 3
			sum($(4)) => 4
			(3 !+ 4)
		(3 !+ 4) => 7
		(3 !+ 7)
	(3 !+ 7) => 10
	sum($(5 ! 6 ! 7 ! 8)) => (sum($(5 ! 6)) !+ sum($(7 ! 8)))
	(sum($(5 ! 6)) !+ sum($(7 ! 8))) =>
		sum($(5 ! 6)) => (sum($(5)) !+ sum($(6)))
		(sum($(5)) !+ sum($(6))) =>
			sum($(5)) => 5
			sum($(6)) => 6
			(5 !+ 6)
		(5 !+ 6) => 11
		sum($(7 ! 8)) => (sum($(7)) !+ sum($(8)))
		(sum($(7)) !+ sum($(8))) =>
			sum($(7)) => 7
			sum($(8)) => 8
			(7 !+ 8)
		(7 !+ 8) => 15
		(11 !+ 15)
	(11 !+ 15) => 26
	(10 !+ 26)
(10 !+ 26) => 36
```

###**NOW WE LOOK AT THE TRACE OF THE SECOND SUM2. KEEP SCROLLING**
```
sum($(1 ! 2 ! 3 ! 9 ! 5 ! 6 ! 7 ! 8)) => (sum($(1 ! 2 ! 3 ! 9)) !+ sum($(5 ! 6 ! 7 ! 8)))
(sum($(1 ! 2 ! 3 ! 9)) !+ sum($(5 ! 6 ! 7 ! 8))) =>
	sum($(1 ! 2 ! 3 ! 9)) => (sum($(1 ! 2)) !+ sum($(3 ! 9)))
	(sum($(1 ! 2)) !+ sum($(3 ! 9))) =>
		sum($(1 ! 2)) => (sum($(1)) !+ sum($(2)))
		(sum($(1)) !+ sum($(2))) =>
			sum($(1)) => 1
			sum($(2)) => 2
			(1 !+ 2)
		(1 !+ 2) => 3
		sum($(3 ! 9)) => (sum($(3)) !+ sum($(9)))
		(sum($(3)) !+ sum($(9))) =>
			sum($(3)) => 3
			sum($(9)) => 9
			(3 !+ 9)
		(3 !+ 9) => 12
		(3 !+ 12)
	(3 !+ 12) => 15
	sum($(5 ! 6 ! 7 ! 8)) => (sum($(5 ! 6)) !+ sum($(7 ! 8)))
	(sum($(5 ! 6)) !+ sum($(7 ! 8))) =>
		sum($(5 ! 6)) => (sum($(5)) !+ sum($(6)))
		(sum($(5)) !+ sum($(6))) =>
			sum($(5)) => 5
			sum($(6)) => 6
			(5 !+ 6)
		(5 !+ 6) => 11
		sum($(7 ! 8)) => (sum($(7)) !+ sum($(8)))
		(sum($(7)) !+ sum($(8))) =>
			sum($(7)) => 7
			sum($(8)) => 8
			(7 !+ 8)
		(7 !+ 8) => 15
		(11 !+ 15)
	(11 !+ 15) => 26
	(15 !+ 26)
(15 !+ 26) => 41
```
###**DID YOU NOTICE ANY DIFFERENCE BETWEEN THE TWO?**
Of course you did! But did you also notice that there were some (sub)sums shared between `sum1` and `sum2`?

Most notably the memoization of (sub)sum of `5 ! 6 ! 7 ! 8` was re-used. That's almost half of the sum. Of course, this is no coincidence. Indeed, it can be proven that - if only 1 element in a sequence is changed - we only need to do `O(log(n))` extra work to re-calculate its sum.

###Conclusion
For know, I'm done explaining the basics of SPREAD. I hope that you liked it.

So what's next?

Probably the coming two months I'll spend writing a formal paper on SplitHash. After that I'm planning to implement some radical new database technology on top of SPREAD:

###SPREAD: A database like Datomic but then with spreadsheet like capabilities!

Oh yeah, and SPREAD *finally* supersedes the [Enchilada Programming Language](http://www.enchiladacode.nl).

* * *

Copyright 2016: Robbert van Dalen