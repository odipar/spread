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

Unlike SPREAD however, spreadsheets are incapable to keep all their versions around. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets. In contrast, SPREAD only permits referentially transparent functions as primitives.

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

Indeed, destroying data in SPREAD is certainly possible, but directly goes against the grain of SPREAD. That said, keeping all traces is not always an option (due to memory constraints). So in that case, it is better to cryptographically sign a trace, rather than to destroy it. With signing, it will still possible to fully authenticate every computation.

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
.. but that's slightly different from the first definition: the first definition dynamically creates an anynomous function object, while the later creates an immutable top-level object. As it turns out, SPREAD only permits user-defined functions to be immutable top-level objects (this will be explained later).

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

when we evaluate fib(4):

```scala
println(%(fib,4).fullEval)
```
we get the following trace:
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
This generates a big trace already, just to calculate `fib(4)`. Although in this case, keeping the trace of `fib(4)` would be relatively cheap, the naïve storage of `fib(25)` would incur 971138 items - which is just too much overhead.

Fortunately, there are multiple strategies to choose from in order reduce the size of the trace. Of course, which strategy to use depends on certain trade-offs.
###Pruning traces
The first strategy is to prune a trace at certain levels. We can encode this pruning directly in another version of the fib function:
```scala
object fib2 extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else if (!i < 5) ~%(fib,!i)
      else %(fib2,!i-1) + %(fib2,!i-2)
    }
}
```
We apply the tilde(`~`) operator on `fib(i)` when `fib2(i)` is called with `i < 5`:
```scala
println(%(fib2,6).fullEval)
```
..  after evaluation, we get the following trace:
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
A possibly more advantageous strategy is to reduce the memory footprint via (dynamic) memoization. Memoization is nothing more than keeping a map (or index) that maps function calls to traces, during evaluation.

Applying memoization has 2 major benefits:

- the resulting trace of a function call can be re-used
- (sub)traces can be structurally shared via their references

If we look at `fib2(6)` then it is easy to spot the potential re-use of `fib(4)`. Via the memoization index, we don't need to calculate fib(4) twice, but only once.

Memoization works pretty good in practice for almost all algorithms. But memoization doesn't work when past (sub)computations don't share a lot of structure with future (sub)computations.

Memoization is easy to set up in SPREAD. We just need to associate every evaluation with a so called MemoizationContext. During evaluation, SPREAD memoizes and re-uses all function calls via the associated MemoizationContext.

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

Althoug all three computations *must* return the same result, the computational and memory bounds are completely different.

- The first computation is very inefficient as it cannot re-use previous `fib` calls. As a consequence, both the time complexity and the memory complexity are bound by `O(exp(n))`. However, memory will be garbage collected (GC'ed) and reclaimed as soon as the result becomes unreachable.

- The second computation is much more very efficient, as it does re-use the previous `fib` calls. Through memoization, both time- and memory complexity becomes `O(log(n)*n)`. However, traces will **never** be GC'ed, as long as the context still holds **strong** references to them.

- The third computation is as efficient as the second. But in this case, only *weak* references were held to every trace, so memory can be fully reclaimed when the result becomes unreachable.

Remember that the trace of  `fib(n)` grows exponentially with `n`. So how is it possible that the second and third computations are only bound by a memory complexity of `O(n*log(n))`?



Copyright 2016: Robbert van Dalen