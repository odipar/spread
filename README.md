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

So SPREAD kinda acts like a spreadsheet. Because spreadsheets also re-use the previous states of a workbook to optimally recalculate the next.

Unlike SPREAD however, spreadsheets are incapable to keep all their versions around. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets. In contrast, SPREAD only permits referentially transparent functions as primitives.

So can SPREAD be used as a drop-in Excel replacement?

Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly the next-generation of software in general.

####Warning
SPREAD is currently in extreme ALPHA stage. For the impatient, there is already something novelty to be found in the repository:

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
Notice the exclamation(`!`) marks. In SPREAD, each operation that is prefixed by an exclamation mark *constructs* an expression, and is not directly evaluated. We need to explicitly request the *evaluation* of an expression to get to its final value.

Now here is where the SPREAD magic comes in: the final evaluation result won't just be a single Int, but a full trace of all (sub)computations that lead up to that Int, including its ancestral one!

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

Notice that the full trace encompasses the final value 21. Alternatively, we may want to destroy the trace by requesting its head, i.e the following expression prints `true`:

```scala
println(e.fullEval.head === 21) // true
```

So destroying traces is certainly possible, but it goes directly against the spirit of SPREAD. When keeping full traces is not an option, it is better to cryptographically sign a trace, and keep that. This way, it is still possible to authenticate a computational trace.

Authenticating a trace can be achieved by prefixing a tilde (`~`) to any expression. When such expression is evaluated, it will yield a default 128 bit cryptographic hash of its trace, together with its final value. Likewise, more cryptographic bits can be acquired by prefixing more consecutive tildes.

### Fibonacci revisited
To see how well this works in practice we first define the infamous fibonacci function in plain Scala:

```scala
object fib extends Function1[Int,Int] {
    def apply(i: Int) = {
        if (i < 2) 1
        else fib(i-1) + fib(i-2)
    }
}
```
Of course, this can be abbreviated to:

```scala
val fib: Int=>Int = i => { if (i < 2) 1 ; else fib(i-1) + fib(i-2) }
```
.. but that's slight differnt from the first definition. The first definition creates an dynamically created function object, while the later creates an immutable top-level object. As it turns out, in SPREAD we want functions to be immutable top-level objects (this will be explained later on).

Now here is the same fib function in SPREAD format:
```scala

object fib extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else %(fib,!i - 1) !+ %(fib,!i - 2)
    }
}
```
.. which is not too bad! Apart from the extra syntactic noise (due to limits of the DSL), the SPREAD function definition pretty much matches the regular Scala version.

Now if we evaluate fib(4):

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
That's a big trace, just to calculate fib(4)! As traces are values on their own, keeping the trace of - say `fac(25)` - would incur a big memory overhead (the trace of `fib(25)` would contain 971138 items).

In SPREAD, there are multiple strategies to reduce the size of the trace with each of them having certain benefits and certain drawbacks.
###Pruning traces
The first strategy is to prune the trace at certain levels. We can encode this pruning directly in another version of the fib function:
```scala
object fib2 extends FA1[Int,Int] {
    def apply(i: $Int) = {
      if (!i < 2) 1
      else if (%i < 5) ~%(fib,!i)
      else %(fib2,!i-1) + %(fib2,!i-2)
    }
}
```
So we just apply the tilde(`~`) operator on `fib(i)` when `fib2(i)` is called with `i < 5`. Now if we evaluate:
```scala
println(%(fib2,6).fullEval)
```
..  we get the following trace:
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

TO BE CONTINUED TONIGHT.

Copyright 2016: Robbert van Dalen