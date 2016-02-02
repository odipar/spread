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

Notice the exclamation(!) marks. In SPREAD, each operation that is prefixed by an exclamation mark *constructs* an expression, and is not directly evaluated. We need to explicitly request the *evaluation* of an expression to get to its final value.

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

Notice that the full trace encompasses the final value 21. Alternatively, we may want to destroy the trace by requesting its head, i.e the following expression prints **true**:

```scala
println(e.fullEval.head === 21) // true
```

Although it does directly go against the spirit of SPREAD, destroying traces is certainly possible. If keeping full traces is not an option it is better to cryptographically sign a trace, and keep that.  This way, it is still possible to authenticate a computational trace.

Authenticating a trace can be achieved by prefixing a tilde (~) to any expression. When such expression is evaluated, it will yield a default 128 bit cryptographic hash of its trace, together with its final value. Likewise, more cryptographic bits can be acquired by prefixing more consecutive tildes.


Copyright 2016: Robbert van Dalen