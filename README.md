###SPREAD
I want to know *exactly* what my software is doing. Better still, I want *cryptographic proof* that my software executes each and every computation step correctly.

Wouldn't you?

Currently, I don't know what my Windows 10 is doing (or it is *very* hard to find out) and I hate that.

That's because most of Windows 10:

* is compiled to machine code that bears no resemblance to the original source code,
* hides intricate networks of mutable objects after abstract interfaces,
* and destroys precious machine state after machine state.

Welcome to SPREAD!

In SPREAD, all data, machines states, code and computations are cryptographically authenticated.

Of course, some practical issues had to be resolved to make SPREAD a reality. First and foremost, SPREAD almost completely eradicates mutable state.

Obviously, keeping *all* the state for authentication purposes would require enormous amounts of storage. SPREAD solves that issue by cryptographically 'signing' states *incrementally*, while still allowing full user-control over which ones need to be signed, and at what level of granularity.

Alternatively, full machine states can also be stored incrementally by SPREAD. In turn, this allows the pervasive re-use of state that was calculated earlier.

So SPREAD kinda acts like a spreadsheet because spreadsheets also re-use the previous states of a workbook to optimally recalculate the next.

Unlike SPREAD however, spreadsheets aren't capable to keep all their versions around. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets. In contrast, SPREAD only allows referentially transparant functions as primitives.

So can SPREAD be used as a drop-in Excel replacement?

Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly the next-generation of software in general.

Let's start with some introductionary code :

```scala
(1 !+ 2) !* (3 !+ 4)
```

which is the SPREAD version of:

```
(1 + 2) * (3 + 4)
```

SPREAD is currently implemented as a Scala DSL. This is not an absolute requirement for SPREAD to work, but Scala does bring some nice benefits to the table:

* It runs on the JVM
* Purely functional (where needed)
* Strongly typed at compile time
* Supports the creation of Domain Specific Languages (DSL)

To be honest, I would have rather liked to implement SPREAD in the Avail Programming Language, but alas Avail's toolchain is not there yet.

TO BE CONTINUED SHORTLY....

####Warning
SPREAD is currently in ALPHA stage. For the impatient, there is already some interesting to be found in the repository:


####SplitHash: an immutable, uniquely represented Sequence Authenticated Data Structure
We will discuss SplitHash's intricacies later. 

Copyright 2016: Robbert van Dalen











