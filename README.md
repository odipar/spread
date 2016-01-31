###SPREAD

I want to *exactly* know what my software is doing. Better still, I want *cryptographic proof* that my software executes each and every computation correctly.

Wouldn't you?
If not, read no further.

Currently, I can't know what my Windows 10 is doing, or it is *very* hard to find out. That's because most of Windows 10:

* Is compiled to machine code that bears no resemblence to the original source code.
* Hides intricate worlds of mutable objects after abstract interfaces.
* And destroys precious machine state after machine state.

Welcome to SPREAD!

In SPREAD, all data, machines states, code and computations are cryptographically authenticated and immutable.

Of course, some practical issues had to be resolved to make SPREAD a reality. First and  foremost, the pervasive use of mutability has been erradicated: nothing *must* ever be destroyed!

Obviously, the naïve storage of all machine states would require enormous amounts of storage. SPREAD solves that issue by compressing consecutive states incrementally, and by allowing user-control over which states need to be stored, and at what granulatity.

As a bonus, SPREAD can re-use computational results, *exactly* because it stores important previous machine states.

So SPREAD kinda acts like a spreadsheet: a spreadsheet also needs the previous evaluation of a workbook in order to optimally evaluate the next version.

That said, spreadsheets don't keep *all* versions like SPREAD does. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets.
In contrast, SPREAD only allows pure functions and fully persistent data structures as primitives.

So can SPREAD be used as a drop-in Excel replacement?
Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly software in general.

####Warning
SPREAD is currently in ALPHA stage. But there are already some goodies to be found in the repository, most notably the new SplitHash data structure.
```
Some example code will be posted soon
```
Copyright 2016: Robbert van Dalen











