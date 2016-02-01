###SPREAD
I want to *exactly* know what my software is doing. Better still, I want *cryptographic proof* that my software executes each and every computation step correctly.

Wouldn't you?

Currently, I don't know what my Windows 10 is doing (or it is *very* hard to find out) and I hate that.

That's because most of Windows 10

* is compiled to machine code that bears no resemblance to the original source code,
* hides intricate worlds of mutable objects after abstract interfaces,
* and destroys precious machine state after machine state.

Welcome to SPREAD!

In SPREAD, all data, machines states, code and computations are cryptographically authenticated.

Of course, some practical issues had to be resolved to make SPREAD a reality. First and foremost, SPREAD almost completely eradicates the use of mutability.

Obviously, keeping all machine states for authentication purposes would require enormous amounts of storage. SPREAD solves that issue by cryptographically 'signing' states *incrementally*, while also allowing full user-control over which ones need to be signed, and at what level of granularity.

Alternatively, full machine states can also be stored incrementally by SPREAD. In turn, this allows the pervasive re-use of results that where calculated earlier.

So SPREAD kinda acts like a spreadsheet because a spreadsheet also re-uses the previous calculation of a workbook to optimally recalculate the next.

Unlike SPREAD however, spreadsheets aren't able to keep *all* their versions around. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets. In contrast, SPREAD only allows pure functions and fully persistent data structures to be used as primitives.

So can SPREAD be used as a drop-in Excel replacement?

Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly software in general.

####Warning
SPREAD is currently in ALPHA stage. But there are already some goodies to be found in the repository, most notably the new SplitHash data structure.
```
Some sample code will be posted soon
```
Copyright 2016: Robbert van Dalen











