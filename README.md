###SPREAD
I want to *exactly* know what my software is doing. I want *cryptographic* proof that my software executes each and every computation correctly.

Wouldn't you?
If not, read no further.

Currently, I *can't* know what my Windows 10 is doing, even when I want it. That's because most of Windows 10:

* Is compiled to machine code that bears no resemblence to the original source code
* Hides complex worlds of mutable objects after abstract interfaces
* And destroys precious machine state after machine state

Welcome to SPREAD!

In SPREAD, all data, code and computations are cryptographically authenticated.

Of course, some practical concerns have to be resolved to make SPREAD a reality. First, the pervasive use of mutability has to be erradicated: we *must* not allow data to be mutated as will be explained later on.

Obviously, storing all machine states would require enormous amounts of storage.
SPREAD solves that by compressing consecutive states incrementally, and it allows user-control over which states need to be stored, and at what granulatity.

As a bonus, SPREAD can re-use previous computational results, *exactly* because it stores important machine states.

So SPREAD kinda acts like a spreadsheet. A spreadsheet also needs the previous evaluation of a workbook in order to optimally evaluate the next version.

That said, spreadsheets don't keep *all* versions like SPREAD does. And typically, Excel plug-ins completely destroy the (otherwise) purely functional nature of spreadsheets.
In contrast, SPREAD only allows pure functions and purely functional data structures as primitives.

So can SPREAD be used as an Excell replacement?
Not *exactly*. Actually not by a mile.

However, the spreadsheet spirit is there. In fact, SPREAD can be considered the next-generation spreadsheet, and possibly software in general.










