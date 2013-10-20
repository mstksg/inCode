The Compromiseless Reconciliation of I/O and Purity
===================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
CreateTime
:   2013/10/04 18:31:44
PostDate
:   never
Identifier
:   io-purity

One of the crazy ideals of functional programming is the idea that your
program is simply a list of definitions of mathematical functions.  And like
"real" math functions, FP functions are *pure* --- that is, they don't have
any "side effects".  What does this mean?  It means that it cannot affect the
state of the world.  Or any state, for that matter.  As a consequence of this,
it also means that computing the same function twice for the same parameters
should give you the same answer.

What does this look like, practically?  The function *sin(x)*, for example,
does not imply any change in the world.  Sure, if you sit down and try to
calculate the sine of some number, you might change the state of your paper
and pencil. But the actual mathematical ideal of the sine function...it does
not involve any change in the world,
[lest we run into some really big problems][smbc]. The entire idea kind of
breaks down if you try to imagine it. Multiplication is an abstract,
non-physical concept.  Not a physical machine you run.  And why should two
times two change every time you calculate it?

[smbc]: http://www.smbc-comics.com/?id=2595

When you first learn functional programming, this manifests as "your variables
are immutable and you can't do loops; use recursion instead."  And if you do
that, everything is "fine".

However, there is an apparent glaring problem with this adherence to purity.
I/O.  Input and output are inherently stateful.  If you call `printf` in C,
you change the state of the terminal.

