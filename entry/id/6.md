The Compromiseless Reconciliation of I/O and Purity

====================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on November 12, 2013.
> [Read online!](https://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity.html)

One of the crazy ideals of functional programming is the idea that your program
is simply a list of definitions of mathematical functions. And like real math
functions, FP functions are **pure**. That means that (1) they cannot affect any
state, and (2) that they must return the same thing every time they are called
with the same arguments.

When you first learn functional programming, this manifests as "your variables
are immutable and you can't do loops; use recursion instead." And if you do
that, everything is "fine".

However, there is an apparent glaring problem with this adherence to purity:
**I/O**. Input and output are inherently stateful.

## The Problem

For the obvious (input) example, consider `getchar()` in C. It returns the
character that a user enters. Obviously, if `getchar()` returned the same thing
every time, you'd have an extraordinarily useless function. Input *inherently
violates* purity, it seems. (Also, consider a [function generating random
numbers](http://xkcd.com/221/))

The idea of output violates purity as well. Consider calling `printf()` in C.
You're going to change the state of the terminal. A benign example, of course;
but what about a function `add_database_row()` that adds a row to your database?
A call of `get_database_row()` will now return something different than it would
have returned before. `get_database_row()` now returns two different things when
run at two different times --- impure! Blasphemy!

Of course, it should be obvious that not being able to perform IO means that
your program is essentially useless in most real world applications. But purity
is pretty cool, and it gives us guarantees that let us [reason with our
code](http://u.jle.im/19JxV5S) in ways that are impossible with impure code, and
with unprecedented safety. It opens the doors to previously inaccessible models
of parallel, concurrent, and distributed programming. It allows the compiler to
do crazy optimization tricks. It allows for powerful mathematical analysis of
our programs. The full benefits of purity are beyond the scope of this article,
but you can trust me when they say that they are too much to give up over a
technicality.

So how can we reconcile the idea of a pure language with ~~anything useful~~
I/O?

## A Functional "Program"

### Declarations

Let's look at an almost-typical Haskell program.

``` haskell
--  factorial n: n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--  fib n: the nth Fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

--  first_n_fibs n: a list of the first n Fibonacci numbers
first_n_fibs :: Int -> [Int]
first_n_fibs n = map fib [1..n]
```

One of the first things you should notice is that this looks strikingly similar
to a list of math equations...and almost not like a program.

Notice one important thing about this (at least, in Haskell): there is no
inherent ordering in any of these statements. By this, I mean that `factorial`,
`fib`, and `first_n_fibs` can be defined in any order. When you write
declarations of mathematical objects on paper, the order in which you declare
them should have no bearing on what they represent. These are functions.
Immortal, unchanging, ethereal, separate from time and space. It is simply
nonsensical to talk about order in this context.[^1]

Also note that these declarations don't always declare integers/numbers.
`first_n_fibs` actually declares a data structure --- a list that contains
integers. Of course this is no big problem...mathematical functions can map
integers to matrices, or matrices to functions, anything you can think of. We
aren't limited to simply defining primitive things. We can also define
structures that contain things.

Of course, this "program" doesn't actually *do* anything. Let's look at some
more programs and see if we can address this.

### Representing Actions

There are a lot of data structures/data types that may be expressed in Haskell.
One in particular is called `IO`. `IO a` represents a computation that returns
something of type `a`. There are a couple of pre-packaged computations included
in the standard library. Let's write another almost-typical Haskell program with
some.

``` haskell
--  getStringFromStdin: returns a computation that represents the act of
--      getting a string from stdin.  or rather, a series of instructions on
--      interacting with the computer and generating a String.
getStringFromStdin :: IO String
getStringFromStdin = getLine

--  printFibN: returns a computation that represents the act of printing the
--      nth Fibonacci number to stdout and returns () (Nothing).  or rather,
--      a series of instruction on interacting with the computer to get it to
--      print a Fibonacci number and returning nothing.
printFibN :: Int -> IO ()
printFibN n = print (fib n)
```

Let's look at these.

These are simply functions/declarations, just like the ones above. Although
instead of returning an integer or a list data structure, it returns a special
data structure that represents a computation. `[a]` represents a list of `a`'s.
`IO a` represents an abstract computation (or a series of system instructions)
that returns an `a`.

These declarations and functions are also simply "math" functions. Instead of
returning a set or a matrix or a vector, it returns another type of object.

Note that this has nothing to do with execution. `printFibN` does *not* execute
a print statement. No more than writing `printFibN` on a piece of paper will
cause it to magically evaluate. It does not execute anything: it is simply an
abstract data structure representing a computation.

Note again that there is no inherent ordering involved. Whether you define one
or the other first, it does not change what the two names really *represent*.
Just like if you defined two matrices on a piece of paper in a different order,
it does not change the matrices they represent.

Also note that all of these declarations are completely pure.
`getStringFromStdin` will return the exact same *representation of a
computation* every single time. `printFibN n` will return the exact same
*computation representation* for every `n` every single time. The exact same
instruction sequence every single time for every `n`.

And yes, the objects themselves don't actually execute anything. That's like
saying writing down a matrix executes something in the real world.

### Data Structures, Not Commands

To illustrate the difference between a data structure representing a computation
and a computation itself, let's look at a possible confusion that might arise
from mixing up the two.

``` haskell
getStringAndPrint :: IO ()
getStringAndPrint = print (getStringFromStdin)
```

What would you expect to happen here?

Remember, `print` is an IO instruction prints out what it is passed.
`getStringFromStdin` is a computation object that gives a string when executed.

In another language, which deals with computations (and not representations of
them), you would expect it to get a string from stdin and then print it. Sort of
like an echo.

However, this is not the case in Haskell. What is `getStringFromStdin`? It is
*not* a string --- it is a computation object.

What will happen is that `print` (when executed by a computer) won't print the
result of `getStringFromStdin`. `print` will print out the **representation of
the computation**! It'll print out the *data structure representing the
computation*, or some string "representing" the act of the computation!

(At least, that's what it's supposed to do. Unfortunately, `IO` data structures
do not come built-in with a method for their string representation in vanilla
Haskell. But the point remains that `print` would *try* to print out the data
structure itself somehow, and not the actual result of the computation)

## Instructions as Data Structures

Let's take a step back and think about what it even means to have a data
structure representing computation.

You can think about it as some kind of list/nested tree (or more accurately, a
graph) of instructions for someone to follow. For the case of `IO Int`, you can
see it as, internally, some kind of tree/nested instruction set for someone to
follow in order to produce an `Int`. In the case of `IO`, for GHC, the "someone"
is a computer. Or more specifically, a processor. GHC directly translates any
standalone IO object into assembly code (or even a less optimal C code).

Technically, you *could* "think" of every IO object as a self-contained and
encapsulated little packet of assembly or C code that you can compose and nest
and merge, etc. with other such packets, without worrying about the lower level
code itself. But don't do this, or you risk confusing a possible representation
of an object for the actual abstract object itself. (Think about it like saying
that a mathematical matrix is a series of pencil swirls on a piece of paper.)
But yes, at any time, you can "compile"/make concrete an IO object into
standalone C code with GHC. This is actually a fact, and every IO object can be
said to correspond directly with a chunk of C code.

Really, though, there are many ways to "translate" this data structure into
instructions for anyone to follow.
[Haste](http://hackage.haskell.org/package/haste-compiler), for example, takes
`IO` data structure and turns it into something that can be run in a Javascript
interpreter. That is, it takes something like `printFibN n`, takes the internal
tree instruction set, and writes it out concretely in javascript.

In fact it would not be too hard to imagine a compiler that would take any
arbitrary `IO` structure and translate it into human-followable (yet very
verbose) instructions on a piece of paper, written in plain English. Or French,
for that matter.

That is because that's all `IO` *is* --- a tree/graph data structure
representing an instruction series, that we assemble/build/compose using Haskell
code. The same way you would assemble/build an array, or a dictionary, or a
linked list in any other language.

### Other Examples

It might help to think about similar "instruction-like" data structures.

Take [Persistent](http://hackage.haskell.org/package/persistent), which (in some
variants) provides the `SqlPersistM` data structure. This data structure
represents an interaction with an SQL Database. In other words, it represents a
tree of instructions for interacting with one. When you give it to the
Persistent library, it'll translate that `SqlPersistM` into a series of **SQL
queries**! Yes, it produces actual SQL query strings, using the instructions
from the data structure, executes them, and returns the result. An
`SqlPersistM Int` is an SQL interaction that returns an Int when run with the
Persistent library.

Then you have [Parsec](http://hackage.haskell.org/package/parsec), which
provides a `Parsec` data structure, which are *instructions for Parsec to parse
a string*. A `Parsec Int` structure[^2] represents instructions for parsing a
string into an `Int`. When you give a `Parsec Int` and a string to parse to the
Parsec library, it will run the parse specified by the `Parsec` object and
return (hopefully) a parsed `Int`. Remember, a `Parsec Int` object does *not*
actually "parse" anything; It is *used by Parsec* to parse a string and return
an `Int`!

The reason why we use these data structures in Haskell, instead of actually
writing SQL queries and parsing rules from scratch, is because they become
*composable*. SQL queries? Not very composable. Parsing rules? Not exactly,
either. In this way, you can build complex SQL queries without ever touching a
query string by composing simple queries. You can create very complex and
intricate parsing rules without every having to "worry" about actually writing
the parser: you just compose simple, smaller parsers.

And this is really what Haskell "does best" (and possibly what Haskell was
really made for): assembling and composing these possibly complex instruction
data structures in a pure way and "passing them on" to things that can take them
and use them to do great things. An `SqlPersistM` is used by Persistent, a
`Parsec` is used by Parsec, and an `IO` is used by...well, what? A computer!

## The "Main" Point

So now we see that Haskell has no problems returning a data structure that
represents computer instructions (well, at least, Haskell's standard library
handles all of it for us by giving us useful instruction primitives that we can
build more complex instructions from).

Now we have an instruction object. How do we actually get a computer to use and
execute it?

For this, we rely on convention (or arbitrary specification, however you like to
see it). A Haskell compiler will "understand" your data structures, and it picks
**one** of them to compile into a binary format for your computer (or whatever
format your executing environment reads best). Out of all of the IO objects you
can return/represent, the Haskell compiler chooses one of them to be the one it
actually compiles into computer-readable code.

And by convention/specification, it is the IO object with the name "main":

``` haskell
--  printFibN: returns a computation that represents the act of printing the
--      nth Fibonacci number to stdout and returns () (Nothing).
printFibN :: Int -> IO ()
printFibN n = print (fib n)

--  main: The IO object that we agree that the compiler will actually compile.
main :: IO ()
main = printFibN 10
```

And here we are. A full, executable Haskell program. You can [download and run
it
yourself](https://github.com/mstksg/inCode/blob/master/code-samples/io-purity/IO-Purity.hs).

As we can see, every function or declaration that makes up our program is
completely pure and side-effectless. In fact, the assembly of `main` itself is
side-effectless and pure. We assemble the `IO ()` that `main` returns in a pure
way. `printFibN 10` will return the exact same computation representation every
single time we run it.

`printFibN 10` is **pure**. Every time we *evaluate* `printFibN 10`, we get the
exact same computation representation/instruction list.

Therefore, `main` is pure, as well. Every time we evaluate `main`, we get the
exact same computational data structure.

### Purity challenged?

Now consider:

``` haskell
--  getStringFromStdin: returns a computation that represents the act of
--      getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--  main: The IO object that we agree that the compiler will actually compile.
main :: IO ()
main = getStringFromStdin >>= (\result -> print result)
```

(Sample can be [downloaded and
run](https://github.com/mstksg/inCode/blob/master/code-samples/io-purity/Challenge.hs))

(An aside: `>>=` here is an operator that takes the result of the left hand
side's computation and passes it as a parameter to the right hand side.
Essentially, it turns two IO computation data structures and combines/chains
them into one big one. The `(\x -> print x)` syntax says "take the `x` passed to
you and use it in `print x`")

`main` gets something from the standard input, and then prints it.

Oh wait. This means that if I type something different into standard input, the
program will return something different, right? How is this pure?

Here is the crucial difference between **evaluation** and **execution**:

`main` will always **evaluate** to the exact same computation data structure.

`main` will always be the *exact* same program, no matter when you run it. (In
particular, the program that gets a string from stdin and prints it)

The computer/processor --- which is given a binary representation of the IO data
structure, and is completely separate from the language itself --- now
**executes** this binary/compiled data structure/program. Its execution of this
binary is, of course, potentially unpredictable and in general
non-deterministic, and can depend on things like the temperature, the network
connection, the person at the keyboard, the database contents, etc. The
*instructions/binary* that it follows will be the same every time. The *result*
of those instructions will be different every time (as someone who has ever
attempted to bake a cake can testify).

`main` is a function that returns/evaluates deterministically to a data
structure representing a computation.

The computation that it represents is not necessarily deterministic.

This distinction between **evaluation** and **execution** is what sets apart
this I/O model that permits its purity.

`main` is a pure value. The instruction data structure `main` represents impure
instructions.

And *that* is how we can deal with I/O in Haskell while remaining a pure
language.

### Illustrating the difference

To really understand the difference between evaluation and execution, let's look
at this example:

``` haskell
ignoreAndSayHello :: IO a -> IO ()
ignoreAndSayHello to_ignore = print "Hello!"

main :: IO ()
main = ignoreAndSayHello getStringFromStdin
```

What does this program do?

Naively, we expect it to ask for a string from standard input, ignore the
result, and print "Hello!".

Actually, this is **not** what it does.

This is because `ignoreAndSayHello getStringFromStdin` will evaluate to
`print "Hello"` (remember, it ignores its argument). So `main` evaluates to one
single IO action: `print "Hello!"`.

So your program returns the simple IO action `print "Hello!"` --- the
computation returned by `main` therefore simply prints "Hello!". This
computation does not represent anything that would ask for input.

The "real" way to do this would be:

``` haskell
ignoreAndSayHello :: IO a -> IO ()
ignoreAndSayHello to_ignore = to_ignore >>= (\result -> print "Hello!")

main :: IO ()
main = ignoreAndSayHello getStringFromStdin
```

Remember, `>>=` "combines" two IO objects into one. It returns a new IO object
that takes the result of the left-hand side and uses it as an argument to the
right hand side. Easy, right?

## Ordering

One major implication that is apparent throughout this entire process is that
statements in Haskell have **no inherent order**. As we saw, we had a list of
declaration of many different IO actions --- none of which were necessarily
evaluated or executed. There is no sense of "this function is 'first', this
function is 'second'". Indeed, the idea of ordering makes no sense when you
think of things as mathematical functions.

While there is no "first" or "second", there is a `main`, which is the function
the compiler/interpreter passes to the runtime environment as the computation we
agree to run. "Order" arrives at this point. We explicitly "create" an IO data
structure and specify the ordering implicitly with `>>=`. More specifically,
`print` requires the result of `getStringFromStdin`, so there arises the first
semblances of "ordering": in the explicit composition of different IO actions
into one big one.

As it turns out, there is a [nice blog
post](http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/) by
Chris Taylor illustrating how this wordering ordering could be implemented in
the internal data structure of IO.

Long story short, `IO`'s interface provides features to chain and combine IO
actions into one big IO action, as we did before with `>>=`. This interface
creates dependency trees in the internal IO data structure that enforces
ordering.

But the real story is that outside of the internals of a single `IO`, there is
no inherent ordering --- not even between different `IO` objects!

## Resolution

In retrospect, the solution seems obvious. A functional program does what it
does best --- return an object, purely. This object is the actual computation
itself, which can be pure or impure, deterministic or nondeterministic --- we
just pass it off, and the execution environment can do whatever it wants with
it. Not our problem anymore! This is the difference between evaluation (the pure
process) and execution (the impure one).

We have the best of both worlds. Purity and...well, usefulness!

In fact, because of how Haskell is structured...an impure function does not even
make sense. How would one even write a traditional "impure" function in this
language? The language itself is centered around the idea of composing
computation instruction data types. What would an impure function even look
like? Even if it were possible, impurity would be a jarring, unnatural
adjustment to the language that doesn't even really "make sense".

More importantly, however, there isn't really any other way Haskell could handle
this and still feel Haskell. The reason for this is that this is why Haskell
succeeds where other languages struggle: Though we have only seen a glimpse of
this in this in this article, Haskell provides very specialized tools for
assembling and composing complex instruction data structures that make it
extremely simple, expressive, and elegant. Tools for combining two parsing rules
into one. Tools for combining two SQL operations into one. For a language that
handles computational data structures so well, *not* handling IO this way would
be a real shame!

### Why?

One might ask about the usefulness of this whole thing. After all, don't most
languages "compile" to the same assembly code every time? Why this game?

The reason is that we can now deal with programs --- entire chunks of assembly
code --- as first-class objects. You can pass in computational instruction
objects to a function the same way you can pass any normal object. You can have
two separate little "assembly code chunks" in complete isolation...and then you
can combine them if you want, as well. You can easily introduce parallel forks
--- you can always pass in an "assembly code chunk", so why not pass an IO
object into a fork function? Every separate IO object is self-contained and
manipulatable. This fact is also true for the other "instruction set"-like
objects mentioned earlier. You can build them up and compose them and pass them
as first-class things.

And like we said before, you get all the benefits of [equational
reasoning](http://u.jle.im/19JxV5S) because you're dealing with pure "inert"
compositions --- this is something you could never get if you dealt with
executing the actual functions themselves!

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

[^1]: So, the astute reader will note that I am slightly blurring the line
    between purity and non-strictness/laziness. While it is true that pure
    languages can be strict, and ordering *can* matter, this demonstration is to
    mostly illustrate that declarations of items and objects don't *necessarily*
    have to correspond to evaluation, execution and IO --- an important point
    for the next section.

[^2]: Technically, the full type would be `ParsecT s u m Int`.

