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
real math functions, FP functions are **pure**.  That means that (1) they
cannot affect any state, and (2) that they must return the same thing every
time they are called.

When you first learn functional programming, this manifests as "your variables
are immutable and you can't do loops; use recursion instead."  And if you do
that, everything is "fine".

However, there is an apparent glaring problem with this adherence to purity:
**I/O**.  Input and output are inherently stateful.

The Problem
-----------

For the obvious (input) example, consider `getchar()` in C.  It returns the
character that a user enters.  Obviously, if `getchar()` returned the same
thing every time, you'd have an extraordinarily useless function.  Input
*inherently violates* purity, it seems.  (Also, consider a function generating
random numbers)

The idea of output violates purity as well.  Consider calling `printf()` in C.
You're going to change the state of the terminal.  A benign example, of
course; but what about a function `add_database_row()` that adds a row to your
database?  A call of `get_database_row()` will now return something different
than it would have returned before.  `get_database_row()` now returns two
different things when run at two different times --- impure!  Blasphemy!

Of course, it should be obvious that not being able to perform IO means that
your program is essentially useless in most real world applications.  But
purity is pretty cool, and it gives us guarantees that let us reason with our
code in ways that are impossible with impure code, and with unprecedented
safety.  It allows the compiler to do crazy optimization tricks.  It allows
for extremely powerful mathematical analysis of our programs.

So how can we reconcile the idea of a pure language with ~~anything useful~~
I/O?

A Functional "Program"
----------------------

### Declarations

Let's look at an almost-typical Haskell program.

~~~haskell
--| factorial n: n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--| fib n: the nth Fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

--| fibs: the list of all Fibonacci numbers
fibs :: [Int]
fibs = map fib [1..]

--| first_n_fibs n: a list of the first n Fibonacci numbers
first_n_fibs :: Int -> Int
first_n_fibs n = take n $ fibs
~~~

One of the first things you should notice is that this looks strikingly
similar to a list of math equations...and almost not like a program.

In particular, look at the "constant" `fibs`.  In Haskell, `[1..]` represents
a list from 1 to infinity.  `fibs` is basically an infinite list of Fibonacci
numbers.

In a normal language, this would explode.  However, imagine writing this
declaration down on a piece of paper in math notation.

Now, do it.

Anything explode?  Did the world freeze and hang?

Probably not.  That's because it's just a *declaration* of what things are,
not an actual instruction to compute/execute.

Now, notice one important thing about this: there is no inherent ordering in
any of these statements.  When you write declarations of mathematical objects
on paper, the order in which you declare them should have no bearing on what
they represent.  These are functions.  Immortal, unchanging, ethereal,
separate from time and space.  It is simply nonsensical to talk about order in
this context.

Also note that these declarations don't always declare integers/numbers.
`fibs` actually declares a data structure --- a list that contains integers.
Of course this is no big problem...mathematical functions can map integers to
matrices, or matrices to functions, anything you can think of.  We aren't
limited to simply defining primitive things.  We can also define structures
that contain things.

Of course, this "program" doesn't actually *do* anything.  Let's look at some
more programs and see if we can address this.

### Representing Actions

There are a lot of data structures/data types that may be expressed in
Haskell.  One in particular is called `IO`.  `IO a` represents a computation
that returns something of type `a`.  There are a couple of pre-packaged
computations included in the standard library.  Let's write another
almost-typical Haskell program with some.


~~~haskell
--| getStringFromStdin: returns a computation that represents the act of
--|     getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--| printFibN: returns a computation that represents the act of printing the
--|     nth Fibonacci number to stdout and returns () (Nothing).
printFibN :: Int -> IO ()
printFibN n = print (fib n)
~~~

Let's look at these.

These are simply functions/declarations, just like the ones above.  Although
instead of returning an integer or a list data structure, it returns a special
data structure that represents a computation.  `[a]` represents a list of
`a`'s.  `IO a` represents an abstract computation that returns an `a`.

These declarations and functions are also simply "math" functions.  Instead of
returning a set or a matrix or a vector, it returns another type of object.

Note that this has nothing to do with execution.  `printFibN` does *not*
execute a print statement.  No more than writing `printFibN` on a piece of
paper will cause it to magically evaluate.  It does not execute anything ---
it is simply an abstract data structure representing a computation.

Note again that there is no inherent ordering involved.  Whether you define
one or the other first, it does not change what the two names really
*represent*.  Just like if you defined two matrices on a piece of paper in a
different order, it does not change the matrices they represent.

Also note that all of these declarations are completely pure.
`getStringFromStdin` will return the exact same *representation of a
computation* every single time.  `printFibN n` will return the exact same
*computation* for every `n` every single time.

So these objects represent computations, but do not actually execute anything
(how can math functions execute anything, anyway?  Nonsense!), how do we
actually *do* something?

The "Main" Point
----------------

So what happens when a Haskell program is interpreted/compiled?

1.  The interpreter/compiler internalizes and understands all of the
    declarations, without actually executing them or evaluating them.

2.  The runtime environment picks one item/declaration, established by
    convention, and then chooses to execute it.

Your program is a list of declarations of numbers, strings, and data
structures.  Some of those data structures that your program defines may be
`IO` objects (data structures that represent computations).

The runtime environment picks one of those data structures with a
pre-agreed-upon name and chooses to execute it.

In Haskell, we have generally agreed that the function we "choose" to pass
onto the run time environment is called `main`.  So let's finish up our
Haskell program:

~~~haskell
--| printFibN: returns a computation that represents the act of printing the
--|     nth Fibonacci number to stdout and returns () (Nothing).
printFibN :: Int -> IO ()
printFibN n = print (fib n)

--| main: The function that we agree that the runtime environment will
--|     execute.
main :: IO ()
main = printFibN 10
~~~

And here we are.  A full, executable Haskell program.

As we can see, every function or declaration that makes up our program is
completely pure and side-effectless. In fact, the assembly of `main` itself is
side-effectless and pure.  We assemble the `IO ()` that `main` returns in a
pure way. `printFibN 10` will return the exact same computation every single
time we run it.

`printFibN 10` is **pure**.  Every time we *evaluate* `printFibN 10`, we get
the exact same computation.

Therefore, `main` is pure, as well.  Every time we evaluate `main`, we get the
exact same computational data structure.

### Purity challenged?

Now consider:

~~~haskell
--| getStringFromStdin: returns a computation that represents the act of
--|     getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--| main: The function that we agree that the runtime environment will
--|     execute.  The `>>=` operator in this case says "take the result of the
--|     first computation and use it as an argument to the second".  The
--|     second thing should be a function that takes one argument for this to
--|     make sense...which is what `print` is.  Phew!
main :: IO ()
main = getStringFromStdin >>= print
~~~

`main` gets something from the standard input, and then prints it.

Oh wait.  This means that if I type something different into standard input,
the program will return something different, right?  How is this pure?

Here is the crucial difference between **evaluation** and **execution**.

`main` will always **evaluate** to the exact same computation data structure.

The runtime environment --- which is somethin that can be considered
completely separate from the language, that is just passed the "football" of
the computation data structure --- can then **execute** this evaluated
computation, which will execute in [potentially unpredictable ways][halting],
and will execute slightly differently every time because you have different
inputs.

[halting]: http://en.wikipedia.org/wiki/Halting_problem

`main` is a function that returns/evaluates deterministically to a data
structure representing a computaiton.

The computation that it represents is not necessarily deterministic.

This distinction between **evaluation** and **execution** is what makes
Haskell unique.

And *that* is how we can deal with I/O in Haskell while remaining a pure
language.

### Illustrating the difference

To really understand the difference between evaluation and execution, let's
look at this example:

~~~haskell
ignoreAndSayHello :: IO a -> IO ()
ignoreAndSayHello to_ignore = print "Hello!"

main :: IO ()
main = ignoreAndPrintNothing getStringFromStdin
~~~

What does this program do?

Naively, we expect it to ask for a string from standard input, ignore the
result, and print "Hello!".

Actually, this is **not** what it does.

Let's see what `main` evaluates to:

~~~haskell
main = ignoreAndPrintNothing getStringFromStdin

-- evaluates to

ignoreAndPrintNothing getStringFromStdin = print "Hello!"

-- which eventually evaluates to

main = print "Hello!"
~~~

So `main` evaluates to one single IO action: `print "Hello!"`.

So your program returns the simple IO action `print "Hello!"` --- the
computation returned by `main` therefore simply prints "Hello!".  This
computation does not represent anything that would ask for input.

Ordering
--------

One major implication that is apparent throughout this entire process is that
statements in Haskell have **no inherent order**.  As we saw, we had a
list of declaration of many different IO actions --- none of which were
necessarily evaluated or executed.  There is no sense of "this function is
'first', this function is 'second'".  Indeed, the idea of ordering makes no
sense when you think of things as mathematical functions.

While there is no "first" or "second", there is a `main`, which is the
function the compiler/interpreter passes to the runtime environment as the
computation we agree to run.  "Order" arrives at this point.  In our last
`main`, `print` requires the result of `getStringFromStdin`, so there arises
the first semblances of "ordering": in the composition of different IO actions
into one big one.

In fact, ordering is slightly more subtle and complex than this.  There is a
[nice blog post][ordering] explaining how ordering is implemented in the
internal data structure of IO.

[ordering]: http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/

Long story short, `IO`'s interface provides features to chain and combine IO
actions into one big IO action, as we did before with `>>=`.  This interface
creates dependency trees in the internal IO data structure that enforces
ordering.

But the real story is that outside of the internals of a single `IO`, there is
no inherent ordering --- not even between different `IO` objects!

Resolution
----------

In retrospect, the solution seems obvious.  A functional program does what it
does best --- return an object, purely.  This object is the actual computation
itself, which can be pure or impure, deterministic or undeterministic --- we
just pass it off, and the runtime environment can do whatever it wants with
it.  Not our problem anymore!  This is the difference between evaluation (the
pure process) and execution (the impure one).

It is also apparent that any "true" pure, functional language is necessarily
lazy, which implies that there is no inherent ordering in your statements.

We have the best of both worlds.  Purity and...well, usefulness!
