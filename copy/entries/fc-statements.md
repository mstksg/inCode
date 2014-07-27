First-Class Statements
======================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
:   haskell
:   io
CreateTime
:   2014/07/26 14:06:00
PostDate
:   never
Identifier
:   fc-statements

One thing I've really always appreciated about Haskell is that all
"statements" in Haskell are first-class members of the language.  That is,
(imperative) statements are literally just normal objects (no different from
numbers, or lists, or booleans) --- they can be saved to variables, passed to
functions, transformed using normal functions, copied, etc.  This really opens
up a whole world of possibilities for not only reasoning about your code, but
also for new ways to frame ideas like parallelism & concurrency.

To clarify, by "statement", I mean it in the sense of a "command" from
traditional imperative programming that, when control flow reaches it,
executes some sort of action or modification to some state.  The [wikipedia
article][wiki] has a nice explanation.  Some typical expressions from common
imperative languages include:

[wiki]: http://en.wikipedia.org/wiki/Statement_(computer_science)

~~~c
int a = 4;              // declaration & assignment
a += 5;                 // modification
printf("hello world");  // call
return false;           // exit points
~~~

In these languages, whenever control flow reaches these statements, something
happens.  We do not differentiate the act of *evaluating* these statements
(figuring out what they are) from *executing* these statements.  Something
just *happens* when you see an assignment.

It is clear that something about these statements are magical or a special
part of the language.  They are wholly different than, say, an integer, or a
boolean.  They aren't normal "objects" or "data" in your system.[^fff]

[^fff]: In some languages with first-class *functions*, you can wrap a call
within a function (to delay execution), save that function to a variable, and
pass that around. This is a good step, and puts you on the way towards
first-class statements.  You can actually sort of fake a (somewhat less
powerful) system of first class statements this way, to some degree.

Statements as data
------------------

In Haskell, `putStrLn "hello world"` is literally just a normal, boring
object, of type `IO ()`.  Like an `Int`, or `Bool`, or a `String`, or a
`[Double]` (a list of `Double`).  *Evaluating* it doesn't really do anything.
It's like evaluating the number `1`, or the expression `2 + 5`.  Cool --- you
evaluated `2 + 5` into a `7`; now what?  Does anything happen?  Not really.
It's still just an `Int`.

`putStrLn "hello world"` is just a normal data structure that represents
(through some abstract representation that isn't really important) the act of
a *computer* printing the string `"hello world"` to stdout.

<div class="note">
**Aside**

The type `IO ()` means "an object/abstracted data structure that represents
the act of a computer computing a `()`" --- or, in other terms, "instructions
for a computer to produce a `()`".  `()` is sort of like the empty
tuple...it's a type that is only inhabited by just...well, `()`.  You can
think of `()` as being analogous to a function returning "void" in other
languages.

There are definitely many values of type `IO Int` or `IO String`, which
represent actions that produce an `Int` or a `String`, respectively.  One
common example is `getLine`, which is of type `IO String` --- `getLine` is an
object that represents the act of a computer getting input from stdin; the
"result" of this action is a `String`.  An `IO Int` would represent a CPU
computation/IO-based computation that produces an `Int`.

For the sake of this discussion, we'll only be considering `IO ()`'s...but in
real life, these other types pop up just as often.
</div>

Haskell gives you a bunch of *combinators*/functions to *work* with these `IO
()`'s (and `IO a`'s in general).  To manipulate then, merge them, sequence
them, compose them...anything you can dream of!

The most popular and common combinator is `(>>)`, which is usually used as an
infix operator.  A common use case: Say you want to create an IO action that
prints "hello", then "world".  But you only have `putStrLn "hello"`, which
represents printing "hello", and `putStrLn "world"`, which represents printing
"world".

If you have those two `IO ()`'s, you can use the `(>>)` combinator to "merge"
them and create a new `IO ()`.  In this case:

~~~haskell
-- :: means "has the type"

-- the (>>) has this type:
(>>) :: IO () -> IO () -> IO ()

putStrLn "hello" :: IO ()
putStrLn "world" :: IO ()
putStrLn "hello" >> putStrLn "world" :: IO ()
~~~

"Give me two `IO ()`s, and I'll give you a shiny new third one."

That new one is a new `IO ()` that represents the act of printing "hello",
then printing "world".

Remember that this new one is, still, only a normal object.  No printing
actually ever "happens" of you evaluate `putStrLn "hello" >> putStrLn
"world"`.  If you ever reach that expression in a Haskell program...nothing is
printed.  It's simply just taking two regular old data structures, running
them through a function, and giving you a third one.

You can even make your own "first class" control flow!

~~~haskell
when :: Bool -> IO () -> IO ()
when True  p = p
when False _ = return ()
~~~

(`return ()` is an `IO ()` that represents the act of doing nothing...it
doesn't actually have anything to do with the `return` keyword in many other
languages.  It basically represents a no-op.)

`when` is just a normal function!  It takes a `Bool` and an `IO ()`; if the
`Bool` is true, then the "result" is just that same `IO ()`.

We can evaluate a call to `when (4 > 0) (putStrLn "it's True!")` by hand:

~~~haskell
when (4 > 0) (putStrLn "it's True!") :: IO ()
when True (putStrLn "it's True!")    :: IO ()  -- evaluate 4 > 0
putStrLn "it's True!"                :: IO ()  -- definition of when True

when (4 < 0) (putStrLn "it's True!") :: IO ()
when False (putStrLn "it's True!")   :: IO ()  -- evaluate 4 < 0
return ()                            :: IO ()  -- definition of when False
~~~

The above is *not* an "execution"...it's an *evaluation*.  `when` is a
function that takes a `Bool` and an `IO ()` object and *evaluates* to that `IO
()` object when the boolean is True.  But remember, calling `when` doesn't
actually execute anything!  It's just a normal function and normal expression.
An `IO ()` goes in, and `IO ()` comes out.  Just a normal function on normal
data.

With only a basic knowledge of functional programming (using a
fold/reduce/inject, basically, or even recursion), you can easy write your own
function:

~~~haskell
sequence :: [IO ()] -> IO ()
~~~

Which says, "give me a list of `IO ()`'s, and I'll give you a new `IO ()` that
represents executing all of those `IO ()`'s one-after-another".

<div class="note">
**Aside**

If you are curious, here is the definition of `sequence` using a fold:

~~~
sequence :: [IO ()] -> IO ()
sequence xs = foldr (>>) (return ()) xs
~~~

If you're familiar with folds/reduces, `return ()` is the "base value", and
`(>>)` is the "accumulating function".

~~~haskell
sequence [putStrLn "hello", putStrLn "world", putStrLn "goodbye!"]

-- evaluates to:
putStrLn "hello" >> (putStrLn "world" >> (putStrLn "goodbye!" >> return ())
~~~
</div>

But wait!  There are a lot of things I can do with two `IO ()`'s besides
executing them one-after-the-other.  I can...merge them *in parallel*!

I can write a combinator:

~~~haskell
par :: IO () -> IO () -> IO ()
~~~

That takes two `IO ()`'s and create a new shiny `IO ()` that represents the
act of executing them *in parallel*.

Then I can also write a new `sequencePar`:

~~~haskell
sequencePar :: [IO ()] -> IO ()
~~~

That takes a list of `IO ()`'s and returns a new shiny `IO ()` that represents
the act of executing them all *in parallel*!

<div class="note">
**Aside**

`sequencePar`'s implementation is pretty much identical to `sequence`'s, but
swapping out `(>>)` for `par`:

~~~haskell
sequencePar :: [IO ()] -> IO ()
sequencePar xs = foldr par (return ()) xs
~~~

By the way, `par` isn't defined by default, but we'll define it really soon.
</div>

There are an entire wealth of combinators by which to compose and sequence and
manipulate `IO ()`'s together.  And many of them you can even write yourself,
from scratch.

There are also many "IO transformers" you have access to --- one notable one
being `forkIO`:[^fio]

[^fio]: It's actually `forkIO :: IO () -> IO ThreadId`, but we can ignore the
"return value" of a `ThreadId` for now, for simplicity.

~~~haskell
forkIO :: IO () -> IO ()
~~~

That takes an `IO ()`, and "transforms" it into a *parallel `IO ()`*.

It takes an object representing a computer action, and *transforms* it into an
object representing a computer action executed in a parallel fork!  That is,
it represents a new computer action that *launches* that `IO ()` in a parallel
fork.

We can write `par` ourselves, then, with this:[^parsimp]

~~~haskell
par :: IO () -> IO () -> IO ()
par x y = forkIO x >> forkIO y
~~~

[^parsimp]: Note that this action doesn't "wait" for both threads to complete;
all it does is launch the two threads.

"To create a new `IO ()` from two `IO ()`'s that represent executing them in
parallel, transform them both into parallel `IO ()`'s, and sequence the
launches."

This is only a small subset of what you can do with "statements as data".  In
fact, there are many frameworks that completely abstract over statements
entirely.  For example, you can "construct" a system declaratively using a
simple DSL, and never even worry about statements or IO.  The DSL might
provide you with a way to specify a high-level overlook of your program in
simple terms.  The DSL might be abstracting/wrapping over *extremely complex*
IO actions, and all you ever see is the simple API.

And then, you might have a function: `DSL -> IO ()`.  Construct the elaborate
high-level thing in simple terms...and then, at the end, *convert it to an `IO
()` object*.  That you can copy, or clone, or throw into a function, or do
*anything* we just mentioned here!

Execution
---------

That's nice and all.  We see how having first class statements as data is
useful for manipulation and abstractions and stuff.  But it all seems kind of
useless if our data structures remain inert and don't actually do anything.

Luckily, one can think of a Haskell compiler as a giant function: `IO () ->
Binary`.  Give the Haskell compiler an `IO ()`, and it'll convert it to a
binary for a given architecture/computer/CPU.  It "translates" the
*representation of a computation* into concrete bytecode that a computer can
actually execute.

Your computer can then execute that generated binary, and...off we go!

In a way, one can think of Haskell as a very elaborate metaprogramming system,
providing a DSL to "generate" byte code.

Haskell
-------

If you have any questions or comments, feel free to leave a comment, drop
by the freenode *#haskell* channel, or find me on [twitter][].

[twitter]: https://twitter.com/mstk "Twitter"

This post is a distillation of concepts I have mentioned in [some
other][iopure] [blog posts][inside] in the past; I've had a lot of new
thoughts after writing both of them and I figured I'd condense them and make a
new post summarizing the new ideas in a new and more concise way, to have them
all in one neat place.  Anyways, if you want to go into this topic in more
detail, those posts above might help!

[iopure]: http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity
[inside]: http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad

In this post I've suggested that `IO ()` is some sort of data structure that
stores the "action" it represents in some abstract way.  If you're curious on
what this representation/storage might look like in concrete terms, [Chris
Taylor has a post][ct] on what you might see if you "peek into" the internal
representation of (a possible implementation of) an IO action type --- you
could even use this to implement first-class statements in your language of
choice![^ghc]

[ct]: http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/

[^ghc]: For performance reasons, this actually isn't the way it's implemented
in the popular Haskell compiler *GHC*.  GHC's implementation is best described
as "hacky", and doesn't really line up too well with the semantic picture of
what `IO ()` is supposed to represent.  But remember that this is really just
an (admittedly ugly) *implementation detail*.  The outward-facing API that it
offers for the `IO ()` type works as you would expect, of course.  One would
hope, at least!

If you're interested in learning Haskell, try picking up [Learn You a
Haskell][lyah] and giving it a read, it's pretty accessible!

[lyah]: http://www.learnyouahaskell.com/

