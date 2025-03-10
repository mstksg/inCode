First-Class "Statements"

=========================

> Originally posted by [Justin Le](https://blog.jle.im/) on July 28, 2014.
> [Read online!](https://blog.jle.im/entry/first-class-statements.html)

One thing I've really always appreciated about Haskell is that all "statements"
in Haskell (or at least, what would be statements in other languages) are
first-class members of the language. That is, (imperative) statements are
literally just normal objects (no different from numbers, or lists, or booleans)
--- they can be saved to variables, passed to functions, transformed using
normal functions, copied, etc. Haskell doesn't have statements --- everything is
an expression, representing normal data! This really opens up a whole world of
possibilities for not only reasoning about your code, but also for new ways to
frame ideas in contexts of parallelism, concurrency, exceptions, DSLs, and more.

To clarify, by "statement", I mean it in the sense of a "command" from
traditional imperative programming that, when control flow reaches it, executes
some sort of action or modification to some state. The [wikipedia
article](http://en.wikipedia.org/wiki/Statement_(computer_science)) has a nice
explanation. Some typical statements from common imperative languages include:

``` c
int a = 4;              // declaration & assignment
a += 5;                 // modification
printf("hello world");  // call
return false;           // exit points
```

In these languages, whenever control flow reaches these statements, something
happens. We do not differentiate the act of *evaluating* these statements
(figuring out what they are) from *executing* these statements. Something just
*happens* when you see an assignment.

It is clear that in these languages, something about these statements are
magical or a special part of the language. They are wholly different than, say,
an integer, or a boolean. They aren't normal "objects" or "data" in your system.

Even if your programming languages have first-class functions, `printf` might be
a first-class value, but the *call* of it (usually indicated with parentheses,
like `printf()`) is definitely something...different altogether. You *can*
simulate this in languages by creating a sub-language inside the language, but
you're always going to have an interplay between the two. There will always be
the dichotomy between statements and data.

## Statements as data

In Haskell, `putStrLn "hello world"` is literally just a normal, boring object,
of type `IO ()` (If you come from an OOP background, `IO a` is sort of like a
generic/template, `IO <a>`). Like an `Int`, or `Bool`, or a `String`, or a
`[Double]` (a list of `Double`). *Evaluating* it doesn't really do anything.
It's like evaluating the number `1`, or the expression `2 + 5`. Cool --- you
evaluated `2 + 5` into a `7`; now what? Does anything happen? Not really. It's
still just an `Int`.

`putStrLn "hello world"` is just a normal data/term/value that represents
(through some abstract representation that isn't really important) the act of a
*computer* printing the string `"hello world"` to stdout.

::: note
**Aside**

The type `IO ()` means "an object/abstracted data structure that represents the
act of a computer computing a `()`" --- or, in other terms, "instructions for a
computer to produce a `()`". `()` is sort of like the empty tuple...it's a type
that is only inhabited by just...well, `()`. You can think of `()` as being
analogous to a function returning "void" in other languages.

If you have experience with object-oriented languages and templates/generics,
`IO a` sort of corresponds to something like `IO<a>`.

There are definitely many values of type `IO Int` or `IO String`, which
represent actions that produce an `Int` or a `String`, respectively. One common
example is `getLine`, which is of type `IO String` --- `getLine` is an object
that represents the act of a computer getting input from stdin; the "result" of
this action is a `String`. An `IO Int` would represent a CPU
computation/IO-based computation that produces an `Int`.

For the sake of this discussion, we'll only be considering `IO ()`s...but in
real life, these other types pop up just as often.
:::

Haskell gives you a bunch of *combinators*/functions to *work* with these
`IO ()`'s (and `IO a`'s in general). To manipulate then, merge them, sequence
them, compose them...anything you can dream of!

The most popular and common combinator is `(>>)`, which is usually used as an
infix operator. A common use case: Say you want to create an IO action that
prints "hello", then "world". But you only have `putStrLn "hello"`, which
represents printing "hello", and `putStrLn "world"`, which represents printing
"world".

If you have those two `IO ()`s, you can use the `(>>)` combinator to "merge"
them and create a new `IO ()`.[^1] In this case:

``` haskell
-- :: means "has the type"

-- putStrLn "hello" is an object with type IO ().
putStrLn "hello" :: IO ()
putStrLn "world" :: IO ()

(>>) :: IO () -> IO () -> IO ()
```

The type signature of `(>>)` says (in simple terms) that it's a function that
takes two `IO ()`s and returns a shiny new `IO ()`. It's a function that takes
two objects and returns one.

We can apply `(>>)` as an infix operator:

``` haskell
helloThenWorld :: IO ()
helloThenWorld = putStrLn "hello" >> putStrLn "world"

-- define `helloThenWorld` as putStrLn "hello" >> putStrLn "world"; and its
type is `IO ()`.
```

That new `IO ()` is a data structure that represents the act of printing
"hello", then printing "world".

Remember that this new one is, still, only a normal object. No printing actually
ever "happens" if you evaluate `putStrLn "hello" >> putStrLn "world"`. If you
ever reach that expression in a Haskell program...nothing is printed. It's
simply just taking two regular old data values, running them through a function,
and giving you a third one. The process of defining `helloThenWorld` --- or even
later evaluating it --- doesn't cause anything to happen. They are inert data
structures.

In many other languages, sequencing actions is a special part of the syntax ---
a semicolon, usually. In Haskell, sequencing is not special --- it's just a
normal function on normal data structures.

You can even make your own "first class" control flow!

``` haskell
when :: Bool -> IO () -> IO ()
when True  p = p
when False _ = return ()
```

(`return ()` is an `IO ()` that represents the act of doing nothing...it doesn't
actually have anything to do with the `return` keyword in many other languages.
It basically represents a no-op.)

`when` is just a normal function! It takes a `Bool` and an `IO ()`; if the
`Bool` is true, then the "result" is just that same `IO ()`.

We can evaluate a call to `when (4 > 0) (putStrLn "it's True!")` by hand:

``` haskell
when (4 > 0) (putStrLn "it's True!") :: IO ()
when True (putStrLn "it's True!")    :: IO ()  -- evaluate 4 > 0
putStrLn "it's True!"                :: IO ()  -- definition of when True

when (4 < 0) (putStrLn "it's True!") :: IO ()
when False (putStrLn "it's True!")   :: IO ()  -- evaluate 4 < 0
return ()                            :: IO ()  -- definition of when False
```

The above is *not* an "execution"...it's an *evaluation*. Execution involves
executing actions on a computer, where evaluation is simply a reduction, like
`1 + 1 ==> 2`. `when` is a function that takes a `Bool` and an `IO ()` object
and *evaluates* to that `IO ()` object when the boolean is True. But remember,
calling `when` doesn't actually execute anything! It's just a normal function
and normal expression. An `IO ()` goes in, and `IO ()` comes out. Just a normal
function on normal data. And we know it's just a normal function, because we
wrote it ourself from scratch!

You can't write `when` in this naive way in a language like, say, Javascript:

``` javascript
var when = function(cond, act) { if (cond) { act; } };

when(false, console.log("hello"));
// "hello" is printed, even though the condition is false
```

(You can simulate something that works by having `when` take functions instead
of statements...but that's the point! You can't pass in a "statement", you have
to pass in a function/data. In this sense, statements and data behave completely
differently.)

With only a basic knowledge of functional programming (using a
fold/reduce/inject, basically, or even recursion), you can easy write this
function:

``` haskell
sequence_ :: [IO ()] -> IO ()
```

Which says, "give me a list of `IO ()`s, and I'll give you a new `IO ()` that
represents executing all of those `IO ()`s one-after-another".

::: note
**Aside**

If you are curious, here is the definition of `sequence` using a fold:

``` haskell
sequence_ :: [IO ()] -> IO ()
sequence_ xs = foldr (>>) (return ()) xs
```

If you're familiar with folds/reduces, `return ()` is the "base value", and
`(>>)` is the "accumulating function".

``` haskell
sequence_ [putStrLn "hello", putStrLn "world", putStrLn "goodbye!"]

-- evaluates to:
putStrLn "hello" >> (putStrLn "world" >> (putStrLn "goodbye!" >> return ()))
```
:::

Note that all of these functions take anything of type `IO ()`...so I could
really be passing in named `IO ()`'s, or the result of combinators, or...

``` haskell
hello :: IO ()
hello = putStrLn "hello"

world :: IO ()
world = putStrLn "world"

helloworld :: IO ()
helloworld = hello >> world

helloworldhelloworld :: IO ()
helloworldhelloworld = sequence_ [hello, world, helloworld]
```

Remember -- nothing is being called or executed. It's just all normal functions
on normal data. The inputs are data, the outputs are data.

But wait! There are a lot of things I can do with two `IO ()`s besides executing
them one-after-the-other. I can...merge them *in parallel*!

I can write a combinator:

``` haskell
bothPar :: IO () -> IO () -> IO ()
```

That takes two `IO ()`s and create a new shiny `IO ()` that represents the act
of executing them *in parallel*.

Then I can also write a new `sequencePar`:

``` haskell
sequencePar :: [IO ()] -> IO ()
```

That takes a list of `IO ()`s and returns a new shiny `IO ()` that represents
the act of executing them all *in parallel*!

This is one great thing about IO-as-data: If you have a bunch of IO actions, you
have the *choice* in how you want to "sequence" or "combine" them. In Haskell,
combining a bunch of actions in sequence and combining them in parallel is just
a matter of swapping out your combining function! There is *no difference* at
the syntax level!

Compare this to other languages, where the syntax for sequencing statements ---
the semicolon --- and the syntax required for launching a bunch of parallel
actions is noticeably different. In Haskell, "sequencing" isn't a part of the
syntax (the semicolon) --- it's just *a regular ol' function*!

::: note
**Aside**

`sequencePar`'s implementation is pretty much identical to `sequence`'s, but
swapping out `(>>)` for `bothPar`:

``` haskell
sequencePar :: [IO ()] -> IO ()
sequencePar xs = foldr bothPar (return ()) xs
```

By the way, `bothPar` isn't defined by default, but we'll define it really soon.
:::

There are an entire wealth of combinators by which to compose and sequence and
manipulate `IO ()`s together. And many of them you can even write yourself, from
scratch.

There are also many "IO action transformers" you have access to --- one notable
one being `makePar`:[^2]

``` haskell
makePar :: IO () -> IO ()
```

That takes an `IO ()`, and "transforms" it into a "parallel" `IO ()`. Or rather,
it takes an object representing a computer action, and returns an object
representing launching that computer action in a parallel fork.

We can write `bothPar` ourselves, then, with this:[^3]

``` haskell
bothPar :: IO () -> IO () -> IO ()
bothPar x y = makePar x >> makePar y
```

Give `bothPar` two `IO ()`'s representing computer actions, and it'll give you a
new one that represents launching both computer actions in parallel. To do that,
simply launch them both one after the other!

Another common transformer on an `IO ()` is `catch`:[^4]

``` haskell
catch :: IO () -> (SomeException -> IO ()) -> IO ()
--       ^         ^                          ^
--       |         |                          +-- modified object
--       |         +-- handler function
--       +-- original object
```

Which takes an `IO ()` object and a handler function, and imbues that `IO ()`
with "error handling capabilities". It returns a new `IO ()` object that
represents doing the same thing as the original one, except with built-in error
handling if things go wrong. Neat!

So if I used `catch (putStrLn "hello world") myHandler`...I'm "transforming" the
`IO ()` (`putStrLn "hello world"`), representing printing a string to the
console, into a new `IO ()` which represents printing a string to the console,
with built in error handling if things go wrong for some reason.

Again --- no execution is being done. We're simply taking an object representing
an IO action, and returning a new, modified one representing a slightly
different IO action.

::: note
**Aside**

This is a pretty aside-y aside, and you can definitely skip it if you want!

One particularly important combinator I have not mentioned yet is called "bind":
`(>>=)`

Let's say you wanted to read a line from stdin, and then print it out right
away. You can do this with `getLine :: IO String` and
`putStrLn :: String -> IO ()`. But wait! This doesn't work:

``` haskell
getLine >> putStrLn "hello?"
```

`(>>)` acts like a semicolon...it just sequences them together one after the
other. Wouldn't it be nice if we had something like a unix pipe? It "sequences"
the two things, but the result of the first can be used by the second?

Well, if `(>>)` is a bash semicolon `;`, then `(>>=)` is a bash pipe `|`!

``` haskell
getLine >>= putStrLn
                      :: IO ()
```

does exactly what we want!

The type of `(>>=)` is:

``` haskell
(>>=) :: IO a -> (a -> IO b) -> IO b
```

And in our specific case, it is:

``` haskell
(>>=) :: IO String -> (String -> IO ()) -> IO ()
```

Which says, "give me an `IO String` and a function taking a `String` and
producing an `IO ()`, and I'll give you a shiny new `IO ()`" This might sound a
little weird at first, but see how `getLine :: IO String` and
`putStrLn :: String -> IO ()` fit into this, and see how it basically works like
a unix pipe in a lot of ways.

As it turns out, `(>>=)` is actually a lot more powerful than it might seem at
first. As soon as you add the `(>>=)` combinator to your arsenal...the space of
programs you can construct using various `IO a`'s opens up in crazy ways. Just
imagine bash with no pipes, and only semicolons! If you ever decide to implement
some system of first-class statements, and it might be tricky to state/model
imperative computations without `(>>=)`.
:::

### Much More

This is only a small subset of what you can do with "statements as data". In
fact, there are many frameworks that completely abstract over statements
entirely. For example, you can "construct" a system declaratively using a simple
DSL, and never even worry about statements or IO. You can specify an *entire
program*, with a full description of its interactions, without ever even
*touching* the IO type. The DSL might provide you with a way to specify a
high-level overlook of your program in simple terms. The DSL might be
abstracting/wrapping over *complex* IO actions, and all you ever see is the
simple API.

And then, you might have a function: `DSL -> IO ()`. Construct the elaborate
high-level thing in simple terms...and then, at the end, *convert it to an
`IO ()` object*. That you can copy, or clone, or throw into a function, or do
*anything* we just mentioned here!

If that DSL is your entire program, then you also have certain guarantees about
what IO your program can even do, if the DSL library forbids users from mixing
in arbitrary IO.

## Execution

That's nice and all. We see how having first class statements as data is useful
for manipulation and abstractions and stuff. But it all seems kind of useless if
our data structures remain inert and don't actually do anything.

Luckily, one can think of a Haskell compiler as a giant function:
`IO () -> Binary`. Give the Haskell compiler an `IO ()`, and it'll convert it to
a "binary" for a given architecture/computer/CPU. It "translates" the
*representation of a computation* into concrete bytecode that a computer can
actually execute.

Your computer can then execute that generated binary, and...off we go!

Every Haskell program by convention compiles *one single `IO ()`*. That is, you
might have a *bunch* of `IO a`s in your program, but you "offer the compiler"
*one* `IO ()` for it to compile, and it compiles it for you. So if you have a
lot of different IO computations you wish to do, you basically continually
sequence, merge, pipe, combine, transform, etc. them until you get one final
`IO ()` which represents your entire desired computation. And then you name it
"main". When your compiler compiles the Haskell file, it'll find the `IO ()`
named "main", and compile that one.

In a way, one can think of Haskell as a very elaborate metaprogramming system,
providing a DSL to "generate" byte code.

By the way, did you see how `IO ()` is *completely separate* from its binary
representation? Hm. Why do we have to compile it to binary, anyway?

Enter in *other Haskell compilers*, like [ghcjs](https://github.com/ghcjs/ghcjs)
--- instead of being `IO () -> Binary`, it's `IO () -> Javascript`! That is,
give it *any* ol' `IO ()` (the same one that you would design for a
CPU/processor), and instead of translating it into bytecode/binary, it
translates it into Javascript! This is another power of having `IO ()` being its
own, abstract object that is independent of the architecture or computer that it
will be eventually run on. Because it's so abstract...it can be compiled and run
really on *anything*!

## Haskell

If you have any questions or comments, feel free to leave a comment, drop by
freenode's *#haskell*, *#nothaskell*, or *#haskell-beginners*, or find me on
[twitter](https://twitter.com/mstk "Twitter").

In this post I've suggested that `IO ()` is some sort of data structure that
stores the "action" it represents in some abstract way. If you're curious on
what this representation/storage might look like in concrete terms, [Chris
Taylor has a
post](http://chris-taylor.github.io/blog/2013/02/09/io-is-not-a-side-effect/) on
what you might see if you "peek into" the internal representation of (a possible
implementation of) an IO action type --- you could even use this to implement
first-class statements in your language of choice! [^5]

This post is a distillation of concepts I have mentioned in [some
other](http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity)
[blog posts](http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad)
in the past; I've had a lot of new thoughts after writing both of them and I
figured I'd condense them and make a new post summarizing the new ideas in a new
and more concise way, to have them all in one neat place. Anyways, if you want
to go into this topic in more detail, those posts above might help!

If you're interested in learning Haskell, try picking up [Learn You a
Haskell](http://www.learnyouahaskell.com/) and giving it a read, it's pretty
accessible! [bitemyapp's guide](https://github.com/bitemyapp/learnhaskell) also
lays out a nice roadmap for learning Haskell.

Also, I encourage you to try to implement your own system of "first class IO" in
languages in which it is possible! Like a normal data structure (like in the
Chris Taylor post), or an abstracted function call. I'd love to hear of any
results or attempts you've made implementing this in your language of choice
(even if Haskell is your language of choice, you can write a `MyIO` type :D );
let me know in the comments or via
[twitter](https://twitter.com/mstk "Twitter")!

(Credit to [computionist](https://twitter.com/computionist) and
[bitemyapp](https://twitter.com/bitemyapp) for proofreading/helpful suggestions)

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

[^1]: The type of `(>>)` is actually more general:
    `(>>) :: IO a -> IO b -> IO b`. We do only use it for `IO ()` in this post,
    though.

    Actually, would you be mad at me if I told you that its true type in Haskell
    is even more general? It's actually `m a -> m b -> m b`, for any type `m`
    that is a member of the `Monad` typeclass. You can think of it like any type
    that "implements" `(>>)` for that type, and more, following some rules on
    how it should behave.

    Ah! This rabbit hole is a bit deep though, so I recommend you not to worry
    about it!

[^2]: `makePar` actually exists in the standard Haskell libraries as
    `forkIO`...kinda. `forkIO :: IO () -> IO ThreadId`. Our `makePar` is
    basically `forkIO`, but with the return value ignored. Basically, it is
    `makePar x = forkIO x >> return ()`

[^3]: Note that this action doesn't "wait" for both threads to complete; all it
    does is launch the two threads.

[^4]: Again, the real definition here is slightly more general.

[^5]: For performance reasons, the way IO is implemented in that article
    actually isn't the way it's implemented in the popular Haskell compiler
    *GHC*. GHC's implementation is best described as "hacky", and doesn't really
    line up too well with the semantic picture of what `IO ()` is supposed to
    represent. But remember that this is really just an (admittedly ugly)
    *implementation detail*. The outward-facing API that it offers for the
    `IO ()` type works as you would expect, of course. One would hope, at least!
    In any case, it's important to remember the difference between *Haskell the
    language*, and what the IO type is supposed to "represent", and *Haskell the
    implementation*, which provides the semantics of the language, abstracting
    over ugly implementation details.

