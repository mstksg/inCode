---
title: "\"Five Point Haskell\" Part 3: Limited Atonement"
categories: Haskell
tags: functional programming, type safety
create-time: 2026/01/03 16:17:50
identifier: five-point-haskell-3
slug: five-point-haskell-part-3-limited-atonement
series: five-point-haskell
---

Hi! We're in Part 3 of *[Five-Point Haskell][]*! I've been trying to build a
framework to describe how I write maintainable and effective Haskell and also
highlight anti-principles I reject, and this the third part of that framework.

[Five-Point Haskell]: https://blog.jle.im/entries/series/+five-point-haskell.html
[Total Depravity]: https://blog.jle.im/entry/five-point-haskell-part-1-total-depravity.html
[Unconditional Election]: https://blog.jle.im/entry/five-point-haskell-part-2-unconditional-election.html

In [Total Depravity][], we talked about how the failure of mental context
windows is always only a matter of time, and how to use types defensively in
that light. In [Unconditional Election][], we talked about how mathematical
properties ensure the behavior of our instantiations regardless of any foreseen
merit of the implementations.

Of course, real code doesn't just live in an ivory tower of perfect, clean pure
abstractions. In some sense, it's where all of the theory meets practice and
finally becomes useful. The goal of Haskell isn't universal purity: it's about
the correct balance between the impure and pure, and how the existence of one
enriches the other. This is **Limited Atonement**.

> Limited Atonement: Every domain has a clean line between what is pure and
> what is not. And yet, impurity is not a failure, but a boundary that gives
> purity its very meaning.
>
> Therefore, in every domain, find that beautiful line made possible only by
> the perfectly effective assertion of purity. Be intentional: let how you
> treat the impure give meaning to the limited purity.

The Case for Purity
-------------------

Before we can start diving into the nuances, let's actually remind ourselves
why it's important to put purity in the type system in the first place. After
all, this isn't exactly a widely accepted "value" in the wider programming
world.

If you've encountered Haskell in popular culture, it might have been [in this
xkcd comic][comic]:

[comic]: https://xkcd.com/1312/

![xkcd --- Haskell](/img/entries/five-point-haskell/xkcd-1312-haskell.png "xkcd --- Haskell"){style="width:50%;height:auto;"}

There's some truth to it, but there's also some truth to the quote that
"Haskell is the best imperative language": we understand that in order to
conquer the beast, we must first _name_ it. If your code has no way to tell the
difference between code that does IO and code that does not, you really have no
hope in reasoning with _anything_.

If you have some familiarity with the topic (or have read my [past][]
[posts][])), _technically_ we can look at IO in Haskell as still "pure" in the
sense that we are purely constructing an IO action. That is, `putStrLn ::
String -> IO ()` purely returns the same `IO ()` action every single time.
`readIORef :: IORef a -> IO a` returns the same `IO a` action every time, even
if the `a` itself might be different every time you execute it.

[past]: https://blog.jle.im/entry/first-class-statements.html
[posts]: https://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity.html

While _technically_ correct, this framing is somewhat facetious (an accusation
I fully accept) and doesn't really get to the point of what impurity really is.
It's not about pure or impure _functions_ (`->`), it's about pure or impure
_logic_, and being able to represent that in the type system.

Remember in [Part 2][Unconditional Election], we compared and contrasted
similar type signatures in Haskell and Java

```haskell
foo :: forall a. a -> a
```

```java
static <T> T foo(T x)
```

In order to even _start talking_ about all the cool things about parametric
polymorphism, etc., we had to add the caveat "Assume no IO, assume no mutation
of arguments".

Once we put them side-by-side:

```haskell
mkString :: Int -> String
mkStringIO :: Int -> IO String
```

Instantly we understand that the first function must:

1.  Always return the same string for any `Int`
2.  Not perform any IO during the computation
3.  Cannot modify the environment

whereas the second has no such guarantee. We have no such expressiveness in

```java
static String mkString(Integer x)
```

which could presumably call out to the network, use a random generator to
produce the string, or modify some internal counter every time it is called.

On one hand, you might end up needlessly introducing overhead and complexity in
your language, akin to the [colored function debate][color]. Sneaking in IO in
a deeply nested function requires completely propagating this to every caller
up to the very top.

[color]: https://journal.stuffwithstuff.com/2015/02/01/what-color-is-your-function/

But on the other hand...maybe it _should_?  If some deeply nested sub-function
call in your function does IO, this seems exactly the situation you _would_
want such a change to be flagged by the compiler. The fact that impurity must
bubble up is a feature, not a bug.

One day in traffic school, I remember a student asking the teacher a question,
"If there is nobody at the 4-way stop sign intersection, do I still have to
stop?"

The teacher answered, "My child, if you don't see the other person at a 4-way
stop, that is exactly the situation you _should_ be stopping in."

And, so what? Why do we care in the first place, if the purpose of our function
is to do IO anyway?

Well, you should care about unmarked IO if you care about global variables. IO
is the ultimate unrestricted global variable: any `IO` action can freely modify
the process environment or the file system: it can call `setEnv` or `lookupEnv`
against global variables that can be access across your entire process.

You should care about IO if you care about memoization: we can safely cache
`mkString 42` and `mkString 67` forever, without worrying that every time we
access them they should have been different numbers.

You should care about unmarked IO if you care about safe refactoring and common
subexpression elimination. Consider populating data with `mkString`:

```haskell
mkUser :: Int -> User
mkUser n = User { name = mkString n, ident = mkString n }
```

This _should_ be the same as:

```haskell
mkUser :: Int -> User
mkUser n = User { name = nameAndIdent, ident = nameAndIdent }
  where
    nameAndIdent = mkString n
```

In many cases, this could be more performant and save space.  You might only
have to allocate only a single heap object instead of multiple. However, this
is _not_ a valid program optimization if `mkString` could do IO! Calling it
twice could give different results, or affect the environment in different
ways, than calling it once!

You should care about unmarked IO if you care about laziness. Granted, this
requires a desire for laziness in the first place (so, Ocaml users, you're off
the hook). But if you can imagine:

```haskell
mkUser :: Int -> User
mkUser n = User { name = b, ident = a }
  where
    a = mkString n
    b = mkString (n + 1)
    c = mkString (n + 3)
```

What...what order are those `mkString`s called, in a lazy language? If there
was unmarked IO, the order _does_ matter. If there wasn't, they _can't_. And
`c` could not even be freely discarded if there was unmarked IO.

You should care about IO if you care about concurrency. Imagine parallel
mapping over multiple numbers:

```haskell
myStrings :: [String]
myStrings = parMap mkString [1..100]
```

If `mkString` had unmarked IO and accessed locks or mutexes, this could easily
be a race condition. But it doesn't, so we can guarantee no race conditions. We
can also be assured that the order in which we schedule our threads will have
no affect on the result.

You should care about unmarked IO if you care about testing. Because it states
no external dependency, you can test `mkString` without requiring any
sandboxing, isolation, or extra interleaving interactions with other functions.

The Real Worlds
---------------

## IO

## ST

## STM

## Scoped Environments

The Simulated Worlds
--------------------

## Pure Short Circuiting

## Reader

## State

The Bespoke World
-----------------

## Free

## Tagless Final

## Extensible Effects

## Wait, did I just write a Monad Tutorial?


