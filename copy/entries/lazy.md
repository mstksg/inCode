---
title: "Lazy Patterns, Strict Matches, and All That: My Mental Model of Haskell Evaluation"
categories: Haskell
tags: haskell, laziness, functional programming
create-time: 2020/01/27 10:37:09
date: never
identifier: lazy
slug: lazy-strict-mental-model
---

Every once in a while I get questions about "what does this `~` do in a
pattern", what strict pattern matching does, etc.  And while you can understand
code like `case x of ~(a,b) -> ...` in terms of one-off special cases, I think
that having a good mental model of how evaluation with ADTs work in Haskell
will allow you to answer these questions with confidence, and without
second-guessing yourself.

This is less an article on formal semantics as it as an article trying to
explain some of the heuristics and mental models I've developed over my years
of writing Haskell to help me understand how things work.  It's not a textbook,
but an experience report :)

Let and Case
------------

A lot of the distinction between lazy and strict matching can be illustrated by
comparing the semantics of `let` vs. `case`.  (We're going to be ignoring
type checking here and assuming all values are nice monomorphic values).

For example, let's see the distinction between:

```haskell
let (x, y) = undefined
in  True

case undefined of
    (x, y) -> True
```

What will either of these return, when you try to run it?

Here we are using `undefined` as a way to directly observe evaluation in
Haskell.  This is a common trick people use when discussing evaluation in
Haskell --- it works because `undefined :: a` can take on any type, and any
attempt to evaluate `undefined` can be immediately detected:

```haskell
ghci> sum [1,2,undefined,100]
*** Exception: Prelude.undefined
ghci> sum (take 2 [1,2,undefined,100])
3
```

Think of it like laying a mine for poor old GHC to encounter.  GHC tries to
evaluate things, but when it tries to evaluate `undefined`, it immediately
explodes and tells you that it has a boo-boo.  Our investigation now becomes
"is this mine ever tripped, or not?"

:::::: {.note}

**Alternatives to `undefined`**


Admittedly, the idea that `undefined` can take on any type can seem a little
spooky -- so often I like to use something within the correct type to make
things feel a little more concrete.  For example, instead of `undefined`, we can
write:

```haskell
myTuple :: (Int, String)
myTuple
  | sum [1..] < 1000 = (3, "hi")
  | otherwise        = (5, "bye")

let (x, y) = myTuple
in  True

case myTuple of
    (x, y) -> True
```

This example should display all of the same nuances of using `undefined` ---
the only practical difference is that instead of seeing `Exception:
Prelude.undefined` if the value is ever evaluated, you'll instead just see a
program locking up if a value is evaluated.

::::::

Let's try our let vs case situation out in ghci:

```haskell
ghci> let (x,y) = undefined in True
True
ghci> case undefined of (x,y) -> True
*** Exception: Prelude.undefined
```

So, there's a difference!  What's going on here?

In my mind, this is how I see things:

1. *let* is **aliasing**
2. *case* is **control flow**

To apply this to what we just saw:

1.  *let* is just aliasing `x` and `y` to whatever is in the body.  If you
    don't use the alias, it's no big deal, really.  It's just like if you
    used `alias abc rm -rf /` in *bash*.  Aliasing `abc` to `rm -rf /` doesn't
    actually erase your entire file system, it's only something that does it if
    you decide to use the alias.

2.  *case* here *decides* what to do based on what the input is.  *If* you are
    given `(x, y)`, *then* return `True`.  It's used to direct the flow of how
    a program executes, so it *has* to look at the value to see what to do.

So, the reason that the `case` version explodes when you attempt to evaluate it
is because it has to ask the question "Does this value match this pattern?" ---
and in the process of investigating it, it has to touch `undefined` and so sets
off the mine.

The *let* situation never has to answer the question "does this value match
this pattern?", and so never needs to investigate it and so doesn't explode.

### Option One of One

One potential confusion here is that answering "does this value match this
pattern" seems silly for a tuple, since "all" tuples match that pattern, right?

For example, let's look at a situation that is uncontroversial

```gaskell
ghci> case undefined of Left x -> False; Right y -> True
*** Exception: Prelude.undefined
```

Of *course* that one has to explode, because it has to check if the `Either` is
`Left` or `Right`.  But in the case of `(x, y)`, it's always going to be `,`,
so why can't it just "know" you are `(x, y)` since there isn't any other
situation?

To me, the underlying reason is *consistency*.  A lot of beginners, when
starting Haskell, come to the impression that Haskell is *magical*: it just
does the best thing in every situation.  For example, a lot of Haskell learners
somehow all independently arrive at the assumption that using the same function
on the same argument will only compute it once, and cache the result.  After
all, it's possible (because all functions are pure, so calling the same
function on the same value will always return the same result), so why not have
it magically be true?

However, as you learn more Haskell, you start to realize that what might have
seemed magical at first is just the straightforward application of a few rules
that apply generally, with rare special-cases.  That's because for a language
to be useful, it's not about *magic*, but *predictability*.

So, case matching on a single-constructor type should not be special-cased from
the mechanics of a two-constructor type.  To check for a match, you always have
to peek at the constructor --- no matter how many there might be.

Another way you can look at it is that ADTs are stored in haskell as a
`(ConstructorTag, Fields)` tuple --- and all `data`-defined ADTs are stored in
that way, without special-case exceptions.

(Of course, the exception to this rule is `newtype`s, which follow different
rules from `data` in general)

And, of course, it's also possible to show "All tuples must match the `(x,y)`
pattern and be constructed with `(,)`" as false --- `undefined :: (Int, Bool)`
is an example of a tuple that isn't constructed with `(,)` and so doesn't match
the `(x, y)` pattern.


