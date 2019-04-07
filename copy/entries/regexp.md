---
title: Applicative Regular Expressions using the Free Alternative
categories: Haskell, Math
tags: haskell, parsers
create-time: 2019/04/04 18:20:32
identifier: regexp
slug: free-alternative-regexp
---

We're going to implement applicative regular expressions and parsers (in the
style of the [regex-applicative][] library) using free structures!

[regex-applicative]: https://hackage.haskell.org/package/regex-applicative

Free structures are some of my favorite tools in Haskell, and I've actually
written a few posts about them before, including [this one using free
groups][groups], [this one on a free monad variation][monads], and [this one on
a "free" applicative on a monoid][const].

[groups]: https://blog.jle.im/entry/alchemical-groups.html
[monads]: https://blog.jle.im/entry/interpreters-a-la-carte-duet.html
[const]: https://blog.jle.im/entry/const-applicative-and-monoids.html

Regular expressions (and parsers) are ubiquitous in computer science and
programming, and I hope that demonstrating that they are pretty straightforward
to implement using free structures will help you see the value in free
structures without getting too bogged down in the details!

Regular Languages
-----------------

A *regular expression* is something that defines a *regular language*.
[Formally][formal], it consists of the following primitives:

[formal]: https://en.wikipedia.org/wiki/Regular_expression#Formal_language_theory

1. The empty set, which always fails to match.
2. The empty string, which always succeeds matching the empty string.
3. The literal character, denoting a single matching character

And the following operations:

1. Concatenation: `RS`, sequence one after the other.  A set product.
2. Alternation: `R|S`, one or the other.  A set union.
3. Kleene Star: `R*`, the repetition of `R` one or more times.

And that's *all* that's in a regular expression.  Nothing more, nothing less.
From these basic tools, you can derive the rest of the regexp operations ---
for example, `a+` can be expressed as `aa*`, and categories like `\w` can be
expressed as alternations of valid characters.

Alternative
-----------

Looking at this, does this look a little familiar?  It reminds me a lot of
the `Alternative` hierarchy.  If a functor `f` has an `Alternative` instance,
it means that it has:

1.  `empty`, the failing operation
2.  `pure x`, the always-succeeding operation (from the `Applicative` class)
3.  `<*>`, the sequencing operation (from the `Applicative` class)
4.  `<|>`, the alternating operation
5.  `many`, the "one or more" operation.

This...looks a lot like the construction of a regular language, doesn't it?
It's almost as if `Alternative` has almost *exactly* what we need.  The only
thing missing is the literal character primitive.

If you're unfamiliar with `Alternative`, the [typeclassopedia][] has a good
step-by-step introduction.  But for the purposes of this article, it's
basically just a "double monoid", with two "combining" actions `<*>` and `<|>`,
which roughly correspond to `*` and `+` in the integers.  It's basically pretty
much nothing more than 1-5 in the list above, and some distributivity laws.

[typeclassopedia]: https://wiki.haskell.org/Typeclassopedia

So, one way we can look at regular expressions is "The entire `Alternative`
interface, plus a character primitive".  *But!*  There's another way of looking
at this, that leads us directly to free structures.

Instead of seeing things as "`Alternative` with a character primitive", we can
look at it as *a character primitive enriched with an blank-slate Alternative
instance*.

Free
----

Let's write this out.  Our character primitive will be:


```haskell
!!!misc/regexp.hs "data Prim"
```

Note that because we're working with functors, applicatives, alternatives,
etc., all of our regular expressions can have an associated "result".  The
value `Prim 'a' 1 :: Prim Int` will represent a primitive that matches on the
character `a`, interpreting it with a result of `1`.

And now...we give it `Alternative` structure using the *Free Alternative*, from
the *[free][]* package:

[free]: https://hackage.haskell.org/package/free

```haskell
import Control.Alternative.Free

!!!misc/regexp.hs "type RegExp"
```

And that's it!  That's our entire regular expression type!  By giving a `Alt` a
`Functor`, we get all of the operations of `Applicative` and `Alternative` over
our base.  That's because we have `instance Applicative (Alt f)` and `instance
Alternative (Alt f)`.  We now have:

1.  The empty set, coming from `empty` from `Alternative`
2.  The empty string, coming from `pure` from `Applicative`
3.  The character primitive, coming from the underlying functor `Prim` that we
    are enhancing
4.  The concatenation operation, from `<*>`, from `Applicative`.
5.  The alternating operation, from `<|>`, from `Alternative`.
6.  The kleene star, from `many`, from `Alternative`.

All of these (except for the primitive) come "for free"!

Essentially, what a free structure gives us is the structure of the abstraction
(`Alternative`, here) automatically for our base type, and *nothing else*.

Remember that regular expressions have these operations, *and nothing else* ---
no more, no less.  That's exactly what the free Alternative gives us: these
these operations and the primitive.  No more, no less.

After adding some convenient wrappers...we're done here!

```haskell
!!!misc/regexp.hs "-- | charAs:" "-- | char:" "-- | string:"
```

### Examples

Let's try it out!  Let's match on `(a|b)(cd)*e` and return `()`:


```haskell
!!!misc/regexp.hs "testRegExp_ ::"
```

`void` from *Data.Functor* discards the results, since we only care if it
matches or not.  But we use `<|>` and `*>` and `many` exactly how we'd expect
to concatenate and alternate things with `Applicative` and `Alternative`.

Or maybe more interesting (but slightly more complicated), let's match on the
same one and return how many `cd`s are repeated

```haskell
!!!misc/regexp.hs "testRegExp ::"
```

This one does require a little more finesse with `*>` and `<*`: the arrows
point towards which result to "keep".  And since `many (string "cd") :: RegExp
[String]` (it returns a list, with an item for each repetition), we can `fmap
length` to get the `Int` result of "how many repetitions".

However, we can also turn on *-XApplicativeDo* and write it using do notation,
which requires a little less thought:

```haskell
!!!misc/regexp.hs "testRegExpDo ::"
```

Parsing
-------

Okay, so all we did was define a data structure that supports character
matching, concatenation, alternation, and starring.  Big whoop.  What we really
want to do is use it to parse things, right?  How does the Free Alternative
help us with *that*?

Well, a lot, actually.  Let's look at two ways of doing this!

### Offloading to another Alternative

#### What is Freeness?

The "canonical" way of using a free structure is by using the "folding"
homomorphism into a concrete structure with the proper instances.  For example,
`foldMap` will turn a free monoid a value of any monoid instance:

```haskell
foldMap :: Monoid m => (a -> m) -> ([a] -> m)
```

`foldMap` lifts an `a -> m` into a `[a] -> m` (for, `FreeMonoid a -> m`), with
a concrete monoid `m`.  The general idea is that using a free structure can
"defer" the concretization from between the time of construction to the time of
use.

For example, we can construct value in the free monoid made from integers:

```haskell
-- | Lift the "primitive" `Int` into a value in the free monoid on `Int`.
liftFM :: Int -> [Int]
liftFM x = [x]

myMon :: [Int]
myMon = liftFM 1 <> liftFM 2 <> liftFM 3 <> liftFM 4
```

And now we can decide how we want to interpret `<>` --- should it be `+`?

```haskell
ghci> foldMap Sum myMon
Sum 10
```

Or should it be `*`?

```haskell
ghci> foldMap Product myMon
Product 24
```

The idea is that we can "defer" the choice of concrete `Monoid` that `<>` is
interpreted under by first pushing 1, 2, 3, and 4 into their free monoid.  The
free monoid on `Int` gives *exactly enough structure* to `Int` to do this job:
no more, no less.

To use `foldMap`, we say "how to handle the base type", and it lets us handle
the free structure in its entirety.

#### Interpreting in State

In this case, we're in luck.  There's a concrete `Alternative` instance that
works just the way we want: `StateT String Maybe`:

*   Its `<*>` works by sequencing changes in state; in this case, we'll
    consider the state as "characters yet to be parsed".
*   Its `<|>` works by backtracking and trying again if it runs into a failure.
    It saves the state of the last successful point and resets to it on
    failure.

The "folding" homomorphism of the free alternative is called `runAlt`:

```haskell
runAlt :: Alternative f
       => (forall b. p b -> f b)
       -> Alt p a
       -> f a
```

And in the case of `RegExp`, we have:

```haskell
runAlt :: Alternative f
       => (forall b. Prim b -> f b)
       -> RegExp a
       -> f a
```

If you're unfamiliar with the RankN type (the `forall b.` stuff), there's a
[nice introduction here][rankn].  But basically, you just need to provide
`runAlt` with a function that can handle a `Prim b` for *any* `b` (and not just
a specific one like `Int` or `Bool`).

[rankn]: https://ocharles.org.uk/guest-posts/2014-12-18-rank-n-types.html

So, like `foldMap`, we need to say "how to handle our base type".  How do we
handle `Prim`?

```haskell
!!!misc/regexp.hs "processPrim ::"
```

This lets us interpret a `Prim` as a `StateT String Maybe` action where the
state is the "string left to be be processed".  Remember, a `Prim a` contains
the character we want to match on, and the `a` value we want it to be
interpreted as.  To process a `Prim`, we:

1.  Get the state's head and tail, using `get`.  If this match fails,
    backtrack. (This is implemented using a pattern match in a do block)
2.  If the head doesn't match what the `Prim` expects, backtrack.
3.  Set the state to be the original tail, using `put`.
4.  The result is what the `Prim` returns.

We can use this to write a function that matches the `RegExp` on a prefix.  We
need to run the state action (using `evalStateT`) on the string we want to
parse:

```haskell
!!!misc/regexp.hs "matchPrefix ::"
```

And that's it!  Our first solution:

<!-- `matchPrefix` will match the *prefix* of the string, so we need to try all -->
<!-- successive prefixes on an input string until we get a match.  Here, `asum :: -->
<!-- [Maybe a] -> Maybe a` finds the first `Just` (success) in a list of attempts, -->
<!-- and `tails :: [a] -> [[a]]` is the list of prefixes of a string. -->

```haskell
ghci> matchPrefix testRegexp_ "acdcdcde"
Just ()
ghci> matchPrefix testRegexp_ "acdcdcdx"
Nothing
ghci> matchPrefix testRegexp "acdcdcde"
Just 3
ghci> matchPrefix testRegexp "acdcdcdcdcdcdcde"
Just 7
```

#### What just happened?

Okay, so that might have happened a little quicker than you expected.  One
minute we were writing our primitive, and the next we had already finished.
Here's the entirety of the code, in a few lines of Haskell:

```haskell
type RegExp = Alt Prim

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)
  where
    processPrim (Prim c x) = do
      d:ds <- get
      guard (c == d)
      put ds
      pure x
```

And now we have a fully functioning regexp parser?  What happened?

From a high-level view, remember that `Alt Prim` has, in its structure, `pure`,
`empty`, `Prim`, `<*>`, `<|>`, and `many`[^many].

[^many]: A caveat exists here.  More on this later!

Essentially, what `runAlt` does is that it uses a given concrete `Alternative`
(here, `StateT String Maybe`) to get the behavior of `pure`, `empty`, `<*>`, `<|>`, and
`many`.  But!  `StateT` doesn't have a built-in behavior for `Prim`.  And so,
that's where `processPrim` comes in.  For `Prim`, `runAlt` uses `processPrim`.
for `pure`, `empty`, `<*>`, `<|>`, and `many`, `runAlt` uses `StateT String
Maybe`'s `Alternative` instance.

So, really, 83% of the work was done for us by `StateT`'s `Alternative`
instance, and the other 17% is in `processPrim`.

Admittedly, this *does* feel a little disappointing, or at least anticlimactic.
This makes us wonder: why even use `Alt` in the first place?  Why not just have
`type RegExp = StateT String Maybe` and write an appropriate `char :: Char ->
StateT String Maybe Char`?  If `StateT` does all of the work anyway, why even
bother with `Alt`, the free Alternative?

One major advantage we get from using `Alt` is that `StateT` is...pretty
powerful.  It's actually *stupid* powerful...it can represent a lot of things.
Especially things that *are not regular expressions*.  For example, something
as simple as `put "hello"` does not correspond to *any* regular expression.

So, while we can say that `Alt Prim` corresponds to "regular expressions,
nothing less and nothing more", we *cannot* say the same about `StateT String
Maybe`.

`Alt Prim` contains a "perfect fit" representation of a regular expression data
type.  Everything it can express is a regular expression, and there is nothing
it can express that *isn't* a regular expression.[^perfect]

[^perfect]: Note that there are some caveats that should be noted here, due to
laziness in Haskell.  We will go deeper into this later.

Here, we can think of `StateT` is the context that we use to *interpret* a
`RegExp` as a *parser*.  But, there might be *other* ways we want to work with
a `RegExp`.  For example, we might want to inspect it and "print" it out for
inspection.  This is something we can't do with `StateT`.

We can't say that `StateT String Maybe` *is* a regular expression --- only that
it can represent a parser based on a regular expression.  But we *can* say that
about `Alt Prim`.

### Using the Free structure directly

Alright, that's great and all.  But what if we didn't want to offload 83% of
the behavior to a type that has already been written for us.  Is there a way we
can directly use the structure of `Alt` itself to write our parser?

I'm glad you asked!  Let's look at the definition of the free alternative:

```haskell
newtype Alt f a = Alt { alternatives :: [AltF f a] }

data AltF f a = forall r. Ap (f r) (Alt f (r -> a))
              |           Pure a
```

It's a mutually recursive type, so it might be a little confusing.  One way to
understand `Alt` is that `Alt xs` contains a *list of alternatives*, or a list
of `<|>`s.  And each of those alternatives is an `AltF`, which is a *sequence
of `f a`s* (as a chain of function applications).

You can essentially think of `AltF f a` as a linked list `[f r]`, except with a
different `r` for each item.  `Ap` is cons (`:`), containing the `f r`, and
`Pure` is nil (`[]`).

It's like a list (`Alt` list) of lists (`AltF` chains), which take turn
alternating between alternative lists and application sequences.

You can think of `Alt f` as a "normalized" form of successive or nested `<*>` and
`<|>`s, similar to how `[a]` is a "normalized" form of successive `<>`s.

Ultimately we want to write a `RegExp a -> String -> Maybe a`, which parses a
string based on a `RegExp`.  TO do this, we can simply pattern match and handle
the cases.

First, the top-level `Alt` case.  When faced with a list of chains, we can try
to parse each one.  The result is the first success.

```haskell
!!!misc/regexp.hs "matchAlts ::"
```

Here, `asum :: [Maybe a] -> Maybe a` finds the first `Just` (success) in a list
of attempts.

Now, we need to handle the chain case.  To do this, we can pattern match on
each constructor, and handle each case.

```haskell
matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) []     = _
matchChain (Ap (Prim c x) next) (d:ds)
    | c == d    = _             -- succesful match
    | otherwise = _             -- bad match
matchChain (Pure x)             []     = _
matchChain (Pure x)             (d:ds) = _
```

From here, it's mostly "type tetris"!  We just continually ask GHC what goes in
what holes (and what types need to change) until we get something that
typechecks.

In the end of the very mechanical process, we get:

```haskell
!!!misc/regexp.hs "matchChain ::"
```

1.  If it's `Ap` (like cons, `:`), it means we're in the middle of the chain.

    *   If the input string is empty, then we fail to match.
    *   Otherwise, here's the interesting thing.  We have the `Prim` with the
        character we want to match, and the first letter in the string.
        *   If the match is a success, we continue down the chain, to `next ::
            RegExp (r -> a)`.  We just need to massage the types a bit to make
            it all work out.
        *   Otherwise, it's a failure.  We're done here.

2.  If it's `Pure x` (like nil, `[]`), it means we're at the end of the chain.
    We return the result in `Just`.

In the end though, you don't really need to understand any of this in order to
write this.  Sure, it's nice to understand what `Ap`, `Pure`, `AltF`, etc.
really "mean". But, we don't have to --- the types take care of all of it for
you :)

<!-- `matchAlts` will match the *prefix* of the string, so we need to try all -->
<!-- successive prefixes on an input string until we get a match. -->

<!-- ```haskell -->
<!-- !!!misc/regexp.hs "match2 ::" -->
<!-- ``` -->

<!-- `matchPrefix` will match the *prefix* of the string, so we need to try all -->
<!-- successive prefixes on an input string until we get a match.  Here, `asum :: -->
<!-- [Maybe a] -> Maybe a` finds the first `Just` (success) in a list of attempts, -->
<!-- and `tails :: [a] -> [[a]]` is the list of prefixes of a string. -->

