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
etc., all of our regular expressions must have an associated "result".  The
value `Prim 'a' 1 :: Prim Int` will represent a primitive character that, when
parsed, will give a result of `1`.

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
There's no structure or properties at *all* about `RegExp`...other that it
contains `Prim` and the minimal structure/scaffolding to be an `Alternative`
instance.

After adding some convenient wrappers...we're done here!

```haskell
!!!misc/regexp.hs "-- | charAs" "-- | char" "-- | string"
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

Parsing
-------

Okay, so all we did was define a data structure that supports character
matching, concatenation, alternation, and starring.  Big whoop.  What we really
want to do is use it to parse things, right?  How does the Free Alternative
help us with *that*?

Well, a lot, actually.  Let's look at the definition of the free alternative:

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

Ultimately we want to write a `RegExp a -> String -> Maybe a`, which parses a
string based on a `RegExp`.  TO do this, we can simply pattern match and handle
the cases.

First, the top-level `Alt` case.  When faced with a list of chains, we can try
to parse each one.  The result is the first success.  `asum :: [Maybe a] ->
Maybe a` finds the first `Just` (success) in a list of attempts.

```haskell
!!!misc/regexp.hs "matchAlts ::"
```

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

`matchAlts` will match the *prefix* of the string, so we need to try all
successive prefixes on an input string until we get a match.

```haskell
!!!misc/regexp.hs "match2 ::"
```

