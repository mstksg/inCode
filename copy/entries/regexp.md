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

This...looks a lot like the construction of a regular language.  The only thing
missing is the literal character primitive.

So, one way we can look at regular expressions is "The entire `Alternative`
interface, plus a character primitive".  *But!*  There's another way of looking
at this, that leads us directly to free structures.

Instead of seeing things as "`Alternative` with a character primitive", we can
look at it as *a character primitive with a Alternative instance*.

Free
----

Let's write this out.  Our character primitive will be:


```haskell
data Prim a = Prim Char a
  deriving Functor
```

Note that because we're working with functors, applicatives, alternatives,
etc., all of our regular expressions must have an associated "result".  The
value `Prim 'a' 1 :: Prim Int` will represent a primitive character that, when
parsed, will give a result of `1`.

And now...we give it an `Alternative` instance using the *Free Alternative*,
from the *[free][]* package:

[free]: https://hackage.haskell.org/package/free

```haskell
import Control.Alternative.Free

type RegExp = Alt Prim
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

After adding some convenient wrappers...we're done here!

```haskell
-- | Parse a given character as a given constant result.
charAs :: Char -> a -> RegExp a
charAs c x = liftAlt (Prim c x)    -- liftAlt lets us use the underlying functor Prim in RegExp

-- | Parse a given character as itself.
char :: Char -> RegExp Char
char c = charAs c c

-- | Parse a given string as itself.
string :: String -> RegExp String
string = traverse char        -- neat, huh
```

### Examples

Let's try it out!  Let's match on `(a|b)(cd)*e` and return `()`:


```haskell
testRegExp_ :: RegExp ()
testRegExp_ = void $ (char 'a' <|> char 'b')
                  *> many (string "cd")
                  *> char 'e'
```

`void` from *Data.Functor* discards the results, since we only care if it
matches or not.  But we use `<|>` and `*>` and `many` exactly how we'd expect
to concatenate and alternate things with `Applicative` and `Alternative`.

Or maybe more interesting (but slightly more complicated), let's match on the
same one and return how many `cd`s are repeated

```haskell
testRegExp :: RegExp Int
testRegExp = (char 'a' <|> char 'b')
          *> (length <$> many (string "cd"))
          <* char 'e'
```

This one does require a little more finesse with `*>` and `<*`: the arrows
point towards which result to "keep".  And since `many (string "cd") :: RegExp
[String]` (it returns a list, with an item for each repetition), we can `fmap
length` to get the `Int` result of "how many repetitions".
