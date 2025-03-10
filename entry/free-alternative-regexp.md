Applicative Regular Expressions using the Free Alternative

===========================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on April 8, 2019.
> [Read online!](https://blog.jle.im/entry/free-alternative-regexp.html)

Today, we're going to implement applicative regular expressions and parsers (in
the style of the
[regex-applicative](https://hackage.haskell.org/package/regex-applicative)
library) using free structures!

Free structures are some of my favorite tools in Haskell, and I've actually
written a few posts about them before, including [this one using free
groups](https://blog.jle.im/entry/alchemical-groups.html), [this one on a free
monad variation](https://blog.jle.im/entry/interpreters-a-la-carte-duet.html),
and [this one on a "free" applicative on a
monoid](https://blog.jle.im/entry/const-applicative-and-monoids.html).

Regular expressions (and parsers) are ubiquitous in computer science and
programming, and I hope that demonstrating that they are pretty straightforward
to implement using free structures will help you see the value in free
structures without getting too bogged down in the details!

All of the code in this post is [available
online](https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs)
as a "stack executable". When you run it (`./regexp.hs`), you'll load up a
*ghci* session with all of the definitions in scope, so you can play around with
the functions and types :)

This post should be accessible to late beginner or early intermediate Haskell
users, and requires some basic familiarity with pattern matching, algebraic data
types, and abstractions like `Monoid` and `Functor`, and do notation.

## Regular Languages

A *regular expression* is something that defines a *regular language*.
[Formally](https://en.wikipedia.org/wiki/Regular_expression#Formal_language_theory),
it consists of the following base elements:

1.  The empty set, which always fails to match.
2.  The empty string, which always succeeds matching the empty string.
3.  The literal character, denoting a single matching character

And the following operations:

1.  Concatenation: `RS`, sequence one after the other. A set product.
2.  Alternation: `R|S`, one or the other. A set union.
3.  Kleene Star: `R*`, the repetition of `R` zero or more times.

And that's *all* that's in a regular expression. Nothing more, nothing less.
From these basic tools, you can derive the rest of the regexp operations --- for
example, `a+` can be expressed as `aa*`, and categories like `\w` can be
expressed as alternations of valid characters.

### Alternative

Looking at this, does this look a little familiar? It reminds me a lot of the
`Alternative` typeclass. If a functor `f` has an `Alternative` instance, it
means that it has:

1.  `empty`, the failing operation
2.  `pure x`, the always-succeeding operation (from the `Applicative` class)
3.  `<*>`, the sequencing operation (from the `Applicative` class)
4.  `<|>`, the alternating operation
5.  `many`, the "zero or more" operation.

This...looks a lot like the construction of a regular language, doesn't it? It's
almost as if `Alternative` has almost *exactly* what we need. The only thing
missing is the literal character primitive.

If you're unfamiliar with `Alternative`, the
[typeclassopedia](https://wiki.haskell.org/Typeclassopedia) has a good
step-by-step introduction. But for the purposes of this article, it's basically
just a "double monoid", with two "combining" actions `<*>` and `<|>`, which
roughly correspond to `*` and `+` in the integers. It's basically pretty much
nothing more than 1-5 in the list above, and some distributivity laws.

So, one way we can look at regular expressions is "The entire `Alternative`
interface, plus a character primitive". *But!* There's another way of looking at
this, that leads us directly to free structures.

Instead of seeing things as "`Alternative` with a character primitive", we can
look at it as *a character primitive enriched with a Alternative instance*.

## Free

Let's write this out. Our character primitive will be:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L17-L18

data Prim a = Prim Char a
  deriving Functor
```

Note that because we're working with functors, applicatives, alternatives, etc.,
all of our regular expressions can have an associated "result". This is because
our regexp values will have a type parameter (which is required for `Functor`,
`Applicative`, and `Alternative`). We can choose to ignore this type parameter,
but we can also have some fun by using it to represent a "result" that a regexp
match will be interpreted as. This is similar to the idea of "capturing" in
industrial regexp applications.

Here, the value `Prim 'a' 1 :: Prim Int` will represent a primitive that matches
on the character `a`, interpreting it with a result of `1`.

And now...we give it `Alternative` structure using the *Free Alternative*, from
the *[free](https://hackage.haskell.org/package/free)* package:

``` haskell
import Control.Alternative.Free

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L20-L20

type RegExp = Alt Prim
```

And that's it! That's our entire regular expression type! By giving a `Alt` a
`Functor`, we get all of the operations of `Applicative` and `Alternative` over
our base. That's because we have `instance Applicative (Alt f)` and
`instance Alternative (Alt f)`. We now have:

1.  The empty set, coming from `empty` from `Alternative`
2.  The empty string, coming from `pure` from `Applicative`
3.  The character primitive, coming from the underlying functor `Prim` that we
    are enhancing
4.  The concatenation operation, from `<*>`, from `Applicative`.
5.  The alternating operation, from `<|>`, from `Alternative`.
6.  The kleene star, from `many`, from `Alternative`.

All of these additions to our primitive come "for free"!

Essentially, what a free structure gives us is the structure of the abstraction
(`Alternative`, here) automatically for our base type, and *nothing else*.

Remember that regular expressions have these operations, *and nothing else* ---
no more, no less. That's exactly what the free Alternative gives us: these
operations and the primitive. No more, no less.

After adding some convenient wrappers...we're done here!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L22-L33

-- | charAs: Parse a given character as a given constant result.
charAs :: Char -> a -> RegExp a
charAs c x = liftAlt (Prim c x)     -- liftAlt lets us use the underlying
                                    -- functor Prim in RegExp

-- | char: Parse a given character as itself.
char :: Char -> RegExp Char
char c = charAs c c

-- | string: Parse a given string as itself.
string :: String -> RegExp String
string = traverse char              -- neat, huh
```

### Examples

Let's try it out! Let's match on `(a|b)(cd)*e` and return `()`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L35-L38

testRegExp_ :: RegExp ()
testRegExp_ = void $ (char 'a' <|> char 'b')
                  *> many (string "cd")
                  *> char 'e'
```

`void` from *Data.Functor* discards the results, since we only care if it
matches or not. But we use `<|>` and `*>` and `many` exactly how we'd expect to
concatenate and alternate things with `Applicative` and `Alternative`.

Or maybe more interesting (but slightly more complicated), let's match on the
same pattern and return how many `cd`s are repeated

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L40-L43

testRegExp :: RegExp Int
testRegExp = (char 'a' <|> char 'b')
          *> (length <$> many (string "cd"))
          <* char 'e'
```

This one does require a little more finesse with `*>` and `<*`: the arrows point
towards which result to "keep". And since
`many (string "cd") :: RegExp [String]` (it returns a list, with an item for
each repetition), we can `fmap length` to get the `Int` result of "how many
repetitions".

However, we can also turn on *-XApplicativeDo* and write it using do notation,
which requires a little less thought:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L45-L50

testRegExpDo :: RegExp Int
testRegExpDo = do
    char 'a' <|> char 'b'
    cds <- many (string "cd")
    char 'e'
    pure (length cds)
```

It's all a little bit like how we often use "captures" in regular expressions to
access a *specific part* of a match. Here's an example in ruby:

``` ruby
irb> /(a|b)((cd)*)e/.match("acdcdcdcde")[2]
=> "cdcdcdcd"
```

except we also include a "post-processing" process to get the length of the
number of repetitions.

Here's another handy regexp that matches on a digit between 0 to 9, and the
result is the digit `Int` itself:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L52-L53

digit :: RegExp Int
digit = asum [ charAs (intToDigit i) i | i <- [0..9] ]
```

Here, `asum [x,y,z] = x <|> y <|> z`: it represents a choice between the items
in a list.

We can again do some fancy things with it, like make a regexp `\[\d\]` that
matches on a digit inside `[` / `]` brackets:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L55-L56

bracketDigit :: RegExp Int
bracketDigit = char '[' *> digit <* char ']'
```

## Parsing

Okay, so all we did was define a data structure that supports character
matching, concatenation, alternation, and starring. Big whoop. What we really
want to do is use it to parse things, right? How does the Free Alternative help
us with *that*?

Well, it does a lot, actually. Let's look at two ways of doing this!

### Offloading Alternative

#### What is Freeness?

The "canonical" way of using a free structure is by using its "folding"
operation into a concrete structure with the proper instances. For example,
`foldMap` will turn a free monoid into a value of any monoid instance:

``` haskell
foldMap :: Monoid m => (a -> m) -> ([a] -> m)
```

`foldMap` lifts an `a -> m` into a `[a] -> m` (or, `FreeMonoid a -> m`), with a
concrete monoid `m`. The general idea is that using a free structure can "defer"
the concretization from between the time of construction to the time of use.

For example, we can construct value in the free monoid made from integers:

``` haskell
-- | Lift the "primitive" `Int` into a value in the free monoid on `Int`.
--
-- Analogous to `liftAlt` from earlier.
liftFM :: Int -> [Int]
liftFM x = [x]

myMon :: [Int]
myMon = liftFM 1 <> liftFM 2 <> liftFM 3 <> liftFM 4
```

And now we can decide how we want to interpret `<>` --- should it be `+`?

``` haskell
ghci> foldMap Sum myMon
Sum 10              -- 1 + 2 + 3 + 4
```

Or should it be `*`?

``` haskell
ghci> foldMap Product myMon
Product 24          -- 1 * 2 * 3 * 4
```

Or maybe even `max`?

``` haskell
ghci> foldMap Max myMon
Max 4          -- 1 `max` 2 `max` 3 `max` 4
```

The idea is that we can "defer" the choice of concrete `Monoid` that `<>` is
interpreted under by first pushing 1, 2, 3, and 4 into a free monoid value. The
free monoid on `Int` gives *exactly enough structure* to `Int` to do this job:
no more, no less.

To use `foldMap`, we say "how to handle the base type", and it lets us handle
the free structure in its entirety by offloading the behavior of `<>` to the
concrete monoid.

#### Interpreting in State

In practice, getting a result from a free structure is often about finding (or
creating) the right concrete `Alternative` that gives us the behavior we want.
In this case, we're in luck. There's a concrete `Alternative` instance that
works just the way we want: `StateT String Maybe`:

-   Its `<*>` works by sequencing changes in state; in this case, we'll consider
    the state as "characters yet to be parsed", so sequential parsing fits
    perfectly with `<*>`. That's because combining regexps sequentially can be
    thought of as statefully chomping down on a string.
-   Its `<|>` works by backtracking and trying again if it runs into a failure.
    It saves the state of the last successful point and resets to it on failure.
    This is exactly how we want regexp alternation `R|S` to behave.

The "folding" operation of the free alternative is called `runAlt`:

``` haskell
runAlt :: Alternative f
       => (forall b. p b -> f b)
       -> Alt p a
       -> f a
```

And in the case of `RegExp`, we have:

``` haskell
runAlt :: Alternative f
       => (forall b. Prim b -> f b)
       -> RegExp a
       -> f a
```

If you're unfamiliar with the RankN type (the `forall b.` stuff), there's a
[nice introduction
here](https://ocharles.org.uk/guest-posts/2014-12-18-rank-n-types.html). But
basically, you just need to provide `runAlt` with a function that can handle a
`Prim b` for *any* `b` (and not just a specific one like `Int` or `Bool`).

So, like `foldMap`, we need to say "how to handle our base type". Here, we have
to answer "How do we handle `Prim`?"

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L58-L63

processPrim :: Prim a -> StateT String Maybe a
processPrim (Prim c x) = do
    d:ds <- get
    guard (c == d)
    put ds
    pure x
```

This lets us interpret a `Prim` as a `StateT String Maybe` action where the
state is the "string left to be be processed". Remember, a `Prim a` contains the
character we want to match on, and the `a` value we want it to be interpreted
as. To process a `Prim`, we:

1.  Get the state's (the string left to be parsed) head and tail, using `get`.
    If this match fails, backtrack.
2.  Using `guard`, backtrack unless the head matches what the `Prim` expects.
3.  Set the state to be the original tail, using `put`. This is because we
    parsed the head already, so now the "string left to be parsed" is just the
    original tail.
4.  The result is what the `Prim` says it should be.

We can use this to write a function that matches the `RegExp` on a prefix. We
need to run the state action (using `evalStateT`) on the string we want to
parse:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L65-L66

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)
```

And that's it! Our first solution:

``` haskell
ghci> matchPrefix testRegexp_ "acdcdcde"
Just ()
ghci> matchPrefix testRegexp_ "acdcdcdx"
Nothing
ghci> matchPrefix testRegexp "acdcdcde"
Just 3
ghci> matchPrefix testRegexp "bcdcdcdcdcdcdcde"
Just 7
ghci> matchPrefix digit "9"
Just 9
ghci> matchPrefix bracketDigit "[2]"
Just 2
ghci> matchPrefix (many bracketDigit) "[2][3][4][5]"
Just [2,3,4,5]
ghci> matchPrefix (sum <$> many bracketDigit) "[2][3][4][5]"
Just 14
```

#### Wait, what just happened?

Okay, so that might have happened a little quicker than you expected. One minute
we were writing our primitive, and the next we had already finished. Here's the
entirety of the code, in a few lines of Haskell:

``` haskell
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

And now we have a fully functioning regexp parser? What happened?

From a high-level view, remember that `Alt Prim` has, in its structure, `pure`,
`empty`, `Prim`, `<*>`, `<|>`, and `many`[^1].

Essentially, what `runAlt` does is that it uses a given concrete `Alternative`
(here, `StateT String Maybe`) to get the behavior of `pure`, `empty`, `<*>`,
`<|>`, and `many`. But! As we can see from that list, `StateT` does *not* have a
built-in behavior for `Prim`. And so, that's where `processPrim` comes in.

-   For `Prim`, `runAlt` uses `processPrim`.
-   For `pure`, `empty`, `<*>`, `<|>`, and `many`, `runAlt` uses
    `StateT String     Maybe`'s `Alternative` instance.

So, really, 83% of the work was done for us by `StateT`'s `Alternative`
instance, and the other 17% is in `processPrim`.

Admittedly, this *does* feel a little disappointing, or at least anticlimactic.
This makes us wonder: why even use `Alt` in the first place? Why not just have
`type RegExp = StateT String Maybe` and write an appropriate
`char :: Char -> StateT String Maybe Char`? If `StateT` does all of the work
anyway, why even bother with `Alt`, the free Alternative?

One major advantage we get from using `Alt` is that `StateT` is...pretty
powerful. It's actually *stupid* powerful. It can represent a lot of
things...most troubling, it can represent things that *are not regular
expressions*. For example, something as simple as
`put "hello" :: StateT String Maybe ()` does not correspond to *any* regular
expression.

So, while we can say that `Alt Prim` corresponds to "regular expressions,
nothing less and nothing more", we *cannot* say the same about
`StateT String Maybe`.

`Alt Prim` contains a "perfect fit" representation of a regular expression data
type. Everything it can express is a regular expression, and there is nothing it
can express that *isn't* a regular expression.[^2]

Here, we can think of `StateT` as the context that we use to *interpret* a
`RegExp` as a *parser*. But, there might be *other* ways we want to work with a
`RegExp`. For example, we might want to inspect it and "print" it out for
inspection. This is something we can't do with `StateT`.

We can't say that `StateT String Maybe` "is" a regular expression --- only that
it can represent a parser based on a regular expression. But we *can* say that
about `Alt Prim`.

### Using the Free structure directly

Alright, that's great and all. But what if we didn't want to offload 83% of the
behavior to a type that has already been written for us. Is there a way we can
directly use the structure of `Alt` itself to write our parser?

This is analogous to asking, what if we wanted to actually write a function on a
list (by pattern matching on `:` and `[]`) instead of always using `foldMap`?
Can we directly operate on the structure of the list instead of using `foldMap`
with some concrete monoid?

I'm glad you asked! Let's look at the definition of the free alternative:

``` haskell
newtype Alt f a = Alt { alternatives :: [AltF f a] }

data AltF f a = forall r. Ap (f r) (Alt f (r -> a))
              |           Pure a
```

It's a mutually recursive (and
[non-regular](https://twanvl.nl/blog/haskell/non-regular1)) type, so it might be
a little confusing. One way to understand `Alt` is that `Alt xs` contains a
*list of alternatives*, or a list of `<|>`s. And each of those alternatives is
an `AltF`, which is a *sequence of `f a`s* chained by `<*>` (as a chain of
function applications).

You can essentially think of `AltF f a` as a linked list `[f r]`, except with a
different `r` for each item. `Ap` is cons (`:`), containing the `f r`, and
`Pure` is nil (`[]`). The `forall r.` here is *-XExistentialQuantification*, and
is what lets us have a different intermediate type for each item in our chain.

All in all, `Alt f` is like a list (`Alt` list) of lists (`AltF` chains), which
take turn alternating between alternative lists and application sequences. A
list of chains of lists of chains of lists of chains ...

In the big picture, you can think of `Alt f` as a "normalized" form of
successive or nested `<*>` and `<|>`s, similar to how `[a]` is a "normalized"
form of successive `<>`s.

Ultimately we want to write a `RegExp a -> String -> Maybe a`, which parses a
string based on a `RegExp`. We can do this using the most basic of all Haskell
function-writing techniques: pattern matching on each case, and handling each
case.

First, the top-level `Alt` case. When faced with a list of chains, we can try to
parse each one. The result is the first success.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L68-L69

matchAlts :: RegExp a -> String -> Maybe a
matchAlts (Alt res) xs = asum [ matchChain re xs | re <- res ]
```

Here, `asum :: [Maybe a] -> Maybe a` finds the first `Just` (success) in a list
of attempts.

Now, we need to handle the chain case. To do this, we can pattern match on each
constructor, and handle each case.

``` haskell
matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) cs = _
matchChain (Pure x)             cs = _
```

From here, it's mostly "type tetris"! We just continually ask GHC what goes in
what holes (and what types need to change) until we get something that
typechecks.

In the end of the very mechanical process, we get:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L71-L76

matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) cs = case cs of
    []  -> Nothing
    d:ds | c == d    -> matchAlts (($ x) <$> next) ds
         | otherwise -> Nothing
matchChain (Pure x)             _      = Just x
```

1.  If it's `Ap` (analogous to cons, `:`), it means we're in the middle of the
    chain.

    -   If the input string is empty, then we fail to match.
    -   Otherwise, here's the interesting thing. We have the `Prim r` with the
        character we want to match, the first letter in the string, and
        `next     :: RegExp (r -> a)`, the next `RegExp` in our sequential
        parsing chain.
        -   If the match is a success, we continue down the chain, to `next`. We
            just need to massage the types a bit to make it all work out.
        -   Otherwise, it's a failure. We're done here.

2.  If it's `Pure x` (analogous to nil, `[]`), it means we're at the end of the
    chain. We return the result in `Just`.

In the end though, you don't really need to understand any of this *too* deeply
in order to write this. Sure, it's nice to understand what `Ap`, `Pure`, `AltF`,
etc. really "mean". But, we don't have to --- the types take care of all of it
for you :)

That should be good enough to implement another prefix parser:

``` haskell
ghci> matchAlts testRegexp_ "acdcdcde"
Just ()
ghci> matchAlts testRegexp_ "acdcdcdx"
Nothing
ghci> matchAlts testRegexp "acdcdcde"
Just 3
ghci> matchAlts testRegexp "bcdcdcdcdcdcdcde"
Just 7
ghci> matchAlts digit "9"
Just 9
ghci> matchAlts bracketDigit "[2]"
Just 2
ghci> matchAlts (many bracketDigit) "[2][3][4][5]"
Just [2,3,4,5]
ghci> matchAlts (sum <$> many bracketDigit) "[2][3][4][5]"
Just 14
```

### What did we do?

The two attempts here can be compared to using lists via `foldMap` vs. using
lists via pattern matching.

Because lists act as a free monoid, *any list function* can be written using
`foldMap` and not directly pattern matching. If this seems unbelievable to you,
try finding a function that can't --- you might be surprised!

However, because lists are an algebraic data type, *any list function* can be
written using straight up pattern matching on `:` and `[]`.

One nice thing about list is that, no matter how you assemble it, it always ends
up as a series of conses and nil. We say that the free monoid is *normalizing*.
That is, `[1,2,3] <> [4]` has the same representation as `[1] <> [2,3] <> [4].`
When we pattern match on `:` and `[]`, we can't distinguish between those two
original methods of creation.

`Alt` is normalizing as well. An example of a possible variant that is *not*
normalizing is:

``` haskell
data RegExp a = Empty
              | Pure a
              | Prim Char a
              | forall r. Seq (RegExp r) (RegExp (r -> a))
              | Union (RegExp a) (RegExp a)
              | Many (RegExp a)
```

This is how we *might* have written `RegExp`, if we didn't know about the free
alternative. However, this representation is not normalizing, because we have
two `RegExp a` values that represent the same regexp:

``` haskell
-- | a|(b|c)
abc1 :: RegExp Int
abc1 = Prim 'a' 1 `Union` (Prim 'b' 2 `Union` Prim 'c' 3)

-- | (a|b)|c
abc2 :: RegExp Int
abc2 = (Prim 'a' 1 `Union` Prim 'b' 2) `Union` Prim 'c' 3
```

These two match the same thing. But they have different representations. This
representation *not* normalizing, since the same regexp can be expressed in two
different ways.

`Alt Prim` is better because if two regexps are the same...they will correspond
to the same `Alt Prim`. It forces each value to exist in a "canonical"
normalized form.

This means that when we eventually write our parsing function `matchAlts`, *we
aren't allowed to care* about "how" the regexps are made. We aren't *allowed* to
distinguish between `(a|b)|c` and `a|(b|c)`. The normalizing property of `Alt`
means that we are forced to treat both of those the *exact* same way. It
*forces* us to obey the laws about the structures of regular languages.

It's easy to imagine a bug that might occur if we accidentally treated `(a|b)|c`
differently than `a|(b|c)` --- and indeed it sounds like an easy bug to
accidentally make if we are using the non-normalizing representation.

Using `Alt` instead of rolling our own regexp expression not only enforces our
integrity, but it also eliminates a huge class of potential bugs.

However, it should be noted that, while `Alt f` is strongly normalizing with
respect to Alternative structure, `Alt Prim` isn't strongly normalizing with
respect to *regular expressions* structure in every single case. For example,
`Alt Prim` will still treat `a|a` as different from `a`. This is mostly because
`Alt` has to be "agnostic" to `f`. But, like with all structural "type safety",
I always follow this rule of thumb: *A lot of safety is better than no safety*.
This method can't eliminate *all* bugs arising from this angle, but it can
eliminate a whoooole lot.

## Some subtle caveats

Before we conclude, let's take some time to clarify a subtle point. Feel free to
skip this whole section if you don't care about the fact that these aren't
identical to the mathematical formalism of regular languages.

While we can *use* `RegExp` just like a regular expression, the formal concept
of regular expressions is actually slightly different, due to one pesky thing:
**laziness**.

We really *shouldn't* be too surprised, since laziness actually throws a wrench
into a *lot* of Haskell abstractions that are based on math. For example,
laziness is the reason that lists aren't "true" mathematical free monoids.

The reason is that because of laziness and unbounded recursion, we can create an
"infinite" regular language: `a|aa|aaa|aaaa|aaaaa|aaaaaa|...`, forever and ever.
However, infinite regular expressions aren't allowed in the mathematical
version. In Haskell, unfortunately, there is no way to "turn off" recursion:
we're stuck with it.

Even more unfortunately, this is actually how the `Alt` encoding of the free
alternative above implements `many`. `a*` is implemented as
`|a|aa|aaa|aaaa|aaaaa|...`, infinitely. So the representation actually *relies*
on laziness and infinite recursion to do its job. If you look at the contents of
`many (char 'a')`, you will see an infinite list.

"Haskell without recursion" is fine with `Alt` for a "[star-free
language](https://en.wikipedia.org/wiki/Star-free_language)", but it won't cut
it for a regular one.

For the purposes we talked about in this post, this doesn't matter. However,
this does create serious issues if we want to write a [non-deterministic finite
automata based
parser](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton) (NFA),
which is the de facto standard for implementing fast regexp parsers. We can only
really generate an NFA if we have a *finite* value, which means anything using
`many` is out of the question.

Not all hope is lost, however. We can actually use the "final encoding" of
`Alt`, from *Control.Alternative.Free.Final*, to gain a `many` that is
non-recursive.

Using the final encoding means we lose the "pattern match" method, and can only
use the `runAlt` method. However, we can off-load to `Alternative` instances
that have non-recursive `many` (like the `RE` type from *regex-applicative*)
that allows us to generate an NFA parser. While this still has issues because
Haskell allows general recursion, at least `many` in specific is no longer
dependent on infinite structures.

There's another interesting point to be made, however, regarding compatibility
with NFAs. Even though this recursive encoding doesn't allow us to create an
*explicit* NFA (a full graph with nodes and transitions), it does allow us to
make an *implicit* one. We can't ever make an *explicit* NFA involving `many`
because the naive `many` in the normal `Alt` gives us an infinite data
structure, so we get an infinite graph.

However, our implementation of `matchPrefix` is actually an *implicit* NFA in
the GHC runtime, where the 'states' can be considered function pointers on the
heap. These pointers refer to other pointers, and the overall behavior works
like an unoptimized NFA under the hood that's assembled as we go. This
circumvents the infinite data structure problem because GHC Haskell internally
implements recursion by cycles in pointer structures.

## Wrapping Up

Let's wrap things up! As a cherry on top, we can write our final function to
find matches anywhere inside a string by using `tails` (which gives us all
prefixes in a string) and `mapMaybe` (which maps `matchPrefix` on every prefix
and keeps the successes). It's also useful to write a function to get the
*first* successful match, using `listToMaybe`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs#L78-L82

matches :: RegExp a -> String -> [a]
matches re = mapMaybe (matchPrefix re) . tails

firstMatch :: RegExp a -> String -> Maybe a
firstMatch re = listToMaybe . matches re
```

This is pretty efficient, due to how `matchPrefix` short-circuits with `Nothing`
as soon as it fails, and how `listToMaybe` short-circuits as soon as it finds a
`Just`.

Hopefully from this, you can see the value of free structures :)

-   Given some base primitive, they give you exactly the structure you need ---
    no more, no less.
-   They let you work with your type safely, and then unsafely "run" it inside
    different useful contexts.
-   They are normalizing, so you are not allowed to make "illegal" distinctions.
    This eliminates a class of bugs where you accidentally treat two cases
    differently when they are meant to be treated the same way.

Our journey from going to regular languages to `Alt Prim` was to recognize that
the structures involved in regular expressions matched an `Alternative`
interface "plus" some extra primitives --- and then shifting our perspective to
enriching a primitive with `Alternative` energy.

Where can we go from here? First, try playing around with the [sample
code](https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs).
One easy addition would be to add other types of primitives:

``` haskell
data Prim a =
    Only Char a                 -- ^ match a char with a given result
  | Letter a                    -- ^ match any letter with the same result
  | Digit    (Int  -> a)        -- ^ match any digit, with a result computed from it
  | Wildcard (Char -> a)        -- ^ match any char, with a computed result
  | Satisfy (Char -> Maybe a)   -- ^ match potentially any char, based on a function
```

so we can support a lot of the basic character classes that many implementations
of regular expressions support. Try this out in the [sample
code](https://github.com/mstksg/inCode/tree/master/code-samples/misc/regexp.hs)
as an exercise!

One fun thing you can do also is to use our regexp type to generate a string
that it would match on. Try doing this both in the `runAlt`-based method and
also the explicit pattern matching method!

Another interesting direction we can take, along the lines of [build systems a
la
carte](https://www.microsoft.com/en-us/research/publication/build-systems-la-carte/),
is experimenting with different free structures to give rise to different types
of languages/expressions. For example, if we use the free *Applicative*, we get
a language that has only concatenation and empty strings and primitives, and no
alternations. It's like regular expressions with no `|`, or basically only
straight up matches. If we use the free *Monad*, we get a context-sensitive
language with no backtracking. If we use the free *MonadPlus*, we get a
context-sensitive language with backtracking. And if we use the (redundant) free
*Functor*...we get a language that can parse a string of one and only one
character. It's nice that we get this sort of "a la carte" scaling system by our
choice of free structure.

I hope that after working through this example, you will begin to start
recognizing opportunities for using free structures everywhere you look! Once
you start, it's hard to stop :)

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporters at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Sam Stites and Josh Vera!
:)

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

[^1]: A caveat exists here for `many`. More on this later!

[^2]: Note that there are some caveats that should be noted here, due to
    laziness in Haskell. We will go deeper into this later.

