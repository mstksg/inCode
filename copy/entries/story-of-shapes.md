---
title: "Functors to Monads: A Story of Shapes"
categories: Haskell
tags: functional programming, haskell, interpreters, functors, functor combinators, monads
create-time: 2024/11/02 15:41:35
date: 2024/11/04 11:44:50
identifier: story-of-shapes
slug: functors-to-monads-a-story-of-shapes
---

For many years now I've been using a mental model and intuition that has guided
me well for understanding and teaching and using functors, applicatives,
monads, and other related Haskell abstractions, as well as for approaching
learning new ones. Sometimes when teaching Haskell I talk about this concept
and assume everyone already has heard it, but I realize that it's something
universal yet easy to miss depending on how you're learning it. So, here it is:
how I understand the Functor and other related abstractions and free
constructions in Haskell.

The crux is this: instead of thinking about what `fmap` changes, ask: what does
`fmap` keep *constant*?

This isn't a rigorous understanding and isn't going to explain *every* aspect
about *every* Functor, and will probably only be useful if you already know a
little bit about Functors in Haskell. But it's a nice intuition trick that has
yet to majorly mislead me.

The Secret of Functors
----------------------

First of all, *what is a Functor*? A capital-F Functor, that is, the Haskell
typeclass and abstraction. Ask a random Haskeller on the street and they'll
tell you that it's something that can be "mapped over", like a list or an
optional. Maybe some of those random Haskellers will feel compelled to mention
that this mapping should follow some laws...they might even list the laws. Ask
them why these laws are so important and maybe you'll spend a bit of time on
this rhetorical street of Haskellers before finding one confident enough to
give an answer.

So I'm going to make a bit of a tautological leap: a *Functor* gives you a way
to "map over" values in a way that *preserves shape*. And what is "shape"? A
shape is *the thing that fmap preserves*.

The Functor typeclass is simple enough: for `Functor f`, you have a function
`fmap :: (a -> b) -> f a -> f b`, along with `fmap id = id` and `fmap f . fmap
g = fmap (f . g)`. Cute things you can drop into quickcheck to prove for your
instance, but it seems like those laws are hiding some sort of deeper,
fundamental truth.

The more Functors you learn about, the more you see that `fmap` seems to always
preserve "something":

*   For lists, `fmap` preserves length and relative orderings.
*   For optionals (`Maybe`), `fmap` preserves _presence_ (the fact that
    something is there or not). It cannot flip a `Just` to a `Nothing` or vice
    versa.
*   For `Either e`, `fmap` preserves *the error* (if it exists) or the fact that
    it was succesful.
*   For `Map k`, `fmap` preserves *the keys*: which keys exist, how many there
    are, their relative orderings, etc.
*   For `IO`, `fmap` preserves *the IO effect*. Every bit of external I/O that
    an IO action represents is unchanged by an `fmap`, as well as exceptions.
*   For `Writer w` or `(,) w`, `fmap` preserves the "logged" `w` value, leaving it
    unchanged. Same for `Const w`.
*   For `Tree`, `fmap` preserves *the tree structure*: how many layers, how big
    they are, how deep they are, etc.
*   For `State s`, `fmap` preserves what happens to the input state `s`. How a
    `State s` transform a state value `s` is unchanged by `fmap`
*   For `ConduitT i o m` from *[conduit][]*, `fmap` preserves what the conduit
    pulls upstream and what it yields downstream. `fmap` will not cause the
    conduit to yield more or different objects, nor cause it to consume/pull
    more or less.
*   For parser-combinator `Parser`, `fmap` preserves what input is consumed or
    would fail to be consumed. `fmap` cannot change whether an input string
    would fail or succeed, and it cannot change how much it consumes.
*   For *[optparse-applicative][]* `Parser`s, `fmap` preserves the command line
    arguments available. It leaves the `--help` message of your program
    unchanged.

[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[conduit]: https://hackage.haskell.org/package/conduit

It seems like as soon as you define a `Functor` instance, or as soon as you
find out that some type has a `Functor` instance, it magically induces some
sort of ... "thing" that must be preserved.[^exceptions]  A *conserved quantity
must exist*. It reminds me a bit of [Noether's Theorem][noether] in Physics,
where any continuous symmetry "induces" a conserved quantity (like how
translation symmetry "causes" conservation of momentum). In Haskell, every
lawful `Functor` instance induces a conserved quantity. I don't know if there
is a canonical name for this conserved quantity, but I like to call it "shape".

[noether]: https://en.wikipedia.org/wiki/Noether%27s_theorem

[^exceptions]: There are *some* exceptions, especially degenerate cases like
`Writer ()` aka `Identity` which add no meaningful structure. So for these this
mental model isn't that useful.

A Story of Shapes
-----------------

The word "shape" is chosen to be as devoid of external baggage/meaning as
possible while still having *some*. The word isn't important as much as saying
that there is *some* "thing" preserved by `fmap`, and not exactly the nature of
that "thing". The *nature* of that thing changes a lot from Functor to Functor,
where we might better call it an "effect" or a "structure" specifically, but
that *some* "thing" exists is almost universal.

Of course, the value if this "thing" having a canonical name at all is
debatable. I were to coin a completely new term I might call it a "conserved
charge" or "gauge" in allusion to physics. But the most useful name probably
would be shape.

For some `Functor` instances, the word shape is more literal than others. For
trees, for instance, you have the literal shape of the tree preserved. For
lists, the "length" could be considered a literal shape. `Map k`'s shape is
also fairly literal: it describes the structure of keys that exist in the map.
But for `Writer w` and `Const w`, shape can be interpreted as some information
outside of the values you are mapping that is left unchanged by mapping. For
`Maybe` and `Either e` shape also considers if there has been any
short-circuiting. For `State s` and `IO` and `Parser`, "shape" involves some
sort of side-computation or consumption that is left unchanged by `fmap`, often
called an effect. For *optparse-applicative*, "shape" involves some sort of
inspectable and observable static aspects of a program. "Shape" comes in all
forms.

But, this intuition of "looking for that conserved quantity" is very helpful
for learning *new* Functors. If you stumble onto a new type that you know is a
`Functor` instance, you can immediately ask "What *shape* is this `fmap`
preserving?", and it will almost always yield insight into that type.

This viewpoint also sheds insight onto why `Set.map` isn't a good candidate for
`fmap` for *[Data.Set][]*: What "thing" does `Set.map f` preserve? Not size,
for sure. In a hypothetical world where we had `ordfmap :: Ord b => (a -> b) ->
f a -> f b`, we would still need `Set.map` to preserve *something* for it to be
useful as an "Ord-restricted Functor".[^setmap]

[Data.Set]: https://hackage.haskell.org/package/containers/docs/Data-Set.html

[^setmap]: Incidentally, `Set.map` *does* preserve one thing: non-emptiness.
You can't `Set.map` an empty set into a non-empty one and vice versa. So, maybe
if we recontextualized `Set` as a "search for at least one result"
`Functor` or `Monad` where you could only ever observe a single value,
`Set.map` would work for Ord-restricted versions of those abstractions,
assuming lawful `Ord` instances.


### A Result

Before we move on, let's look at another related and vague concept that is
commonly used when discussing functors: `fmap` is a way to map a function that
*preserves the shape* and *changes the result*.

If *shape* is the thing that is *preserved* by `fmap`, *result* is the thing
that is *changed* by it. `fmap` cleanly splits the two.

Interestingly, most introduction to Functors begin with describing functor
values as having a result and `fmap` as the thing that changes it, in some way.
Ironically, though it's a more common term, it's by far the more vague and
hard-to-intuit concept.

For something like `Maybe`, "result" is easy enough: it's the value present if
it exists. For parser-combinator `Parser`s too it's relatively simple: the
"shape" is the input consumed but the "result" is the Haskell value you get as
a result of the consumption. For *optparse-applicative* parser, it's the actual
parsed command line arguments given by the user at runtime. But sometimes it's
more complicated: for the technical List functor, the "non-determinism"
functor, the "shape" is the number of options to choose from and the order you
get them in, and the "result" (to use precise semantics) is the
non-deterministic choice that you eventually pick or iterate over.

So, the "result" can become a bit confusing to generalize. So, in my mind, I
usually reduce the definitions to:

*   *Shape*: the "thing" that `fmap` preserves: the `f` in `f a`
*   *Result*: the "thing" that `fmap` changes: the `a` in `f a`

With this you could "derive" the Functor laws:

*   `fmap id == id`: `fmap` leaves the shape unchanged, `id` leaves the result
    unchanged. So entire thing must remain unchanged!
*   `fmap f . fmap g == fmap (f . g)`. In both cases the shape remains
    unchanged, but one changes the result by f after g, and the other changes
    the result by `f . g`. They must be the same transformation!

All neat and clean, right? So, maybe the big misdirection is focusing too much
on the "result" when learning Functors, when we *should* really be focusing
more on the "shape", or at least the two together.

Once you internalize "`Functor` gives you shape-preservation", this helps you
understand the value of the other common typeclass abstractions in Haskell as
well, and how they function based on how they manipulate "shape" and "result".

Traversable
-----------

For example, what does the `Traversable` typeclass give us?  Well, if `Functor`
gives us a way to map *pure* functions and preserve shape, then `Traversable`
gives us a way to map *effectful* functions and preserve shape.

Whenever someone asks me about my favorite `Traversable` instance, I always say
it's the `Map k` traversable:

```haskell
traverse :: Applicative f => (a -> f b) -> Map k a -> f (Map k b)
```

Notice how it has no constraints on `k`?  Amazing isn't it?  `Map k b` lets
us map an `(a -> f b)` over the values at each key in a map, and collects the
results under the key the `a` was originally under.

In essence, you can be assured that the result map has the *same keys* as the
original map, perfectly preserving the "shape" of the map. The `Map k`
instance is the epitome of beautiful `Traversable` instances. We can recognize
this by identifying the "shape" that `traverse` is forced to preserve.

Applicative
-----------

What does the `Applicative` typeclass give us? It has `ap` and
`pure`, but [its laws][Applicative] are infamously difficult to understand.

[Applicative]: https://hackage.haskell.org/package/base/docs/Control-Applicative.html#t:Applicative

But, look at `liftA2 (,)`:

```haskell
liftA2 (,) :: Applicative f => f a -> f b -> f (a, b)
```

It lets us take "two things" and *combine their shapes*. And, more importantly,
it combines the shapes *without considering the results*.

*   For `Writer w`, `<*>` lets us combine the two logged values using
    `mappend` while ignoring the actual `a`/`b` results.
*   For list, `<*>` (the cartesian product) lets us multiply the lengths of the
    input lists together. The *length* of the new list ignores the actual
    *contents* of the list.
*   For `State s`, `<*>` lets you *compose* the `s -> s` state functions
    together, ignoring the `a`/`b`s
*   For `Parser`, `<*>` lets you sequence input consumption in a way that
    doesn't depend on the actual values you parse: it's "context-free" in a
    sense, aside from [some caveats][contextfree].
*   For *optparse-applicative*, `<*>` lets you combine your command line
    argument specs together, without depending on the actual values provided at
    runtime by the caller.

[contextfree]: https://byorgey.wordpress.com/2012/01/05/parsing-context-sensitive-languages-with-applicative/

The key takeaway is that the "final shape" *only depends* on the input shapes,
and not the results. You can know the length of `<*>`-ing two lists together
with only knowing the length of the input lists, and you can also know the
relative ordering of inputs to outputs. Within the specific context of the
semantics of `IO`, you can know what "effect" `<*>`-ing two IO actions would
produce only knowing the effects of the input IO actions[^io]. You can know
what command line arguments `<*>`-ing two *optparse-applicative* parsers would
have only knowing the command line arguments in the input parsers. You can know
what strings `<*>`-ing two parser-combinator parsers would consume or reject,
based only on the consumption/rejection of the input parsers. You can know the
final log of `<*>`-ing two `Writer w a`s together by only knowing the logs of
the input writer actions.

[^io]: That is, if we take the sum consideration of all input-output with the
outside world, independent of what happens within the Haskell results, we can
say the combination of effects is deterministic.

And hey...some of these combinations feel "monoidal", don't they?

*   `Writer w` sequences using `mappend`
*   List lengths sequence by multiplication
*   `State s` functions sequence by composition

You can also imagine "no-op" actions:

*   `Writer w`'s no-op action would log `mempty`, the identity of `mappend`
*   List's no-op action would have a length 1, the identity of multiplication
*   `State s`'s no-op action would be `id`, the identity of function
    composition

That might sound familiar --- these are all `pure` from the `Applicative`
typeclass!

So, the Applicative typeclass laws aren't that mysterious at all. If you
understand the "shape" that a Functor induces, `Applicative` gives you a
*monoid* on that shape! This is why `Applicative` is often called the
"higher-kinded" `Monoid`.

This intuition takes you pretty far, I believe. Look at the examples above
where we clearly identify specific `Applicative` instances with specific
`Monoid` instances (`Monoid w`, `Monoid (Product Int)`, `Monoid (Endo s)`).

Put in code:

```haskell
-- A part of list's shape is its length and the monoid is (*, 1)
length (xs <*> ys) == length xs * length ys
length (pure r) == 1

-- Maybe's shape is isJust and the monoid is (&&, True)
isJust (mx <*> my) == isJust mx && isJust my
isJust (pure r) = True

-- State's shape is execState and the monoid is (flip (.), id)
execState (sx <*> sy) == execState sy . execState sx
execState (pure r) == id

-- Writer's shape is execWriter and the monoid is (<>, mempty)
execWriter (wx <*> wy) == execWriter wx <> execWriter wy
execWriter (pure r) == mempty
```

We can also extend this to non-standard `Applicative` instances: the `ZipList`
newtype wrapper gives us an `Applicative` instance for lists where `<*>` is
`zipWith`. These two have the same `Functor` instances, so their "shape"
(length) is the same. And for both the normal `Applicative` and the `ZipList`
`Applicative`, you can know the length of the result based on the lengths of
the input, but `ZipList` combines shapes using the `Min` monoid, instead of the
`Product` monoid. And the identity of `Min` is positive infinity, so `pure` for
`ZipList` is an infinite list.

```haskell
-- A part of ZipList's shape is length and its monoid is (min, infinity)
length (xs <*> ys) == length xs `min` length ys
length (pure r) == infinity
```

The "know-the-shape-without-knowing-the-results" property is actually leveraged
by many libraries. It's how *optparse-applicative* can give you `--help`
output: the *shape* of the optparse-applicative parser (the command line
arguments list) can be computed *without knowing the results* (the actual
arguments themselves at runtime). You can list out what arguments are expecting
without ever getting any input from the user.

This is also leveraged by the *[async][]* library to give us the `Concurrently`
`Applicative` instance. Normally `<*>` for IO gives us sequential combination
of IO effects. But, `<*>` for `Concurrently` gives us *parallel* combination of
IO effects. We can launch all of the IO effects in parallel at the same time
because *we know what the IO effects are* before we actually have to execute
them to get the results. If we needed to know the results, this wouldn't be
possible.

[async]: https://hackage.haskell.org/package/async

This also gives some insight into the [`Backwards` Applicative
wrapper][backwards] --- because the shape of the final does not depend on the
*result* of either, we are free to combine the shapes in whatever order we
want. In the same way that every monoid gives rise to [a "backwards"
monoid][dual]:

[backwards]: https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Applicative-Backwards.html
[dual]: https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Monoid.html#t:Dual

```haskell
ghci> "hello" <> "world"
"helloworld"
ghci> getDual $ Dual "hello" <> Dual "world"
"worldhello"
```

Every `Applicative` gives rise to a "backwards" `Applicative` that does the
shape "mappending" in reverse order:

```haskell
ghci> putStrLn "hello" *> putStrLn "world"
hello
world
ghci> forwards $ Backwards (putStrLn "hello") *> Backwards (putStrLn "world")
world
hello
```

The monoidal nature of Applicative with regards to shapes and effects is the
heart of the original intent, and I've discussed this [in earlier blog
posts][const].

[const]: https://blog.jle.im/entry/const-applicative-and-monoids.html

Alternative
-----------

The main function of the *[Alternative][]* typeclass is `<|>`:

[Alternative]: https://hackage.haskell.org/package/base-4.20.0.1/docs/Control-Applicative.html#g:2

```haskell
(<|>) :: Alternative f => f a -> f a -> f a
```

At first this might look a lot like `<*>` or `liftA2 (,)`

```haskell
liftA2 (,) :: Applicative f => f a -> f b -> f (a, b)
```

Both of them take two `f a` values and squish them into a single one. Both of
these are also monoidal on the shape, independent of the result. They have a
*different* monoidal action on `<|>` than as `<*>`:

```haskell
-- A part of list's shape is its length:
-- the Ap monoid is (*, 1), the Alt monoid is (+, 0)
length (xs <*> ys) == length xs * length ys
length (pure r) == 1
length (xs <|> ys) == length xs + length ys
length empty == 0

-- Maybe's shape is isJust:
-- The Ap monoid is (&&, True), the Alt monoid is (||, False)
isJust (mx <*> my) == isJust mx && isJust my
isJust (pure r) = True
isJust (mx <|> my) == isJust mx || isJust my
isJust empty = False
```

If we understand that functors have a "shape", `Applicative` implies that the
shapes are monoidal, and `Alternative` implies that the shapes are a
"double-monoid". The exact nature of how the two monoids relate to each other,
however, is not universally agreed upon. For many instances, however, it does
happen to form a [semiring][], where `empty` "annihilates" via `empty <*> x ==
empty`, and `<*>` distributes over `<|>` like `x <*> (y <|> z) == (x <*> y) <|>
(x <*> z)`. But this is not universal.

[semiring]: https://en.wikipedia.org/wiki/Semiring

However, what does `Alternative` bring to our shape/result dichotomy that
`Applicative` did not? Notice the subtle difference between the two:

```haskell
liftA2 (,) :: Applicative f => f a -> f b -> f (a, b)
(<|>) :: Alternative f => f a -> f a -> f a
```

For `Applicative`, the "result" comes from the results of both inputs. For
`Alternative`, the "result" could come from one or the other input. So, this
introduces a fundamental data dependency for the *results*:

*   Applicative: Shapes merge monoidally independent of the results, but to get
    the result of the final, you need to produce the results of both of the two
    inputs in the general case.
*   Alternative: Shapes merge monoidally independent of the results, but to get
    the result of the final, you need the results of one or the other input in
    the general case.

This also implies that choice of combination method for shapes in `Applicative`
vs `Alternative` aren't arbitrary: the former has to be "conjoint" in a sense,
and the latter has to be "disjoint".

See again that clearly separating the shape and the result gives us the
vocabulary to say precisely what the different data dependencies are.

Monad
-----

Understanding shapes and results also help us appreciate more the sheer *power*
that Monad gives us. Look at `>>=`:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Using `>>=` means that the shape of the final action is allowed to *depend on
the result* of the first action!  We are no longer in the
Applicative/Alternative world where shape only depends on shape.

Now we can write things like:

```haskell
greet = do
  putStrLn "What is your name?"
  n <- getLine
  putStrLn ("Hello, " ++ n ++ "!")
```

Remember that for "IO", the shape is the IO effects (In this case, what exactly
gets sent to the terminal) and the "result" is the haskell value computed from
the execution of that IO effect. In our case, the *action* of the result (what
values are printed) depends on the *result* of of the intermediate actions (the
`getLine`). You can no longer know in advance what action the program will have
without actually running it and getting the results.

The same thing happens when you start sequencing parser-combinator parsers: you
can't know what counts as a valid parse or how much a parser will consume until
you actually start parsing and getting your intermediate parse results.

`Monad` is also what makes `guard` and co. useful. Consider the purely
Applicative:

```haskell
evenProducts :: [Int] -> [Int] -> [Bool]
evenProducts xs ys = (\x y -> even (x * y)) <$> xs <*> ys
```

If you passed in a list of 100 items and a list of 200 items, you can know that
the result has 100 * 200 = 20000 items, without actually knowing any of the
items in the list.

But, consider an alternative formulation where we are allowed to use Monad
operations:

```haskell
evenProducts :: [Int] -> [Int] -> [(Int, Int)]
evenProducts xs ys = do
  x <- xs
  y <- ys
  guard (even (x * y))
  pure (x, y)
```

Now, *even if you knew* the lengths of the input lists, you can *not* know the
length of the output list without actually knowing what's inside your lists.
You need to actually start "sampling".

That's why there is no `Monad` instance for `Backwards` or
*optparse-applicative* parsers. For `Backwards` doesn't work because we've now
introduced an asymmetry (the `m b` depends on the `a` of the `m a`) that can't
be reversed. For *optparse-applicative*, it's because we want to be able to
inspect the shape without knowing the results at runtime (so we can show a
useful `--help` without getting any actual arguments): but, with `Monad`, we
can't know the shape without knowing the results!

In a way, Monad simply "is" the way to combine `Functor` shapes together where
the final shape is allowed to depend on the results. Hah, I tricked you into
reading a monad tutorial!

Free Structures
---------------

I definitely write [way too much about free structures][freeblog] on this blog.
But this "shapeful" way of thinking also gives rise to why free structures are
so compelling and interesting to work with in Haskell.

[freeblog]: https://blog.jle.im/entry/functor-combinatorpedia.html

Before, we were describing shapes of Functors and Applicatives and Monads that
already existed. We had *this* `Functor`, what was *its* shape?

However, what if we had a shape that we had in mind, and wanted to *create* an
`Applicative` or `Monad` that manipulated that shape?

For example, let's roll our own version of *optparse-applicative* that only
supported `--myflag somestring` options. We could say that the "shape" is the
list of supported option and parsers. So a single element of this shape would be
the specification of a single option:

```haskell
data Option a = Option { optionName :: String, optionParse :: String -> Maybe a }
  deriving Functor
```

The "shape" here is the name and also what values it would parse, essentially.
`fmap` won't affect the name of the option and won't affect what would succeed
or fail.

Now, to create a full-fledged multi-argument parser, we can use [`Ap` from the
*free* library][ap]:

[ap]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html

```haskell
type Parser = Ap Option
```

We specified the shape we wanted, now we get the `Applicative` of that shape
for free! We can now combine our shapes monoidally using the `<*>` instance,
and then use `runAp_` to inspect it:

```haskell
data Args = Args { myStringOpt :: String, myIntOpt :: Int }

parseTwo :: Parser args
parseTwo = Args <$> liftAp stringOpt <*> liftAp intOpt
  where
    stringOpt = Option "string-opt" Just
    intOpt = Option "int-opt" readMaybe

getAllOptions :: Parser a -> [String]
getAllOptions = runAp_ (\o -> [optionName o])
```

```haskell
ghci> getAllOptions parseTwo
["string-opt", "int-opt"]
```

Remember that `Applicative` is like a "monoid" for shapes, so `Ap` gives you a
free "monoid" on your custom shape: you can now create list-like "sequences" of
your shape that merge via concatenation through `<*>`. You can also know that
`fmap` on `Ap Option` will not add or remove options: it'll leave the actual
options unchanged. It'll also not affect what options would fail or succeed to
parse.

You could also write a parser combinator library this way too! Remember that
the "shape" of a parser combinator `Parser` is the string that it consumes or
rejects. The single element might be a parser that consumes and rejects a
single `Char`:


```haskell
newtype Single a = Single { satisfies :: Char -> Maybe a }
  deriving Functor
```

The "shape" is whether or not it consumes or rejects a char. Notice that `fmap`
for this cannot *change* whether or not a char is rejected or accepted: it can
only change the Haskell result `a` value. `fmap` can't flip the `Maybe` into a
`Just` or `Nothing`.

Now we can create a full monadic parser combinator library by using [`Free`
from the *free* library][free]:

[free]: https://hackage.haskell.org/package/free/docs/Control-Monad-Free.html

```haskell
type Parser = Free Single
```

Again, we specified the shape we wanted, and now we have a Monad for that
shape!  For more information on using this, I've written [a blog post in the
past][regex]. `Ap` gives you a free "monoid" on your shapes, but in a way
`Free` gives you a "tree" for your shapes, where the sequence of shapes
depends on which way you go down their results. And, again, `fmap` won't
ever change what would or would not be parsed.

[regex]: https://blog.jle.im/entry/free-alternative-regexp.html

How do we know what free structure to pick? Well, we ask questions about what
we want to be able to do with our shape. If we want to inspect the shape
without knowing the results, we'd use the free Applicative or free Alternative.
As discussed earlier, using the free Applicative means that our final result
must require producing all of the input results, but using the free Alternative
means it doesn't. If we wanted to allow the shape to depend on the results
(like for a context-sensitive parser), we'd use the free Monad. Understanding
the concept of the "shape" makes this choice very intuitive.

The Shape of You
----------------

Next time you encounter a new Functor, I hope these insights can be useful. Ask
yourself, what is `fmap` preserving? What is `fmap` changing? And from there,
its secrets will unfold before you. [Emmy Noether][noether] would be proud.

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
