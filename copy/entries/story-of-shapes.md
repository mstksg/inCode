---
title: "Functors to Monads: A Story of Shapes"
categories: Haskell
tags: functional programming, haskell, interpreters, functors, functor combinators, monads
create-time: 2024/11/02 15:41:35
identifier: story-of-shapes
slug: functors-to-monads-a-story-of-shapes
---

For many years now I've been using a mental model and intuition that has guided
me well for understanding and teaching and using functors, applicatives,
monads, and other related Haskell abstractions, as well as for approaching
learning new ones. I have sprinkled this into many of my previous posts but I
haven't really made one that explained it in its purest form. Sometimes when
teaching Haskell I talk about this concept and assume everyone already has
heard it, but I realize that it's something universal yet easy to miss
depending on how you're learning it. So, here it is: how I understand the
Functor and other related abstractions and free constructions in Haskell.

This isn't a rigorous understanding and isn't going to explain *every* aspect
about *every* Functor, and will probably only be useful if you already know a
little bit about Functors in Haskell. But it's a nice intuition trick that has
yet to majorly mislead me.

The Secret of Functors
----------------------

First of all, *what is a Functor*? A capital-F Functor, that is, the Haskell
typeclass and abstraction. Ask a random Haskeller on the street and they'll
tell you that it's something that can be "mapped over", like a list or an
optional. Maybe some of those random Haskellers on the street will feel
compelled to mention that this mapping should follow some laws...they might
even list the laws. Ask them why these laws are so important and maybe you'll
spend a bit of time on this rhetorical street of Haskellers before finding one
confident enough to give an answer.

So I'm going to make a bit of a tautological leap: a *Functor* gives you a way
to "map over" values in a way that *preserves shape*. And what is "shape"? A
shape is *the thing that fmap preserves*.[^structure]

[^structure]: This concept is also sometimes called "structure". I'm not going
to argue about which is better; I just use "shape" because it carries slightly
less semantic baggage to me.

The Functor typeclass is simple enough: for `Functor f`, you have a function
`fmap :: (a -> b) -> f a -> f b`, along with `fmap id = id` and `fmap f . fmap
g = fmap (f . g)`. Cute things you can drop into quickcheck to prove for your
instance, but it seems like those laws are hiding some sort of deeper,
fundamental truth.

The more Functors you learn about, the more you see that `fmap` seems to always
preserve "something":

*   For lists, `fmap` preserves length
*   For optionals (`Maybe`), `fmap` preserves _presence_ (the fact that
    something is there or not)
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
*   For parser-combinator `Parser`, `fmap` preserves what input is consumed or
    would fail to be consumed.
*   For [*optparse-applicative*][optparse-applicative] `Parser`s, `fmap` preserves the command line
    arguments available. It leaves the `--help` message of your program
    unchanged.

[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative

It seems like as soon as you define a `Functor` instance, or as soon as you
find out that some type has a `Functor` instance, it magically induces some
sort of ... "thing" that must be preserved.[^reader]  A *conserved quantity
must exist*. It reminds me a bit of [Noether's Theorem][noether] in Physics,
where any continuous symmetry "induces" a conserved quantity (like how
translation symmetry "causes" conservation of momentum). In Haskell, every
lawful `Functor` instance induces a conserved quantity. I don't know if there
is a canonical name for this conserved quantity, but I like to call it "shape".

[noether]: https://en.wikipedia.org/wiki/Noether%27s_theorem
[^reader]: There are *some* exceptions, like `Reader r`, or some degenerate
cases like `Writer ()` aka `Identity` which add no meaningful structure. So for
these this mental model isn't that useful.

A Story of Shapes
-----------------

The word "shape" is chosen to be as devoid of external baggage/meaning as
possible. The point isn't that we want to describe a literal "shape" as much
as we want to say that there is *some* "thing" preserved by `fmap`, and not
exactly the nature of that "thing". The *nature* of that thing changes a
lot from Functor to Functor, but that *some* "thing" exists is almost
universal.

For some `Functor` instances, the shape is more literal than others. For
trees, for instance, you have the literal shape of the tree preserved. For
lists, the "length" could be considered a literal shape. `Map k`'s shape is
also fairly literal: it describes what keys exist in the map. But for `Writer
w` and `Const w`, shape can be interpreted as some information outside of the
values you are mapping that is left unchanged by mapping. For `Maybe` and
`Either e` shape also considers if there has been any short-circuiting. For
`State s` and `IO` and `Parser`, "shape" involves some sort of side-computation
or consumption that is left unchanged by `fmap`. For *optparse-applicative*,
"shape" involves some sort of inspectable and observable static aspects of a
program. "Shape" comes in all forms.

But, this intuition of "looking for that conserved quantity" is very helpful
for learning *new* Functors. If you stumble onto a new type that you know is a
`Functor` instance, you can immediately ask "What *shape* is this `fmap`
preserving?", and it will almost always yield insight into that type.

### An Unfortunate Result

Before we move on, let's look at another related vague term that is commonly
used when discussing functors: `fmap` is a way to map a function that
*preserves the shape* and *changes the result*.

If *shape* is the thing that is *preserved* by `fmap`, *result* is the thing
that is *changed* by it. `fmap` cleanly splits the two.

Interestingly, most introduction to Functors begin with describing actions as
having a "result". Ironically, though it's a more common term, it's by far the
more vague and hard-to-intuit concept. We usually learning Functors in terms of
how they change the *results*, but I'd argue that this is the more confusing
path.

For something like `Maybe`, "result" is easy enough: it's the value present if
it exists. For parser-combinator `Parser`s too it's relatively simple: the
"shape" is the input consumed but the "result" is the Haskell value you get as
a result of the consumption. For *optparse-applicative* parser, it's the actual
parsed command line arguments given by the user at runtime. But for the List
Functor, the "shape" is the *number* of items, and the "result" is the
"non-deterministic choice" from your list: while `fmap` doesn't change how many
possible choices you can pick from, it changes the value that you do eventually
pick. But day to day, you can think of it as the actual values inside each cell
of a list.

Even for a Functor as "simple" as List, the "result" becomes pretty
ill-defined as a concept. So, in my mind, I usually reduce the definitions to:

*   *Shape*: the "thing" that `fmap` preserves
*   *Result*: the "thing" that `fmap` changes

Neat and clean, right? So, maybe the big misdirection is focusing too much on
the "result" when learning Functors, when we *should* really be focusing more
on the "shape".

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
    sense.
*   For *optparse-applicative*, `<*>` lets you combine your command line
    argument specs together, without depending on the actual values provided at
    runtime by the caller.

The key takeaway is that the "final shape" *only depends* on the input shapes,
and not the results. You can know the length of `<*>`-ing two lists together
with only knowing the length of the input lists. You can know what effect
`<*>`-ing two IO actions would produce only knowing the effects of the input IO
actions. You can know what command line arguments `<*>`-ing two
*optparse-applicative* parsers would have only knowing the command line
arguments in the input parsers. You can know what strings `<*>`-ing two
parser-combinator parsers would consume or reject, based only on the
consumption/rejection of the input parsers. You can know the final log of
`<*>`-ing two `Writer w a`s together by only knowing the logs of the input
writer actions.

And hey...some of these combinations feel "monoidal", don't they?

*   `Writer w` sequences using `mappend`
*   List lengths sequence by multiplication
*   `State s` functions sequence by composition

You can also imagine "no-op" actions:

*   `Writer w`'s no-op action would log `mempty`, the identity of `mappend`
*   List's no-op action would have a length 1, the identity of multiplication
*   `State s`'s no-op action would be `id`, the identity of function
    composition

Hey, wait a minute...that sounds familiar! That's just `pure` from the
`Applicative` typeclass!

So, the Applicative typeclass laws aren't that mysterious at all. If you
understand the "shape" that a Functor induces, `Applicative` gives you a
*monoid* on that shape! This is why `Applicative` is often called the
"higher-kinded" `Monoid`.

This intuition takes you pretty far, I believe. Look at the examples above
where we clearly identify specific `Applicative` instances with specific
`Monoid` instances (`Monoid w`, `Monoid (Product Int)`, `Monoid (Endo s)`).

We can also extend this to non-standard `Applicative` instances: the `ZipList`
newtype wrapper gives us an `Applicative` instance for lists where `<*>` is
`zipWith`. These two have the same `Functor` instances, so their "shape"
(length) is the same. And for both the normal `Applicative` and the `ZipList`
`Applicative`, you can know the length of the result based on the lengths of
the input: `length (xs <*> ys) == length xs * length ys`, `length (ZipList xs
<*> ZipList ys) == min (length xs) (length ys)`. `ZipList` combines shapes
using the `Min` monoid, instead of the `Product` monoid. And the identity of
`Min` is positive infinity, so `pure` for `ZipList` is an infinite list.

The "know-the-shape-without-knowing-the-results" property is actually leveraged
by many libraries. It's how *optparse-applicative* can give you `--help`
output: the *shape* of the optparse-applicative parser (the command line
arguments list) can be computed *without knowing the results* (the actual
arguments themselves at runtime). You can list out what arguments are expecting
without ever getting any input from the user.

This is also leveraged by the [*async*][async] library to give us the `Concurrently`
`Applicative` instance. Normally `<*>` for IO gives us sequential combination
of IO effects. But, `<*>` for `Concurrently` gives us *parallel* combination of
IO effects. This is only possible because we can launch all of the IO effects in
parallel at the same time, because *we know what the IO effects are* before we
actually have to execute them to get the results. If we needed to know the
results, this wouldn't be possible.

[async]: https://hackage.haskell.org/package/async

This also gives some insight into the [`Backwards` Applicative
wrapper][backwards] --- because the shape of the final does not depend on the
*result* of either, we are free to combine the shapes in whatever order we
want. It's how every monoid gives rise to a "backwards" monoid:

[backwards]: https://hackage.haskell.org/package/transformers-0.6.1.2/docs/Control-Applicative-Backwards.html

```haskell
ghci> "hello" <> "world"
"helloworld"
ghci> getDual $ Dual "hello" <> Dual "world"
"worldhello"
```

Every `Applicative` gives rise to a "backwards" `Applicative` that does the
"mappending" in reverse order:

```haskell
ghci> putStrLn "hello" *> putStrLn "world"
hello
world
ghci> forwards $ Backwards (putStrLn "hello") *> Backwards (putStrLn "world")
world
hello
```

Monad
-----

Understanding shapes and results also help us appreciate more the sheer *power*
that Monad gives us. Look at `>>=`:

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

Using `>>=` means that the shape of the final action is allowed to *depend on
the result* of the first action!

Now we can write things like:

```haskell
greet = do
  putStrLn "What is your name?"
  n <- getLine
  putStrLn ("Hello, " ++ n ++ "!")
```

Remember that for "IO", the shape is the IO effects (ie, what gets sent to the
terminal) and the "result" is the haskell value computed from the execution of
that IO effect. In our case, the *action* of the result (what values are
printed) depends on the *result* of of the intermediate actions (the
`getLine`). You can no longer know in advance what action the program will have
without actually running it and getting the results.

`Monad` is also what makes `guard` and co. useful. Consider the purely
Applicative:

```haskell
evenProducts :: [Bool]
evenProducts xs ys = (\x y -> even (x * y)) <$> xs <*> ys
```

If you passed in a list of 100 items and a list of 200 items, you can know that
the result has 100 * 200 = 20000 items, without actually knowing any of the
items in the list.

But, consider an alternative formulation:

```haskell
evenProducts :: [Int] -> [Int] -> [(Int, Int)]
evenProducts xs ys = do
  x <- xs
  y <- ys
  guard (even (x * y))
  pure (x, y)
```

Now, *even if you knew* the lengths of the input lists, you cannot know the
length of the output list without actually knowing what's inside your lists.
You need to start "sampling".

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

I definitely write [way too much about free structures][free] on this blog. But
this "shapeful" way of thinking also gives rise to why free structures are so
compelling and interesting to work with in Haskell.

[free]: https://blog.jle.im/entry/functor-combinatorpedia.html

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

The "shape" here is the name, essentially.

Now, to create a full-fledged multi-argument parser, we can use [`Ap` from the
*free* library][ap]:

[ap]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html

```haskell
type Parser = Ap Option
```

We specified the shape we wanted, now we get the `Applicative` of that shape
for free! We can now combine our shapes monoidally using the `<*>` instance, and then use
`runAp_` to inspect it:

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
your shape that merge via concatenation through `<*>`.

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
depends on which way you go down their results.

[regex]: https://blog.jle.im/entry/free-alternative-regexp.html

How do we know what free structure to pick? Well, we ask questions about what
we want to be able to do with our shape. If we want to inspect the shape
without knowing the results, we'd use the free Applicative. If we wanted to
allow the shape to depend on the results (like for a context-sensitive parser),
we'd use the free Monad. Understanding the concept of the "shape" makes this
choice very intuitive.

The Shape of You
----------------

Next time you encounter a new Functor, I hope these insights can be useful. Ask
yourself, what is `fmap` preserving? What is `fmap` changing? And from there,
its secrets will unfold before you. [Emmy Noether][noether] would be proud.

