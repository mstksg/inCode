---
title: "Practical Dependent Types in Haskell: Type-Safe Neural Networks (Part 2)"
categories: Haskell, Ramblings
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, numerical, haskell, singletons, types, linear algebra, artificial neural networks, machine learning, existential types
create-time: 2016/05/27 22:20:42
date: Never
identifier: dependent-haskell-2
slug: practical-dependent-types-in-haskell-2
---

We're back to continue on [our journey][series] in using practical dependent
types to write type-safe neural networks!  In [Part 1][], we wrote things out
in normal, untyped Haskell, and looked at red flags and general design
principles that nudged us in the direction of adding dependent types to our
program.  We learned to appreciate what dependent types offered in terms of
guiding us in writing our code, helping the compiler check our correctness,
providing a better interface for users, and more.

[series]: https://blog.jle.im/entries/series/+practical-dependent-types-in-haskell.html
[Part 1]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html

We also learned how to use singletons to work around some of Haskell's
fundamental limitations to let us "pattern match" on the structure of types,
and how to use typeclasses to generate singletons reflecting the structure of
types we are dealing with.

(If you read [Part 1][] *before* the singletons section was re-written to use
the [singletons][] library, [here's a link to the section][new-section] in
specific.  This tutorial will assume familiarity with what is discussed there!)

[singletons]: https://hackage.haskell.org/package/singletons
[new-section]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html#singletons-and-induction

All of what we've dealt with so far has essentially been with types that are
fixed at compile-time.  All the networks we've made have had "static" types,
with their sizes in their types indicated directly in the source code.

Today, we're going to dive into the world of types that *depend* on factors
unknown until runtime, and see how dependent types in a strongly typed language
like Haskell helps us write safer, more correct, and more maintainable code.
Along the way, we'll encounter and learn first-hand about techniques and
guiding high-level principles that we can apply to our other dependently typed
coding endeavours.

Serializing Networks
--------------------

Just to warm up, let's talk about serializing networks: writing them to binary
and re-reading them.

### Recap on the Binary Library

Serializing networks of *known* size --- whose sizes are statically in their
types --- is pretty straightforward.  I'm going to be using the *[binary][]*
library, which offers a very standard typeclass-based approach for serializing and
deserializing data.  There are a lot of tutorials online (and I even [wrote a small
one][huffman] myself a few years ago), but a very high-level view is that the
library offers a typeclass for describing serialization schemes for different
types.

[binary]: https://hackage.haskell.org/package/binary
[huffman]: https://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-2-binary.html

For example, for serializing lists, you have:

~~~haskell
putList :: Binary a => [a] -> Put
putList []     =
    put False           -- signal the end
putList (x:xs) = do
    put True            -- signal a cons
    put x
    putList xs
~~~

Where `put :: Binary a => a -> Put` is a polymorphic way of describing
serialization of things with a `Binary` instance --- every type provides its
own `put`.  Sequencing `Put`s using do notation is saying "put this, then
that, then that".

We can deserialize by describing a monadic `Get` plan:

~~~haskell
getList :: Binary a => Get [a]
getList = do
    isCons <- get
    if isCons
      then do
        x  <- get
        xs <- getList
        return (x:xs)
      else
        return []
~~~

And `get :: Binary a => Get a` is a polymorphic way of describing a strategy of
deserializing things with a `Binary` instance --- again, every type provides
its own `get`.  Sequencing `Get`s is again simply "do-this-then-that".

We can write our own instances for our own types by providing `put` and `get`
together:

~~~haskell
instance Binary a => Binary [a] where
    put = putList
    get = getList
~~~

In practice, we usually don't write our own instances from scratch, and use
GHC's generics features to give us instances for free:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "data Weights" "instance (KnownNat i, KnownNat o) => Binary (Weights i o)"
~~~

For simple types like `Weights`, which simply "contain" serializable things,
the *binary* is smart enough to write your instances automatically!

### `Binary` for `Network`

Writing `putNet` and `getNet` is pretty nice because the entire structure is
already known ahead of time, and we don't need to do any tricks with flags like
for lists.

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "putNet ::"
~~~

Even simpler than for lists!  If it's an `O w`, just serialize the `w`.  If
it's a `w :&~ net`, serialize the `w` then the rest of the `net`.  The reason
we can get away without any flags is because we already *know* how many `:&~`
layers to expect *just from the type*.  If we want to deserialize/load a
`Network 5 '[10,6,3] 2`, we *know* we want three `(:&~)`'s and one `O` --- no
need for dynamically sized networks like we had to handle for lists.

We'll write `getNet` the similarly to how wrote [`randomNet`][randomNet] from
the last post:

!!![randomNet]:dependent-haskell/NetworkTyped.hs "randomNet ::"

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "getNet ::"
~~~

Now would be a good time to refresh on the [singletons section][new-section] of
my last post again.  To deserialize a `Network i hs o`, we have to "pattern
match" on `hs` to see what constructor we are expecting to deserialize, and we
do that by using singletons with constructors we *can* literally pattern match
on (`SNil` and `SCons`), which tell GHC what type to expect through "dependent
pattern matching".

If you see a `SNil :: Sing '[]`, that means that `hs` is `'[]`, so expect a `O`
constructor.  If you see `SCons s ss :: Sing (h ': hs)`, that means that `hs`
is `h ': h'`, so expect a `(:&~)` constructor.  (If this is a little confusing
still, try re-reading the [singleton section][new-section] from the last post
again for a more thorough description!)

Note that here we decide to implement `getNet` by asking for an explicit
singleton input (`Sing hs ->`) instead of an implicit one (`SingI hs =>`) like
we did for `randomNet`.  Remember that the two methods are technically
equivalent, really, and compile to the same thing at runtime.  We need one or
the other because of type erasure --- so either we pass in `Sing hs`, or
provide a `SingI hs` constraint so that we can use `sing :: Sing hs` to
construct the `Sing hs`.

There's a trade-off either way, and it can be a bit annoying because switching
between different modes can potentially be diverse.  For the most part, always
try to *take explicit singleton arguments* where you can, especially for
internal functions.  The simple reason is because in Haskell, we like to really
only do typeclass-level programming as a last, last resort.  Typeclasses in
Haskell are very magical and not really first-class in a satisfying way.
Normal values (like singletons) *are* first-class and easily passed.

Explicit singleton arguments can sometimes pose a burden for the caller, so my
personal approach is to *always* use explicit `Sing a` whenever possible for
*internal functions*, and to expose a `SingI a =>` interface for *user-facing
functions* (including for typeclass instances)

So, we're going to write our `Binary` instance for `Network`.  Of course, we
can't have `put` or `get` take a `Sing hs` (that'd change the arity/type of the
function), so what we can do is have their `Binary` instances require a `SingI
hs` constraint, essentially doing the same thing:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where"
~~~

To go from "`SingI` world" to "`Sing` world", we use `sing` to generate the
explicit `Sing hs` from `SingI hs =>`.

<!-- #### `Sing` to `SingI` -->

<!-- As a quick aside, note that we can go "backwards" too by using `withSingI :: -->
<!-- Sing a -> (SingI a => r) -> r`: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "sillyGetNet ::" -->
<!-- ~~~ -->

<!-- `withSingI` takes a `Sing a` and a "thing you can evaluate if only you had a -->
<!-- `SingI a` instance available", and evaluates it.  We can only evaluate `get :: -->
<!-- (KnownNat i, SingI hs, KnownNat o) => Get (Network i hs o)` if we have that -->
<!-- `SingI` instance, so we can pass it into `withSingI` as the second argument, -->
<!-- and, voilà --- we get that `Get (Network i hs o)` right out. -->

Existential Crisis
------------------

Now, having the entire structure of your neural network in the type is nice and
all for cool tricks like `randomNet`...but do you *really* want to work with
this directly?  After all, from the user's perspective, the user really only
ever needs to know `i` and `o`: What vectors the network *expects*, and what
vectors the network *outputs*.  In the end, all a (feed-forward) Neural Network
really is is an abstraction over a function `R i -> R o`.

Remember, the main benefits of having the entire structure in the type was to
help us *implement* our functions more safely, with the compiler's help, and
also for cute return type polymorphism tricks like `randomNet` and `getNet`.
The *first* type of benefit really doesn't benefit the *user* of the network.
So let's talk about a way to "abstract" away the internal structure of hidden
nodes from the type, and maybe even have it depend on runtime values!

The big key to "hiding" parts of types and letting them depend on runtime
values is called the *existential type*.  Existential types are sort of the
"opposite" of the normal polymorphic (universally quantified) types you
normally see in Haskell.

For a function like

~~~haskell
map :: (a -> b) -> [a] -> [b]
~~~

`a` and `b` are universally quantified, which means that the person who *uses*
`map` gets to *decide* what `a` and `b` are.  To be more explicit, that type
signature can be written as:

~~~haskell
map :: forall a b. (a -> b) -> [a] -> [b]
~~~

For a universally quantified type, the *caller* gets to decide what is
what...and the function has to *adapt* to handle it.

For the *existentially* quantified type, the *function* gets to decide what is
what, and the *caller* has to adapt to handle it.

There are two main ways to work with existential types in Haskell, and we'll
go over both of them now and talk about their relative strengths and weaknesses
and what situations to use either of them in.  It pays to be aware of both!

### Existential Data Type

Arguably the more natural way in Haskell to work with existential types is to
wrap them in a data type:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "data OpaqueNet ::"
~~~

So, if you have `net :: Network 6 '[10,6,3] 2`, you can create
`ONet sing net :: OpaqueNet 6 2`.  When you use the `ONet` constructor, the
structure of the hidden layers disappears from the type!

How do we use this type?  When we *pattern match* on `ONet`, we get the
singleton and the net back!

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "numHiddens ::"
~~~

Note that it's important for us to stuff in the singleton in addition to the
network itself, because of type erasure.  If we didn't pop the singleton in,
there'd be no way for us to recover the original `hs`!  (Note that we could
have had `ONet :: SingI hs => Network i hs o -> OpaqueNet i o`, which is
essentially the same thing)

Once you *do* pattern match on `ONet`, you have to handle the `hs` in a
*completely polymorphic way*.  You're not allowed to assume anything about
`hs`...you have to provide a completely parametrically polymorphic way of
dealing with it!

Note that this function is completely not ok:

~~~haskell
bad :: OpaqueNet i o -> Network i hs o
bad = \case ONet _ n -> n
~~~

Why not?  Because a type signature like `OpaqueNet i o -> Network i hs o`
means that the *caller* can decide what `hs` can be --- just like `read :: Read
a => String -> a`, where the caller decides what `a` is.

Of course, this *isn't* the case with the way we've written the function...the
function only returns a *specific* `hs` that the *function* decides.  The
*caller* has to accommodate whatever is inside `ONet`.

#### Binary

Now, let's find out a way to serialize this type!  Now, the structure of our
network is *not* known in the type, so we do have to plant flags in our data
somehow.  We need to store a witness to the structure of the network, as well.

To do this, we can move `hs` from the type level to the value level.  In
Haskell, this is called **reflection**.  The *singletons* library provides the
`fromSing` function for this purpose:

~~~haskell
ghci> fromSing (sing :: Sing '[1,2,3])
[1,2,3]
ghci> fromSing (sing :: Sing '[True, False])
[True, False]
~~~

And with that, we can write a serializer for `OpaqueNet`:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "putONet ::"
~~~

Put the structure (as a flag), and then put the network itself.

Now, to deserialize, we want to *load* the list of `Integer`s and move that
*back* into the type level.  In Haskell, this is called **reification**, the
dual of reflection.

The *singletons* library provides the `toSing` function, which returns a
`SomeSing` (an existentially quantified `Sing` wrapped in a constructor that we
can pattern match on):

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "getONet ::"
~~~

We first `get` the `[Integer]`, then *reify* the list of integers into the type
level by getting our `ss :: Sing hs`.  Then we `getNet ss`, remembering that
`getNet` takes a singleton to figure out what structure to get.  Then we wrap
it all up in the `ONet` constructor.

Phew!  We load our flag, reify it, and once we're back in the typed land again,
we can do our normal business!

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o)"
~~~

#### The Boundary

Did you notice what we just did there?  The *type* of `Sing hs` was
*dynamically generated* based on the *value* of the `[Integer]` we load at
runtime.  We just worked with types that *depended* on runtime values.

With the power of existentially quantified types (like in `SomeSing`), we
essentially gained the ability to work with types that depend on runtime
results.

In a way, you can consider the `toSing` and the `SomeSing` as our "boundary"
between the "untyped world" and the "typed world".  This layer (and the process
of reification) cleanly separates the two.

This "boundary" can be thought of as a lot like the boundary we talk about
between "pure" functions and values and "impure" (IO, etc.) ones.  We say to
always write as much of your program as possible in the "pure" world, and
to separate and pull out as much logic as you can to be pure logic.  That's
sort of one of the first things you learn about as a Haskell programmer: how
to separate logic that *can* be pure from logic that is "impure" (IO, etc.),
and then "combine them" at the very end, as late as possible.

Well, if the final program is going to be IO in the end anyway, why bother
separating out pure and impure parts of your logic?  Separation of concerns,
the increased ability to reason with your code and analyze what it does, the
compiler's ability to check what you write, the limitation of implementations,
and etc. are all reasons any Haskeller should be familiar with reciting.

You can think of the general philosophy of working with typed/untyped worlds as
being the same thing.  You can write as much of your program as possible in the
"typed" world, like we did in Part 1.  Take advantage of the increased ability
to reason with your code, parametric polymorphism helping you *write* your
code, limit your implementations, nab you compiler help, etc.  All of those are
benefits of working in the typed world.

Then, write what you must in your "untyped" world, such as dealing with values
that pop up at runtime like the `[Integer]` above.

Finally, at the end, *unite* them at the boundary.  Pass the control football
from the untyped world to the typed world!

#### Reifying for Fun and Profit

Before we move on, let's see a more direct example of reification: creating
networks with variable internal structure based on runtime input, like
configuration files.

Maybe you're reading the internal size from a configuration file, or maybe you
want it to be determined by user input.  Our old `randomNet` won't work:

~~~haskell
randomNet :: (MonadRandom m, SingI hs)
          => m (Network i hs o)
~~~

Because we need a static type signature to use it directly.  But, we can return
an `OpaqueNet`!

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "randomONet ::"
~~~

Note that the implementation is slightly awkward because we had the lack of
foresight to implement `randomNet` using `SingI hs =>` instead of `Sing hs ->`,
so we have to "go from `Sing` to `SingI`".

You go from the `SingI` style to the `Sing` style with `sing`, like we saw
earlier, and you can go backwards with `singInstance`:

~~~haskell
singInstance :: Sing a -> SingInstance a
~~~

Where the data type `SingInstance` has a single constructor that's a lot like
`SNat` from the last part:

~~~haskell
data SingInstance a where
    SingInstance :: SingI a => SingInstance a
~~~

Basically, it's impossible to *use* the `SingInstance` constructor unless you
have a `SingI a` instance in scope, so if you ever *pattern match* on it, it's
a "proof" that `SingI a` exists.  It's the same as how, for the constructor
`SNat :: KnownNat n => Sing n`, if you pattern match on `SNat` and see that
it's not something silly like `undefined`/`error`/bottom, GHC knows that there
is a `KnownNat n` instance.  It's sort of like pattern matching out the
instance itself.

Now you can get from your untyped world into the world of dependent types ---

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "main ::"
~~~

### Continuation-Based Existentials

There's another way in Haskell that we work with existential types that can be
more natural and easy to work with in a lot of cases.

Note that when we pattern match on an existential data type, you have to work
with the values in the constructor in a parametrically polymorphic way.  For
example, if we had:

~~~haskell
toONet :: OpaqueNet i o -> Foo
toONet = \case ONet s n -> f s n
~~~

What does the type of `f` have to be?  It has to take a `Sing hs` and a
`Network i hs o`, but deal with it in a way that works *for all* `hs`.  It has
to be:

~~~haskell
f :: forall (hs :: [Nat]). Sing hs -> Network i hs o -> Foo
~~~

That is, it can't be written for *only* `Sing '[5]` or *only* `Sing
'[6,3]`...it has to work for *any* `hs`.

Well, we could really also just skip the data type together and represent an
existential type as something *taking* the continuation `f` and giving it what
it needs.

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "type OpaqueNet'"
~~~

This "continuation transformation" is known as formally
**skolemization**.[^skolemization]

[^skolemization]: Skolemization is probably one of the coolest words you'll
encounter working with dependent types in Haskell, and sometimes just knowing
that you're "skolemizing" something makes you feel cooler.  Thank you [Thoralf
Skolem][].  If you ever see a "rigid, skolem" error in GHC, you can thank him
for that too!

[Thoralf Skolem]: https://en.wikipedia.org/wiki/Thoralf_Skolem

We can "wrap" a `Network i hs o` into an `OpaqueNet' i o r` pretty
straightforwardly:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "oNet' ::"
~~~

To prove that the two `OpaqueNet`s are the same (and to help us see more about
how they relate), we can write functions that convert back and forth from them:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "withONet ::" "toONet ::"
~~~

Note that `withONet` is *really*:

~~~haskell
withONet :: OpaqueNet i o
         -> (forall hs. Sing hs -> Network i hs o -> r)
         -> r
~~~

Which you can sort of interpret as, "do *this function* on the existentially
quantified contents of an `OpaqueNet`."

#### Trying it out

To sort of compare how the two methods look like in practice, we're going to
Rosetta stone it up and re-implement serialization with the continuation-based
existentials:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "putONet' ::" "getONet' ::"
~~~

To be cute, I used the skolemized partners of `toSing` and `SomeSing`:

~~~haskell
withSomeSing :: [Integer]
             -> (forall (hs :: [Nat]). Sing hs -> r)
             -> r
~~~

Instead of returning a `SomeSing` like `toSing` does, `withSomeSing` returns
the continuation-based existential.

I expanded out the type signature of `getONet'`, because you'll see the
explicit form more often.  It's:

~~~haskell
getONet' :: (forall hs. Sing hs -> Network i hs o -> Get r)
         -> Get r
~~~

Which basically says "Give what you would do if you *had* a `Sing hs` and a
`Network i hs o`", and I'll get them for you and give you the result."

And let's also see how we'd return a random network with a continuation:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "withRandomONet' :"
~~~

Again, to be cute, I used the continuation-based version of `singInstance`,
`withSingI`:

~~~haskell
withSingI :: Sing a -> (SingI a => r) -> r
~~~

The signature, in English, is "give me a `Sing a` and a value that you could
make *if only you had* a `SingI` instance, and I'll give you that value as if
you had the instance, magically!"

Of course, we know it's not magic:[^magic]

~~~haskell
withSingI :: Sing a -> (SingI a => r) -> r
withSingI s x = case singInstance s of
                  SingInstance -> x
~~~

[^magic]: Actually, it kind of *is* magic, because `singInstance` is implemented
with `unsafeCoerce`.  But don't tell anyone ;)

And we see another way we can "move past the untyped/typed boundary":

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "main' :"
~~~






<!-- sameNat and existentials -->
