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

To warm up, let's talk about serializing networks: writing them to binary and
re-reading them.

### Binary

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

!!![randomNet]:dependent-haskell/NetworkUntyped.hs "randomNet ::"

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

We can go "backwards" too by using `withSingI :: Sing a -> (SingI a => r) -> r`:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "sillyGetNet ::"
~~~

`withSingI` takes a `Sing a` and a "thing you can evaluate if only you had a
`SingI a` instance available", and evaluates it.  We can only evaluate `get ::
(KnownNat i, SingI hs, KnownNat o) => Get (Network i hs o)` if we have that
`SingI` instance, so we can pass it into `withSingI` as the second argument,
and, voilà --- we get that `Get (Network i hs o)` right out.



<!-- At the Boundary -->
<!-- --------------- -->

<!-- There's a sort of mode of thinking that comes with -->

<!-- You can see in the last post a definite demarcation of two "worlds": the world -->
<!-- of "untyped", non-dependently typed programming, and the world of typed, -->
<!-- dependently typed programming. -->

<!-- So far we've worked completely -->

<!-- When I first heard about types that depend on runtime values, my mind -->
<!-- immediately jumped to the idea of dynamic types ... which is, of course, the -->
<!-- thing that all Haskellers are indoctrinated to hate from day 1.  But -->








<!-- sameNat and existentials -->
