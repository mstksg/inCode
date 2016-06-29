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

Run-time Types
--------------

Recall the type we had for our neural networks:

~~~haskell
ghci> :k Network
Network :: Nat -> [Nat] -> Nat -> *
~~~

They're of the form `Network i hs o`, where `i` is the size of the input vector
it expects, `hs` is the list of hidden layer sizes, and `o` is the size of the
output vector it produces.  Something of type `Network 10 '[6, 4] 3` is a
network with 10 input nodes, two input layers of size 6 and 4, and 3 output
nodes.

This is great and all, but there's an apparent severe limitation to this:
Haskell is a statically typed language, right?  So doesn't this mean that
using a network requires that you know the entire structure of the network at
compile-time?

It's conceivable that you might be able to have the input and output sizes
known at compile-time, but it's probably likely that you *don't* know the what
you want your hidden layer structure to be in advance.  You might want to load
it from a configuration file, or have it depend on user input.  But can a type
really depend on things that you can't know until runtime?

To illustrate more clearly:

~~~haskell
main :: IO ()
main = do
    putStrLn "What hidden layer structure do you want?"
    hs  <- readLn        :: IO [Integer]
    net <- randomNetwork :: IO 10 ??? 3   -- what is ???
    -- ...?
~~~

You would *want* to put `hs` there where `???` is, but...`???` has to be a type
(of kind `[Nat]`).  `hs` is a value (of type `[Integer]`).  It's clear here
that the *type* of our network depends on something we can't write down or
decide until runtime.

### An Existential Crisis

There are a couple of ways to go about this, actually --- we'll go through
them, and we'll also see at the end how they are all really fundamentally the
same thing.

#### Types hiding behind constructors

Now, having the entire structure of your neural network in the type is nice and
all for cool tricks like `randomNet`...but do you *really* want to work with
this directly?  After all, from the user's perspective, the user really only
ever needs to know `i` and `o`: What vectors the network *expects*, and what
vectors the network *outputs*.  In the end, all a (feed-forward) Neural Network
really is is an abstraction over a function `R i -> R o`.

Remember, the main benefits of having the entire structure in the type was to
help us *implement* our functions more safely, with the compiler's help, and
also for cute return type polymorphism tricks like `randomNet` and `getNet`.
The first type of benefit really doesn't benefit the *user* of the network.

Imagine that we had written a `Network` type that *didn't* have the internal
structure in the type ---

~~~haskell
data OpaqueNet i o
~~~

Recall that our issue earlier was that we had to write `Network i ??? o`,
but we had no idea what to put in for `???`.  But, what if we worked with an
`OpaqueNet i o`, we wouldn't even care!  We wouldn't have to tell GHC what the
internal structure is.

We can implement it as an "existential" wrapper over `Network`, actually:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "data OpaqueNet ::"
~~~

So, if you have `net :: Network 6 '[10,6,3] 2`, you can create
`ONet sing net :: OpaqueNet 6 2`.  When you use the `ONet` constructor, the
structure of the hidden layers disappears from the type!

How do we use this type?  We *pattern match* on `ONet` to get the singleton and
the net back, and we can use them:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "numHiddens ::"
~~~

With the `ScopedTypeVariables` extension, we can even bring `hs` back into
scope, as in `ONet (ss :: Sing hs) _ ->`.

Note that it's important for us to include the singleton in addition to the
network itself, because of type erasure (our best friend).  If we didn't pop
the singleton in, we'd have to do some work to recover the original `hs`.

Another way we could have counter-acted type erasure would be to have:

~~~haskell
data OpaqueNet :: * -> * where
    ONet :: SingI hs => Network i hs o -> OpaqueNet i o
~~~

Like we learned last time, the `Sing hs ->` and `SingI hs =>` styles are just
two ways of doing the same thing.

This pattern is known as the **dependent pair**: pair a representation of `hs`
with some type that includes `hs`, and pattern match on the `Sing hs` to reveal
both!

And here's the key to making this all work: once you *do* pattern match
on `ONet`, you have to handle the `hs` in a *completely polymorphic way*.
You're not allowed to assume anything about `hs`...you have to provide a
completely parametrically polymorphic way of dealing with it!

For example, this function is completely *not* ok:

~~~haskell
bad :: OpaqueNet i o -> Network i hs o
bad = \case ONet _ n -> n          -- nope, not ok at all.
~~~

Why not?  Well, a type signature like `OpaqueNet i o -> Network i hs o` means
that the *caller* can decide what `hs` can be --- just like `read :: Read a =>
String -> a`, where the caller decides what `a` is.

Of course, this *isn't* the case with the way we've written the function...the
function only returns a *specific* `hs` that the *function* decides.  The
*caller* has to accommodate whatever is inside `ONet`.

#### The Universal and the Existential

We just brushed here on something at the heart of using existential types in
Haskell: the issue of who has the power to decide what the types will be
instantiated as.

Most polymorphic functions you work with in Haskell are "universally
qualified".  For example, for a function like

~~~haskell
map :: (a -> b) -> [a] -> [b]
~~~

`a` and `b` are universally quantified, which means that the person who *uses*
`map` gets to decide what `a` and `b` are.  To be more explicit, that type
signature can be written as:

~~~haskell
map :: forall a b. (a -> b) -> [a] -> [b]
~~~

This means that `map` is defined in a way that will work for *any* `a` and `b`
that the *caller* wants.  As a caller, you can request:

~~~haskell
map :: (Int -> Bool)    -> [Int]    -> [Bool]
map :: (Double -> Void) -> [Double] -> [Void]
map :: (String -> (Bool -> Char)) -> [String] -> [Bool -> Char]
~~~

Consequentially, the function has to be implemented in a way that will work for *any* `a`
and `b`.  The function's implementation has the burden of being flexible enough
to handle whatever the caller asks for.

But, for a function like:

~~~haskell
foo :: [Int] -> OpaqueNet i o
~~~

While the caller can choose what `i` and `o` are, the *function* gets to
choose what `hs` (in the hidden `Network i hs o`) is.

If I want to *use* the thing that `foo` returns...then *I* have to be flexible.
*I* have the burden of being flexible enough to handle whatever the *function*
returns.

In summary:

*   For universally quantified types, the *caller* chooses the type being
    instanced, and the *function's implementation* has to accommodate any
    choice.

*   For existentially quantified types, the *function's implementation* chooses
    the type being instanced, and the *caller* has to accommodate any choice.

Indeed, we saw earlier that if we ever wanted to *use* the `Network i hs o`
inside the `OpaqueNet i o`, we were forced to deal with it in a parametrically
polymorphic way.  We had to be able to handle *any* `hs` that the `ONet` could
throw at us!

#### A familiar friend

I called `OpaqueNet i o` a "dependent pair" earlier, which existentially
quantifies over `hs`.  But there's another common term for it: a **dependent
sum**.

People familiar with Haskell might recognize that "sum types" are `Either`-like
types that can be one thing or another.  Sum types are one of the first things
you learn about in Haskell --- heck, even `Maybe a` is the sum of `a` and `()`.

Dependent pairs/existential types actually are very similar to `Either`/sum
types, in spirit, and it might help to see the parallel so that you can see
that they're nothing scary, and that the fundamentals/intuition of working with
existential types in Haskell is no different than working with `Either`!

If I had:

~~~haskell
foo :: String -> Either Int Bool
~~~

I have to handle the result...but I have to handle it for both the case where I
get an `Int` and the case where I get a `Bool`.  The *function* gets to pick
what type I have to handle (`Int` or `Bool`), and *I* have to adapt to whatever
it returns.  Sound familiar?

In fact, you can even imagine that `OpaqueNet i o` is implemented like a fancy
`Either`:

~~~haskell
type OpaqueNet i o = Either (Network i '[] o) (
                       Either (Network i '[1] o) (
                         Either (Network i '[1,1] o) (
                           Either (Network i '[2] o) (
                             Either (Network i '[2,1] o) (
                               Either (Network i '[2,2] o) (
                                 -- .. literally infinitely ..
                               )
                             )
                           )
                         )
                       )
                     )
~~~

In other words, an infinite sum of all of the different combinations of hidden
layer structures.

And, remember that the basic way of handling an `Either` you get and figuring
out what the type of the value is inside is by *pattern matching* on it.  You
can't know if an `Either Int Bool` contains an `Int` or `Bool` until you
pattern match.  But, once you do, all is revealed.

For `OpaqueNet i o`, it's the same!  You don't know what the actual type of the
`Network i hs o` it contains is until you *pattern match* on the `Sing hs`! (Or
potentially, the network itself)  But, once you pattern match on it, all is
revealed.

### Reification

Let's pull it all together!

For simplicity, let's re-write `randomNet` the more sensible way --- with the
explicit singleton input style:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "randomNet' ::" "randomNet ::"
~~~

Remember earlier that I recommend (personally, and subjectively) a style where
your external API functions are implemented in `SingI a =>` style, and your
internal ones in `Sing a ->` style.  This lets all of your internal functions
fit together more nicely (`Sing a ->` style tends to be easier to write in,
especially if you stay in it the entire time) while at the same time removing
the burden of calling with explicit singletons from people using the
functionality externally.[^style]

[^style]: This is a completely personal style, and I can't claim to speak for
all of the Haskell dependent typing community.  In fact, I'm not even sure that
you could even say that there is a consensus at all.  But this is the style
that has worked personally for me in both writing and using libraries!  And
hey, some libraries I've seen in the wild even offer *both* styles in their
external API.

Now, we still need to somehow get our list of integers to the type level
somehow, so we can create a `Network i hs o` to stuff into our `ONet`.  And for
that, the *singletons* library offers the proper tooling.  It gives us
`SomeSing`, which is a lot like our `OpaqueNet` above, wrapping the `Sing a`
inside an existential data constructor.  `toSing` takes the term-level value
(for us, an `[Integer]`) and returns a `SomeSing` wrapping the type-level value
(for us, a `[Nat]`).  When we pattern match on the `SomeSing` constructor, we
get `a` in scope!

In an ideal world, `SomeSing` would look like this:

~~~haskell
data SomeSing :: * -> * where
    SomeSing :: Sing (a :: k) -> SomeSing k
~~~

And you can have

~~~haskell
foo :: SomeSing Bool
foo = SomeSing STrue

bar :: SomeSing Nat
bar = SomeSing (SNat :: Sing 10)
~~~

But because *singletons* was implemented before the `TypeInType` extension in
GHC 8, it has to be implemented with clunky "Kind Proxies".  In a future
version of *singletons*, they'll be implemented this way.  But for now, the
usage is more or less identical.  It's just right now, in the current system,
`SomeSing STrue :: SomeSing (KProxy :: KProxy Bool)`, and `bar :: SomeSing
(KProxy :: KProxy Nat)`.[^somesing]

[^somesing]: Gross, right?  Hopefully some day this will be as far behind us as
that whole Monad/Functor debacle is now!

Pattern matching looks like:

~~~haskell
main :: IO ()
main = do
    putStrLn "How many cats do you own?"
    c <- readLn :: IO Integer
    case toSing c of
      SomeSing (SNat :: Sing n) -> -- ...
~~~

Now, inside the case statement branch (the `...`), we have *type* `n :: Nat` in
scope!  And by pattern matching on the `SNat` constructor, we also have a
`Knownnat n` instance (As discussed in [previous part][new-section)].  We now
have enough to write our `randomONet`:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "randomONet ::"
~~~

This process of bringing a term-level value into the type level is known in
Haskell as **reification**.  Witht his, our original goal is (finally) within
reach:

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "main ::"
~~~

#### The Boundary

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
separating out pure and impure parts of your logic?  You gain separation of
concerns, the increased ability to reason with your code and analyze what it
does, the compiler's ability to check what you write, the limitation of
implementations, and etc. are all reasons any Haskeller should be familiar with
reciting.

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

### Continuation-Based Existentials

There's another way in Haskell that we work with existential types that can be
more natural and easy to work with in a lot of cases.

Note that when we pattern match on an existential data type, you have to work
with the values in the constructor in a parametrically polymorphic way.  For
example, if we had:

~~~haskell
oNetToFoo :: OpaqueNet i o -> Foo
oNetToFoo = \case ONet s n -> f s n
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

"Tell me how you would make an `r` if you had a `Sing hs` and a `Network i hs
o`, and I'll make it for you!"

This "continuation transformation" is known as formally
**skolemization**.[^skolemization]  We can "wrap" a `Network i hs o` into an
`OpaqueNet' i o r` pretty straightforwardly:

[^skolemization]: Skolemization is probably one of the coolest words you'll
encounter working with dependent types in Haskell, and sometimes just knowing
that you're "skolemizing" something makes you feel cooler.  Thank you [Thoralf
Skolem][].  If you ever see a "rigid, skolem" error in GHC, you can thank him
for that too!  He also inspired me to decide to name my first son
Thoralf.[^curry]

[Thoralf Skolem]: https://en.wikipedia.org/wiki/Thoralf_Skolem

[^curry]: My second son's name will be Curry.

~~~haskell
!!!dependent-haskell/NetworkTyped2.hs "oNet' ::"
~~~

<!-- To prove that the two `OpaqueNet`s are the same (and to help us see more about -->
<!-- how they relate), we can write functions that convert back and forth from them: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "withONet ::" "toONet ::" -->
<!-- ~~~ -->

<!-- Note the expanded type signature of `withONet`, which you can sort of interpret -->
<!-- as, "do *this function* on the existentially quantified contents of an -->
<!-- `OpaqueNet`." -->

<!-- #### Trying it out -->

<!-- To sort of compare how the two methods look like in practice, we're going to -->
<!-- Rosetta stone it up and re-implement serialization with the continuation-based -->
<!-- existentials: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "putONet' ::" "getONet' ::" -->
<!-- ~~~ -->

<!-- To be cute, I used the skolemized partners of `toSing` and `SomeSing`: -->

<!-- ~~~haskell -->
<!-- withSomeSing :: [Integer] -->
<!--              -> (forall (hs :: [Nat]). Sing hs -> r) -->
<!--              -> r -->
<!-- ~~~ -->

<!-- Instead of returning a `SomeSing` like `toSing` does, `withSomeSing` returns -->
<!-- the continuation-based existential. -->

<!-- The expanded type signature of `getONet'` can be read: "Give what you would do -->
<!-- if you *had* a `Sing hs` and a `Network i hs o`", and I'll get them for you and -->
<!-- give you the result." -->

<!-- Let's also see how we'd return a random network with a continuation: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "withRandomONet' :" -->
<!-- ~~~ -->

<!-- Again, to be cute, I used the continuation-based version of `singInstance`, -->
<!-- `withSingI`: -->

<!-- ~~~haskell -->
<!-- withSingI :: Sing a -> (SingI a => r) -> r -->
<!-- ~~~ -->

<!-- The signature, in English, is "give me a `Sing a` and a value that you could -->
<!-- make *if only you had* a `SingI` instance, and I'll give you that value as if -->
<!-- you had the instance, magically!" -->

<!-- Of course, we know it's not magic:[^magic] -->

<!-- ~~~haskell -->
<!-- withSingI :: Sing a -> (SingI a => r) -> r -->
<!-- withSingI s x = case singInstance s of -->
<!--                   SingInstance -> x -->
<!-- ~~~ -->

<!-- [^magic]: Actually, it kind of *is* magic, because `singInstance` is implemented -->
<!-- with `unsafeCoerce`.  But don't tell anyone ;) -->

<!-- And we see another way we can "move past the untyped/typed boundary": -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "main' ::" -->
<!-- ~~~ -->



<!-- ### A Story of Parametricity -->

<!-- Let's look at the type of `randomNet` again: -->

<!-- ~~~haskell -->
<!-- randomNet :: m (Network i hs o) -->
<!--              -- plus some constraints yadda yadda -->
<!-- ~~~ -->

<!-- To get more insight onto what the type is really saying, let's see the same -->
<!-- signature with the explicit forall's: -->

<!-- ~~~haskell -->
<!-- randomNet :: forall m i hs o. m (Network i hs o) -->
<!--              -- plus some constraints yadda yadda -->
<!-- ~~~ -->

<!-- In English, this means that `randomNet` lets you produce a `Network i hs o` for -->
<!-- *any* `i` you want, for *any* `hs` you want, for *any* `o` you want.  It's -->
<!-- implemented in a *parametrically polymorphic way*, meaning that it's -->
<!-- implemented in a uniform way to handle *any* `i`, `hs`, or `o` the caller could -->
<!-- possibly ask for. (Provided that they satisfy the typeclass constraints, of -->
<!-- course) -->

<!-- We have some other functions we defined in the last part, as well, that are -->
<!-- defined in such a parametrically polymorphic way: -->

<!-- ~~~haskell -->
<!-- runNet :: forall i hs o. (KnownNat i, KnownNat o) -->
<!--        => Network i hs o -->
<!--        -> R i -->
<!--        -> R o -->

<!-- train  :: forall i hs o. (KnownNat i, KnownNat o) -->
<!--        => Double -->
<!--        -> R i -->
<!--        -> R o -->
<!--        -> Network i hs o -->
<!--        -> Network i hs o -->
<!-- ~~~ -->

<!-- `runNet` is written a way that it can work for *any* `i` or *any* `hs` or *any* -->
<!-- `o` you want or need (provided they satisfy the typeclass constraints).  It -->
<!-- takes a `Network i hs o` for any `i`, `hs`, or `o`, and an `R i` for the same -->
<!-- `i`, and returns an `R o` for the same `o`.  The way they are implemented does -->
<!-- not depend on the specific structure of the network. -->

<!-- Contrast this to: -->

<!-- ~~~haskell -->
<!-- foo :: Network 10 '[6,4] 3 -> R 3 -->
<!-- ~~~ -->

<!-- This is specifically implemented to *only* work on networks with 10 inputs, 6 -->
<!-- and 4-node hidden layers, and 3 outputs.  It *does* care about the structure of -->
<!-- the network you put in. -->

<!-- Now, the concept of "types that depend on run-time" seems to jive with this -->
<!-- concept.  If we want to write functions that work on types that depend on -->
<!-- run-time, it would make sense that they have to be able to anticipate and work -->
<!-- for *any* type that the run-time processes might demand. -->

<!-- And that's sort of the secret to working with "types that depend on run-time" -->
<!-- in Haskell --- you have to deal with them in a way that adapts to any type that -->
<!-- can come at you! -->

<!-- ### Reification -->

<!-- So we know that `randomNet` is such a function that works with *any* `hs` you -->
<!-- could possibly throw at it.  The last missing piece is a function can *run* -->
<!-- `randomNet` with an `hs` that depends on run-time factors. -->

<!-- Enter `withSomeSing`: -->

<!-- ~~~haskell -->
<!-- withSomeSing -->
<!--     :: [Integer] -->
<!--     -> (forall hs. Sing (hs :: [Nat]) -> r) -->
<!--     -> r -->
<!-- ~~~ -->

<!-- (I've specialized it there to work with `[Integer]` and `[Nat]`, but it works -->
<!-- in general with any singleton and the values it represents) -->

<!-- The type signature says "Give me an `[Integer]` and a function that is ready to -->
<!-- take *any* `hs`, and I'll run it with the `hs` from the `[Integer]` for you". -->

<!-- For example, if we had `foo :: Sing hs -> Int`, and we pass it to `withSomeSing -->
<!-- [1,2,3]`, it'll *run* `foo`, setting with `hs` to be `'[1,2,3]`. -->

<!-- We can re-write `randomNet` to take a `Sing hs ->` instead of a `SingI hs =>` -->
<!-- (remember, the two are just two ways of writing the same thing.  Re-read the -->
<!-- [singletons section][new-section] for a review!) to make things more clear: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "randomNet' ::" -->
<!-- ~~~ -->

<!-- And we can implement the concept we had before: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "main ::" -->
<!-- ~~~ -->

<!-- `someSing xs :: (forall hs. Sing hs -> r) -> r` is a function that's waiting -->
<!-- for a `Sing hs -> r` that works for *any* `hs`.  It'll "run" the function with -->
<!-- the `hs` corresponding to `xs :: [Integer]`. -->

<!-- In our case, we gave it a function `forall hs. Sing hs -> IO ()`, and it *runs* -->
<!-- that function *with* the `hs` corresponding to list we give it. -->

<!-- So, if we gave `withSomeSing` the list `[6,3]`, I'll *run* our action as if it -->
<!-- had `hs ~ '[6,3]`.  And remember --- because our function is parametrically -->
<!-- polymorphic to work with any `hs`, it'll happily accept it! -->


















<!-- It's conceivable that you might be able to have the input and output sizes -->
<!-- known at compile-time, but it's probably likely that you *don't* know the what -->
<!-- you want your hidden layer structure to be in advance.  You might want to load -->
<!-- it from a configuration file, or have it depend on user input.  But can a type -->
<!-- really depend on things that you can't know until runtime? -->




<!-- Having the entire structure of your neural network in the type is nice and -->
<!-- all for cool tricks like `randomNet`...but do you *really* want to work with -->
<!-- this directly?  After all, from the user's perspective, the user really only -->
<!-- ever needs to know `i` and `o`: What vectors the network *expects*, and what -->
<!-- vectors the network *outputs*.  In the end, all a (feed-forward) Neural Network -->
<!-- really is is an abstraction over a function `R i -> R o`. -->

<!-- Remember, the main benefits of having the entire structure in the type was to -->
<!-- help us *implement* our functions more safely, with the compiler's help, and -->
<!-- also for cute return type polymorphism tricks like `randomNet` and `getNet`. -->
<!-- The *first* type of benefit really doesn't benefit the *user* of the network. -->
<!-- So let's talk about a way to "abstract" away the internal structure of hidden -->
<!-- nodes from the type, and maybe even have it depend on runtime values! -->

<!-- Serializing Networks -->
<!-- -------------------- -->

<!-- Just to warm up, let's talk about serializing networks: writing them to binary -->
<!-- and re-reading them. -->

<!-- ### Recap on the Binary Library -->

<!-- Serializing networks of *known* size --- whose sizes are statically in their -->
<!-- types --- is pretty straightforward.  I'm going to be using the *[binary][]* -->
<!-- library, which offers a very standard typeclass-based approach for serializing and -->
<!-- deserializing data.  There are a lot of tutorials online (and I even [wrote a small -->
<!-- one][huffman] myself a few years ago), but a very high-level view is that the -->
<!-- library offers a typeclass for describing serialization schemes for different -->
<!-- types. -->

<!-- [binary]: https://hackage.haskell.org/package/binary -->
<!-- [huffman]: https://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-2-binary.html -->

<!-- For example, for serializing lists, you have: -->

<!-- ~~~haskell -->
<!-- putList :: Binary a => [a] -> Put -->
<!-- putList []     = -->
<!--     put False           -- signal the end -->
<!-- putList (x:xs) = do -->
<!--     put True            -- signal a cons -->
<!--     put x -->
<!--     putList xs -->
<!-- ~~~ -->

<!-- Where `put :: Binary a => a -> Put` is a polymorphic way of describing -->
<!-- serialization of things with a `Binary` instance --- every type provides its -->
<!-- own `put`.  Sequencing `Put`s using do notation is saying "put this, then -->
<!-- that, then that". -->

<!-- We can deserialize by describing a monadic `Get` plan: -->

<!-- ~~~haskell -->
<!-- getList :: Binary a => Get [a] -->
<!-- getList = do -->
<!--     isCons <- get -->
<!--     if isCons -->
<!--       then do -->
<!--         x  <- get -->
<!--         xs <- getList -->
<!--         return (x:xs) -->
<!--       else -->
<!--         return [] -->
<!-- ~~~ -->

<!-- And `get :: Binary a => Get a` is a polymorphic way of describing a strategy of -->
<!-- deserializing things with a `Binary` instance --- again, every type provides -->
<!-- its own `get`.  Sequencing `Get`s is again simply "do-this-then-that". -->

<!-- We can write our own instances for our own types by providing `put` and `get` -->
<!-- together: -->

<!-- ~~~haskell -->
<!-- instance Binary a => Binary [a] where -->
<!--     put = putList -->
<!--     get = getList -->
<!-- ~~~ -->

<!-- In practice, we usually don't write our own instances from scratch.  Instead, -->
<!-- we use GHC's generics features to give us instances for free: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "data Weights" "instance (KnownNat i, KnownNat o) => Binary (Weights i o)" -->
<!-- ~~~ -->

<!-- For simple types like `Weights`, which simply "contain" serializable things, -->
<!-- the *binary* library is smart enough to write your instances automatically for -->
<!-- you! -->

<!-- ### `Binary` for `Network` -->

<!-- Writing `putNet` and `getNet` to put/get `Network`s is pretty nice because the -->
<!-- entire structure is already known ahead of time, and we don't need to do any -->
<!-- tricks with flags like for lists. -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "putNet ::" -->
<!-- ~~~ -->

<!-- Even simpler than for lists!  If it's an `O w`, just serialize the `w`.  If -->
<!-- it's a `w :&~ net`, serialize the `w` then the rest of the `net`.  The reason -->
<!-- we can get away without any flags is because we already *know* how many `:&~` -->
<!-- layers to expect *just from the type*.  If we want to deserialize/load a -->
<!-- `Network 5 '[10,6,3] 2`, we *know* we want three `(:&~)`'s and one `O` --- no -->
<!-- need for dynamically sized networks like we had to handle for lists. -->

<!-- We'll write `getNet` similarly to how wrote [`randomNet`][randomNet] from the -->
<!-- last post: -->

<!-- !!![randomNet]:dependent-haskell/NetworkTyped.hs "randomNet ::" -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "getNet ::" -->
<!-- ~~~ -->

<!-- To deserialize a `Network i hs o`, we have to "pattern match" on `hs` to see -->
<!-- what constructor we are expecting to deserialize.  We use singletons (with -->
<!-- constructors we *can* literally pattern match on) to tell GHC what types we are -->
<!-- handling, in a process called "dependent pattern matching". -->

<!-- If you see a `SNil :: Sing '[]`, that means that `hs` is `'[]`, so expect a `O` -->
<!-- constructor.  If you see `SCons s ss :: Sing (h ': hs)`, that means that `hs` -->
<!-- is `h ': h'`, so expect a `(:&~)` constructor.  (If this is a little confusing -->
<!-- still, try re-reading the [singleton section][new-section] from the last post -->
<!-- again for a more thorough description!) -->

<!-- Note that here we decide to implement `getNet` by asking for an explicit -->
<!-- singleton input (`Sing hs ->`) instead of an implicit one (`SingI hs =>`) like -->
<!-- we did for `randomNet`.  Remember that the two methods are technically -->
<!-- equivalent, really, and compile to the same thing at runtime.  Because of type -->
<!-- erasure in Haskell, we need one or the other, at least.  We either pass in -->
<!-- `Sing hs`, or provide a `SingI hs` constraint so that we can use `sing :: Sing -->
<!-- hs` to construct the `Sing hs`. -->

<!-- There's a trade-off either way, and it can be a bit annoying because switching -->
<!-- between different modes can potentially be diverse.  For the most part, always -->
<!-- try to *take explicit singleton arguments* where you can, especially for -->
<!-- internal functions.  The simple reason is because in Haskell, we like to really -->
<!-- only do typeclass-level programming as a last, last resort.  Typeclasses in -->
<!-- Haskell are very magical, but normal values (like singletons) *are* first-class -->
<!-- and easily passed and manipulable. -->

<!-- Explicit singleton arguments can sometimes pose a burden for the caller, -->
<!-- though, so my personal approach is to always use explicit `Sing a` whenever -->
<!-- possible for *internal functions*, and to expose a `SingI a =>` interface for -->
<!-- *user-facing functions* (including for typeclass instances) -->

<!-- Let's write our `Binary` instance for `Network`.  Of course, we can't have -->
<!-- `put` or `get` take a `Sing hs` (that'd change the arity/type of the function), -->
<!-- so what we can do is have their `Binary` instances require a `SingI hs` -->
<!-- constraint, essentially doing the same thing: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where" -->
<!-- ~~~ -->

<!-- To go from "`SingI` style" to "`Sing` style", we use `sing` to generate the -->
<!-- explicit `Sing hs` from `SingI hs =>`. -->

<!-- Existential Crisis -->
<!-- ------------------ -->

<!-- Now, having the entire structure of your neural network in the type is nice and -->
<!-- all for cool tricks like `randomNet`...but do you *really* want to work with -->
<!-- this directly?  After all, from the user's perspective, the user really only -->
<!-- ever needs to know `i` and `o`: What vectors the network *expects*, and what -->
<!-- vectors the network *outputs*.  In the end, all a (feed-forward) Neural Network -->
<!-- really is is an abstraction over a function `R i -> R o`. -->

<!-- Remember, the main benefits of having the entire structure in the type was to -->
<!-- help us *implement* our functions more safely, with the compiler's help, and -->
<!-- also for cute return type polymorphism tricks like `randomNet` and `getNet`. -->
<!-- The *first* type of benefit really doesn't benefit the *user* of the network. -->
<!-- So let's talk about a way to "abstract" away the internal structure of hidden -->
<!-- nodes from the type, and maybe even have it depend on runtime values! -->

<!-- The big key to "hiding" parts of types and letting them depend on runtime -->
<!-- values is called the *existential type*.  Existential types are sort of the -->
<!-- "opposite" of the normal polymorphic (universally quantified) types you -->
<!-- normally see in Haskell. -->

<!-- For a function like -->

<!-- ~~~haskell -->
<!-- map :: (a -> b) -> [a] -> [b] -->
<!-- ~~~ -->

<!-- `a` and `b` are universally quantified, which means that the person who *uses* -->
<!-- `map` gets to *decide* what `a` and `b` are.  To be more explicit, that type -->
<!-- signature can be written as: -->

<!-- ~~~haskell -->
<!-- map :: forall a b. (a -> b) -> [a] -> [b] -->
<!-- ~~~ -->

<!-- For a universally quantified type, the *caller* gets to decide what is -->
<!-- what...and the function has to *adapt* to handle it. -->

<!-- For the *existentially* quantified type, the *function* gets to decide what is -->
<!-- what, and the *caller* has to adapt to handle it. -->

<!-- There are two main ways to work with existential types in Haskell, and we'll -->
<!-- go over both of them now and talk about their relative strengths and weaknesses -->
<!-- and what situations to use either of them in.  It pays to be aware of both! -->

<!-- ### Existential Data Type -->

<!-- Arguably the more natural way in Haskell to work with existential types is to -->
<!-- wrap them in a data type: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "data OpaqueNet ::" -->
<!-- ~~~ -->

<!-- So, if you have `net :: Network 6 '[10,6,3] 2`, you can create -->
<!-- `ONet sing net :: OpaqueNet 6 2`.  When you use the `ONet` constructor, the -->
<!-- structure of the hidden layers disappears from the type! -->

<!-- How do we use this type?  When we *pattern match* on `ONet`, we get the -->
<!-- singleton and the net back! -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "numHiddens ::" -->
<!-- ~~~ -->

<!-- Note that it's important for us to stuff in the singleton in addition to the -->
<!-- network itself, because of type erasure.  If we didn't pop the singleton in, -->
<!-- there'd be no way for us to recover the original `hs`!  (Note that we could -->
<!-- have had `ONet :: SingI hs => Network i hs o -> OpaqueNet i o`, which is -->
<!-- essentially the same thing) -->

<!-- Once you *do* pattern match on `ONet`, you have to handle the `hs` in a -->
<!-- *completely polymorphic way*.  You're not allowed to assume anything about -->
<!-- `hs`...you have to provide a completely parametrically polymorphic way of -->
<!-- dealing with it! -->

<!-- Note that this function is completely not ok: -->

<!-- ~~~haskell -->
<!-- bad :: OpaqueNet i o -> Network i hs o -->
<!-- bad = \case ONet _ n -> n -->
<!-- ~~~ -->

<!-- Why not?  Because a type signature like `OpaqueNet i o -> Network i hs o` -->
<!-- means that the *caller* can decide what `hs` can be --- just like `read :: Read -->
<!-- a => String -> a`, where the caller decides what `a` is. -->

<!-- Of course, this *isn't* the case with the way we've written the function...the -->
<!-- function only returns a *specific* `hs` that the *function* decides.  The -->
<!-- *caller* has to accommodate whatever is inside `ONet`. -->

<!-- #### Binary -->

<!-- Now, let's find out a way to serialize this type!  Now, the structure of our -->
<!-- network is *not* known in the type, so we do have to plant flags in our data -->
<!-- somehow.  We need to store a witness to the structure of the network, as well. -->

<!-- To do this, we can move `hs` from the type level to the value level.  In -->
<!-- Haskell, this is called **reflection**.  The *singletons* library provides the -->
<!-- `fromSing` function for this purpose: -->

<!-- ~~~haskell -->
<!-- ghci> fromSing (sing :: Sing '[1,2,3]) -->
<!-- [1,2,3] -->
<!-- ghci> fromSing (sing :: Sing '[True, False]) -->
<!-- [True, False] -->
<!-- ~~~ -->

<!-- And with that, we can write a serializer for `OpaqueNet`: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "putONet ::" -->
<!-- ~~~ -->

<!-- Put the structure (as a flag), and then put the network itself. -->

<!-- Now, to deserialize, we want to *load* the list of `Integer`s and move that -->
<!-- *back* into the type level.  In Haskell, this is called **reification**, the -->
<!-- dual of reflection. -->

<!-- The *singletons* library provides the `toSing` function, which returns a -->
<!-- `SomeSing` (an existentially quantified `Sing` wrapped in a constructor that we -->
<!-- can pattern match on): -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "getONet ::" -->
<!-- ~~~ -->

<!-- We first `get` the `[Integer]`, then *reify* the list of integers into the type -->
<!-- level by getting our `ss :: Sing hs`.  Then we `getNet ss`, remembering that -->
<!-- `getNet` takes a singleton to figure out what structure to get.  Then we wrap -->
<!-- it all up in the `ONet` constructor. -->

<!-- Phew!  We load our flag, reify it, and once we're back in the typed land again, -->
<!-- we can do our normal business! -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o)" -->
<!-- ~~~ -->

<!-- #### The Boundary -->

<!-- Did you notice what we just did there?  The *type* of `Sing hs` was -->
<!-- *dynamically generated* based on the *value* of the `[Integer]` we load at -->
<!-- runtime.  We just worked with types that *depended* on runtime values. -->

<!-- With the power of existentially quantified types (like in `SomeSing`), we -->
<!-- essentially gained the ability to work with types that depend on runtime -->
<!-- results. -->

<!-- In a way, you can consider the `toSing` and the `SomeSing` as our "boundary" -->
<!-- between the "untyped world" and the "typed world".  This layer (and the process -->
<!-- of reification) cleanly separates the two. -->

<!-- This "boundary" can be thought of as a lot like the boundary we talk about -->
<!-- between "pure" functions and values and "impure" (IO, etc.) ones.  We say to -->
<!-- always write as much of your program as possible in the "pure" world, and -->
<!-- to separate and pull out as much logic as you can to be pure logic.  That's -->
<!-- sort of one of the first things you learn about as a Haskell programmer: how -->
<!-- to separate logic that *can* be pure from logic that is "impure" (IO, etc.), -->
<!-- and then "combine them" at the very end, as late as possible. -->

<!-- Well, if the final program is going to be IO in the end anyway, why bother -->
<!-- separating out pure and impure parts of your logic?  Separation of concerns, -->
<!-- the increased ability to reason with your code and analyze what it does, the -->
<!-- compiler's ability to check what you write, the limitation of implementations, -->
<!-- and etc. are all reasons any Haskeller should be familiar with reciting. -->

<!-- You can think of the general philosophy of working with typed/untyped worlds as -->
<!-- being the same thing.  You can write as much of your program as possible in the -->
<!-- "typed" world, like we did in Part 1.  Take advantage of the increased ability -->
<!-- to reason with your code, parametric polymorphism helping you *write* your -->
<!-- code, limit your implementations, nab you compiler help, etc.  All of those are -->
<!-- benefits of working in the typed world. -->

<!-- Then, write what you must in your "untyped" world, such as dealing with values -->
<!-- that pop up at runtime like the `[Integer]` above. -->

<!-- Finally, at the end, *unite* them at the boundary.  Pass the control football -->
<!-- from the untyped world to the typed world! -->

<!-- #### Reifying for Fun and Profit -->

<!-- Before we move on, let's see a more direct example of reification: creating -->
<!-- networks with variable internal structure based on runtime input, like -->
<!-- configuration files. -->

<!-- Maybe you're reading the internal size from a configuration file, or maybe you -->
<!-- want it to be determined by user input.  Our old `randomNet` won't work: -->

<!-- ~~~haskell -->
<!-- randomNet :: (MonadRandom m, SingI hs) -->
<!--           => m (Network i hs o) -->
<!-- ~~~ -->

<!-- Because we need a static type signature to use it directly.  But we can return -->
<!-- an `OpaqueNet`! -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "randomONet ::" -->
<!-- ~~~ -->

<!-- Note that the implementation is slightly awkward because we had the lack of -->
<!-- foresight to implement `randomNet` using `SingI hs =>` instead of `Sing hs ->`, -->
<!-- so we have to "go from `Sing` to `SingI`". -->

<!-- You go from the `SingI` style to the `Sing` style with `sing`, like we saw -->
<!-- earlier, and you can go backwards with `singInstance`: -->

<!-- ~~~haskell -->
<!-- singInstance :: Sing a -> SingInstance a -->
<!-- ~~~ -->

<!-- Where the data type `SingInstance` has a single constructor that's a lot like -->
<!-- `SNat` from the last part: -->

<!-- ~~~haskell -->
<!-- data SingInstance a where -->
<!--     SingInstance :: SingI a => SingInstance a -->
<!-- ~~~ -->

<!-- Basically, it's impossible to *use* the `SingInstance` constructor unless you -->
<!-- have a `SingI a` instance in scope, so if you ever *pattern match* on it, it's -->
<!-- a "proof" that `SingI a` exists.  It's the same as how, for the constructor -->
<!-- `SNat :: KnownNat n => Sing n`, if you pattern match on `SNat` and see that -->
<!-- it's not something silly like `undefined`/`error`/bottom, GHC knows that there -->
<!-- is a `KnownNat n` instance.  It's sort of like pattern matching out the -->
<!-- instance itself. -->

<!-- Now you can get from your untyped world into the world of dependent types --- -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "main ::" -->
<!-- ~~~ -->

<!-- ### Continuation-Based Existentials -->

<!-- There's another way in Haskell that we work with existential types that can be -->
<!-- more natural and easy to work with in a lot of cases. -->

<!-- Note that when we pattern match on an existential data type, you have to work -->
<!-- with the values in the constructor in a parametrically polymorphic way.  For -->
<!-- example, if we had: -->

<!-- ~~~haskell -->
<!-- oNetToFoo :: OpaqueNet i o -> Foo -->
<!-- oNetToFoo = \case ONet s n -> f s n -->
<!-- ~~~ -->

<!-- What does the type of `f` have to be?  It has to take a `Sing hs` and a -->
<!-- `Network i hs o`, but deal with it in a way that works *for all* `hs`.  It has -->
<!-- to be: -->

<!-- ~~~haskell -->
<!-- f :: forall (hs :: [Nat]). Sing hs -> Network i hs o -> Foo -->
<!-- ~~~ -->

<!-- That is, it can't be written for *only* `Sing '[5]` or *only* `Sing -->
<!-- '[6,3]`...it has to work for *any* `hs`. -->

<!-- Well, we could really also just skip the data type together and represent an -->
<!-- existential type as something *taking* the continuation `f` and giving it what -->
<!-- it needs. -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "type OpaqueNet'" -->
<!-- ~~~ -->

<!-- "Tell me how you would make an `r` if you had a `Sing hs` and a `Network i hs -->
<!-- o`, and I'll make it for you!" -->

<!-- This "continuation transformation" is known as formally -->
<!-- **skolemization**.[^skolemization]  We can "wrap" a `Network i hs o` into an -->
<!-- `OpaqueNet' i o r` pretty straightforwardly: -->


<!-- [^skolemization]: Skolemization is probably one of the coolest words you'll -->
<!-- encounter working with dependent types in Haskell, and sometimes just knowing -->
<!-- that you're "skolemizing" something makes you feel cooler.  Thank you [Thoralf -->
<!-- Skolem][].  If you ever see a "rigid, skolem" error in GHC, you can thank him -->
<!-- for that too! -->

<!-- [Thoralf Skolem]: https://en.wikipedia.org/wiki/Thoralf_Skolem -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "oNet' ::" -->
<!-- ~~~ -->

<!-- To prove that the two `OpaqueNet`s are the same (and to help us see more about -->
<!-- how they relate), we can write functions that convert back and forth from them: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "withONet ::" "toONet ::" -->
<!-- ~~~ -->

<!-- Note the expanded type signature of `withONet`, which you can sort of interpret -->
<!-- as, "do *this function* on the existentially quantified contents of an -->
<!-- `OpaqueNet`." -->

<!-- #### Trying it out -->

<!-- To sort of compare how the two methods look like in practice, we're going to -->
<!-- Rosetta stone it up and re-implement serialization with the continuation-based -->
<!-- existentials: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "putONet' ::" "getONet' ::" -->
<!-- ~~~ -->

<!-- To be cute, I used the skolemized partners of `toSing` and `SomeSing`: -->

<!-- ~~~haskell -->
<!-- withSomeSing :: [Integer] -->
<!--              -> (forall (hs :: [Nat]). Sing hs -> r) -->
<!--              -> r -->
<!-- ~~~ -->

<!-- Instead of returning a `SomeSing` like `toSing` does, `withSomeSing` returns -->
<!-- the continuation-based existential. -->

<!-- The expanded type signature of `getONet'` can be read: "Give what you would do -->
<!-- if you *had* a `Sing hs` and a `Network i hs o`", and I'll get them for you and -->
<!-- give you the result." -->

<!-- Let's also see how we'd return a random network with a continuation: -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "withRandomONet' :" -->
<!-- ~~~ -->

<!-- Again, to be cute, I used the continuation-based version of `singInstance`, -->
<!-- `withSingI`: -->

<!-- ~~~haskell -->
<!-- withSingI :: Sing a -> (SingI a => r) -> r -->
<!-- ~~~ -->

<!-- The signature, in English, is "give me a `Sing a` and a value that you could -->
<!-- make *if only you had* a `SingI` instance, and I'll give you that value as if -->
<!-- you had the instance, magically!" -->

<!-- Of course, we know it's not magic:[^magic] -->

<!-- ~~~haskell -->
<!-- withSingI :: Sing a -> (SingI a => r) -> r -->
<!-- withSingI s x = case singInstance s of -->
<!--                   SingInstance -> x -->
<!-- ~~~ -->

<!-- [^magic]: Actually, it kind of *is* magic, because `singInstance` is implemented -->
<!-- with `unsafeCoerce`.  But don't tell anyone ;) -->

<!-- And we see another way we can "move past the untyped/typed boundary": -->

<!-- ~~~haskell -->
<!-- !!!dependent-haskell/NetworkTyped2.hs "main' ::" -->
<!-- ~~~ -->

<!-- A Tale of Two Styles -->
<!-- -------------------- -->








<!-- sameNat and existentials -->
