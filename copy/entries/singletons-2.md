---
title: "Introduction to Singletons (Part 2)"
categories: Haskell
series: Practical Dependent Types in Haskell
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2017/12/22 10:33:03
date: never
identifier: singletons-2
slug: introduction-to-singletons-2
---

Welcome back to our journey through the singleton design pattern and the
great *[singletons][]* library!

[singletons]: http://hackage.haskell.org/package/singletons

This post is a direct continuation of [Part 1][], so be sure to check that out
first if you haven't already!  If you hare just jumping in now, I suggest
taking some time to to through the exercises if you haven't already!

[Part 1]: https://blog.jle.im/entry/introduction-to-singletons-1.html

Again, code is built on *GHC 8.2.2* with the *[lts-10.0][snapshot]*
snapshot (so, singletons-2.3.1).

[snapshot]: https://www.stackage.org/lts-10.0

Review
------

Let's return to our `Door` type:

```haskell
!!!singletons/Door2.hs "$(singletons " "data Door "
```

`Door` is great!  It is an *indexed data type*, in that picking a different
type variable gives a different "type" of Door:

*   `Door 'Opened` is a type that represents the type of an opened door
*   `Door 'Closed` is a *different* type that represents the type of a *closed*
    door
*   `Door 'Locked` is yet another (third) type that represents the type of a
    *locked* door.

So, really, when we define `Door s`, we really are defining *three distinct*
types (and also a not-so-obvious fourth one, which we will discuss later).

This is great and all, but isn't Haskell a language with static, compile-time
types?  Doesn't that mean that we have to know if our doors are opened, closed,
or locked at compile-time?

This is something we can foresee being a big issue.  It's easy enough to create
a `Door s` if you know `s` at compile-time by just typing in a type annotation
(`UnsafeMkDoor "Oak" :: Door 'Opened`).  But what if we *don't* know `s` at
compile-time?

To learn how to do this, we first need to learn how to *not care*.

Ditching the Phantom
--------------------

Sometimes we don't *actually* care about the state of the door in the *type* of
the door.  We don't want `Door 'Opened` and `Door 'Closed`...we want a type to
just represent a door, without the status in its type.

This might come about a bunch of different ways.  Maybe you're reading a `Door`
data from a serialization format, and you want to be able to parse *any* door
(whatever door is serialized).

More concretely, we've seen this in `lockAnyDoor`, as well -- `lockAnyDoor`
doesn't care about the type of its input (it can be *any* `Door`).  It only
cares about the type of its output (`Door 'Locked`)

To learn how to not care, we can describe a type for a door that does *not*
have its status in its type.

We have a couple of options here.  First, we can create a new type `SomeDoor`
that is the same as `Door`, except instead of keeping its status in its type,
it keeps it as a runtime value:

```haskell
data SomeDoor = MkSomeDoor
    { someDoorState    :: DoorState
    , someDoorMaterial :: String
    }

-- or, in GADT syntax
data SomeDoor :: Type where
    MkSomeDoor ::
      { someDoorState    :: DoorState
      , someDoorMaterial :: String
      } -> SomeDoor
```

Note the similarity of `SomeDoor`'s declaration to `Door`'s declaration above.
It's mostly the same, except, instead of `DoorState` being a type parameter, it
is instead a runtime value inside `SomeDoor`.

Now, this is actually a type that we *could* have been using this entire time,
if we didn't care about type safety.  In the real world and in real
applications, we actually might have written `SomeDoor` *before* we ever
thought about `Door` with a phantom type.  It's definitely the more typical
"standard" Haskell thing.

It's possible to "construct" this from our original typed `Door`, using a smart
constructor/conversion function:

```haskell
fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor SOpened (UnsafeMkDoor m) = MkSomeDoor Opened m
formDoor SClosed (UnsafeMkDoor m) = MkSomeDoor Closed m
formDoor SLocked (UnsafeMkDoor m) = MkSomeDoor Locked m

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing
```

We can now write functions on this type:

```haskell
closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor Opened m) = Just (MkSomeDoor Closed m)
closeSomeOpenedDoor (MkSomeDoor Closed m) = Nothing
closeSomeOpenedDoor (MkSomeDoor Locked m) = Nothing
```

### SomeDoor to Door

`SomeDoor` is great.  But because it's a completely different type, we
potentially have to write the same function for both `Door` and `SomeDoor`,
because they have different implementations. Wouldn't it be nice if we can
*re-use* our original `closeDoor`?  This is a toy example, and in real life,
closing a door might have some complicated runtime logic, and it'd be annoying
to have to *re-implement* it for both `SomeDoor` and `Door`.

#### Converting into an existential

One thing we can do is write a function to convert a `SomeDoor` into a `Door`,
so we can re-use our original `closeDoor`.  We'd convert our `SomeDoor` into a
`Door` to re-use our `closeDoor :: Door 'Opened -> Door 'Closed` on it if
possible!

However, going from `SomeDoor` to `Door s` is slightly trickier in Haskell than
going the other way around.  The main thing stopping us is that normal Haskell
type variables are universally qualified, meaning that the *caller* can pick
how to instantiate `s` (and not the conversion function).  However, we want a
function where the *function* can pick the `s`, and the caller must handle
whatever `s` is given by the function:

One trick we often use is a CPS-style existential type:

```haskell
withSomeDoor :: SomeDoor -> (forall s. Sing s -> Door s -> r) -> r
withSomeDoor (MkSomeDoor Opened m) f = f SOpened (UnsafeMkDoor m)
withSomeDoor (MkSomeDoor Closed m) f = f SClosed (UnsafeMkDoor m)
withSomeDoor (MkSomeDoor Locked m) f = f SLocked (UnsafeMkDoor m)
```

Notice the funky CPS-like type signature of `withSomeDoor`.  To use
`withSomeDoor` and access the `Door`, you have to pass in a function to handle
*any possible `s`*.  And, as you can see, the function passed in might be given
an `SOpened`, an `SClosed`, or an `SLocked`.  It has to be able to handle all
three!

Here, we call `s` *existentially quantified*.  The `withSomeDoor` function gets
to pick which `s` to give `f`.  So, the `s` type variable is directly chosen by
the *function*, and not by the caller.

We can implement `closeSomeOpenedDoor` (and even a `lockAnySomeDoor`) using this
conversion function:

```haskell
closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor sd = withSomeDoor sd $ \case
    SOpened -> \d -> Just . fromDoor_ . closeDoor $ d
    SClosed -> \_ -> Nothing
    SLocked -> \_ -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor sd = withSomeDoor sd $ \s d ->
    fromDoor_ $ lockAnyDoor s d
```

Now, our goal is complete -- we can *re-use* our previous `closeDoor` and
`lockAnyDoor`!  We *convert* our `SomeDoor` into a `Door s` (with an
existentially quantified `s`), so we can use `closeDoor :: Door 'Opened -> Door
'Closed` and `lockAnyDoor :: Door s -> Door 'Locked` on it.  Then we "convert
it back" using `fromDoor`.

### The Existential Datatype

However, there's another path we can take.  With the power of singletons, we
can actually implement `SomeDoor` *in terms of* `Door`, using an **existential
data type**:

```haskell
-- using existential constructor syntax
data SomeDoor = forall s. MkSomeDoor (Sing s) (Door s)

-- or, using GADT syntax (preferred)
!!!singletons/Door2.hs "data SomeDoor ::"
```

`MkSomeDoor` is a constructor for an existential data type, meaning that the
data type "hides" a type variable `s`.

Note the similarities between our original `SomeDoor` and this one.

```haskell
-- | Re-implementing door
data SomeDoor where
    MkSomeDoor :: DoorState -> String -> SomeDoor

-- | Re-using Door, as an existential type
data Door :: DoorState -> DoorState where
    UnsafeMkDoor :: String -> Door s

data SomeDoor where
    MkSomeDoor  :: Sing s  -> Door s -> SomeDoor
```

Basically, our type before re-implements `Door`.  But the new one actually
directly uses the original `Door s`.  This means we can *directly* re-use our
`Door` functions on `SomeDoor`s, without needing to convert our
implementations.

In Haskell, existential data types are pretty nice, syntactically, to work
with.  For a comparison, let's re-implement our previous functions with our new
data type:

```haskell
!!!singletons/Door2.hs "fromDoor ::" "fromDoor_ ::" "closeSomeOpenedDoor ::" "lockAnySomeDoor ::"
```

Much more convenient, because *we already have a `Door`!*  And we don't have to
re-implement one like we did for our original `SomeDoor` -- all of our original
code works directly!

### The Link

It's important to remember that our original separate-implementation `SomeDoor`
is, functionally, identical to the new code-reusing `Door`.  The reason why
they are the same is that *having an existentially quantified singleton is the
same as having a value of the corresponding type.*  Having an existentially
quantified `SingDS s` is *the same as* having a value of type `DoorState`.

In fact, the *singletons* library gives us a direct existential wrapper:

```haskell
-- (not the actual definition)
data SomeSing DoorState :: Type where
    SomeSing :: Sing s -> SomeSing DoorState
```

There are three values of type `SomeSing DoorState`:

```haskell
SomeSing SOpened :: SomeSing DoorState
SomeSing SClosed :: SomeSing DoorState
SomeSing SLocked :: SomeSing DoorState
```

A value of type `SomeSing DoorState` (which contains an existentially
quantified `Sing s` -- a `SingDS`) is *the same* as a value of type
`DoorState`.  The two types are identical!  (Or, well, isomorphic.  As a fun
exercise, write out the explicit isomorphism -- the `SomeSing DoorState ->
DoorState` and the `DoorState -> SomeSing DoorState`).

Our new `SomeDoor` containing an existentially quantified `Sing s` is the same
as our first `SomeDoor` containing just a `DoorState`.

#### Why Bother

If they're identical, why use a `Sing` or the new `SomeDoor` at all?  Why not
just use a `DoorState` value?

The main reason (besides allowing code-reuse) is that *using the singleton lets
us directly recover the type*.  Essentially, a `SingDS s` not only contains whether it
is Opened/Closed/Locked...it contains it in a way that GHC can use to *bring it
all back* to the type level.

A `forall s. SomeDoor (Sing s) (Door s)` essentially contains `s` *with* `Door
s`.  When you see this, you *should read this as* `forall s. SomeDoor s (Door
s)` (and, indeed, this is similar to how it is written in dependently typed
languages.)

It's kind of like how, when you're used to reading Applicative style, when you
see `f <$> x <*> y`, you should read `f x y`.  When you see `forall s. SomeDoor
(Sing s) (Door s)`, you should read `forall s. SomeDoor s (Door s)`.  The role
of `Sing s` there is, like in Part 1, simply to be a run-time stand-in for the
type `s` itself.

So, for our original `Door s` functions, we need to know `s` at runtime --
storing the `Sing s` gives GHC exactly that.  Once you get the `Sing s` back,
you can now use it in all of our type-safe functions from Part 1, and you're
back in type-safe land.

### Some Lingo

In the language of dependently typed programming, we call `SomeDoor` a
**dependent sum**, because you can imagine it basically as:

```haskell
data SomeDoor = SDOpened (Door 'Opened)
              | SDClosed (Door 'Closed)
              | SDLocked (Door 'Locked)
```

A three-way sum between a `Door 'Opened`, a `Door 'Closed`, and a `Door
'Locked`, essentially.  If you have a `SomeDoor`, it's *either* an opened door,
a closed door, or a locked door.  Try looking at this new `SomeDoor` until you
realize that this type is the same type as the previous `SomeDoor`!

You might also see `SomeDoor` called a **dependent pair** -- it's a "tuple"
where the *type* of the second item (our `Door s`) is determined by the *value*
of the first item (our `Sing s`).

In Idris, we could write `SomeDoor` as a type alias, using its native dependent
sum syntax, as `s ** Door s`.  The *value* of the first item reveals to us
(through a pattern match, in Haskell) the *type* of the second.

### Types at Runtime

With this last tool, we finally have enough to build a function to "make" a
door with the status unknown until runtime:

```haskell
mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
    Opened -> MkSomeDoor SOpened . mkDoor SOpened
    Closed -> MkSomeDoor SClosed . mkDoor SClosed
    Locked -> MkSomeDoor SLocked . mkDoor SLocked

```

```haskell
ghci> let mySomeDoor = mkSomeDoor Opened "Birch"
ghci> :t mySomeDoor
SomeDoor
ghci> putStrLn $ case mySomeDoor of
        MkSomeDoor SOpened _ -> "mySomeDoor was opened!"
        MkSomeDoor SClosed _ -> "mySomeDoor was closed!"
        MkSomeDoor SLocked _ -> "mySomeDoor was locked!"
mySomeDoor was opened!
```

Using `mkSomeDoor`, we can truly pass in a `DoorState` that we generate at
runtime (from IO, or a user prompt, or a configuration file, maybe), and create
a `Door` based on it.

Take *that*, type erasure! :D

We could even directly return a `Door` with an existentially quantified door
status in CPS style:

```haskell
withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor s m f = case s of
    Opened -> f SOpened (UnsafeMkDoor m)
    Closed -> f SClosed (UnsafeMkDoor m)
    Locked -> f SLocked (UnsafeMkDoor m)
```

```haskell
ghci> withDoor Opened "Birch" $ \s d -> case s of
         SOpened -> "Opened door!"
         SClosed -> "Closed door!"
         SLocked -> "Locked door!"
Opened door!
```

This allows us to *directly* generate a `Door s` with an `s` that can
vary at runtime.

### Reification

The general pattern we are exploiting here is called **reification** -- we're
taking a dynamic run-time value, and lifting it to the type level as a type
(here, the type variable `s`).  You can think of reification as the opposite of
*reflection*, and imagine the two as being the "gateway" between the type-safe
and unsafe world.  In the dynamic world of a `DoorState` term-level value, you
have no type safety.  You live in the world of `SomeDoor`,
`closeSomeOpenedDoor`, `lockAnySomeDoor`, etc.  But, you can *reify* your
`DoorState` value to a *type*, and enter the type-safe world of `Door s`,
`closeDoor`, `lockDoor`, and `lockAnyDoor`.

The *singletons* library automatically generates functions to directly reify
`DoorState` values:

```haskell
toSing       :: DoorState -> SomeSing DoorState
withSomeSing :: DoorState -> (forall s. Sing s -> r) -> r
```

The first one reifies a `DoorState` as an existentially quantified data type,
and the second one reifies in CPS-style, without the intermediate data type.

We can use these to write `mkSomeDoor` and `withDoor`:

```haskell
!!!singletons/Door2.hs "mkSomeDoor ::" "withDoor ::"
```

<!-- ## A Reflection on Subtyping -->

<!-- Let's take a step back to look at the concept of "subtyping" in Haskell, and -->
<!-- how it relates to what we've done here.  Without phantom types you might have -->
<!-- imagined being able to do something like this: -->

<!-- ```hskell -->
<!-- data DoorOpened = MkDoorOpened String -->
<!-- data DoorClosed = MkDoorClosed String -->
<!-- data DoorLocked = MkDoorLocked String -->
<!-- ``` -->

<!-- And, for the most part, you get a similar API: -->

<!-- ```haskell -->
<!-- closeDoor :: DoorOpened -> DoorClosed -->
<!-- lockDoor  :: DoorClosed -> DoorLocked -->
<!-- ``` -->

<!-- This is all stuff we can do in "normal Haskell", and get the same type-safety. -->

<!-- The advantage of the *indexed type* ("type family", in dependent types -->
<!-- lingo[^fam]) is that we can write functions that work on *any* door state: -->

<!-- [^fam]: Again, not to be confused with GHC's Type Families language feature -->

<!-- ```haskell -->
<!-- doorMaterial :: Door s -> String -->
<!-- lockAnyDoor  :: Door s -> Door 'Locked -->
<!-- ``` -->

<!-- Remember how I said earlier that our declaration of `Door` created three types? -->
<!-- `Door 'Opened`, `Door 'Closed`, and `Door 'Locked`?  I lied -- it actually -->
<!-- gives us a *fourth* type, `forall s. Door s` -- a type that can be instantiated -->
<!-- as *any* status door.  This is the return type of `UnsafeMkDoor :: String -> -->
<!-- (forall s. Door s)`. -->

<!-- As a return type, `forall s. Door s` is what we call a **subtype** of `Door -->
<!-- 'Opened`, `Door 'Closed`, and `Door 'Locked`.  In type theory, a subtype is -->
<!-- something that can be used whenever something expects a value of its supertype, -->
<!-- but not necessarily the other way around. -->

## Zooming Out

Alright!  We've spent two blog posts going over a lot of different things in
the context of our humble `Door s` type.  Let's zoom out and take a large-scale
look at how *singletons* (the design pattern, and the library) helps us in
general.

### Sing

The crux of everything is the `Sing :: Type -> Type` indexed type.  If you see
a value of type `Sing s`, you should really just think "a runtime witness for
`s`".  If you see:

```haskell
lockAnyDoor :: Sing s -> Door s -> Door 'Locked
MkSomeDoor  :: Sing s -> Door s -> SomeDoor
```

You should read it as (in pseudo-Haskell)

```haskell
lockAnyDoor :: { s } -> Door s -> Door 'Locked
MkSomeDoor  :: { s } -> Door s -> SomeDoor
```

This is seen clearly if we look at the partially applied type signatures:

```haskell
lockAnyDoor SOpened :: Door 'Opened -> Door 'Locked
MkSomeDoor  SLocked :: Door 'Locked -> SomeDoor
```

If you squint, this kinda looks like:

```haskell
lockAnyDoor 'Opened :: Door 'Opened -> Door 'Locked
MkSomeDoor  'Locked :: Door 'Locked -> SomeDoor
```

And indeed, when we get real dependent types in Haskell, we will really be
directly passing types (that act as their own runtime values) instead of
singletons.

It is important to remember that `Sing` is poly-kinded, so we can have `Sing
'Opened`, but also `Sing 'True`, `Sing 5`, and `Sing '['Just 3, 'Nothing, 'Just
0]` as well.  This is the real benefit of using the *singletons* library
instead of writing our own singletons -- we get to work uniformly with
singletons of all kinds.

#### SingI

`SingI` is a bit of typeclass trickery that lets us implicitly pass `Sing`s to
functions:

```haskell
class SingI s where
    sing :: Sing s
```

If you see:

```haskell
lockAnyDoor :: Sing  s -> Door s -> Door 'Locked
MkSomeDoor  :: Sing  s -> Door s -> SomeDoor
```

These are *identical* to

```haskell
lockAnyDoor :: SingI s => Door s -> Door 'Locked
MkSomeDoor  :: SingI s => Door s -> SomeDoor
```

Either way, you're passing in the ability to get a runtime witness on `s` --
just in one way, it is asked for as an explicit argument, and the second way,
it is passed in using a typeclass.

We can *convert* from `SingI s ->` style to `SingI s =>` style using `sing`:

```haskell
!!!singletons/Door2.hs "lockAnyDoor_ ::" "fromDoor_ ::"
```

And we can convert from `SingI s =>` style to `SingI s ->` style using
`withSingI`:

```haskell
lockAnyDoor :: Sing s -> Door s -> Door 'Locked
lockAnyDoor s d = withSingI s (lockAnyDoor_ d)

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor s d = withSingI s (fromDoor_ d)
```

Again, the same function -- just two different styles of calling them.

### Reflection and Reification

Reflection is the process of bringing a type-level thing to a value at the term
level ("losing" the type information in the process) and reification is the
process of bringing a value-level Reification is the process of going from a
value at the *term level* to the *type level*.

One limitation in Haskell is that there is no actual link between the type
`DoorState` and its *values* with the *kind* `DoorState` with its *types*.
Sure, the constructors have the same names, but the language doesn't actually
link them together for us.

The *singletons* library handles this by using a typeclass with associated
types to implement a generalized reflection and reification process.  It gives
us the `SingKind` typeclass:


<!-- *   `toSing :: DoorState -> SomeSing DoorState` takes us from values to their -->
<!--     (existentially quantified) singletons -->

<!--     ```haskell -->
<!--     ghci> let s = toSing Opened -->
<!--     ghci> :t s -->
<!--     s :: SomeSing DoorState -->
<!--     ghci> putStrLn $ case s of -->
<!--             SomeSing SOpened -> "Opened." -->
<!--             SomeSing SClosed -> "SClosed." -->
<!--             SomeSing SLocked -> "SLocked." -->
<!--     "Opened." -->
<!--     ``` -->

<!--     `SomeSing` is like `SomeDoor` in that it is an existentially quantified -->
<!--     singleton: -->

<!--     ```haskell -->
<!--     data SomeSing DoorState :: Type where -->
<!--         SomeSing :: Sing s -> SomeSing DoorState -->

<!--     -- or, more accurately, since `SomeSing` is polykinded -->
<!--     data SomeSing :: k -> Type where -->
<!--         SomeSing :: Sing (a :: k) -> SomeSing k -->
<!--     ``` -->


<!-- 3.  Implement `withSomeDoor` for the existentially quantified `SomeDoor` type. -->

<!--     ```haskell -->
<!--     !!!singletons/DoorSingletons.hs "data SomeDoor" "withSomeDoor ::"1 -->
<!--     ``` -->

<!-- 4.  Implement `openAnySomeDoor`, which should work like `lockAnySomeDoor`, just -->
<!--     wrapping an application of `openAnyDoor` inside a `SomeDoor`. -->

<!--     ```haskell -->
<!--     !!!singletons/DoorSingletons.hs "openAnySomeDoor ::"1 -->
<!--     ``` -->

<!--     You **shouild not** use `UnsafeMkDoor` directly. -->

<!--     Note that because we wrote `openAnyDoor` in "implicit style", we might have -->
<!--     to convert between `SingI s =>` and `Sing s ->` style, using `withSingI`. -->

<!-- However, full expressively with phantom types is still out of our reach.  If we -->
<!-- want to express more complicated relationships and to be able to treat phantom -->
<!-- types (and *types*, in general) as first-class values, and delve into the -->
<!-- frighteningly beautiful world of "type-level programming", we are going to have -->
<!-- to dig a bit deeper.  Come back for the next post to see how!  Singletons will -->
<!-- be our tool, and we'll also see how the singletons library is a very clean -->
<!-- unification of a lot of concepts. -->


<!-- ### A Reflection on Subtyping -->

<!-- Without phantom types you might have imagined being able to do something like -->
<!-- this: -->

<!-- ```hskell -->
<!-- data DoorOpened = MkDoorOpened { doorMaterial :: String } -->
<!-- data DoorClosed = MkDoorClosed { doorMaterial :: String } -->
<!-- data DoorLocked = MkDoorLocked { doorMaterial :: String } -->
<!-- ``` -->

<!-- Which is even possible now with `-XDuplicateRecordFields`.  And, for the most -->
<!-- part, you get a similar API: -->

<!-- ```haskell -->
<!-- closeDoor :: DoorOpened -> DoorClosed -->
<!-- lockDoor  :: DoorClosed -> DoorLocked -->
<!-- ``` -->

<!-- But what about writing things that take on "all" door types? -->

<!-- The only real way (besides typeclass magic) would be to make some sum type -->
<!-- like: -->

<!-- ```haskell -->
<!-- data SomeDoor = DO DoorOpened | DC DoorClosed | DL DoorLocked -->

<!-- lockAnyDoor :: SomeDoor -> DoorLocked -->
<!-- ``` -->

<!-- However, we see that if we parameterize a single `Door` type, we can have it -->
<!-- stand in *both* for a "known status" `Door` *and* for a "polymorphic status" -->
<!-- `Door`. -->

<!-- This actually leverages Haskell's *subtyping* system.  We say that `forall s. -->
<!-- Door s` (a `Door` that is polymorphic on all `s`) is a *subtype* of `Door -->
<!-- 'Opened`.  This means that a `forall s. Door s` can be used anywhere a function -->
<!-- would expect a `Door 'Opened`...but not the other way around. -->

