---
title: "Introduction to Singletons (Part 3)"
categories: Haskell
series: Introduction to Singletons
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2018/09/16 20:22:40
identifier: singletons-3
slug: introduction-to-singletons-3
---

Welcome back!  This article is part 3 of our journey through the *singleton
design pattern*, and the great *[singletons][]* library!

[singletons]: http://hackage.haskell.org/package/singletons

This post will be a continuation of [Part 1][] and [Part 2][], so if you
haven't read those first, now would be a good time to pause and do so and also
work on some of the exercises.  Today we will be expanding on the ideas in
those posts by working with more complex relationships between values and
lifting functions on values to functions on types.  Like the previous posts, we
will start by writing things "by hand", and then jumping into the singletons
library and seeing how the framework gives you tools to work with these ideas
in a smoother way.

[Part 1]: https://blog.jle.im/entry/introduction-to-singletons-1.html
[Part 2]: https://blog.jle.im/entry/introduction-to-singletons-2.html

Code in this post is built on *GHC 8.4.3* with the *[lts-12.9][snapshot]*
snapshot (so, singletons-2.4.1).

[snapshot]: https://www.stackage.org/lts-12.9

Review
------

In the first post we looked at the `Door` type, indexed with a phantom type of
kind `DoorState`.

```haskell
$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s
```

This gives us (at least) three distinct types `Door 'Opened`, `Door 'Closed`,
and `Door 'Locked`, which can be used to represent opened, closed, and locked
doors, respectively.

This scheme gives us a few super-powers:

*   The fact that these are three *distinct* types allows us to enforce
    type-safety by prohibiting operations on certain types of doors.
    
    We saw this in the types of functions like `openDoor :: Door 'Opened ->
    Door 'Closed`, which can only work on non-locked and closed doors.

*   Because the types are distinct, this allows our functions to be more
    expressive by stating how they change door states, programmatically.
    
    We saw this to a certain extent in functions like `openDoor`, `closeDoor`,
    and `lockDoor`, where the function type signatures tell the user how the
    input and output doors are related.  However, we will be taking this to a
    new level in this post.

*   But, because these types are all "derived" from the same type, we can also
    write functions that work on *all* `Door`s.  We saw this in functions like
    `lockAnyDoor`, and we also exploit this in our definition of `SomeDoor`.

    Essentially we also get a fourth type "for free": `forall s. Door s`, the
    type that can be used as any door.  It's a subtype of all three of the
    above types![^subtype]

[^subtype]: Some subtle points for those more familiar with Haskell's type
system: In Haskell, we say that a type `B` is a subtype of type `A` if,
wherever a function expects an `A`, we can give a `B` instead.  Any function
that expects a `Door 'Opened` will take a `forall s. Door s` (a type that can
be instantiated with any `s`).  However, the opposite is not true --- if a
Rank-N function expects a `forall s. Door s`, you cannot give it a `Door
'Opened`.

Then we talked about situations where we want to "not care" about the door
status in the type system, or when we want to return a door with a state that
is not known statically, and must be determined dynamically at runtime.  After
going through many "analogous" and equivalent type, we arrived at the
existential wrapper `SomeDoor`:

```haskell
data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor
```

```haskell
mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing mat
```

We must be careful to pack the `Sing s` with the `Door s`, so that we can
pattern match at runtime to determine what the original `s` was.

```haskell
checkOpened :: SomeDoor -> Bool
checkOpened (MkSomeDoor SOpened _) = True
checkOpened (MkSomeDoor SClosed _) = False
checkOpened (MkSomeDoor SLocked _) = False
```

```haskell
ghci> let x = mkSomeDoor Opened "Oak"
ghci> let y = mkSomeDoor Closed "Spruce"
ghci> checkOpened x
True
ghci> checkOpened y
False
```

Finally, we talked a bit about the "unified" singleton system that the
*singleton* library offers.  This included things like `SingI` to implicitly
pass singletons, and the `SingKind` kind-class that associates types with their
lifted kinds and lets you reify and reflect with functions like `withSomeSing`
and `fromSing`.

Path to Expressive Relationships
--------------------------------

Let's write a function that "knocks" on a door in IO:

```haskell
knock :: Door s -> IO ()
knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

Hm.  This doesn't seem right.  We can't knock on an opened door..can we?  We
could try enforcing this by writing:

```haskell
knockClosed :: Door 'Closed -> IO ()
knockClosed d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockLocked :: Door 'Locked -> IO ()
knockLocked d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

But, that means that the user must pick between one of two functions to open
their door.  This isn't quite what we wanted...we want the function to work
only for closed or locked doors, and not opened doors.

There are a couple of ways of doing this --- we're going to look at two
possible ways involving singletons and the *singletons* library.  Both of these
methods allow us to write dependently typed functions that are "type-safe" in
more expressive ways than before.

Dependently Typed Proofs
------------------------

The first way to do this is with a dependently-typed "proof" that an operation
is legal.

*Proofs* (in the dependently typed and Curry-Howard sense) are witnesses to some
type-level predicate or proposition.

A value-level predicate in Haskell is (generally) a function of type `a ->
Bool`.  Given a value of type `a`, if the function returns `True`, then the
predicate is satisfied.  If it returns `False`, it is not.

A type-level predicate is (generally) a type constructor of kind `k -> Type`.
Given a type of kind `k`, if *a value exists of that type*, then the predicate
is satisfied.  If no value exists, it is not.  A value that "proves" a
predicate is called a *witness* or a *proof*.

We can define a predicate `Knockable :: DoorState -> Type` as a GADT that only
has values if given `'Closed` and `'Locked`, but not `'Opened`:

```haskell
data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked
```

Now, we have a value of type `Knockable 'Closed` and `Knockable 'Locked`
(`KnockClosed` and `KnockLocked`, respectively), but no value of type
`Knockable 'Opened`.  How can we use this?

Well, we can make a version of `knock` that requires a proof that `s` is
`Knockable`:

```haskell
knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

`knock` can now only be called with `Closed` and `Locked` doors --- do you see
why?  There is no way to call `knock` with `s ~ 'Opened`...because there is no
way to pass a value of `Knockable 'Opened`.  No such value exists!  There's no
compiler error because it's "not even wrong"!

This works well if we want to do things at compile-time

```haskell
ghci> knock KnockClosed (UnsafeMkDoor @'Closed "Birch")
Knock knock on Birch door!
```

### Let the compiler prove it for you

We can even make it more seamless to use by auto-generating proofs at
compile-time, with a general class like `Auto`:

```haskell
class Provable p a where
    auto :: p a

instance Provable Knockable 'Closed where
    auto = KnockClosed

instance Provable Knockable 'Locked where
    auto = KnockLocked
```

```haskell
ghci> knock auto (UnsafeMkDoor @'Closed "Acacia")
Knock knock on Acacia door!

ghci> knock auto (UnsafeMkDoor @'Opened "Jungle")
COMPILER ERROR!! COMPILER ERROR!!
```

Such a typeclass exists in libraries like *[type-combinators][]* (called
`Known`), and in dependently typed languages like Idris `auto` is actually a
built-in language keyword that does this automatically!

[type-combinators]: http://hackage.haskell.org/package/type-combinators

<!-- TODO: exercise for `Sing` as predicate `-->

### Decidable Preducates

However, all of this only works if you know what `s` is at compile-time.  What
if you don't?  What if you are retrieving `s` at runtime (like from a
`SomeDoor` or `withSomeSing`), or you are forced to handle all possible `s`s?

There's a property of some predicates called "decidability".  We say that a
predicate is *decidable* if, for any input type, we can say whether or not the
predicate is satisfiable.

We say that a predicate `p` in Haskell is decidable if we can write:

```haskell
decidePred
    :: Sing a               -- ^ given a type
    -> Decision (p a)       -- ^ return a decision
```

Where:

```haskell
data Decision a = Proved a                  -- ^ a value of a exists
                | Disproved (Refuted a)     -- ^ a value of a cannot exist


-- | The data type with no values
data Void

-- | 'a' cannot exist.  Commonly also called `Not`
type Refuted a = a -> Void
```

(These types are all from the *base* and *singletons* library, mostly in the
*Data.Singletons.Decide* module)

`Decision a` is kinda like a `Maybe a`, except instead of `Nothing`, we include
a proof that the predicate is *not* true.

For those unfamiliar with the `a -> Void` idiom (often called `Not a`, or
`Refuted a`), `a -> Void` is a type we use in Haskell to represent the fact
that it is impossible to construct a value of type `a`.  That's because if you
could, then you could give it to an `a -> Void` to get a value of type `Void`,
which is impossible to have.

It's a lot to handle up-front, so let's look at an example.  Is `Knockable` a
decidable predicate?  Yes!

```haskell
isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
    SOpened -> Disproved $ \case            -- s ~ 'Opened
    SClosed -> Proved KnockClosed           -- s ~ 'Closed
    SLocked -> Proved KnockLocked           -- s ~ 'Locked

```

This definition should seem pretty straightforward for the `SClosed` and
`SLocked` branches.  `isKnockable SClosed :: Decision (Knockable 'Closed)`, we
give `Proved KnockClosed`, which gives us just that!

However, `isKnockable SOpened :: Decision (Knockable 'Opened)`.  We can't use
`Proved :: a -> Decision a`, because no such value of type `Knockable 'Opened`
exists.  So, we have to say that we *disprove* it: we have to prove to GHC that
no such type could possibly exist.  We do this by providing a function of type
`Refuted (Knockable 'Opened)`, or type `Knockable 'Opened -> Void`.

We can write it like this:

```haskell
disproveOpened :: Knockable 'Opened -> Void
disproveOpened = \case
```

And we're good to go!

How does this work?

Well, remember, we have to pattern match on the possible inputs.  However, we
can't use any of the "legal" patterns:

```haskell
disproveOpened :: Knockable 'Opened -> Void
disproveOpened = \case
    KnockClosed -> ...    -- not a valid pattern, since it's `Knockable 'Closed`
    KnockLocked -> ...    -- not a valid pattern, since it's `Knockable 'Locked`
```

If you include either of those patterns, GHC will complain.  So, there is no
valid pattern to match on... so `disproveOpened = \case` is enough to write the
function `Knockable 'Opened -> Void`.

We can use this decision function, finally, to handle an arbitrary `Door` whose
status we not know until runtime:

```haskell
knockSomeDoor
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
    Proved k    -> knock k d
    Disproved _ -> putStrLn "No knocking allowed!"
```

### Perspective on Proofs

We briefly touched on a very simple version of a dependently typed proof, and
how to "prove" properties.

If you have heard things about dependently typed programming before, you might
have heard that a lot of it involves "proving properties about your programs"
and "forcing you to provide proofs for all of your actions".  The idea of a
"proof" might seem a bit scary and "mathematical" to those coming from a
software development world.

However, as we just saw, working with proofs and decisions of proofs can be as
simple as a couple lines of GADTs and dependent pattern matches:

```haskell
data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
    SOpened -> Disproved $ \case
    SClosed -> Proved KnockClosed
    SLocked -> Proved KnockLocked
```

`Knockable` is our *predicate*, values of type `Knockable s` are our *proofs*
(or witnesses), and `isKnockable` is our *decision function*.  So, when we see
a function like:

```haskell
knock :: Knockable s -> Door s -> IO ()
```

We can read the type signature as: "Knocking requires both a `Door s` and a
*proof* that the door's status is `Knockable`".  It makes it impossible for us
to run `knock` on a status that is not `Knockable`, like, say, `'Opened`.

In this light, the role of a proof is like a "key" that a type (like `'Closed`)
must provide to "unlock" functions like `knock`.[^metaphor]  A *decision
function* is a function to generate these proofs (or prove that they are
impossible) for given types.

[^metaphor]: Sorry to mix up similar metaphors like this!  Definitely not
intentional :)

A lot of people think of proofs as "compiler tricks", or things that exist only
to appease the compiler.  And, in a sense, this is true.  Compilers of
languages that encourage heavy usage of proofs (like Agda, Coq, Idris) actually
implement something called *proof erasure*.  That is, values like `KnockClosed`
and `KnockLocked` might never exist at runtime, since they never actually *do*
anything at runtime.  They only exist as ways to limit or enable specific
programs from compiling, and serve no purpose after compilation.  GHC Haskell
does not implement proof erasure at the time of this post (Current GHC version
8.4), but if proofs like this become commonplace, you might be reading this
during a time where GHC Haskell erases proofs like `Knockable` witnesses :)

### Singletons and Proofs

Proofs might not play a role at run-time, but generating them with types
requires being able to pattern match and work with *types* at run-time.
Because of this, singletons play an important role in working with proofs in
Haskell.

After all, remember the type of our decision function:

```haskell
isKnockable :: Sing a -> Decision (Knockable a)
```

Decision functions should be able to fully exploit the structure of any types
they are scrutinizing in order to make their decision.

In this light, the *singletons* library provides many tools for working with
proofs and decisions.  In fact, the entire *Data.Singletons.Decide* module is
dedicated to working with proofs and decisions.  It provides the `Decision` 
data type and `Refuted` type synonym, both featured above.

It also re-exports a particularly useful predicate from *base*, *propositional
equality*:

```haskell
data (:~:) :: k -> k -> Type where
    Refl :: a :~: a
```

Like how `Knockable` is a predicate that a given status is "knockable",
`'Blah :~:` is a predicate that a given type is *equal to* `'Blah`.  A value of
type `Knockable s` is a proof that `s` is knockable, and a value of type
`'Blah :~: a` is a proof that `a` is *equal to* `'Blah`.

If you are having trouble seeing how, note the constructors that it allows.
Remember that we limit `Knockable s` to only having "knockable" `s` by only
allowing two constructors, so we can only construct valid values.  The same
thing happens here -- `'Blah :~:` only has *one single constructor*: `Refl ::
'Blah :~: 'Blah`.  The only valid constructor is one where the left hand side
is equal to the right hand side.

It also offers the "kindclass" `SDecide`, which provides *decision functions*
for the `a :~:` predicate:

```haskell
class SDecide k where
    (%~) :: Sing (a :: k) -> Sing (b :: k) -> Decision (Sing (a :~: b))
```

For example, `Bool` is an instance of `SDecide`, so we have a function:

```haskell
(STrue %~) :: Sing b -> Decision ('True :~: b)
```

Which is a decision function to check if `b` is equal to `'True`.

<!-- TODO: Should we mention Sigma? -->

Type Level Functions
--------------------

Hi

<!-- Because we are proving properties about *types*, the ability to pattern match -->
<!-- on types and work with *types* at run-time is essential[^runtime]. -->

<!-- [^runtime]: --> 

<!-- There are a couple of ways to do this --- we're going to explore three -->
<!-- different ones, moving from a "classic" haskell approach to a type-level -->
<!-- programming-based approach. -->

<!-- ### Trusty ol' typeclasses -->

<!-- To ground all of this before we take off to dizzying heights, let's see how we -->
<!-- might implement this using an ad-hoc typeclass in "classic Haskell". -->

<!-- Remember that we can essentially imagine `Door 'Opened`, `Door 'Closed` and -->
<!-- `Door 'Locked` as three distinct types.  We're trying to write a function that -->
<!-- works only on two different types, and give them both the same name. -->

<!-- Sounds exactly like the basis of the humble typeclass! -->

<!-- ```haskell -->
<!-- class Knockable s -->

<!-- instance Knockable 'Closed -->
<!-- instance Knockable 'Locked -->

<!-- knock :: Kockable s => Door s -> IO () -->
<!-- knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!" -->
<!-- ``` -->

<!-- We write instances for `'Closed` and `'Locked`.  So now, if we try to call -->
<!-- `knock` on a `Door 'Opened`, we'll have a compile-time error because there is -->
<!-- no instance for `Knockable 'Opened`. -->

<!-- Simple enough, right? -->

<!-- ### Dependently Typed Proofs -->

<!-- The next strategy we'll look at is the --> 


<!-- So far, we've used phantom types to restrict our functions in a very simple -->
<!-- way.  What about some more complex predicates? -->

<!-- For a simple example, we can assign labels to our door states.  A `Pass` type -->
<!-- that is `Obstruct` when the door is opened, and `Allow` otherwise? -->

<!-- ```haskell -->
<!-- data Pass = Obstruct | Allow -->

<!-- passState :: DoorState -> Pass -->
<!-- passState Opened = Allow -->
<!-- passState Closed = Obstruct -->
<!-- passState Locked = Obstruct -->
<!-- ``` -->

<!-- Bear with me here; `Pass` is a bit of a simple data type, but in practice it -->
<!-- could be more complicated, representing more possibilities! -->

<!-- Let's write a function that "knocks" on a door in IO -->
