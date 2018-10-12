---
title: "Introduction to Singletons (Part 3)"
categories: Haskell
series: Introduction to Singletons
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2018/09/16 20:22:40
date: 2018/10/01 13:56:55
identifier: singletons-3
slug: introduction-to-singletons-3
---

Welcome back!  This article is part 3 of our journey through the *singleton
design pattern*, and the great *[singletons][]* library!

[singletons]: http://hackage.haskell.org/package/singletons

This post will be a continuation of [Part 1][] and [Part 2][], so if you
haven't read those first, now would be a good time to pause and do so (and also
try to complete the exercises).  Today we will be expanding on the ideas in
those posts by working with more complex ways to *restrict functions* based on
types.  Like the previous posts, we will start by writing things "by hand", and
then jumping into the singletons library and seeing how the framework gives you
tools to work with these ideas in a smoother way.

The first half of today's post will introduce a new application and design
pattern that the usage of singletons greatly enhances.  The second part of
today's post deals directly with the lifting of functions to the type level,
which is made practical by the usage of singletons and the *singletons*
library.

[Part 1]: https://blog.jle.im/entry/introduction-to-singletons-1.html
[Part 2]: https://blog.jle.im/entry/introduction-to-singletons-2.html

Code in this post is built on *GHC 8.6.1* with the
*[nightly-2018-09-29][snapshot]* snapshot (so, *singletons-2.5*).  However,
unless noted, all of the code should still work with *GHC 8.4* and
*singletons-2.4*. Again, you can download the source for this file
[here][source], and, if *stack* is installed, you can drop into a ghci session
with all of the bindings in scope executing it:

[snapshot]: https://www.stackage.org/nightly-2018-09-29
!!![source]:singletons/Door3.hs

```bash
$ ./Door3.hs
```

Review
------

In the first post we looked at the `Door` type, indexed with a phantom type of
kind `DoorState`.

```haskell
!!!singletons/Door3.hs "$(singletons " "data Door "
```

This gives us (at least) three distinct types `Door 'Opened`, `Door 'Closed`,
and `Door 'Locked`, which can be used to represent opened, closed, and locked
doors, respectively.  We talked in previous posts about how we can use this for
for a lot of enat tings, including enforcing type-safety, talking about how
inputs relate to outputs, and uniting functions polymorphic on all door states.

Then we talked about situations where we want to "not care" about the door
status in the type system, or when we want to return a door with a state that
is not known statically, and must be determined dynamically at runtime.  After
going through many "analogous" and equivalent type, we arrived at the
existential wrapper `SomeDoor`:

```haskell
!!!singletons/Door3.hs "data SomeDoor" "mkSomeDoor"
```

(We must be careful to pack the `Sing s` with the `Door s`, so that we can
pattern match at runtime to determine what the original `s` was.)

For the rest of this post, `SomeDoor` will essentially be used as a stand-in
for a `Door s` that we do not know the state (the `s`) of until runtime,
because to use a `SomeDoor`, we pattern-match at runtime.  In general you'll
encounter types at runtime in a variety of different situations (discussed more
deeply in [Part 2][]), but `SomeDoor` is a nice nugget that we can examine to
demonstrate more general points.

A Need for More Expressive Restrictions
---------------------------------------

Let's write a function that "knocks" on a door in IO:

```haskell
knock :: Door s -> IO ()
knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

Hm.  This doesn't feel right.  We can't knock on an opened door..can we?  Is
there a way we can restrict this function to only work on *non-opened* doors?
Or, more generally, is there a way to be more expressive in the manner in which
we can restrict functions?

There are a couple of ways of doing this --- we're going to look at two
possible ways that singletons and the *singletons* library help with.  Both of
these methods allow us to write dependently typed functions that are
"type-safe" in more expressive ways than before.

Note that we'll be exploring ways that are "generalizable" --- to different
types of restrictions that might be more complicated than just "cannot be
`'Opened`".

Dependently Typed Proofs
------------------------

To look at our first way of tackling this restriction problem, we're going to
explore a fun *new application* of singletons and DataKinds.

This new application is the usage of the dependently-typed "proof" to prove
that an operation is legal.  *Proofs* (in the dependently
typed/constructivist/Curry-Howard sense) are witnesses to some type-level
*predicate* or proposition.

A **value-level predicate** in Haskell is (generally) a function of type `a ->
Bool`.  Given a value of type `a`, if the function returns `True`, then the
predicate is satisfied.  If it returns `False`, it is not.

A **type-level predicate** is (generally) a type constructor of kind `k ->
Type`. Given a type of kind `k`, if *a value exists of that type* (or, if a
value can be constructed), then the predicate is satisfied.  If no value
exists, it is not.  That value, if it exists, is called a *witness* or a
*proof*.[^bottom]

[^bottom]: All of this is ignoring the "bottom" value that is an occupant of
every type in Haskell.  We can use bottom to subvert pretty much all proofs in
Haskell, unfortunately, so the discussion from this point forward assumes we
are talking about a subset of Haskell where all values are non-bottom and all
functions are total.

We can define a predicate `Knockable :: DoorState -> Type` as a GADT that only
has values if given `'Closed` and `'Locked`, but not `'Opened`:

```haskell
!!!singletons/Door3.hs "data Knockable"
```

Now, we have a value of type `Knockable 'Closed` and `Knockable 'Locked`
(`KnockClosed` and `KnockLocked`, respectively), but no value of type
`Knockable 'Opened`.  How can we use this?

Well, we can make a version of `knock` that requires a proof that `s` is
`Knockable`:

```haskell
!!!singletons/Door3.hs "knock"
```

`knock` can now only be called with `Closed` and `Locked` doors --- do you see
why?  There is no way to call `knock` with `s ~ 'Opened`...because there is no
way to pass a value of `Knockable 'Opened`.  No such value exists!  There's no
compiler error because it's "not even wrong"!

```haskell
ghci> knock KnockClosed (UnsafeMkDoor @'Closed "Birch")
Knock knock on Birch door!
```

This works well if we want to do things at compile-time

### Let the compiler prove it for you

We can even make it more seamless to use by auto-generating proofs at
compile-time, with a general class like `Auto`:

```haskell
!!!singletons/Door3.hs "class Provable" "instance Provable Knockable 'Closed" "instance Provable Knockable 'Locked"
```

```haskell
ghci> knock auto (UnsafeMkDoor @'Closed "Acacia")
Knock knock on Acacia door!

ghci> knock auto (UnsafeMkDoor @'Opened "Jungle")
COMPILER ERROR!! COMPILER ERROR!!
```

Such a typeclass exists in libraries like *[type-combinators][]* (called
`Known`).  In dependently typed languages like Idris, `auto` is actually a
built-in language keyword that does this automatically!

[type-combinators]: http://hackage.haskell.org/package/type-combinators

### Decidable Predicates

However, all of this only works if you know what `s` is at compile-time.  What
if you don't?  What if you are retrieving `s` at runtime (like from a
`SomeDoor` or `withSomeSing`), or you are forced to handle all possible `s`s?

To do this, we're going to take advantage of a property of some predicates
called "decidability".  We say that a predicate is *decidable* if, for any
input type, we can say whether or not the predicate is satisfiable.

We say that a predicate `P` in Haskell is *decidable* if we can always prove,
for any input, if the predicate holds or does not hold.  Concretely, it means
that we can write a total function:

```haskell
decidePred
    :: Sing x               -- ^ given a type
    -> Decision (P x)       -- ^ return a decision
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

`Decision a` is like a `Maybe a`, except instead of `Nothing`, we include a
proof that the predicate is *not* true.

The `a -> Void` idiom (often called `Not a`, or `Refuted a`) is type we use in
Haskell and other languages to represent the fact that it is impossible to
construct a value of type `a`.  That's because if you could, then you could
give it to an `a -> Void` to get a value of type `Void`, which is impossible to
have.  So, if a possible function `a -> Void` exists, it necessarily means that
a value of type `a` cannot exist.

It's a lot to handle all at once, so let's look at an example.  Is `Knockable`
a decidable predicate?  Yes!

We need to write a function:

```haskell
isKnockable :: Sing s -> Decision (Knockable s)
```

I recommend taking a moment and trying to implement this yourself.  Remember to
enable `-Werror=incomplete-patterns`[^wip] (or at least `-Wall`) to make sure
you're handling all potential pattern matching cases.

[^wip]: Thanks to [Darwin226 on reddit][darwin226] for this tip!

[darwin226]: https://www.reddit.com/r/haskell/comments/9kkbci/introduction_to_singletons_part_3_dependently/e70nc7k/

Are you ready?  Here's a solution:

```haskell
!!!singletons/Door3.hs "isKnockable"
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
!!!singletons/Door3.hs "disproveOpened"
```

And we're good to go!

How does this work?

Well, remember, we have to pattern match on the possible inputs.  However, we
can't use any of the "legal" patterns:

```haskell
disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of
    KnockClosed -> ...    -- not a valid pattern, since it's `Knockable 'Closed`
    KnockLocked -> ...    -- not a valid pattern, since it's `Knockable 'Locked`
```

If you include either of those patterns, GHC will complain.  So, there is no
valid pattern to match on... so `disproveOpened = \case {}` is enough to write
the function `Knockable 'Opened -> Void`, since there is no constructor for a
value of type `Knockable 'Opened` to match on.  This only works because
`disproveOpened` is a **complete pattern match**, and therefore total.

We can use this decision function, finally, to handle an arbitrary `Door` whose
status we not know until runtime:

```haskell
!!!singletons/Door3.hs "knockSomeDoor"
```

While arguably less useful than one provable predicates, a typeclass for
decidable predicates is also possible; the aptly-named [decidable][] package
offers such a typeclass, called `Decidable`!

[decidable]: http://hackage.haskell.org/package/decidable

### Decision

The definition of the `Decision` data type might be surprising if you're first
seeing it.  You want to prove something...so why would you care about the case
where it's "not true"?  Why not just have something like `Maybe`, where you
have `data Decision a = Proved a | Disproved`?

In other words, why do we care about proving both true or false, when it looks
like we only ever use the true situation?  After all, we ignore the `Refuted
(Knockable s)` in our implementation of `knockSomeDoor`.

One answer is that we *do* use the contents of `Disproved` in practice.  In
`knockSomeDoor`, we matched on `Disproved _` and threw away the
counter-proof...however, as we see later in the exercises, there are situations
where the contents of `Disproved` are used.

However, a deeper answer to me is that it keeps the author of the function
accountable.  You can't just say "this predicate isn't true"...you have to
*earn* it.  And often, the act of trying to earn your disproof (or not being
able to) helps you iron out bad assumptions you've made.

For example, if we used `Maybe` instead of `Decision`, we could write:

```haskell
isKnockable :: Sing s -> Maybe s
isKnockable = \case
    SOpened -> Nothing
    SClosed -> Nothing
    SLocked -> Just KnockLocked
```

We might falsely claim that `SClosed` is not knockable.  So, if the user of our
bad `isKnockable` gets `Nothing`, they don't know if their input is not
knockable or knockable...they know *nothing* about the knockability status of
`'Opened` or `'Closed`.

However, we can't write this bad implementation with `Decision`:

```haskell
isKnockable :: Sing s -> Decision s
isKnockable = \case
    SOpened -> Disproved $ \case {}
    SClosed -> Disproved $ -- ????
    SLocked -> Proved KnockLocked
```

There is no valid thing you can put in the `????`!  That's because you need to
write a function of type `Knocked 'Closed -> Void`...but no such (total or
non-partial) function exists.  We can't write `\case {}`, because that's an
incomplete pattern match --- it's missing a match on the `KnockClosed` pattern.

Note also that this is why it's very important to always have
`-Werror=incomplete-patterns` (or at least `-Wall` --- warn all) on when
writing dependently typed proofs, to ensure that GHC warns you when your
pattern matches are incomplete and you know your proof is invalid.

In the examples, we see two more non-trivial examples of decision functions
(`and p q` and `or p q`) that are impossible to implement incorrectly, due to
the structure of the predicates.

### Perspective on Proofs

We just briefly touched on a very simple version of a dependently typed proof,
and how to "prove" properties.

If you have heard things about dependently typed programming before, you might
have heard that a lot of it involves "proving properties about your programs"
and "forcing you to provide proofs for all of your actions".  The idea of a
"proof" might seem a bit scary and "mathematical" to those coming from a
software development world.

However, as we just saw, working with proofs and decisions of proofs can be as
simple as a couple lines of GADTs and dependent pattern matches.

So, when we see a function like:

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

On one level, you can think of proofs as "compiler tricks", or things that
exist only to appease the compiler.  In fact, compilers of languages that
encourage heavy usage of proofs (like Agda, Coq, Idris) actually implement
something called *proof erasure*.  That is, in those languages, values like
`KnockClosed` and `KnockLocked` might never exist at runtime, since they never
actually *do* anything at runtime.  They only exist as ways to limit or enable
specific programs from compiling, and serve no purpose after compilation.  GHC
Haskell does not implement proof erasure at the time of this post (current GHC
version 8.6), but if proofs like this become commonplace, you might be reading
this during a time where GHC Haskell erases proofs like `Knockable`
witnesses![^erase]

[^erase]: Note, however, that we are a little lucky in our case.  In the case
of our implementation of `knock`, we match on a wildcard pattern, so
the input proof is never evaluated.

### The Role of Singletons

Proofs themselves might not play a role at run-time, but generating/deciding
them with types requires being able to pattern match and work with *types* at
run-time.  Because of this, singletons play an important practical role in
working with proofs in Haskell.

After all, remember the type of our decision function:

```haskell
isKnockable :: Sing a -> Decision (Knockable a)
```

The `Sing` allows `isKnockable` to pattern match and inspect the *type* `a` to
create your proof.

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
`('Blah :~:)` is a predicate that a given type is *equal to* `'Blah`.  A value of
type `Knockable s` is a proof that `s` is knockable, and a value of type
`'Blah :~: a` is a proof that `a` is *equal to* `'Blah`.

To see how, note the constructors that it allows. Remember that we limit
`Knockable s` to only having "knockable" `s` by only allowing two constructors,
so we can only construct valid values.  The same thing happens here --
`('Blah :~:)` only has *one single constructor*: `Refl :: 'Blah :~: 'Blah`.  The
only valid constructor is one where the left hand side is equal to the right
hand side.

It also offers the "kindclass" `SDecide`, which provides *decision functions*
for the `(a :~:)` predicate:

```haskell
class SDecide k where
    (%~) :: Sing (a :: k)
         -> Sing (b :: k)
         -> Decision (a :~: b)
```

For example, `Bool` is an instance of `SDecide`, so we have a function:

```haskell
(STrue %~) :: Sing b -> Decision ('True :~: b)
```

which is a decision function to check if `b` is equal to `'True`.  You can sort
of imagine `SDecide` as a type-level `Eq` typeclass, but for "type equality".

Type Level Functions
--------------------

We're now going to look at a different method useful for restricting how we can
call functions.  Something we can do is define a type that expresses
knockable-or-not-knockable, as a value:[^bool]

[^bool]: Really, we could just use `Bool` instead of defining a `Pass` type.
We're just going through a new type for the sake of example, and it can be
useful because a type like `Pass` might potentially have even more
constructors!

```haskell
$(singletons [d|
  data Pass = Obstruct | Allow
  |])
```

And we can write a *type-level function* (implemented as *type family*) from
`DoorState` to a `Pass`:

```haskell
type family StatePass (s :: DoorState) :: Pass where
    StatePass 'Opened = 'Allow
    StatePass 'Closed = 'Obstruct
    StatePass 'Locked = 'Obstruct
```

We've briefly touched on type families before (in talking about `SingKind`),
but, as a quick review: type families act a bit like type-level functions. They
take types as input arguments and return types in return.

We can inspect how type families are applied by using the `:kind!` command in
ghci:

```haskell
ghci> :kind! StatePass 'Opened
'Allow
ghci> :kind! StatePass 'Closed
'Obstruct
```

Like type synonyms, type families can't be partially applied.  They only ever
make sense in "fully applied" form, with all arguments given syntactically.

Armed with this type family, we can write a new version of `knock`:

```haskell
!!!singletons/Door3.hs "knockP"
```

`a ~ b` is a constraint for *type equality*.  This constraint means that
calling `knock` requires that `StatePass s` is *equal to* (or unifies with)
`'Allow`.  So, if we attempt to call `knock` with a `'Locked` door, then
because `StatePass 'Locked` is `'Allow`, the constraint is satisfied and
everyone is happy.  If we attempt to call `knock` with an `'Opened` door,
`StatePass 'Opened` is `'Obstruct`, so the constraint is not satisfied and
everyone is sad.

```haskell
ghci> let door1 = UnsafeMkDoor @'Closed "Oak"
ghci> let door2 = UnsafeMkDoor @'Opened "Spruce"
ghci> knock door1
-- Knock knock on Oak door!
ghci> knock door2
COMPILE ERROR!
--     • Couldn't match type ‘'Allow’ with ‘'Obstruct’
--             arising from a use of ‘knock’
```

### Deciding at Runtime

One nice thing is that, if we know `s` at compile-time, we can call this
function without having to pass any manual proofs.  However, we have to deal
with the same issue as before: what happens if we don't know `s` until runtime?
How do we prove to the compiler that `Passable s` is `'Allow`?

Remember that type families take *types* as inputs, so we can't write:

```haskell
knockSomeDoor :: SomeDoor -> IO ()
knockSomeDoor (MkSomeDoor s d) =
    case StatePass s of
      -- ...
```

because `s`, a value, can't be given to `StatePass`.

What we really want to do is pass `s` (the singleton representing a type) to
`StatePass` (the type family).  And then, we want to match on the *resulting
type*, so we can decide what to do based on the result.

If you think about this predicament long enough, you might begin to see a
solution.  Essentially, we want a function that takes a *singleton* of `s`, and
return a *singleton* of `StatePass s`.

What we want, in the end, is a *mirror* of the type-level function *at the
value level*. We need to write a function of type `Sing s -> Sing (StatePass
s)`: given a singleton of a type, return a singleton of the type family applied
to the type.

```haskell
type family StatePass (s :: DoorState) :: Pass where
    StatePass 'Opened = 'Allow
    StatePass 'Closed = 'Obstruct
    StatePass 'Locked = 'Obstruct

sStatePass :: Sing s -> Sing (StatePass s)
sStatePass = \case
    SOpened -> SAllow
    SClosed -> SObstruct
    SLocked -> SObstruct
```

We have to be very careful with how we define `sStatePass`, because GHC isn't
too smart.  It'll reject any definition that isn't structurally identical to
the type family it's mirroring.

With our new tool, we can now write:

```haskell
!!!singletons/Door3.hs "knockSomeDoorP"
```

First we use `sStatePass s` to check the "pass" of the `s`.  Then, we match on
the `Pass`: if it's `'Obstruct`, like the type signature of `knock` requires, we
can run `knock`.  If not, then we cannot!

### Singletons Library to the Rescue

At the high level, we defined a "function" on types (`StatePass`), using type
families.

And, just like we have to define singletons (`SOpened`, `SClosed`, etc.) at the
value level to mirror what is happening at the type level, we also have to
define *singleton functions* (`sStatePass`) at the value level to mirror what
is happening at the type level.

Defining singletons for our types is a tedious and mechanical process. Defining
singletonized functions for our type families is also similarly tedious and
mechanical.  This is where the *singletons* library comes in: it provides us
Template Haskell tools to automatically define type families and their
associated singleton functions:

```haskell
$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
```

The above declaration would normally declare only the value-level function
`statePass` with the type `DoorSate -> Pass`.

However, with singleton's template haskell, this also generates:[^gen]

*   The *type family* `StatePass (s :: DoorState) :: Pass`, like we defined above
*   The *singleton function* `sStatePass`, with the type `Sing s -> Sing
    (StatePass s)`, like we defined above.

[^gen]: In the spirit of full disclosure, the Template Haskell also generates
some other things (known as *defunctionalization symbols*), which we will be
talking about in the next part of this series.

The naming convention for functions with non-symbol names takes a function like
`myFunction` and generates the type family `MyFunction` and the singleton
function `sMyFunction`.

The naming convention for functions with symbolic names (operators) takes an
operator like `++` and generates the type family `++` (keeping the identical
name) and the singleton function `%++`.[^oper]

[^oper]: Note that this is a change since *singletons-2.4*.  In previous
versions, `++` would generate the type family `:++` and the singleton function
`%:++`.

A Comparison
------------

We went over two methods of using phantom types with the singleton library and
dependent types to restrict how certain functions can be called, on a more
non-trivial level.

Our first method was leveraging "dependently typed proofs".  These are useful
because they are constructed to exploit the "structure" of the types you
create.  Essentially, we create a data type (predicate) in a way so that it is
impossible to create an "invalid" proof. And, often, if we write our proofs in
a clever enough way, we can actually use and combine proofs to generate new
proofs. (More examples in the exercises)

Personally, I find this to be the source of a lot of the "fun" of dependently
typed programming --- our proofs become first class values, and if we define
them in a nice enough way, we can use manipulate them to create new proofs. (A
full exploration of this is well beyond the scope of this post)

However, in practice, carefully constructing predicates and proofs (ones more
complicated than the one we just looked at) requires some up-front cost in
thinking about how to best express your predicate, and is sometimes not
straightforward.

I consider the second method, using type-level functions, to be the more
"mechanical" way, with less upfront cost in thinking time.  For the most part,
if you can write a normal term-level function (something that most Haskell
programmers are comfortable doing), you can write a type-level function. This
is even made simpler with singletons --- you can just write your term-level
relationship as a normal function, and you can now just directly use your
function at the type level.

In fact, consider if there were more than two `Pass` (maybe allow,
obstruct, or partial?).  In that case, we can easily restrict a function based
on the `Pass` being equal to any of the three or more by using the `~`
constraint.  Using the dependently typed proof version, though, we would have
to create a new GADT for each situation.

In a way, type-level functions deliver on the promise of blurring the line
between type and value.  Our term-level functions are now type-level
functions!  We just need to remember to switch our capitalizations!

But this strength is also its weakness.  Remember that the problem of normal
term-level functions was that they are potentially "incorrect", and not
directly verifiable.  So, if you just lift your potentially incorrect
term-level functions to the type level...what you get is potentially incorrect
type-level functions!  You get the *same* logic errors.  Really, writing
type-level functions (unsurprisingly) brings all of the error-proneness of
writing at the term-level.

In contrast, if you use dependently typed proofs correctly, these proofs can
*compose*, and GHC can check that *these proofs compose correctly*, or that the
compositions of your proofs are also valid proofs.  That's because this is
enforced at the *structural level*.  (We'll look at some examples in the
exercises)  GHC can't do that directly with functions;
it can't check that the composition of functions gives correct answers.

These two approaches aren't necessarily mutually exclusive, and you often might
mix the two.  It's good to understand the trade-offs in up-front cost,
expressiveness, and correctness!  But, however way you play, the *singletons*
library is here to make our life easier.

Singleton Library Functions
---------------------------

As we have seen, working with type-level functions with singletons involves at
least two parts --- the type family working on the type-level values, and the
singleton functions mirroring the type family, working on the term-level
singletons.

The singletons library offers template haskell to make working with these
things pretty seamless.  In fact, a good portion of Prelude and base is
promoted and exported by singletons!

You can find most of these in the *Data.Singletons.Prelude* module namespace.
So, with singletons, you get functions like:

```haskell
fst :: (a, b) -> a

type family Fst (t :: (a, b)) :: a

sFst :: Sing t -> Sing (Fst t)
```

and

```haskell
isLeft :: Either a b -> Bool

type family IsLeft (t :: Either a b) :: Bool

sIsLeft :: Sing t -> Sing (IsLeft t)
```

and

```haskell
(++) :: [a] -> [a] -> [a]

type family (xs :: [a]) ++ (ys :: [a]) :: [a]

(%++) :: Sing xs -> Sing ys -> Sing (xs ++ ys)
```

### Promoted Typeclasses

But, how can we promote functions like `(==)` and `max`, which are
typeclass-polymorphic?

With kindclasses (typeclasses for kinds), of course!

Let's remember what we need for these promoted functions to work: the type
families, and the singleton functions.

The *singletons* library handles this by providing each of these in a separate
typeclass.  Let's look at the humble `Eq` typeclass as an example:

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
```

The *singletons* library promotes this as:

```haskell
class PEq a where
    type (x :: a) == (y :: a) :: Bool       -- ^ associated type / type family
    type (x :: a) /= (y :: a) :: Bool

class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
    (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
```

The naming convention is to just add `P` for the "promoted" type family
functions, and `S` for the singleton functions.

In fact, you can even promote your own custom typeclasses:

```haskell
$(singletons [d|
  class MyClass a where
    myFunc :: a -> a
  |])
```

This would create:

1.  The *typeclass* `MyClass` with method `myFunc :: MyClass a => a -> a`
2.  The *promoted typeclass* `PMyClass` with associated type/type family
    `MyFunc (x :: a) :: a`
3.  The *singletonized* typeclass `SMyClass` with method `sMyFunc :: Sing x ->
    Sing (MyFunc x)`.

### Automatically Promoting Instances

The *singletons* library is smart enough to automatically promote instances, as
well, including derived ones!

```haskell
$(singletons [d|
  data Pass = Obstruct | Allow

  instance Eq Pass where
      Obstruct == Obstruct = True
      Obstruct == Allow    = False
      Allow    == Obstruct = False
      Allow    == Allow    = True

      Obstruct /= Obstruct = True
      Obstruct /= Allow    = False
      Allow    /= Obstruct = False
      Allow    /= Allow    = True
  |])
```

This automatically also generates `PEq` and `SEq` instances for `Pass`:

```haskell
ghci> :kind! 'Obstruct == 'Obstruct
'True
ghci> SAllow %== SObstruct
SFalse
```

But, you can also just write:

```haskell
$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])
```

And this works as well!

```haskell
ghci> :kind! Show_ 'Obstruct      -- is named Show_ to not conflict with prelude Show
"Obstruct"
ghci> sMax SObstruct SAllow
SAllow
```


Next Steps
----------

In this article we tackled the problem of more expressive ways to *restrict*
the ways users can manipulate our data types.  We talked about "dependently
typed proofs" (a staple tool of dependently typed programming) and about "type
level functions" (a familiar friend in a new context), their trade-offs, and
how the *singletons* library provides tools to make working with both easier.

When we first looked at the idea of phantom type parameters, using them to
*restrict* how functions are called was definitely one of the promises I made.
By now, this promise has hopefully been fully realized.

However, the *other* promise we made about the usefulness of phantom type
parameters is that we can use them be more expressive in what our functions do.
One huge benefit of using phantom types in this way is that we can express how
our input values relate to our output values in ways that we couldn't before.
(as a simple example, we had previously written `closeDoor :: Door 'Opened ->
Door 'Closed`, which we know closes a door just by looking at its type)

This goes beyond simple restrictions, and we will begin discussing this in the
next post!  We'll explore using type-level functions to express more
non-trivial and complex relationships, and also talk about code re-use using
*higher-order functions* via singleton's defunctionalization system.

That's it for now --- check out the exercises, and feel free to ask any
questions in the comments, or in freenode `#haskell`, where I idle as *jle\`*!

Exercises
---------

Here are some exercises to help cement your understanding of the concepts here!
Feel free to start from [the sample source code][source]; it contains all of
the solutions, but you can delete everything after the comment `-- Exercises`
if you wish to start on your own!

**Remember to enable `-Werror=incomplete-patterns` or `-Wall`** to ensure that
all of your functions are total! None of these implementations should require
any incomplete pattern matches!

!!![solution1]:singletons/Door3.hs "-- | 1."
!!![solution2]:singletons/Door3.hs "refuteRefute"
!!![solution3]:singletons/Door3.hs "data And" "data Or" "decideAnd" "decideOr" "knockableNotOpened" "knockableOrOpened"
!!![solution4]:singletons/Door3.hs "knockedRefute" "refuteKnocked"
!!![solution5]:singletons/Door3.hs "knockRefl" "knockSomeDoorRefl"
!!![solution6]:singletons/Door3.hs "knockInv" "knockSomeDoorInv"
!!![solution7]:singletons/Door3.hs "instance Cycle DoorState" "instance PCycle DoorState" "instance SCycle DoorState"

1.  We talk about predicates as type constructors with type `k -> Type`.  This
    fits a lot of things we've seen before (all instances of `Functor`, for
    example), but some predicates are more interesting than others.

    What is the interpretation of `SDoorState` as a predicate? (remember,
    `SDoorState s` is the type synonym for `Sing (s :: DoorState)`)  What
    "traditional" (that is, `a -> Bool`) predicate does it correspond to?

    What is the type of its *decision function*?  Can you implement it?

    Solution available [here][solution1]!

2.  Now let's practice working with predicates, singletons, and negation via
    `Refuted` together.

    You may have heard of the principle of "double negation", where *not (not
    p)* implies *p*.  So, we should be able to say that `Refuted (Refuted
    (Knockable s))` implies `Knockable s`.[^doubleneg]  If something is not
    "not knockable", then it must be knockable, right?

    [^doubleneg]: Double negation is not true in general, but it is true in the
    case that our predicate is *decidable*.  That's because `Decision a` is
    essentially a witness to the [excluded middle][lem] for that specific
    predicate, from which double negation can be derived.

    [lem]: https://en.wikipedia.org/wiki/Law_of_excluded_middle

    Try writing `refuteRefuteKnockable` to verify this principle --- at least
    for the `Knockable` predicate.

    ```haskell
    !!!singletons/Door3.hs "refuteRefuteKnockable"4
    ```

    While not required, I recommend using `isKnockable` and writing your
    implementation in terms of it!  Use `sing` to give `isKnockable` the
    singleton it needs.

    Solution available [here][solution2]!

    *Hint:* You might find `absurd` (from *Data.Void*) helpful:

    ```haskell
    absurd :: forall a. Void -> a
    ```

    If you have a `Void`, you can make a value of any type![^explosion]

    [^explosion]: It's the good ol' [Principle of Explosion][poe]

    [poe]: https://en.wikipedia.org/wiki/Principle_of_explosion

3.  (This next one is fairly difficult compared to the others, and is only
    tangentially related to singletons, so feel free to skip it!)

    Type-level predicates are logical constructs, so we should be able to
    define concepts like "and" and "or" with them.

    a.  Define a predicate constructor `And` that takes two predicates and
        returns a new predicate.  This new predicate is true (aka, has an
        inhabitant) if and only if the two original predicates are true (aka,
        have inhabitants)

        ```haskell
        !!!singletons/Door3.hs "And"1
        ```

    b.  Define a predicate constructor `Or` that takes two predicates and
        returns a new predicate.  This new predicate is true (aka, has an
        inhabitant) if and only if at least one of the two original predicates
        are true (aka, have inhabitants)

        ```haskell
        !!!singletons/Door3.hs "Or"1
        ```

        There are potentially multiple non-trivial variations of this type.

        Do `And` and `Or` look similar to any types you might have encountered
        in the past?  Maybe, perhaps, similiar to types that are a part of
        basic beginner Haskell concepts?

    c.  Maybe surprisingly, `And p q` and `Or p q` are decidable if `p` and `q`
        are.  Can we write the decision functions?

        ```haskell
        !!!singletons/Door3.hs "decideAnd"5 "decideOr"5
        ```

        These functions actually demonstrate, I feel, why `Decision` having
        both a `Proved a` and `Disproved (Refuted a)` branch is very useful.
        This is because, if you wrote the *structure* of `And` and `Or`
        correctly, it's *impossible* to incorrectly define `decideAnd` and
        `decideOr`.  You can't accidentally say false when it's true, or true
        when it's false --- your implementation is guarunteed correct.


    d.  Now let's use `And` and `Or` to prove some useful facts about
        `Knockable` and `('Opened :~:)`.  We know that it's impossible for
        something to be both `Knockable` *and* `('Opened :~:)` (that is, both
        knockable *and* equal to `'Opened`).  Write such a witness:

        ```haskell
        !!!singletons/Door3.hs "knockableNotOpened"3
        ```

        We also know that a given `DoorState` is either `Knockable` or
        `('Opened :~:)`...there's no in-between.  Write such a witness:

        ```haskell
        !!!singletons/Door3.hs "knockableOrOpened"3
        ```


    Solutions available [here][solution3]!

4.  Instead of creating an entire `Knocked` type, we could have just said "as
    long as the door is not `'Opened`, you can knock".  This means we could
    write `knock` as:


    ```haskell
    knock :: Refuted (s :~: 'Opened) -> Door s -> IO ()
    ```

    Which we must pass a proof that `s` is not equal to `'Opened` in order to
    open our door.

    Is this really the same thing?  Is `Refuted (s :~: 'Opened)` the same
    thing as `Knockable s`?

    Let's try to say that the two things are the same!  Write the following
    functions to show that `Refuted (s :~: 'Opened)` is the same logical
    predicate as `Knockable s`!

    ```haskell
    !!!singletons/Door3.hs "knockedRefute"4 "refuteKnocked"4
    ```

    Solution available [here][solution4]!

    *Note:* `knockedRefute` is fairly straightforward, but `refuteKnocked` is
    definitely trickier, so don't be discouraged!

    *Hint:* See the note about `absurd` from Exercise 2!

5.  On our type level function version of `knock`, we wrote, with a constraint:

    ```haskell
    knock :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
    knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
    ```

    We can muddy the waters a bit, for fun, by having this take a proof of the
    constraint instead:

    ```haskell
    !!!singletons/Door3.hs "knockRefl"
    ```

    Rewrite a version of `knockSomeDoor` in terms of `knockRefl`, called
    `knockSomeDoorRefl`:

    ````haskell
    !!!singletons/Door3.hs "knockSomeDoorRefl"4
    ````

    Remember not to use `knock`!

    Solution available [here][solution5].

    Assume that `DoorState` has an instance of `SDecide`, so you can use
    `(%~)`.  This should be derived automatically as long as you derive `Eq`:

    ```haskell
    $(singletons [d|
      data DoorState = Opened | Closed | Locked
        deriving (Show, Eq)
      |])
    ```

6.  With the function that inverts `Pass`:

    ```haskell
    $(singletons [d|
      invertPass :: Pass -> Pass
      invertPass Obstruct = Allow
      invertPass Allow    = Obstruct
    |])
    ```

    Implement `knock` in a way that lets you knock if `invertPass` is `Allow`:

    ```haskell
    !!!singletons/Door3.hs "knockInv"
    ```

    And write `knockSomeDoor` in terms of it:

    ````haskell
    !!!singletons/Door3.hs "knockSomeDoorInv"4
    ````

    Remember again to implement it in terms of `knockInv`, *not* `knock`.

    Solution available [here][solution6]!

7.  Let's work with a toy typeclass called `Cycle`, based on `Enum`

    ```haskell
    $(singletons [d|
      class Cycle a where
        next :: a -> a
        prev :: a -> a
      |])
    ```

    `next` is like `succ`, but loops over to the first item after the last
    constructor.  `prev` is like `pred`, but loops over to the last item if
    pred-ing the first item

    ```haskell
    !!!singletons/Door3.hs "instance Cycle DoorState"
    ```

    Can you manually promote this instance for `DoorState` to the type level?

    ```haskell
    !!!singletons/Door3.hs "instance PCycle DoorState"1 "instance SCycle DoorState"1
    ```

    Solution available [here][solution7]!

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts.  Very special
thanks to my two supporters at the "Amazing" level on [patreon][], Sam Stites
and Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview

Thanks also to [Koz Ross][koz_] for helping proofread this post!

[koz_]: https://twitter.com/KozRoss
