---
title: "Introduction to Singletons (Part 4)"
categories: Haskell
series: Introduction to Singletons
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2018/09/28 22:02:02
identifier: singletons-4
slug: introduction-to-singletons-4
---

Hi again!  Welcome back; let's jump right into part 4 of our journey through
the *singleton design pattern* and the great *[singletons][]* library!

[singletons]: http://hackage.haskell.org/package/singletons

Please check out [the first three parts of the series][series] and make sure
you are comfortable with them before reading on.  I definitely also recommend
trying out some or all of the exercises, since we are going to be building on
the concepts in those posts in a pretty heavy way.

[series]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

Today we're going to jump straight into *functional programming* at the type
level!

Review
------

Just as a quick review, this entire series we have been working with a `Door`
type:

```haskell
$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s
```

And we talked about using `Sing s`, or `SDoorState s`, to represent the state
of the door (in its type) as a run-time value.  We've been using a wrapper to
existentially hide the door state type, but also stuffing in a singleton to let
us recover the type information once we want it again:

```haskell
data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing (UnsafeMkDoor mat)
```

In Part 3 we talked about a `Pass` data type that we used to talk about whether
or not we can walk through or knock on a door:

```haskell
$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq)
  |])
```

And we defined type-level functions on it using *singletons* Template Haskell:

```haskell
$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
```

This essentially generates these three things:

```haskell
statePass :: DoorState -> Pass
statePass Opened = Allow
statePass Closed = Obstruct
statePass Locked = Obstruct

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

And we can use `StatePass` as a type-level function while using `sStatePass` to
manipulate the singletons representing `s` and `StatePass s`.

We used this as a constraint to restrict how we can call our functions:

```haskell
knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

But then we wondered...is there a way to not only *restrict* our functions,
but to describe how the inputs and outputs are related to each other?

Inputs and Outputs
------------------

In the past we have settled with very simple relationships, like:

```haskell
closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m
```

This means that the relationship between the input and output is that the input
is opened...and is then closed.

However, armed with promotion of type-level functions, writing more complex
relationships becomes fairly straightforward!

We can write a function `mergeDoor` that "merges" two doors together, in
sequence:

```haskell
mergeDoor :: Door s -> Door t -> Door ????
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e
```

A merged door will have a material that is composite of the original materials.
But, what will the new `DoorState` be?  What goes in the `???` above?

Well, if we can write the function as a normal function in
values...*singletons* lets us use it as a function on types.  Let's write that
relationship.  Let's say merging takes on the higher "security" option ---
merging opened with locked is locked, merging closed with opened is closed,
merging locked with closed is locked.

```haskell
$(singletons [d|
  mergeState :: DoorState -> DoorState -> DoorState
  mergeState Opened d      = d
  mergeState Closed Opened = Closed
  mergeState Closed Closed = Closed
  mergeState Closed Locked = Locked
  mergeState Locked _      = Locked
  |])

-- Alternatively, taking advantage of the derived Ord instance:
$(singletons [d|
  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max
  |])
```

This makes writing `mergeDoor`'s type fairly straightforward!

```haskell
mergeDoor
    :: Door s
    -> Door t
    -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e
```

And, with the help of singletons, we can also write this for our doors where we
don't know the types until runtime:

```haskell
mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergSomeDoor (MkSomeDoor s d) (MkSomeDoor t e) =
    MkSomeDoor (sMergeState s t) (mergeDoor d e)
```

To see why this typechecks properly, compare the types of `sMergeState` and
`mergeDoor`:

```haskell
sMergeState :: Sing s -> Sing t -> Sing (MergeState s t)
mergeDoor   :: Door s -> Door t -> Sing (MergeState s t)

MkSomeDoor  :: Sing (MergeState s t) -> Door (MergeState s t) -> SomeDoor
```

Because the results both create types `MergeState s t`, `MkSomeDoor` is happy
to apply them to each other, and everything typechecks.  However, if, say, we
directly stuffed `s` or `t` into `MkSomeDoor`, things would fall apart and not
typecheck.

<!-- TODO: exercise where Knockable s -> Knockable t -> Knockable (MergeState s
t) -->

And so now we have full expressiveness in determining input and output
relationships!  Once we unlock the power of type-level functions with
*singletons*, writing type-level relationships become as simple as writing
value-level ones.  If you can write a value-level function, you can write a
type-level function!

### Kicking it up a notch

Alright, so let's see how far we can really take this!

Let's make a data type that represents a *series of hallways*, each linked by a
door.  A hallway is either an empty stretch with no door, or two hallways
linked by a door.  We'll structure it like a linked list, and store the list of
all door states as a type-level list as a type parameter:

```haskell
data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]
    -- ^ end of the hallway, a stretch with no doors
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)
    -- ^ A door connected to a hallway is a new
    --   hallway, and we track the door's state in the list
    --   of hallway door states

infixr 5 :<#
```

So we might have:

```haskell
ghci> let door1 = UnsafeMkDoor @'Closed "Oak"
ghci> let door2 = UnsafeMkDoor @'Opened "Spruce"
ghci> let door3 = UnsafeMkDoor @'Locked "Acacia"
ghci> :t door1 :<# door2 :<# door3 :<# HEnd
Hallway '[ 'Closed, 'Opened, 'Locked ]
```

That is, a `Hallway '[ s, t, u ]` is a hallway consisting of a `Door s`, a
`Door t`, and a `Door u`, constructed like a linked list in Haskell.

<!-- TODO: append halls as exerise -->

Now, let's write a function to *collapse all doors in a hallway down to a
single door*:

```haskell
collapseHallway :: Hallway ss -> Door ?????
```

Basically, we want to merge all of the doors one after the other, collapsing it
until we have a single door state.  Luckily, `MergeState` is both commutative
and associative and has an identity, so this can be defined sensibly.

First, let's think about the type we want.  What will the result of merging
`ss` be?

We can pattern match and collapse an entire list down item-by-item:

```haskell
$(singletons [d|
  mergeStates :: [DoorState] -> DoorState
  mergeStates []     = Opened               -- ^ the identity of mergeState
  mergeStates (s:ss) = s `mergeState` mergeStates ss
  |])
```

Again, remember that this also defines the type family `MergeStateList` and the
singleton function `sMergeStateList :: Sing ss -> Sing (MergeStateList ss)`.

With this, we can write `collapseHallway`:

```haskell
collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds
```

Now, because the structure of `collapseHallway` perfectly mirrors the structure
of `mergeStates`, this all typechecks, and we're done!

```haskell
ghci> collapseHallway (door1 :<# door2 :<# door3 :<# HEnd)
UnsafeMkDoor "Oak and Spruce and Acacia and End of Hallway"
    :: Door 'Locked
```

Note one nice benefit -- the door state of
`collapseHallway (door1 :<# door2 :<# door3 :<# HEnd)` is known at compile-time
to be `Door 'Locked`, if the types of all of the component doors are also known!

Functional Programming
----------------------

We went over that all a bit fast, but some of you might have noticed that the
definition of `mergeStates` bears a really strong resemblance to a very common
Haskell list processing pattern:
 
```haskell
mergeStates :: [DoorState] -> DoorState
mergeStates []     = Opened               -- ^ the identity of mergeState
mergeStates (s:ss) = s `mergeState` mergeStates ss
```

We replace all `[]` with `Opened`, and all `(:)` with `mergeState`.  Yup ---
this is exactly a `foldr`!

```haskell
mergeStates :: [DoorState] -> DoorState
mergeState = foldr mergeState Opened
```

In Haskell, we are always encouraged to use higher-order functions whenever
possible instead of explicit recursion, both because explicit recursion opens
you up to a lot of potential bugs, and also because using established
higher-order functions make your code more readable.

So, as Haskellers, let us hold ourselves to a higher standard and not be
satisfied with a `MergeState` written using explicit recursion.  Let us instead
go *full fold*!  ONWARD HO!

### The Problem

Initial attempts to write a higher-order type-level function as a type family,
however, serve to temper our enthusiasm.

```haskell
type family MergeState (s :: DoorState) (t :: DoorState) :: DoorState where
    MergeState s t = Max s t

type family Foldr (f :: j -> k -> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = f x (Foldr f z xs)
```

So far so good right?  So we should expect to be able to write `MergeStateList`
using `Foldr`, `MergeState`, and `'Opened`

```haskell
type MergeStateList ss = Foldr MergeState 'Opened ss
```

Ah, but the compiler is here to tell you this isn't allowed in Haskell:

```
    • The type family ‘MergeState’ should have 2 arguments, but has been given none
    • In the equations for closed type family ‘MergeStateList’
      In the type family declaration for ‘MergeStateList’
```

What happened?  To figure out, we have to remember that pesky restriction on
type synonyms and type families: they *cannot* be partially applied, and must
always be fully applied.  For the most part, only *type constructors* (like
`Maybe`, `Either`, `IO`) and lifted DataKinds data constructors (like `'Just`,
`'(:)`) in Haskell can ever be partially applied at the type level.  We
therefore can't use `MergeState` as an argument to `Foldr`, because
`MergeState` must always be fully applied.

Unfortunately for us, this makes our `Foldr` effectively useless.  That's
because we're always going to want to pass in type families (like
`MergeState`), so there's pretty much literally no way to ever actually call
`Foldr` except with type constructors or lifted DataKinds data constructors.

So...back to the drawing board?

Defunctionalization
-------------------

I like to mentally think of the *singletons* library as having two parts: the
first is linking lifted DataKinds types with run-time values to allow us to
manipulate types at runtime as first-class values.  The second is a system for
effective *functional programming* at the type level.

To make a working `Foldr`, we're going to have to jump into that second half:
*[defunctionalization][]*.

[defunctionalization]: https://en.wikipedia.org/wiki/Defunctionalization

Defunctionalization is a technique invented in the early 70's to convert
higher-order functions into first-order functions.  The main idea is:

*   Instead of working with *functions*, work with *symbols representing
    functions*.
*   Build your final functions and values by composing and combining these
    symbols.
*   At the end of it all, have a single `Apply` function interpret all of your
    symbols and produce the value you want.

In *singletons* these symbols are implemented as "dummy" empty data
constructors, and `Apply` is a type family.

To help us understand singleton's defunctionalization system better, let's
build our own defunctionalization system from scratch.

First, a little trick to make things easier to read:

```haskell
data TyFun a b
type a ~> b = TyFun a b -> Type

infixr 0 ~>
```

### Our First Symbols

Now we can define a dummy data type like `Id`, which represents the identity
function `id`:

```haskell
data Id :: a ~> a
```


Don't worry too much about `TyFun`, it's all just a type-level tag that makes
it convenient to write `Id :: a ~> a`.  The actual kind of `Id` is `Id :: TyFun
a a -> Type`; you can imagine `TyFun a a` as a phantom parameter that signifies
that `Id` represents a function from `a` to `a`.

Now, `Id` is not a function...it's a *dummy type constructor* that *represents*
a function `a -> a`.  A type constructor of kind `a ~> a` represents a
*defunctionalization symbol* -- a type constructor that represents a function
from `a` to `a`.

To interpret it, we need to write our global interpreter function:

```haskell
type family Apply (f :: a ~> b) (x :: a) :: b
```

That's the syntax for the definition of an *open* type family in Haskell:
users are free to add their own instances, just like how type classes are
normally open in Haskell.

Let's tell `Apply` how to interpret `Id`:

```haskell
type instance Apply Id x = x
```

The above is the actual function definition, like writing `id x = x`.  We can
now *call* `Id` to get an actual type in return:

```haskell
ghci> :kind! Apply Id 'True
'True
```

Let's define another one!  We'll implement `Not`:

```haskell
data Not :: Bool ~> Bool
type instance Apply Not 'False = 'True
type instance Apply Not 'True  = 'False
```

We can try it out:

```haskell
ghci> :kind! Apply Not 'True
'False
ghci> :kind! Apply Not 'False
'True
```

It can be convenient to define an infix synonym for `Apply`:

```haskell
type f @@ a = Apply f a

infixl 9 @@
```

Then we can wrote:

```haskell
ghci> :kind! Not @@ 'False
'True
ghci> :kind! Id @@ 'True
'True
```

Remember, `Id` and `Not` are not actual functions --- they're just dummy data
types ("defunctionalization symbols"), and we define the functions they
represent through the global `Apply` type function.

### A Bit of Principle

So we've got the basics of defunctionalization --- instead of using functions
directly, use dummy symbols that encode your functions that are interpreted
using `Apply`.  Let's add a bit of principle to make this all a bit more
scalable.

The singletons library adopts a few conventions for linking all of these
together.  Using the `Not` function as an example, if we wanted to lift the
function:

```haskell
not :: Bool -> Bool
not False = True
not True  = Flse
```

We already know about the type family and singleton function this would
produce:

```haskell
type family Not (x :: Bool) :: Bool where
    Not 'False = 'True
    Not 'True  = 'False

sNot :: Sing x -> Sing (Not x)
sNot SFalse = STrue
sNot STrue  = SFalse
```

But the singletons library also produces the following *defunctionalization
symbols*, according to a naming convention:

```haskell
data NotSym0 :: Bool ~> Bool
type instance Apply NotSym0 x = Not x

-- also generated for consistency
type NotSym1 x = Not x
```

`NotSym0` is the *defunctionalization symbol* associated with the `Not` type
family, defined so that `NotSym0 @@ x = Not x`.  Its purpose is to allow us to
*pass in* `Not` as an *un-applied function*.  The `Sym0` suffix is a naming
convention, and the 0 stands for "expects 0 arguments".  Similarly for
`NotSym1` -- the 1 stands for "expects 1 argument".

Let's look at a slightly more complicated example -- a two-argument function.
Let's define the boolean "and":

```haskell
$(singletons [d|
  and :: Bool -> (Bool -> Bool)
  and False _ = False
  and True  x = x
  ])
```

this will generate:

```haskell
type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'False x = 'False
    And 'True  x = x

sAnd :: Sing x -> Sing y -> Sing (And x y)
sAnd SFalse x = SFalse
sAnd STrue  x = x
```

And the defunctionalization symbols:

```haskell
data AndSym0 :: Bool ~> (Bool ~> Bool)
type instance Apply AndSym0 x = AndSym1 x

data AndSym1 (x :: Bool) :: (Bool ~> Bool)
-- or
data AndSym1 :: Bool -> (Bool ~> Bool)
type instance Apply (AndSym1 x) y = And x y

type AndSym2 x y = And x y
```

`AndSym0` is a defunctionalization symbol representing a "fully unapplied"
version of `And`. `AndSym1 x` is a defunctionalization symbol representing a
"partially applied" version of `And` --- partially applied to `x` (its kind is
`AndSym1 :: Bool -> (Bool ~> Bool)`).

The application of `AndSym0` to `x` gives you `AndSym1 x`:

```haskell
ghci> :kind! AndSym0 @@ 'False
AndSym1 'False
```

Remember its kind `AndSym0 :: Bool ~> (Bool ~> Bool)`
(or just `AndSym0 :: Bool ~> Bool ~> Bool`): it takes a `Bool`, and returns a
`Bool ~> Bool` defunctionalization symbol.

The application of `AndSym1 x` to `y` gives you `And x y`:

```haskell
ghci> :kind! AndSym1 'False @@ 'True
'False
ghci> :kind! AndSym1 'True  @@ 'True
'True
```

A note to remember: `AndSym1 'True` is the defunctionalization symbol, and
*not* `AndSym1` itself.  `AndSym1` has kind `Bool -> (Bool ~> Bool)`, but
`AndSym1 'True` has kind `Bool ~> Bool` --- the kind of a defunctionalization
symbol.

One extra interesting defunctionalization symbol we can write: we turn lift any
type constructor into a "free" defunctionalization symbol:

```haskell
data TyCon1
        :: (j -> k)     -- ^ take a type constructor
        -> (j ~> k)     -- ^ return a defunctionalization symbol

-- alternatively
data TyCon1 (t :: j -> k) :: j ~> k

type instance Apply (TyCon1 t) a = t a
```

Basically the `Apply` instance just applies the type constructor `t` to its
input `a`.

```haskell
ghci> :kind! TyCon1 Maybe @@ Int
Maybe Int
ghci> :kind! TyCon1 'Right @@ 'False
'Right 'False
```

We can use this to give a normal `j -> k` type constructor to a function that
expects a `j ~> k` defunctionalization symbol.

Bring Me a Higher Order
-----------------------

Okay, so now we have these tokens that represent "unapplied" versions of
functions.  So what?

Well, remember the problem with our implementation of `Foldr`?  We couldn't
pass in a type family, since type families must be passed fully applied.  So,
instead of having `Foldr` expect a type family...we can make it expect a
*defunctionalization symbol* instead!  Remember, defunctionalization symbols
represent the "unapplied" versions of type families, so they are exactly the
tools we need!

```haskell
type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs
```

<!-- TODO: note on new foldr -->

The difference is that instead of taking a type family or type constructor `f :: j
-> k -> k`, we have it take the *defunctionalization symbol* `f :: j ~> (k ~>
k)`.

Instead of taking a type family or type constructor, we take that dummy type
constructor.

Now we just need to have our defunctionalization symbols for `MergeStateList`:

```haskell
type family MergeState (s :: DoorState) (t :: DoorState) :: DoorState where
    MergeState s t = s

data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
type instance Apply MergeStateSym0 s = MergeStateSym1 s

data MergeStateSym1 :: DoorState -> DoorState ~> DoorState
type instance Apply (MergeStateSym1 s) t = MergeState s t

type MergeStateSym2 s t = MergeState s t
```

And now we can write `MergeStateList`!

```haskell
type MergeStateList ss = Foldr MergeStateSym0 'Opened ss
```

(If you "see" `MergeStateSym0`, you should *read* it was `MergeState`, but
partially applied)

This compiles!

```haskell
ghci> :kind! MergeStateList '[ 'Closed, 'Opened, 'Locked ]
'Locked
ghci> :kind! MergeStateList '[ 'Closed, 'Opened ]
'Closed
```

```haskell
collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds
```

### Singletons to to make things nicer

Admittedly this is all a huge mess of boilerplate.  The code we had to write
more than tripled, and we also have an unsightly number of defunctionalization
symbols and `Apply` instance boilerplate for every function.

Luckily, the *singletons* library is here to help.  You can just write:

```haskell
$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)

  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max

  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)

  mergeStateList :: [DoorState] -> DoorState
  mergeStateList = foldr mergeState Opened
  |])
```

And all of these defunctionalization symbols are generated for you;
*singletons* is also able to recognize that `foldr` is a higher-order function
and translate its lifted version to take a defunctionalization
symbol `a ~> b ~> b`.

It's okay to stay "in the world of singletons" for the most part, and let
singletons handle the composition of functions for you.  However, it's still
important to know what the *singletons* library generates, because sometimes
it's still useful to manually create defunctionalization symbols and work with
them.

Thoughts on Symbols
-------------------

Defunctionalization symbols may feel like a bit of a mess, and the naming
convention is arguably less than aesthetically satisfying.  But, as you work
with them more and more, you start to appreciate them on a deeper level.

At the end of the day, you can compare defunctionalization as turning
"functions" into just constructors you can *match* on, just like any other data
or type consturctor.  That's because they *are* just type constructors!

In a sense, defining defunctionalization symbols is a lot like working with
*pattern synonyms* of your functions, instead of directly passing the functions
themselves.  At the type family and type class level, you can "pattern match"
on these functions.

[matchable]: https://github.com/ghc-proposals/ghc-proposals/pull/52#issuecomment-300485400

For a comparison at the value level -- you can't pattern match on `(+)`, `(-)`,
`(*)`, and `(/)`:

```haskell
-- Doesn't work like you think it does
invertOperation :: (Double -> Dobule -> Double) -> (Double -> Double -> Double)
invertOperation (+) = (-)
invertOperation (-) = (+)
invertOperation (*) = (/)
invertOperation (/) = (*)
```

You can't quite match on the equality of functions to some list of patterns.
But, what you *can* do is create constructors representing your functions, and
match on those.

This essentially fixes the "type lambda problem" of type inference and
typeclass resolution.  You can't match on arbitrary lambdas, but you *can*
match on dummy constructors representing type functions.

And a bit of the magic here, also, is the fact that you don't always need to
make our own defunctionalization symbols from scratch --- you can create them
based on other ones in a compositional way.

For example, suppose we wanted to build defunctionalization symbols for
`MergeStateList`.  We can actually build them directly from defunctionalization
symbols for `Foldr`!

Check out the defunctionalization symbols for `Foldr`:

```haskell
data FoldrSym0 :: (j ~> k ~> k) ~> k ~> [j] ~> k

data FoldrSym1 :: (j ~> k ~> k) -> k ~> [j] ~> k

data FoldrSym2 :: (j ~> k ~> k) -> k -> [j] ~> k
```

We can actually use these to *define* our `MergeStateList` defunctionalization
symbols, since *defunctionalization symbols are first-class*:

```haskell
type MergeStateListSym0 = FoldrSym2 MergeStateSym0 'Opened
```

And you can just write `collapseHallway` as:

```haskell
collapseHallway :: Hallway ss -> Door (MergeStateListSym0 @@ ss)
-- or
collapseHallway :: Hallway ss -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
```

You never have to actually define `MergeStateList` as a function or type
family!

The whole time, we're just building defunctionalization symbols in terms of
other defunctionalization symbols.  And, at the end, when we finally want to
interpret the complex function we construct, we use `Apply`, or `@@`.

You can think of `FoldrSym1` and `FoldrSym2` as *defunctionalization
symbol constructors* -- they're combinators that take in defunctionalization
symbols (like `MergeStateSym0`) and *return new ones*.

### Sigma

Let's look at a nice tool that is made possible using defunctionalization
symbols: *dependent pairs*.  I talk a bit about dependent pairs (or dependent
sums) in [part 2][] of this series, and also in my [dependent types in
Haskell][dth] series.

[part 2]: https://blog.jle.im/entry/introduction-to-singletons-2.html
[dependent types in Haskell]: https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html

Essentially, a dependent pair is a tuple where the *type* of the second
field depends on the *value* of the first one.  This is basically what
`SomeDoor` was:

```haskell
data SomeDoor :: Type where
    MkSomeDoor :: Sing x -> Door x -> SomeDoor
```

The *type* of the `Door x` depends on the *value* of the `Sing x`, which you
can read as essentially storing the `x`.

We made `SomeDoor` pretty ad-hoc.  But what if we wanted to make some other
predicate?  Well, we can make a *generic* dependent pair by *parameterizing it
on  he dependence* between the first and second field.

```haskell
data Sigma k :: (k ~> Type) -> Type where
    (:&:) :: Sing x -> (f @@ x) -> Sigma k f
```

If you squint carefully, you can see that `Sigma k` is just `SomeDoor`,
but *parameterized over `Door`*.  Instead of always holding `Door x`, we can have
it parameterized on an arbitrary function `f` and have it hold an `f @@ x`.

We can actually define `SomeDoor` in terms of `Sigma`:

```haskell
type SomeDoor = Sigma DoorState (TyCon1 Door)
```

(Remember `TyCon1` is the defunctionalization symbol constructor that turns any
normal type constructor `j -> k` into a defunctionalization symbol `j ~> k`)

That's because a `Sigma DoorState (TyCon1 Door)` contains a `Sing (x ::
DoorState)` and a `TyCon1 Door @@ x`, or a `Door x`.

This is a simple relationship, but one can imagine a `Sigma` parameterized on
an even more complex type-level function.  We'll explore more of these in the
exercises!

### Singletons of Functions

One last thing to tie it all together -- let's write `collapseHallway` in a way
that we don't know the types of the doors.

Luckily, we now have a `SomeHallway` type for free:

```haskell
type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)
```

The easy way would be to just use `sMergeStateList` that we defned:

```haskell
collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss
                             :&: collapseHallway d
```

But what if we didn't write `sMergeStateList`, and we constructed our
defunctionalization symbols from scratch?

```haskell
collapseHallway
    :: Hallway ss
    -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = ???    -- what goes here?
                             :&: collapseHallway d
```

This will be our final defunctionalization lesson.  How do we turn a singleton
of `ss` into a singleton of `FoldrSym2 MergeStateSym0 'Opened @@ s` ?

First -- we have `Foldr` at the value level, as `sFoldr`.  We glossed over this
earlier, but *singletons* generates the following function for us:

```haskell
type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs

sFoldr
    :: Sing (f :: j ~> k ~> k)
    -> Sing (z :: k)
    -> Sing (xs :: [j])
    -> Sing (Foldr f z xs :: k)
sFoldr f z SNil           = z
sFoldr f z (x `SCons` xs) = (f @@ x) @@ sFoldr f z xs
```

Where `(@@) :: Sing f -> Sing x -> Sing (f @@ x)` (or `applySing`) is the
singleton/value-level counterpart of `Apply` or `(@@)`.

So we can write:

```haskell
collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sFoldr ???? SOpened ss
                             :&: collapseHallway d
```

But how do we get a `Sing MergeStateSym0`?

We can use the `singFun` family of functions:

```haskell
singFun2 @MergeStateSym0 :: Sing MergeStateSym0
```

And finally, we get our answer:

```haskell
collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sFoldr (singFun2 @MergeStateSym0) SOpened ss
                             :&: collapseHallway d
```

Closing Up
----------


<!-- For example, there is only one inhabitant of the type `Sigma DoorState -->
<!-- (MergeStateSym1 'Locked)`.  What is it? -->

<!-- TODO: make hallway form somedoors -->

<!-- For --> 

<!-- We can then even create our own --> 

<!-- TODO: Exercise, store proof of evenness -->

<!-- But that's just a simple example.  Here's an example that stores a proof that -->
<!-- the number `n` is even: -->

<!-- ```haskell -->
<!-- data IsDoubleOf n :: Nat ~> Nat -->
<!-- type instance Apply (IsDoubleOf n) m = n :~: (m * 2)  -- * is provided in GHC.TypeNats -->

<!-- type IsEven n = Sigma Nat (IsDoubleOf n) -->
<!-- ``` -->

<!-- And in fact, you never even have to fully write `MergeStateList` at all -- you -->
<!-- can just directly -->

<!-- /* You can do this all within the */ -->
<!-- /* world */ -->


<!-- /* TODO: sigma */ -->

<!-- /* TODO: SingI instance for functions */ -->
