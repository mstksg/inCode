Introduction to Singletons (Part 4)

====================================

> Originally posted by [Justin Le](https://blog.jle.im/) on October 22, 2018.
> [Read online!](https://blog.jle.im/entry/introduction-to-singletons-4.html)

Hi again! Welcome back; let's jump right into the fourth and final part of our
journey through the *singleton design pattern* and the great
*[singletons](http://hackage.haskell.org/package/singletons)* library.

Please check out [the first three parts of the
series](https://blog.jle.im/entries/series/+introduction-to-singletons.html) and
make sure you are comfortable with them before reading on. I definitely also
recommend trying out some or all of the exercises, since we are going to be
building on the concepts in those posts in a pretty heavy way.

Today we're going to jump straight into *functional programming* at the type
level. Code in this post is built on *GHC 8.6.1* with the
*[nightly-2018-09-29](https://www.stackage.org/nightly-2018-09-29)* snapshot
(so, *singletons-2.5*). However, unless noted, all of the code should still work
with *GHC 8.4* and *singletons-2.4*.

## Review

Just as a quick review, this entire series we have been working with a `Door`
type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4.hs#L24-L33

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor
```

And we talked about using `Sing s`, or `SDoorState s`, to represent the state of
the door (in its type) as a run-time value. We've been using a wrapper to
existentially hide the door state type, but also stuffing in a singleton to let
us recover the type information once we want it again:

``` haskell
data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing (mkDoor dsSing mat)
```

In Part 3 we talked about a `Pass` data type that we used to talk about whether
or not we can walk through or knock on a door:

``` haskell
$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])
```

And we defined type-level functions on it using *singletons* Template Haskell:

``` haskell
$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
```

This essentially generates these three things:

``` haskell
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

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door3.hs#L89-L90

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
```

But then we wondered...is there a way to not only *restrict* our functions, but
to describe how the inputs and outputs are related to each other?

## Inputs and Outputs

In the past we have settled with very simple relationships, like:

``` haskell
closeDoor :: Door 'Opened -> Door 'Closed
```

This means that the relationship between the input and output is that the input
is opened...and is then closed.

However, armed with promotion of type-level functions, writing more complex
relationships becomes fairly straightforward!

We can write a function `mergeDoor` that "merges" two doors together, in
sequence:

``` haskell
mergeDoor :: Door s -> Door t -> Door ????
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e
```

A merged door will have a material that is composite of the original materials.
But, what will the new `DoorState` be? What goes in the `???` above?

Well, if we can write the function as a normal function in values...*singletons*
lets us use it as a function on types. Let's write that relationship. Let's say
merging takes on the higher "security" option --- merging opened with locked is
locked, merging closed with opened is closed, merging locked with closed is
locked.

``` haskell
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

This makes writing `mergeDoor`'s type clean to read:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4.hs#L43-L47

mergeDoor
    :: Door s
    -> Door t
    -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e
```

And, with the help of singletons, we can also write this for our doors where we
don't know the types until runtime:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4.hs#L49-L51

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (MkSomeDoor s d) (MkSomeDoor t e) =
    MkSomeDoor (sMergeState s t) (mergeDoor d e)
```

To see why this typechecks properly, compare the types of `sMergeState` and
`mergeDoor`:

``` haskell
sMergeState :: Sing s -> Sing t -> Sing (MergeState s t)
mergeDoor   :: Door s -> Door t -> Door (MergeState s t)

MkSomeDoor  :: Sing (MergeState s t) -> Door (MergeState s t) -> SomeDoor
```

Because the results both create types `MergeState s t`, `MkSomeDoor` is happy to
apply them to each other, and everything typechecks. However, if, say, we
directly stuffed `s` or `t` into `MkSomeDoor`, things would fall apart and not
typecheck.

And so now we have full expressiveness in determining input and output
relationships! Once we unlock the power of type-level functions with
*singletons*, writing type-level relationships become as simple as writing
value-level ones. If you can write a value-level function, you can write a
*type-level* function.

### Kicking it up a notch

How far we can really take this?

Let's make a data type that represents a *series of hallways*, each linked by a
door. A hallway is either an empty stretch with no door, or two hallways linked
by a door. We'll structure it like a linked list, and store the list of all door
states as a type-level list as a type parameter:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4.hs#L53-L61

data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
                                --   doors
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
                                --   hallway, and we track the door's state
                                --   in the list of hallway door states
infixr 5 :<#
```

(If you need a refresher on type-level lists, check out [the quick introduction
in Part
1](https://blog.jle.im/entry/introduction-to-singletons-1.html#the-singletons-library)
and [Exercise 4 in Part
2](https://blog.jle.im/entry/introduction-to-singletons-2.html#exercises))

So we might have:

``` haskell
ghci> let door1 = mkDoor SClosed "Oak"
ghci> let door2 = mkDoor SOpened "Spruce"
ghci> let door3 = mkDoor SLocked "Acacia"
ghci> :t door1 :<# door2 :<# door3 :<# HEnd
Hallway '[ 'Closed, 'Opened, 'Locked ]
```

That is, a `Hallway '[ s, t, u ]` is a hallway consisting of a `Door s`, a
`Door t`, and a `Door u`, constructed like a linked list in Haskell.

Now, let's write a function to *collapse all doors in a hallway down to a single
door*:

``` haskell
collapseHallway :: Hallway ss -> Door ?????
```

Basically, we want to merge all of the doors one after the other, collapsing it
until we have a single door state. Luckily, `MergeState` is both commutative and
associative and has an identity, so this can be defined sensibly.

First, let's think about the type we want. What will the result of merging `ss`
be?

We can pattern match and collapse an entire list down item-by-item:

``` haskell
$(singletons [d|
  mergeStateList :: [DoorState] -> DoorState
  mergeStateList []     = Opened               -- ^ the identity of mergeState
  mergeStateList (s:ss) = s `mergeState` mergeStateList ss
  |])
```

Again, remember that this also defines the type family `MergeStateList` and the
singleton function `sMergeStateList :: Sing ss -> Sing (MergeStateList ss)`.

With this, we can write `collapseHallway`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4.hs#L69-L71

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds
```

Now, because the structure of `collapseHallway` perfectly mirrors the structure
of `mergeStateList`, this all typechecks, and we're done!

``` haskell
ghci> collapseHallway (door1 :<# door2 :<# door3 :<# HEnd)
UnsafeMkDoor "Oak and Spruce and Acacia and End of Hallway"
    :: Door 'Locked
```

Note one nice benefit -- the door state of
`collapseHallway (door1 :<# door2 :<# door3 :<# HEnd)` is known at compile-time
to be `Door 'Locked`, if the types of all of the component doors are also known!

## Functional Programming

We went over that all a bit fast, but some of you might have noticed that the
definition of `mergeStateList` bears a really strong resemblance to a very
common Haskell list processing pattern:

``` haskell
mergeStateList :: [DoorState] -> DoorState
mergeStateList []     = Opened               -- ^ the identity of mergeState
mergeStateList (s:ss) = s `mergeState` mergeStateList ss
```

The algorithm is to basically `[]` with `Opened`, and all `(:)` with
`mergeState`. If this sounds familiar, that's because this is exactly a *right
fold*! (In fact, [hlint](http://hackage.haskell.org/package/hlint) actually made
this suggestion to me while I was writing this)

``` haskell
mergeStateList :: [DoorState] -> DoorState
mergeStateList = foldr mergeState Opened
```

In Haskell, we are always encouraged to use higher-order functions whenever
possible instead of explicit recursion, both because explicit recursion opens
you up to a lot of potential bugs, and also because using established
higher-order functions make your code more readable.

So, as Haskellers, let us hold ourselves to a higher standard and not be
satisfied with a `MergeState` written using explicit recursion. Let us instead
go *full fold* --- ONWARD HO!

### The Problem

Initial attempts to write a higher-order type-level function as a type family,
however, serve to temper our enthusiasm.

``` haskell
type family Foldr (f :: j -> k -> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = f x (Foldr f z xs)
```

So far so good right? So we should expect to be able to write `MergeStateList`
using `Foldr`, `MergeState`, and `'Opened`

``` haskell
type MergeStateList ss = Foldr MergeState 'Opened ss
```

Ah, but the compiler is here to tell you this isn't allowed in Haskell:

        • The type family ‘MergeState’ should have 2 arguments, but has been given none
        • In the equations for closed type family ‘MergeStateList’
          In the type family declaration for ‘MergeStateList’

What happened? To figure out, we have to remember that pesky restriction on type
synonyms and type families: they can *not* be used partially applied
("unsaturated"), and must always be fully applied ("saturated"). For the most
part, only *type constructors* (like `Maybe`, `Either`, `IO`) and lifted
DataKinds data constructors (like `'Just`, `'(:)`) in Haskell can ever be
partially applied at the type level. We therefore can't use `MergeState` as an
argument to `Foldr`, because `MergeState` must always be fully applied.

Unfortunately for us, this makes our `Foldr` effectively useless. That's because
we're always going to want to pass in type families (like `MergeState`), so
there's pretty much literally no way to ever actually call `Foldr` except with
type constructors or lifted DataKinds data constructors.

So...back to the drawing board?

## Defunctionalization

I like to mentally think of the *singletons* library as having two parts: the
first is linking lifted DataKinds types with run-time values to allow us to
manipulate types at runtime as first-class values. The second is a system for
effective *functional programming* at the type level.

To make a working `Foldr`, we're going to have to jump into that second half:
*[defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization)*.

Defunctionalization is a technique invented in the early 70's as a way of
compiling higher-order functions into first-order functions in target languages.
The main idea is:

-   Instead of working with *functions*, work with *symbols representing
    functions*.
-   Build your final functions and values by composing and combining these
    symbols.
-   At the end of it all, have a single `Apply` function interpret all of your
    symbols and produce the value you want.

In *singletons* these symbols are implemented as "dummy" empty data
constructors, and `Apply` is a type family.

To help us understand singleton's defunctionalization system better, let's build
our own defunctionalization system from scratch.

First, a little trick to make things easier to read:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L14-L17

data TyFun a b
type a ~> b = TyFun a b -> Type

infixr 0 ~>
```

### Our First Symbols

Now we can define a dummy data type like `Id`, which represents the identity
function `id`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L25-L25

data Id :: a ~> a
```

The "actual" kind of `Id` is `Id :: TyFun a a -> Type`; you can imagine
`TyFun a a` as a phantom parameter that signifies that `Id` represents a
function from `a` to `a`. It's essentially a nice trick to allow you to write
`Id :: a ~> a` as a kind signature.

Now, `Id` is *not* a function...it's a *dummy type constructor* that
*represents* a function `a -> a`. A type constructor of kind `a ~> a` represents
a *defunctionalization symbol* -- a type constructor that represents a function
from `a` to `a`.

To interpret it, we need to write our global interpreter function:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L19-L19

type family Apply (f :: a ~> b) (x :: a) :: b
```

That's the syntax for the definition of an *open* type family in Haskell: users
are free to add their own instances, just like how type classes are normally
open in Haskell.

Let's tell `Apply` how to interpret `Id`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L26-L26

type instance Apply Id x = x
```

The above is the actual function definition, like writing `id x = x`. We can now
*call* `Id` to get an actual type in return:

``` haskell
ghci> :kind! Apply Id 'True
'True
```

(Remember, `:kind!` is the ghci command to evaluate a type family)

Let's define another one! We'll implement `Not`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L28-L30

data Not :: Bool ~> Bool
type instance Apply Not 'False = 'True
type instance Apply Not 'True  = 'False
```

We can try it out:

``` haskell
ghci> :kind! Apply Not 'True
'False
ghci> :kind! Apply Not 'False
'True
```

It can be convenient to define an infix synonym for `Apply`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L21-L23

type f @@ a = Apply f a

infixl 9 @@
```

Then we can write:

``` haskell
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
using `Apply`. Let's add a bit of principle to make this all a bit more
scalable.

The singletons library adopts a few conventions for linking all of these
together. Using the `Not` function as an example, if we wanted to lift the
function:

``` haskell
not :: Bool -> Bool
not False = True
not True  = Flse
```

We already know about the type family and singleton function this would produce:

``` haskell
type family Not (x :: Bool) :: Bool where
    Not 'False = 'True
    Not 'True  = 'False

sNot :: Sing x -> Sing (Not x)
sNot SFalse = STrue
sNot STrue  = SFalse
```

But the singletons library also produces the following *defunctionalization
symbols*, according to a naming convention:

``` haskell
data NotSym0 :: Bool ~> Bool
type instance Apply NotSym0 x = Not x

-- also generated for consistency
type NotSym1 x = Not x
```

`NotSym0` is the *defunctionalization symbol* associated with the `Not` type
family, defined so that `NotSym0 @@ x = Not x`. Its purpose is to allow us to
*pass in* `Not` as an *un-applied function*. The `Sym0` suffix is a naming
convention, and the 0 stands for "expects 0 arguments". Similarly for `NotSym1`
-- the 1 stands for "expects 1 argument".

#### Two-Argument Functions

Let's look at a slightly more complicated example -- a two-argument function.
Let's define the boolean "and":

``` haskell
$(singletons [d|
  and :: Bool -> (Bool -> Bool)
  and False _ = False
  and True  x = x
  ])
```

this will generate:

``` haskell
type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'False x = 'False
    And 'True  x = x

sAnd :: Sing x -> Sing y -> Sing (And x y)
sAnd SFalse x = SFalse
sAnd STrue  x = x
```

And the defunctionalization symbols:

``` haskell
data AndSym0 :: Bool ~> (Bool ~> Bool)
type instance Apply AndSym0 x = AndSym1 x

data AndSym1 (x :: Bool) :: (Bool ~> Bool)
-- or
data AndSym1 :: Bool -> (Bool ~> Bool)
type instance Apply (AndSym1 x) y = And x y

type AndSym2 x y = And x y
```

`AndSym0` is a defunctionalization symbol representing a "fully unapplied"
("completely unsaturated") version of `And`. `AndSym1 x` is a
defunctionalization symbol representing a "partially applied" version of `And`
--- partially applied to `x` (its kind is `AndSym1 :: Bool -> (Bool ~> Bool)`).

The application of `AndSym0` to `x` gives you `AndSym1 x`:

``` haskell
ghci> :kind! AndSym0 @@ 'False
AndSym1 'False
```

Remember its kind `AndSym0 :: Bool ~> (Bool ~> Bool)` (or just
`AndSym0 :: Bool ~> Bool ~> Bool`): it takes a `Bool`, and returns a
`Bool ~> Bool` defunctionalization symbol.

The application of `AndSym1 x` to `y` gives you `And x y`:

``` haskell
ghci> :kind! AndSym1 'False @@ 'True
'False              -- or FalseSym0, which is a synonym for 'False
ghci> :kind! AndSym1 'True  @@ 'True
'True
```

A note to remember: `AndSym1 'True` is the defunctionalization symbol, and *not*
`AndSym1` itself. `AndSym1` has kind `Bool -> (Bool ~> Bool)`, but
`AndSym1 'True` has kind `Bool ~> Bool` --- the kind of a defunctionalization
symbol. `AndSym1` is a sort of "defunctionalization symbol constructor".

Also note here that we encounter the fact that singletons also provides
"defunctionalization symbols" for "nullary" type functions like `False` and
`True`, where:

``` haskell
type FalseSym0 = 'False
type TrueSym0  = 'True
```

Just like how it defines `AndSym0` for consistency, as well.

#### Symbols for type constructors

One extra interesting defunctionalization symbol we can write: we turn lift any
type constructor into a "free" defunctionalization symbol:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L32-L38

data TyCon1
        :: (j -> k)     -- ^ take a type constructor
        -> (j ~> k)     -- ^ return a defunctionalization symbol
-- alternatively
-- data TyCon1 (t :: j -> k) :: j ~> k

type instance Apply (TyCon1 t) a = t a
```

Basically the `Apply` instance just applies the type constructor `t` to its
input `a`.

``` haskell
ghci> :kind! TyCon1 Maybe @@ Int
Maybe Int
ghci> :kind! TyCon1 'Right @@ 'False
'Right 'False
```

We can use this to give a normal `j -> k` type constructor to a function that
expects a `j ~> k` defunctionalization symbol.

## Bring Me a Higher Order

Okay, so now we have these tokens that represent "unapplied" versions of
functions. So what?

Well, remember the problem with our implementation of `Foldr`? We couldn't pass
in a type family, since type families must be passed fully applied. So, instead
of having `Foldr` expect a type family...we can make it expect a
*defunctionalization symbol* instead. Remember, defunctionalization symbols
represent the "unapplied" versions of type families, so they are exactly the
tools we need!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L40-L42

type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs
```

The difference is that instead of taking a type family or type constructor
`f :: j -> k -> k`, we have it take the *defunctionalization symbol*
`f :: j ~> (k ~> k)`.

Instead of taking a type family or type constructor, we take that dummy type
constructor.

Now we just need to have our defunctionalization symbols for `MergeStateList`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L54-L60

data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
type instance Apply MergeStateSym0 s = MergeStateSym1 s

data MergeStateSym1 :: DoorState -> DoorState ~> DoorState
type instance Apply (MergeStateSym1 s) t = MergeState s t

type MergeStateSym2 s t = MergeState s t
```

And now we can write `MergeStateList`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L62-L62

type MergeStateList ss = Foldr MergeStateSym0 'Opened ss
```

(If you "see" `MergeStateSym0`, you should *read* it was `MergeState`, but
partially applied)

This compiles!

``` haskell
ghci> :kind! MergeStateList '[ 'Closed, 'Opened, 'Locked ]
'Locked
ghci> :kind! MergeStateList '[ 'Closed, 'Opened ]
'Closed
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L77-L79

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds
```

(Note: Unfortunately, we do have to use our our *own* `Foldr` here, that we just
defined, instead of using the one that comes with *singletons*, because of some
[outstanding issues](https://github.com/goldfirere/singletons/issues/339) with
how the singletons TH processes alternative implementations of `foldr` from
Prelude. In general, the issue is that we should only expect type families to
work with singletons if the definition of the type family perfectly matches the
structure of how we implement our value-level functions like `collapseHallway`)

### Singletons to make things nicer

Admittedly this is all a huge mess of boilerplate. The code we had to write more
than tripled, and we also have an unsightly number of defunctionalization
symbols and `Apply` instance boilerplate for every function.

Luckily, the *singletons* library is here to help. You can just write:

``` haskell
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

And all of these defunctionalization symbols are generated for you; *singletons*
is also able to recognize that `foldr` is a higher-order function and translate
its lifted version to take a defunctionalization symbol `a ~> b ~> b`.

That the template haskell also generates `SingI` instances for all of your
defunctionalization symbols, too (more on that in a bit).

It's okay to stay "in the world of singletons" for the most part, and let
singletons handle the composition of functions for you. However, it's still
important to know what the *singletons* library generates, because sometimes
it's still useful to manually create defunctionalization symbols and work with
them.

The naming convention for non-symbolic names (non-operators) like `myFunction`
are just to call them `MyFunctionSym0` for the completely unapplied
defunctionalization symbol, `MyFunctionSym1` for the type constructor that
expects one argument before returning a defunctionalization symbol,
`MyFunctionSym2` for the type constructor that expects two arguments before
returning a defunctionalization symbol, etc.

For operator names like `++`, the naming convention is to have `++@#@$` be the
completely unapplied defunctionalization symbol, `++@#@$$` be the type
constructor that expects one argument before returning a defunctionalization
symbol, `++@#@$$$` be the type constructor that takes two arguments before
returning a defunctionalization symbol, etc.

Another helpful thing that *singletons* does is that it also generates
defunctionalization symbols for type families and type synonyms you define in
the Template Haskell, as well --- so if you write

``` haskell
$(singletons [d|
  type MyTypeFamily (b :: Bool) :: Type where
    MyTypeFamily 'False = Int
    MyTypeFamily 'True  = String
  |])
```

and

``` haskell
$(singletons [d|
  type MyTypeSynonym a = (a, [a])
  |])
```

*singletons* will generate:

``` haskell
data MyTypeFamilySym0 :: Bool ~> Type
type instance Apply MyTypeFamilySym0 b = MyTypeFamily b

type MyTypeFamilySym1 b = MyTypeFamily b
```

and

``` haskell
data MyTypeSynonymSym0 :: Type ~> Type
type instance Apply MyTypeSynonym b = MyTypeSynonym a

type MyTypeSynonymSym1 a = MyTypeSynonym a
```

#### Bringing it All Together

Just to show off the library, remember that *singletons* also promotes
typeclasses?

Because `DoorState` is a monoid with respect to merging, we can actually write
and promote a `Monoid` instance: (requires *singletons-2.5* or higher)

``` haskell
$(singletons [d|
  instance Semigroup DoorState where
      (<>) = mergeState
  instance Monoid DoorState where
      mempty  = Opened
      mappend = (<>)
  |])
```

We can promote `fold`:

``` haskell
$(singletons [d|
  fold :: Monoid b => [b] -> b
  fold []     = mempty
  fold (x:xs) = x <> fold xs
  |])
```

And we can write `collapseHallway` in terms of those instead :)

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L109-L118

collapseHallway'
    :: Hallway ss
    -> Door (Fold ss)
collapseHallway' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway' (d :<# ds) = d `mergeDoor` collapseHallway' ds

collapseSomeHallway' :: SomeHallway -> SomeDoor
collapseSomeHallway' (ss :&: d) =
        sFold ss
    :&: collapseHallway' d
```

(Note again unfortunately that we have to define our own `fold` instead of using
the one from *singletons* and the `SFoldable` typeclass, because of [issue
#339](https://github.com/goldfirere/singletons/issues/339))

## Thoughts on Symbols

Defunctionalization symbols may feel like a bit of a mess, and the naming
convention is arguably less than aesthetically satisfying. But, as you work with
them more and more, you start to appreciate them on a deeper level.

At the end of the day, you can compare defunctionalization as turning
"functions" into just constructors you can *match* on, just like any other data
or type constructor. That's because they *are* just type constructors!

In a sense, defining defunctionalization symbols is a lot like working with
*pattern synonyms* of your functions, instead of directly passing the functions
themselves. At the type family and type class level, you can "pattern match" on
these functions.

For a comparison at the value level -- you can't pattern match on `(+)`, `(-)`,
`(*)`, and `(/)`:

``` haskell
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

This essentially fixes the "type lambda problem" of type inference and typeclass
resolution. You can't match on arbitrary lambdas, but you *can* match on dummy
constructors representing type functions.

And a bit of the magic here, also, is the fact that you don't always need to
make our own defunctionalization symbols from scratch --- you can create them
based on other ones in a compositional way. This is the basis of libraries like
*[decidable](http://hackage.haskell.org/package/decidable)*.

For example, suppose we wanted to build defunctionalization symbols for
`MergeStateList`. We can actually build them directly from defunctionalization
symbols for `Foldr`.

Check out the defunctionalization symbols for `Foldr`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L81-L90

data FoldrSym0 :: (j ~> k ~> k) ~> k ~> [j] ~> k
type instance Apply FoldrSym0 f = FoldrSym1 f

data FoldrSym1 :: (j ~> k ~> k) -> k ~> [j] ~> k
type instance Apply (FoldrSym1 f) z = FoldrSym2 f z

data FoldrSym2 :: (j ~> k ~> k) -> k -> [j] ~> k
type instance Apply (FoldrSym2 f z) xs = Foldr f z xs

type FoldrSym3 f z xs = Foldr f z xs
```

We can actually use these to *define* our `MergeStateList` defunctionalization
symbols, since *defunctionalization symbols are first-class*:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Defunctionalization.hs#L92-L92

type MergeStateListSym0 = FoldrSym2 MergeStateSym0 'Opened
```

And you can just write `collapseHallway` as:

``` haskell
collapseHallway :: Hallway ss -> Door (MergeStateListSym0 @@ ss)
-- or
collapseHallway :: Hallway ss -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
```

You never have to actually define `MergeStateList` as a function or type family!

The whole time, we're just building defunctionalization symbols in terms of
other defunctionalization symbols. And, at the end, when we finally want to
interpret the complex function we construct, we use `Apply`, or `@@`.

You can think of `FoldrSym1` and `FoldrSym2` as *defunctionalization symbol
constructors* -- they're combinators that take in defunctionalization symbols
(like `MergeStateSym0`) and *return new ones*.

### Sigma

Let's look at a nice tool that is made possible using defunctionalization
symbols: *dependent pairs*. I talk a bit about dependent pairs (or dependent
sums) in [part 2](https://blog.jle.im/entry/introduction-to-singletons-2.html)
of this series, and also in my [dependent types in
Haskell](https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html)
series.

Essentially, a dependent pair is a tuple where the *type* of the second field
depends on the *value* of the first one. This is basically what `SomeDoor` was:

``` haskell
data SomeDoor :: Type where
    MkSomeDoor :: Sing x -> Door x -> SomeDoor
```

The *type* of the `Door x` depends on the *value* of the `Sing x`, which you can
read as essentially storing the `x`.

We made `SomeDoor` pretty ad-hoc. But what if we wanted to make some other
predicate? Well, we can make a *generic* dependent pair by *parameterizing it on
the dependence* between the first and second field. Singletons provides the
`Sigma` type, in the
*[Data.Singletons.Sigma](http://hackage.haskell.org/package/singletons-2.5/docs/Data-Singletons-Sigma.html)*
module:

``` haskell
data Sigma k :: (k ~> Type) -> Type where
    (:&:) :: Sing x -> (f @@ x) -> Sigma k f

-- also available through fancy type synonym
type Σ k = Sigma k
```

If you squint carefully, you can see that `Sigma k` is just `SomeDoor`, but
*parameterized over `Door`*. Instead of always holding `Door x`, we can have it
parameterized on an arbitrary function `f` and have it hold an `f @@ x`.

We can actually define `SomeDoor` in terms of `Sigma`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L54-L58

type SomeDoor = Sigma DoorState (TyCon1 Door)

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    dsSing :&: mkDoor dsSing mat
```

(Remember `TyCon1` is the defunctionalization symbol constructor that turns any
normal type constructor `j -> k` into a defunctionalization symbol `j ~> k`)

That's because a `Sigma DoorState (TyCon1 Door)` contains a
`Sing (x :: DoorState)` and a `TyCon1 Door @@ x`, or a `Door x`.

This is a simple relationship, but one can imagine a `Sigma` parameterized on an
even more complex type-level function. We'll explore more of these in the
exercises.

For some context, `Sigma` is an interesting data type (the "dependent sum") that
is ubiquitous in dependently typed programming.

### Singletons of Defunctionalization Symbols

One last thing to tie it all together -- let's write `collapseHallway` in a way
that we don't know the types of the doors.

Luckily, we now have a `SomeHallway` type for free:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L84-L84

type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)
```

The easy way would be to just use `sMergeStateList` that we defined:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L86-L88

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss
                             :&: collapseHallway d
```

But what if we didn't write `sMergeStateList`, and we constructed our
defunctionalization symbols from scratch?

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L122-L126

collapseHallway''
    :: Hallway ss
    -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway'' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway'' (d :<# ds) = d `mergeDoor` collapseHallway'' ds

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) = ???    -- what goes here?
                               :&: collapseHallway'' d
```

This will be our final defunctionalization lesson. How do we turn a singleton of
`ss` into a singleton of `FoldrSym2 MergeStateSym0 'Opened @@ s` ?

First -- we have `Foldr` at the value level, as `sFoldr`. We glossed over this
earlier, but *singletons* generates the following function for us:

``` haskell
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
singleton/value-level counterpart of `Apply` or `(@@)`.[^1]

So we can write:

``` haskell
collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) = sFoldr ???? SOpened ss
                               :&: collapseHallwa''y d
```

But how do we get a `Sing MergeStateSym0`?

We can use the `singFun` family of functions:

``` haskell
singFun2 @MergeStateSym0 sMergeState
    :: Sing MergeStateSym0
```

But, also, conveniently, the *singletons* library generates a `SingI` instance
for `MergeStateSym0`, if you defined `mergeState` using the *singletons*
template haskell:

``` haskell
sing :: Sing MergeStateSym0
-- or
sing @_ @MergeStateSym0         -- singletons 2.4
sing @MergeStateSym0            -- singletons 2.5
```

And finally, we get our answer:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L128-L133

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
        sFoldr (singFun2 @MergeStateSym0 sMergeState) SOpened ss
     -- or
     -- sFoldr (sing @MergeStateSym0) SOpened ss
    :&: collapseHallway'' d
```

## Closing Up

Woo! Congratulations, you've made it to the end of the this Introduction to
Singletons tetralogy! This last and final part understandably ramps things up
pretty quickly, so don't be afraid to re-read it a few times until it all sinks
in before jumping into the exercises.

I hope you enjoyed this journey deep into the motivation, philosophy, mechanics,
and usage of this great library. Hopefully these toy examples have been able to
show you a lot of ways that type-level programming can help your programs today,
both in type safety and in writing more expressive programs. And also, I hope
that you can also see now how to leverage the full power of the *singletons*
library to make those gains a reality.

There are a few corners of the library we haven't gone over (like the TypeLits-
and TypeRep-based singletons -- if you're interested, check out [this
post](https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html) where
I talk a lot about them), but I'd like to hope as well that this series has
equipped you to be able to dive into the library documentation and decipher what
it holds, armed with the knowledge you now have. (We also look at TypeLits
briefly in the exercises)

You can download the source code here ---
[Door4Final.hs](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs)
contains the final versions of all our definitions, and
[Defunctionalization.hs](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door3.hs)
contains all of our defunctionalization-from-scratch work. These are designed as
stack scripts that you can load into ghci. Just execute the scripts:

``` bash
$ ./Door4Final.hs
ghci>
```

And you'll be dropped into a ghci session with all of the definitions in scope.

As always, please try out the exercises, which are designed to help solidify the
concepts we went over here! And if you ever have any future questions, feel free
to leave a comment or find me on [twitter](https://twitter.com/mstk "Twitter")
or in freenode `#haskell`, where I idle as *jle\`*.

### Looking Forward

Some final things to note before truly embracing singletons: remember that, as a
library, *singletons* was always meant to become obsolete. It's a library that
only exists because Haskell doesn't have real dependent types yet.

[Dependent Haskell](https://ghc.haskell.org/trac/ghc/wiki/DependentHaskell) is
coming some day! It's mostly driven by one solo man, Richard Eisenberg, but
every year buzz does get bigger. In a [recent progress
report](https://typesandkinds.wordpress.com/2016/07/24/dependent-types-in-haskell-progress-report/#comment-13327),
we do know that we realistically won't have dependent types before 2020. That
means that this tutorial will still remain relevant for at least another two
years :)

How will things be different in a world of Haskell with real dependent types?
Well, for a good guess, take a look at [Richard Eisenberg's
Dissertation](https://github.com/goldfirere/thesis)!

One day, hopefully, we won't need singletons to work with types at the
value-level; we would just be able to directly pattern match and manipulate the
types within the language and use them as first-class values, with a nice story
for dependent sums. And some day, I hope we won't need any more dances with
defunctionalization symbols to write higher-order functions at the type level
--- maybe we'll have a nicer way to work with partially applied type-level
functions (maybe they'll just be normal functions?), and we don't need to think
any different about higher-order or first-order functions.

So, as a final word --- Happy Haskelling, everyone! May you leverage the great
*singletons* library to its full potential, and may we also all dream of a day
where *singletons* becomes obsolete. But may we all enjoy the wonderful journey
along the way.

Until next time!

## Exercises

Here are your final exercises for this series! Start from [this sample source
code](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs),
which has all of the definitions that the exercises and their solutions require.
Just make sure to delete all of the parts after the `-- Exercises` comment if
you don't want to be spoiled. Remember again to enable
`-Werror=incomplete-patterns` or `-Wall` to ensure that all of your functions
are total.

1.  Let's try combining type families with proofs! In doing so, hopefully we can
    also see the value of using dependent proofs to show how we can manipulate
    proofs as first-class values that the compiler can verify.

    Remember `Knockable` from Part 3?

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L139-L141

    data Knockable :: DoorState -> Type where
        KnockClosed :: Knockable 'Closed
        KnockLocked :: Knockable 'Locked
    ```

    Closed and Locked doors are knockable. But, if you merge two knockable
    doors...is the result *also* always knockable?

    I say yes, but don't take my word for it. Prove it using `Knockable`!

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L156-L159

    mergedIsKnockable
        :: Knockable s
        -> Knockable t
        -> Knockable (MergeState s t)
    ```

    `mergedIsKnockable` is only implementable if the merging of two DoorStates
    that are knockable is also knockable. See if you can write the
    implementation!

    [Solution
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L154-L154)

2.  Write a function to append two hallways together.

    ``` haskell
    appendHallways
        :: Hallway ss
        -> Hallway ts
        -> Hallway ????
    ```

    from *singletons* --- implement any type families you might need from
    scratch!

    Remember the important principle that your type family must mirror the
    implementation of the functions that use it.

    Next, for fun, use `appendHallways` to implement `appendSomeHallways`:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L84-L187

    type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

    appendSomeHallways
        :: SomeHallway
        -> SomeHallway
        -> SomeHallway
    ```

    [Solution
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L169-L169)

3.  Can you use `Sigma` to define a door that must be knockable?

    To do this, try directly defining the defunctionalization symbol
    `KnockableDoor :: DoorState ~> Type` (or use singletons to generate it for
    you --- remember that *singletons* can also promote type families) so that:

    ``` haskell
    type SomeKnockableDoor = Sigma DoorState KnockableDoor
    ```

    will contain a `Door` that must be knockable.

    Try doing it for both (a) the "dependent proof" version (with the
    `Knockable` data type) and for (b) the type family version (with the
    `StatePass` type family).

    [Solutions
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L192-L192)
    I gave four different ways of doing it, for a full range of manual
    vs. auto-promoted defunctionalization symbols and `Knockable`
    vs. `Pass`-based methods.

    *Hint:* Look at the definition of `SomeDoor` in terms of `Sigma`:

    ``` haskell
    type SomeDoor = Sigma DoorState (TyCon1 Door)
    ```

    *Hint*: Try having `KnockableDoor` return a tuple.

4.  Take a look at the API of the
    *[Data.Singletons.TypeLits](http://hackage.haskell.org/package/singletons-2.5/docs/Data-Singletons-TypeLits.html)*
    module, based on the API exposed in
    *[GHC.TypeNats](http://hackage.haskell.org/package/base-4.12.0.0/docs/GHC-TypeNats.html)*
    module from *base*.

    Using this, you can use `Sigma` to create a predicate that a given `Nat`
    number is even:

    ``` haskell
    data IsHalfOf :: Nat -> Nat ~> Type
    type instance Apply (IsHalfOf n) m = n :~: (m * 2)

    type IsEven n = Sigma Nat (IsHalfOf n)
    ```

    `(*)` is multiplication from the
    *[Data.Singletons.Prelude.Num](http://hackage.haskell.org/package/singletons-2.5/docs/Data-Singletons-Prelude-Num.html)*
    module. (**You must have the *-XNoStarIsType* extension on** for this to
    work in GHC 8.6+), and `:~:` is the predicate of equality from Part 3:

    ``` haskell
    data (:~:) :: k -> k -> Type where
        Refl :: a :~: a
    ```

    (It's only possible to make a value of type `a :~: b` using
    `Refl :: a :~: a`, so it's only possible to make a value of that type when
    `a` and `b` are equal. I like to use `Refl` with type application syntax,
    like `Refl @a`, so it's clear what we are saying is the same on both sides;
    `Refl @a :: a :~: a`)

    The only way to construct an `IsEven n` is to provide a number `m` where
    `m * 2` is `n`. We can do this by using `SNat @m`, which is the singleton
    constructor for the `Nat` kind (just like how `STrue` and `SFalse` are the
    singleton constructors for the `Bool` kind):

    ``` haskell
    tenIsEven :: IsEven 10
    tenIsEven = SNat @5 :&: Refl @10
        -- Refl is the constructor of type n :~: (m * 2)
        -- here, we use it as Refl @10 :: 10 :~: 10

    -- won't compile
    sevenIsEven :: IsEven 10
    sevenIsEven = SNat @4 :&: Refl
        -- won't compile, because we need something of type `(4 * 2) :~: 7`,
        -- but Refl must have type `a :~: a`; `8 :~: 7` is not constructable
        -- using `Refl`.  Neither `Refl @8` nor `Refl @7` will work.
    ```

    Write a similar type `IsOdd n` that can only be constructed if `n` is *odd*.

    ``` haskell
    type IsOdd n = Sigma Nat (???? n)
    ```

    And construct a proof that `7` is odd:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L226-L226

    sevenIsOdd :: IsOdd 7
    ```

    [Solution
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L220-L220)

    On a sad note, one exercise I'd like to be able to add is to ask you to
    write decision functions and proofs for `IsEven` and `IsOdd`. Unfortunately,
    `Nat` is not rich enough to support this out of the box without a lot of
    extra tooling!

5.  A common beginner Haskeller exercise is to implement `map` in terms of
    `foldr`:

    ``` haskell
    map :: (a -> b) -> [a] _> [b]
    map f = foldr ((:) . f) []
    ```

    Let's do the same thing at the type level, manually.

    Directly implement a type-level `Map`, with kind `(j ~> k) -> [j] -> [k]`,
    in terms of `Foldr`:

    ``` haskell
    type Map f xs = Foldr ???? ???? xs
    ```

    Try to mirror the value-level definition, passing in `(:) . f`, and use the
    promoted version of `(.)` from the *singletons* library, in
    *[Data.Singletons.Prelude](http://hackage.haskell.org/package/singletons-2.5/docs/Data-Singletons-Prelude.html)*.
    You might find `TyCon2` helpful!

    [Solution
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L229-L229)

6.  Make a `SomeHallway` from a list of `SomeDoor`:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L54-L233

    type SomeDoor = Sigma DoorState (TyCon1 Door)

    type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

    mkSomeHallway :: [SomeDoor] -> SomeHallway
    ```

    Remember that the singleton constructors for list are `SNil` (for `[]`) and
    `SCons` (for `(:)`)!

    [Solution
    here!](https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door4Final.hs#L229-L229)

## Special Thanks

None of this entire series would be possible without the hard work and effort of
the amazing *singletons* library authors and maintainers --- especially [Richard
Eisenberg](https://github.com/goldfirere) and [Ryan
Scott](https://github.com/RyanGlScott).

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my two supporters at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Sam Stites and Josh Vera!
:)

Thanks also to [Koz Ross](https://twitter.com/KozRoss) for helping proofread
this post!

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

[^1]: `(@@)` (and as we see shortly, the `singFun` functions) are all
    implemented in terms of `SLambda`, the "singleton" for functions.
    Understanding the details of the implementation of `SLambda` aren't
    particularly important for the purposes of this introduction.

