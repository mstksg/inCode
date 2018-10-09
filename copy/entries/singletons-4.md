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

Kicking it up a notch
---------------------

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

Again, remember that this also defines the type family `MergeStates` and the
singleton function `sMergeStates :: Sing ss -> Sing (MergeStates ss)`.

With this, we can write `collapseHallway`:

```haskell
collapseHallway :: Hallway ss -> Door (MergeStates ss)
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
