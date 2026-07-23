Extreme Haskell: Typed State Machines (Part 2)
==============================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-haskell-typed-state-machines.html)

Welcome back to our journey through the limits of polite Haskell! In [Part
1](/entry/extreme-haskell-typed-expression-edsls-1.html), we built a typed
expression language whose terms track their free variables and result types, and
whose evaluator is total. If GHC lets you construct the `Expr`, it will
evaluate.

That is already useful, but I would not have written a thousand lines about it
if the payoff was just "a lambda calculus that typechecks." The real payoff is
that the expression language is now a reusable typed component. You can embed it
inside larger typed structures, and the type indices thread through, carrying
their guarantees with them. In this post, we will put it inside a typed
state-machine description: a graph of states and triggered transitions where the
guards, the payloads, and the destinations are all verified at compile time. We
will then render those machines as Graphviz diagrams directly from the typed
structure.

This is where I personally started feeling the approach justify itself. You
write one machine definition, and you get a Graphviz diagram for free, a
JavaScript simulator for free, and the guarantee that they all agree with each
other for free. When I first got this working on a real project, the feeling was
less "oh neat" and more "why would anyone do this any other way."

Again, all of the code here is [available
online](https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/flake.nix),
and you can load it all in ghci from the sample directory:

``` bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ ghci
ghci> :load MachineStage5.hs
```

## The State Machine

Okay, now here is where all of this finally starts paying rent.

The expression language we made is not merely something that evaluates to a
value in Haskell. It is a typed description of a computation. And if it is a
description, we can stick it inside a larger typed description: a state machine.

### A Machine Made of Strings

For our running example, we will build a controller for a software-defined radio
(SDR) receiver. An SDR is a radio where the signal processing that traditionally
happens in hardware (tuning, filtering, demodulation) is instead done in
software. The receiver scans across a band of frequencies looking for signals of
interest, locks onto them when found, and handles the various failure modes
(signal drift, timeout, thermal limits) that come up in practice.

This is a natural fit for a state machine. The controller has a small number of
discrete modes (idle, scanning, locked onto a signal, cooling down), and the
transitions between them depend on real-time sensor readings (received signal
strength, noise floor, frequency offset). Each mode carries different data: a
scanning receiver tracks its current frequency and how long it has dwelt there;
a locked receiver additionally knows the signal strength at lock time. The
transitions have guards ("only lock if the signal is strong enough") and must
construct the right payload for the destination state.

State machines like this show up everywhere in embedded systems, protocol
implementations, and game logic. They are also where I have personally seen the
most insidious runtime bugs: transitions that reference data from the wrong
state, destinations with incomplete payloads, guards that accidentally test the
wrong condition. The typed version we build here eliminates all three classes of
bug at compile time.

So: we have a few states (idle, scanning, locked, cooling down), some triggers
(start, peak detected, drift, timeout, reset), and some transitions between them
with guards and payload constructors.

An obvious first pass in Haskell is something like:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineUntyped.hs#L8-L16

data UntypedTransition = UntypedTransition
  { utFrom    :: String
  , utTrigger :: String
  , utTo      :: String
  , utGuard   :: Map String Int -> Bool
  , utBuild   :: Map String Int -> Map String Int
  }

type UntypedMachine = [UntypedTransition]
```

States are strings. Triggers are strings. The payload is a bag of string-keyed
integers. Guards are opaque Haskell functions over that bag. This is pleasant to
write! You can throw a machine together in five minutes, and there is zero
ceremony around declaring your state space or its structure.

Here is a complete machine in this style:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineUntyped.hs#L37-L61

scanMachine :: UntypedMachine
scanMachine =
  [ UntypedTransition "Idle" "Start" "Scanning"
      (const True)
      (\_ -> M.fromList [("freq", 2400), ("dwell", 0)])
  , UntypedTransition "Scanning" "Peak" "Locked"
      (\env -> (env M.! "rssi") >= (env M.! "noise") + 4)
      (\env -> M.fromList
        [ ("freq", (env M.! "freq") + (env M.! "peakOffset"))
        , ("lockRssi", env M.! "rssi")
        , ("dwell", (env M.! "dwell") + 1)
        ])
  , UntypedTransition "Scanning" "Timeout" "CoolingDown"
      (\env -> (env M.! "dwell") >= 3)
      (\env -> M.singleton "lastFreq" (env M.! "freq"))
  , UntypedTransition "Locked" "Drift" "Scanning"
      (const True)
      (\env -> M.fromList
        [ ("freq", (env M.! "freq") + (env M.! "peakOffset"))
        , ("dwell", env M.! "dwell")
        ])
  , UntypedTransition "CoolingDown" "Reset" "Idle"
      (const True)
      (const M.empty)
  ]
```

Looks reasonable. But now consider:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineUntyped.hs#L18-L25

badTransition1 :: UntypedTransition
badTransition1 = UntypedTransition
  { utFrom    = "Scanning"
  , utTrigger = "Peak"
  , utTo      = "Lockde"          -- typo: "Lockde" instead of "Locked"
  , utGuard   = \env -> (env M.! "rssi") > 4
  , utBuild   = \env -> M.singleton "lockRssi" (env M.! "rssi")
  }
```

This transitions to `"Lockde"` instead of `"Locked"`. GHC sees two perfectly
valid `String` values and has nothing to say about it. The typo is only
observable at runtime, when the machine steps into a state that has no outgoing
edges.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineUntyped.hs#L27-L35

badTransition2 :: UntypedTransition
badTransition2 = UntypedTransition
  { utFrom    = "Scanning"
  , utTrigger = "Peak"
  , utTo      = "Locked"
  , utGuard   = \env -> (env M.! "lockRssi") > 4  -- lockRssi doesn't exist while Scanning!
  , utBuild   = \env -> M.singleton "freq" (env M.! "rssi")
                        -- missing "lockRssi" and "dwell" that Locked needs
  }
```

This one reads `"lockRssi"` from the environment while in the `"Scanning"`
state. But `lockRssi` only exists after locking on to a signal. It also builds
only `"freq"` for the destination payload, but `"Locked"` needs `"freq"`,
`"lockRssi"`, and `"dwell"`. Both compile. Both pass any test that doesn't
exercise exactly these edges. Both explode at runtime with a key lookup failure.

This is the state-machine version of `1 && 2` from Part 1. The system cannot
distinguish a well-formed transition from garbage because the types carry no
information about which states exist, what variables they carry, or what shape
the destination payload should have.

And unlike `1 && 2`, which you would catch immediately in testing, a bad state
machine transition can hide for months. In my experience, state machine bugs are
the worst kind: they only manifest under specific sequences of events, they are
hard to reproduce, and by the time you see the failure in production the causal
chain is three state transitions deep. I have personally debugged enough of
these that I refuse to write another untyped state machine if I can help it.

We are going to fix it the same way as Part 1: by making the illegal versions
un-representable.

### Closing the World of States

The first repair is the same one we applied to `Ty` in Part 1: instead of
letting any string be a state or trigger, we define a closed kind.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L22-L33

type data ScanState
  = Idle
  | Scanning
  | Locked
  | CoolingDown

type data Trigger
  = Start
  | Peak
  | Drift
  | Timeout
  | Reset
```

`ScanState` is a kind with exactly four types: `Idle`, `Scanning`, `Locked`,
`CoolingDown`. `Trigger` is a kind with five: `Start`, `Peak`, `Drift`,
`Timeout`, `Reset`. These are declared with `type data`, the same mechanism we
used for `Ty` in Part 1. Remember: `type data` creates types that exist only at
the type level. There is no value-level `Idle` constructor floating around.
`Idle` is a *type* of kind `ScanState`, and that is all it is.

The singletons `SState` and `STrigger` are the runtime handles we keep around so
we can render state and trigger names later (for Graphviz labels, JavaScript
codegen, etc.):

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L49-L60

data SState :: ScanState -> Type where
  SIdle :: SState Idle
  SScanning :: SState Scanning
  SLocked :: SState Locked
  SCoolingDown :: SState CoolingDown

data STrigger :: Trigger -> Type where
  SStart :: STrigger Start
  SPeak :: STrigger Peak
  SDrift :: STrigger Drift
  STimeout :: STrigger Timeout
  SReset :: STrigger Reset
```

This is exactly the same pattern as `STy` from Part 1, just applied to a
different domain. `SState` has one constructor per inhabitant of `ScanState`,
each one indexed by the corresponding type. Pattern matching on `SIdle` recovers
the knowledge `s ~ Idle`.

We also define name functions that extract the string label from each singleton,
which we will use later for rendering:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L62-L75

stateName :: SState s -> String
stateName = \case
  SIdle -> "Idle"
  SScanning -> "Scanning"
  SLocked -> "Locked"
  SCoolingDown -> "CoolingDown"

triggerName :: STrigger t -> String
triggerName = \case
  SStart -> "Start"
  SPeak -> "Peak"
  SDrift -> "Drift"
  STimeout -> "Timeout"
  SReset -> "Reset"
```

A transition from `"Lockde"` is now a type error: `Lockde` simply does not exist
in the `ScanState` kind. If you mistype it, GHC will tell you immediately that
there is no such type, rather than silently building a value that refers to a
nonexistent state.

### State Payloads Are Not All the Same

The untyped machine stored everything in a `Map String Int`. But not every state
carries the same data. An idle controller has no payload at all. A scanning
controller tracks its current frequency and dwell count. A locked controller
additionally knows the signal strength at which it locked. And a cooling-down
controller only remembers the last frequency it was on.

In Part 1, we used type-level lists to track the variables in scope:
`Expr ["x" ::: TInt, "y" ::: TBool] TInt` meant "an integer expression that can
reference `x` (an int) and `y` (a bool)." The variable environment was
*explicit* in the type. We want the same thing here, but now the variable
environment depends on *which state we are in*. We need a way to compute a
type-level list from a type-level state.

This is what a *type family* does. A type family is a function at the type
level: given a type as input, it produces a type as output. We have not needed
one until now because in Part 1 the scope was always provided directly. Here,
the scope is *derived* from the state:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L35-L47

type family NodeVars (s :: ScanState) where
  NodeVars Idle = '[]
  NodeVars Scanning = '["freq" ::: TInt, "dwell" ::: TInt]
  NodeVars Locked = '["freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt]
  NodeVars CoolingDown = '["lastFreq" ::: TInt]

type Env =
  '[ "rssi" ::: TInt,
     "noise" ::: TInt,
     "peakOffset" ::: TInt
   ]

type Scope s = Env ++ NodeVars s
```

`NodeVars` is a closed type family: you give it a `ScanState` and it returns the
corresponding variable list. `NodeVars Idle` reduces to `[]`.
`NodeVars Scanning` reduces to `["freq" ::: TInt, "dwell" ::: TInt]`.
`NodeVars Locked` reduces to
`["freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt]`.
`NodeVars CoolingDown` reduces to `["lastFreq" ::: TInt]`. These reductions
happen at compile time: GHC evaluates the type family when it needs to check
whether two types are equal.

The ambient `Env` is the external observations (rssi, noise floor, peak offset)
that are always available regardless of the current state. These are things the
machine reads from its environment on every tick. `Env` is a plain type synonym,
not a type family, because it does not depend on anything.

And `Scope s = Env ++ NodeVars s` is the *full* variable environment available
to guards and builders while in state `s`. This is the direct analog of the `vs`
parameter in `Expr vs t` from Part 1. A guard expression in state `Scanning` has
type `Expr (Scope Scanning) TBool`, which means it can reference `rssi`,
`noise`, `peakOffset`, `freq`, and `dwell`. It *cannot* reference `lockRssi`,
because that variable only exists in `Scope Locked`.

We need a type-level append to concatenate the environment and state-local
variables:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L18-L20

type family xs ++ ys where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)
```

Another closed type family. It peels elements off the left list and conses them
onto the right, exactly like `(++)` at the value level but operating on promoted
lists. Let us trace through the reduction manually for `Scope Scanning`:

    Scope Scanning
      = Env ++ NodeVars Scanning
      = '["rssi" ::: TInt, "noise" ::: TInt, "peakOffset" ::: TInt]
          ++ '["freq" ::: TInt, "dwell" ::: TInt]
      = '[ "rssi" ::: TInt, "noise" ::: TInt, "peakOffset" ::: TInt
         , "freq" ::: TInt, "dwell" ::: TInt ]

The result is a flat type-level list of five named, typed variables. This is the
scope that guards and builders operate over when the machine is in the
`Scanning` state. Compare with `Scope Idle`:

    Scope Idle
      = Env ++ NodeVars Idle
      = Env ++ '[]
      = Env
      = '["rssi" ::: TInt, "noise" ::: TInt, "peakOffset" ::: TInt]

Only three variables. A guard in the `Idle` state can reference the environment
but nothing else, because there is no state-local payload. And `Scope Locked`:

    Scope Locked
      = Env ++ NodeVars Locked
      = '[ "rssi" ::: TInt, "noise" ::: TInt, "peakOffset" ::: TInt
         , "freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt ]

Six variables. The key insight: these are all just type-level lists with the
same `(Symbol, Ty)` element type. They plug directly into the `Expr vs t` and
`Rec f vs` machinery from Part 1 with zero additional infrastructure. The
expression language already knows how to work with any type-level list of named
typed variables. We just need to compute the right list for each state.

If you try to use a `Scope Locked` expression as a guard while in `Scanning`,
GHC will reject it with a type mismatch on the variable environment. The
"reading a variable from the wrong state" class of bug is gone.

### A Guard Is Just an Expression

Let us build up a concrete guard step by step. While scanning, we want to lock
onto a signal if it is strong enough: specifically, if
`rssi >= noise + threshold`. In our expression language, we need to reference
variables from `Scope Scanning`.

Recall from Part 1 that variable references are de Bruijn-style indices into the
scope. An `EVar` takes an `Index vs (name ::: t)`, which counts how many steps
from the head of the list we need to skip. The scope for `Scanning` is:

    Scope Scanning = Env ++ NodeVars Scanning
                   = '[ "rssi"       ::: TInt     -- position 0 (IZ)
                      , "noise"      ::: TInt     -- position 1 (IS IZ)
                      , "peakOffset" ::: TInt     -- position 2 (IS (IS IZ))
                      , "freq"       ::: TInt     -- position 3 (IS (IS (IS IZ)))
                      , "dwell"      ::: TInt     -- position 4 (IS (IS (IS (IS IZ))))
                      ]

So to reference `rssi` while scanning, we write `EVar IZ`. To reference `noise`,
we write `EVar (IS IZ)`. And so on. We define named aliases so we do not have to
count indices by hand every time:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L149-L156

rssiScanning :: Expr (Scope Scanning) TInt
rssiScanning = EVar IZ

noiseScanning :: Expr (Scope Scanning) TInt
noiseScanning = EVar (IS IZ)

peakOffsetScanning :: Expr (Scope Scanning) TInt
peakOffsetScanning = EVar (IS (IS IZ))
```

Each one is an `EVar` with a specific `Index` that picks out the correct
position in the scope. `rssiScanning` is `EVar IZ` because `rssi` is the first
variable in `Scope Scanning` (which is `Env ++ NodeVars Scanning`).
`noiseScanning` is `EVar (IS IZ)` because it's one step past the head.

Similarly for the `Scanning`-specific locals:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L167-L171

scanFreq :: Expr (Scope Scanning) TInt
scanFreq = EVar (IS (IS (IS IZ)))

scanDwell :: Expr (Scope Scanning) TInt
scanDwell = EVar (IS (IS (IS (IS IZ))))
```

`scanFreq` is at position 3 (`IS (IS (IS IZ))`): three `IS` wrappers skip past
the three `Env` entries, then `IZ` lands on `"freq"`. `scanDwell` is at position
4.

Now we can write the guard itself:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L197-L198

strongEnoughScanning :: Expr (Scope Scanning) TBool
strongEnoughScanning = EOp OLte (EOp OPlus noiseScanning threshold) rssiScanning
```

This is `Expr (Scope Scanning) TBool`: a boolean expression whose free variables
are exactly those available while scanning. It says `(noise + 4) <= rssi`. The
`threshold` and other constants are polymorphic in scope:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L182-L186

threshold :: Expr scope TInt
threshold = EPrim (PInt 4)

maxDwell :: Expr scope TInt
maxDwell = EPrim (PInt 3)
```

Because a literal like `EPrim (PInt 4)` does not reference any variables, it can
live in any scope. So `threshold :: Expr scope TInt` is parametrically
polymorphic in the scope, and can be used in any state's guard.

Because the guard must be `Expr (Scope s) TBool`, you get two guarantees for
free:

1.  It can only reference variables that exist in state `s`.
2.  It must actually produce a boolean. You cannot accidentally use an integer
    expression as a transition predicate.

Similarly for the `Locked` state, we have a separate set of variable references
into `Scope Locked`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L158-L165

rssiLocked :: Expr (Scope Locked) TInt
rssiLocked = EVar IZ

noiseLocked :: Expr (Scope Locked) TInt
noiseLocked = EVar (IS IZ)

peakOffsetLocked :: Expr (Scope Locked) TInt
peakOffsetLocked = EVar (IS (IS IZ))
```

And the `Locked`-specific locals:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L173-L180

lockFreq :: Expr (Scope Locked) TInt
lockFreq = EVar (IS (IS (IS IZ)))

lockRssi :: Expr (Scope Locked) TInt
lockRssi = EVar (IS (IS (IS (IS IZ))))

lockDwell :: Expr (Scope Locked) TInt
lockDwell = EVar (IS (IS (IS (IS (IS IZ)))))
```

Note that `lockFreq` is `EVar (IS (IS (IS IZ)))` and `rssiLocked` is `EVar IZ`.
The types prevent you from accidentally using `rssiScanning` (which is
`Expr (Scope Scanning) TInt`) inside a `Locked` guard. Even though both
reference position 0, they live in different scopes and GHC treats them as
incompatible.

### A Destination Has to Build the Right Payload

When a transition fires, the machine enters a new state. That new state has its
own `NodeVars`, which means we need to *construct* the correct payload from what
is available in the source state.

Why can't we just use an expression for this? Because the destination payload is
a *record* of multiple fields, each with its own type. We need to produce
`NodeVars Locked = ["freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt]`,
which is three separate typed values. A single `Expr` can only produce one
value. What we need is a collection of expressions, one per field, where each
expression's output type matches the corresponding field's type, and each
expression's scope matches the source state.

This is `Rec` again. In Part 1, we used `Rec EValueField vs` to store evaluated
values for each variable in scope. Here we use
`Rec (BuildField scope) (NodeVars to)` to store *expressions* for each variable
in the destination state. Same container, different payload:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L124-L125

data BuildField scope :: (Symbol, Ty) -> Type where
  BuildField :: String -> Expr scope t -> BuildField scope (name ::: t)

data BuildField scope :: (Symbol, Ty) -> Type where
  BuildField :: String -> Expr scope t -> BuildField scope (name ::: t)
```

A `BuildField scope` is indexed by a `(Symbol, Ty)` pair (just like
`EValueField` was). But instead of holding an evaluated `EValue t`, it holds an
`Expr scope t`: a typed expression in the source state's scope that will
*produce* a value of the correct type. The `Build` wrapper bundles a name (for
rendering in diagrams) with the full `Rec` of field expressions.

The `Rec` GADT enforces that we provide exactly one `BuildField` per entry in
`NodeVars to`, in order, with matching types. If `NodeVars Locked` has three
fields, the `Rec` must have exactly three entries. If the first field is
`"freq" ::: TInt`, the first `BuildField` must hold an `Expr scope TInt`.

For example, transitioning from `Scanning` to `Locked` means building the
payload for `Locked`, which requires
`["freq" ::: TInt, "lockRssi" ::: TInt, "dwell" ::: TInt]`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L230-L238

lockOnPeak :: Build (Scope Scanning) Locked
lockOnPeak =
  Build
    "LockOnPeak"
    ( BuildField "freq" scanNextFreq
        :& BuildField "lockRssi" rssiScanning
        :& BuildField "dwell" scanNextDwell
        :& RNil
    )
```

The `BuildField "freq" scanNextFreq` provides the `"freq" ::: TInt` slot using
an expression `scanNextFreq :: Expr (Scope Scanning) TInt`. The
`BuildField "lockRssi" rssiScanning` grabs the current RSSI from the scanning
scope. And `BuildField "dwell" scanNextDwell` increments the dwell counter.

The helper expressions used in builders compose the same way as guards:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L209-L216

scanNextFreq :: Expr (Scope Scanning) TInt
scanNextFreq = EOp OPlus scanFreq peakOffsetScanning

scanNextDwell :: Expr (Scope Scanning) TInt
scanNextDwell = EOp OPlus scanDwell one
```

`scanNextFreq` adds `peakOffset` to the current `freq`. `scanNextDwell`
increments `dwell` by one. Both are `Expr (Scope Scanning) TInt`, so they can
only be used inside builders or guards where the source state is `Scanning`.

If you forget a field, GHC will tell you the `Rec` doesn't match
`NodeVars Locked`. If you add an extra field, same error. If you provide an
expression of the wrong type for one of the fields, the index won't unify. The
shape of the destination payload is enforced entirely by the type.

Here is a simpler builder that constructs `NodeVars Scanning` from `Scope Idle`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L221-L228

startScan :: Build (Scope Idle) Scanning
startScan =
  Build
    "StartScan"
    ( BuildField "freq" baseFreq
        :& BuildField "dwell" zero
        :& RNil
    )
```

`Scope Idle` is just `Env` (since `NodeVars Idle = []`), so the builder can
reference `rssi`, `noise`, and `peakOffset` if it wants to, but here it just
uses constants: `baseFreq = 2400` and `zero = 0`. And since both are polymorphic
in scope (`Expr scope TInt`), they work fine in `Scope Idle`.

And one that goes the other direction, from `Locked` back to `Scanning`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L240-L247

keepScanning :: Build (Scope Locked) Scanning
keepScanning =
  Build
    "TrackDrift"
    ( BuildField "freq" lockNextFreq
        :& BuildField "dwell" lockDwell
        :& RNil
    )
```

This builder constructs
`NodeVars Scanning = ["freq" ::: TInt, "dwell" ::: TInt]` using expressions from
`Scope Locked`. `lockNextFreq` adds the peak offset to the current locked
frequency, and `lockDwell` carries the existing dwell count forward.

### One Transition

Now we can put it all together. A transition packages a source state, a trigger,
a destination state, a guard expression, and a destination builder:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L132-L139

data Transition env from trigger to where
  Transition ::
    SState from ->
    STrigger trigger ->
    SState to ->
    Expr (env ++ NodeVars from) TBool ->
    Build (env ++ NodeVars from) to ->
    Transition env from trigger to
```

The type indices enforce:

-   The guard expression lives in `Expr (env ++ NodeVars from) TBool`: it can
    only reference variables available in the source state, and must return a
    boolean.
-   The builder lives in `Build (env ++ NodeVars from) to`: it builds the
    destination payload from the source scope.

The `env` parameter is shared across all transitions in a machine. It represents
the external environment (sensor readings, in our case). The type parameter is
kept abstract so that the same `Transition` type could be instantiated against
different environments for different machine families.

Here is a simple one: from `Idle`, on `Start`, transition to `Scanning`
unconditionally, building the initial scanning payload:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L221-L228

startScan :: Build (Scope Idle) Scanning
startScan =
  Build
    "StartScan"
    ( BuildField "freq" baseFreq
        :& BuildField "dwell" zero
        :& RNil
    )
```

Wait, that is the builder we already defined. The actual *transition* that uses
it looks like:

``` haskell
Transition SIdle SStart SScanning true startScan
```

The guard is `true` (always fire), and the builder provides `freq = 2400` and
`dwell = 0`. Let us trace through the types. `Transition` expects:

-   `SState from` -\> we give `SIdle`, so `from ~ Idle`
-   `STrigger trigger` -\> `SStart`, so `trigger ~ Start`
-   `SState to` -\> `SScanning`, so `to ~ Scanning`
-   `Expr (env ++ NodeVars from) TBool` -\> `true :: Expr scope TBool`, which
    unifies with any `scope`, so this is fine
-   `Build (env ++ NodeVars from) to` -\>
    `startScan :: Build (Scope Idle)   Scanning`. The type
    `Scope Idle = Env ++ NodeVars Idle = Env ++ [] = Env`, and
    `env ++ NodeVars Idle = env ++ []`. This unifies when `env ~ Env`.

Everything checks out. Now, what happens if we try to transition to `Locked`
using `startScan`?

``` haskell
ghci> :t Transition SIdle SStart SLocked true startScan
<interactive> error:
    Couldn't match type 'Scanning' with 'Locked'
      Expected: Build (env ++ NodeVars Idle) Locked
        Actual: Build (Scope Idle) Scanning
```

`startScan :: Build (Scope Idle) Scanning`, but we need a
`Build (Scope Idle) Locked`. GHC rejects it because `Scanning` is not `Locked`.

And what if we try to use `strongEnoughScanning` (which reads `freq` and
`dwell`) as the guard for an `Idle` transition?

``` haskell
ghci> :t Transition SIdle SStart SScanning strongEnoughScanning startScan
<interactive> error:
    Couldn't match type: '[]
                   with: ["freq" ::: TInt, "dwell" ::: TInt]
      Expected: Build (env ++ NodeVars Idle) Scanning
        Actual: Build (Scope Idle) Scanning
```

The error is on `startScan` here, because GHC tries to unify
`env ++ NodeVars Idle` (from the `Transition` signature) with `Scope Scanning`
(from `strongEnoughScanning`), which forces `env ~ Env ++ NodeVars Scanning`.
But then `startScan` expects
`env ++ NodeVars Idle = (Env ++ NodeVars Scanning) ++ []`, which doesn't reduce
to `Env ++ [] = Env`. The lists don't match up, and GHC reports the residual
mismatch.

The takeaway: you cannot mix expressions from different scopes in the same
transition. The type indices propagate from the singletons through the guard and
builder, and any inconsistency shows up as a unification failure.

### Many Transitions Need a Wrapper

We have a problem. `Transition SIdle SStart SScanning true startScan` has type
`Transition Env Idle Start Scanning`. And the peak-lock transition has type
`Transition Env Scanning Peak Locked`. These are different types. You cannot put
them in the same list.

This is the same problem we faced with `EValue` in Part 1: an `EValue TInt` and
an `EValue TBool` are different types, so we needed `SomeValue` to erase the
index and store them together. Here we need to erase `from`, `trigger`, and
`to`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L141-L144

data SomeTransition env where
  SomeTransition :: Transition env from trigger to -> SomeTransition env

newtype Machine env = Machine [SomeTransition env]
```

`SomeTransition env` says: "I am a transition in the given environment, but I am
not telling you which states or trigger I connect. Trust that my internals are
consistent." The `Machine` is then just a list of these wrapped transitions.

We lose the ability to statically distinguish transitions from each other in the
list, but we *keep* the guarantee that each individual transition is internally
consistent. The existential seals the proof inside: by the time you
pattern-match on `SomeTransition (Transition from trigger to guard build)`, you
recover fresh type variables for `from`, `trigger`, and `to`, and you know that
`guard` and `build` are consistent with them, even though you do not know what
those types are from the outside. This is enough for rendering, because the
singletons (`SState from`, `STrigger trigger`, `SState to`) are stored inside
and give you back the runtime names when you need them.

### A Small Machine

With all the pieces in hand, let's define a minimal scan controller with five
transitions:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L264-L302

quickScanMachine :: Machine Env
quickScanMachine =
  Machine
    [ SomeTransition $
        Transition
          SIdle
          SStart
          SScanning
          true
          startScan,
      SomeTransition $
        Transition
          SScanning
          SPeak
          SLocked
          strongEnoughScanning
          lockOnPeak,
      SomeTransition $
        Transition
          SScanning
          STimeout
          SCoolingDown
          scanTooLong
          coolFromScanning,
      SomeTransition $
        Transition
          SLocked
          SDrift
          SScanning
          true
          keepScanning,
      SomeTransition $
        Transition
          SCoolingDown
          SReset
          SIdle
          true
          idle
    ]
```

Read it as a graph: Idle -Start-\> Scanning, Scanning -Peak/strongEnough-\>
Locked, Scanning -Timeout/tooLong-\> CoolingDown, Locked -Drift-\> Scanning,
CoolingDown -Reset-\> Idle. Each transition carries a typed guard and a typed
builder, both verified against the source state's scope.

### A Less Toy Machine

The `quickScanMachine` is fine for demonstrating the DSL, but it does not
justify the ceremony. Here is a more interesting one with self-transitions,
multiple paths through the same nodes, and timeout behavior at different stages:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L304-L363

dwellScanMachine :: Machine Env
dwellScanMachine =
  Machine
    [ SomeTransition $
        Transition
          SIdle
          SStart
          SScanning
          true
          startScan,
      SomeTransition $
        Transition
          SScanning
          SPeak
          SLocked
          strongEnoughScanning
          lockOnPeak,
      SomeTransition $
        Transition
          SScanning
          STimeout
          SCoolingDown
          scanTooLong
          coolFromScanning,
      SomeTransition $
        Transition
          SLocked
          SPeak
          SLocked
          strongEnoughLocked
          ( Build
              "RefreshLock"
              ( BuildField "freq" lockFreq
                  :& BuildField "lockRssi" rssiLocked
                  :& BuildField "dwell" lockNextDwell
                  :& RNil
              )
          ),
      SomeTransition $
        Transition
          SLocked
          SDrift
          SScanning
          true
          keepScanning,
      SomeTransition $
        Transition
          SLocked
          STimeout
          SCoolingDown
          lockedTooLong
          coolFromLock,
      SomeTransition $
        Transition
          SCoolingDown
          SReset
          SIdle
          true
          idle
    ]
```

The interesting additions: `Locked -Peak/strongEnough-> Locked` is a
self-transition that refreshes the lock (updates `lockRssi` and increments
`dwell`). `Locked -Timeout/lockedTooLong-> CoolingDown` adds a timeout path out
of the locked state. Both share the same expression language, the same
`Scope Locked`, and the same `Build` mechanism. The self-transition is
particularly satisfying: a `Build (Scope Locked) Locked` constructs
`NodeVars Locked` from `Scope Locked`, which *contains* `NodeVars Locked`. So a
self-transition can read its own payload and produce an updated version.

Notice that `dwellScanMachine` reuses the same named expressions
(`strongEnoughLocked`, `lockNextFreq`, `lockNextDwell`) without any risk of
accidentally mixing up which state they belong to. The types keep them honest.

### Things GHC Will Not Let Us Say

To make this concrete, here are the exact classes of error that are now
compile-time rejections, with real GHC output from attempting them in ghci.

**Using a variable from the wrong state:**

We have `lockRssi :: Expr (Scope Locked) TInt`. Let's try using it as a guard
while `Scanning`:

``` haskell
ghci> :t Transition SScanning SPeak SLocked lockRssi lockOnPeak
<interactive> error:
    Couldn't match type 'TInt' with 'TBool'
      Expected: Expr (env ++ NodeVars Scanning) TBool
        Actual: Expr (Scope Locked) TInt
```

Two problems: `lockRssi` returns `TInt` (not `TBool`), and it lives in
`Scope Locked` (not `Scope Scanning`). GHC reports whichever mismatch it
encounters first.

**A guard that returns the wrong type:**

We have `scanFreq :: Expr (Scope Scanning) TInt`. Even though it lives in the
right scope, it is not boolean:

``` haskell
ghci> :t Transition SScanning SPeak SLocked scanFreq lockOnPeak
<interactive> error:
    Couldn't match type 'TInt' with 'TBool'
      Expected: Expr (env ++ NodeVars Scanning) TBool
        Actual: Expr (Scope Scanning) TInt
```

The guard slot demands `TBool`. An integer expression does not fit.

**A builder with the wrong destination:**

We have `startScan :: Build (Scope Idle) Scanning`. Let's try using it to
transition into `Locked`:

``` haskell
ghci> :t Transition SIdle SStart SLocked true startScan
<interactive> error:
    Couldn't match type 'Scanning' with 'Locked'
      Expected: Build (env ++ NodeVars Idle) Locked
        Actual: Build (Scope Idle) Scanning
```

The builder constructs `NodeVars Scanning`, but the transition's destination is
`Locked`. `Scanning` is not `Locked`.

The important thing here is not that this is ergonomic. It is absolutely not
ergonomic. Writing `EVar (IS (IS (IS IZ)))` to say `freq` is painful and
error-prone in its own way (though Part 1's `ListIx` helper takes the edge off).
The important thing is that the illegal versions are not values. If either
machine's `Peak` guard accidentally returned a `TInt`, or if a transition to
`Locked` forgot to construct `lockRssi`, there would be no JavaScript to debug
because the Haskell program that generates the JavaScript would not compile.

I want to dwell on this for a moment, because it is genuinely different from
"writing good tests." Tests check specific scenarios. Types check *all*
scenarios simultaneously. When I add a new field to `NodeVars Locked`, every
builder that targets `Locked` immediately becomes a type error until I provide
the new field. Every single one. I do not need to remember which transitions go
to `Locked`. I do not need to grep. GHC will enumerate them for me in the form
of errors. This is the kind of property you cannot get from testing, no matter
how thorough, because tests are intensional (they check what you wrote) while
types are extensional (they check what you *didn't* write).

### Two Machines, One Language

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L365-L369

machines :: [(String, Machine Env)]
machines =
  [ ("Quick scan", quickScanMachine),
    ("Dwell scan", dwellScanMachine)
  ]
```

One generated machine is a demo. Two generated machines show that we built a
*language*. `quickScanMachine` and `dwellScanMachine` have different numbers of
transitions, different graph topologies, and different self-transition
behaviors, but they share the same `Transition`/`Build`/`Expr` types, the same
`ScanState` vocabulary, and the same rendering backends. The DSL is general
enough to describe an entire family of machines, not just one.

And because the machines are just values (lists of existentially-wrapped
transition records), we can store them, pass them around, and process them with
any function that knows how to walk the structure. There is no metaprogramming,
no Template Haskell, no code generation step. The machines are Haskell values
that happen to carry proofs of their own consistency.

## Rendering

The first backend does not need to run the machine at all. Because the machine
is data (a list of typed transition records), we can walk the structure and
render it as a graph.

### The Expression Renderer

Before we can render transitions, we need to render guard expressions as
human-readable labels. This reuses the same `Rec NameField` / `indexRec` pattern
from Part 1's pretty-printer: we carry a record of variable names indexed by the
scope, and when we hit an `EVar i`, we `indexRec i names` to get the string name
at that position.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L77-L78

data NameField :: (Symbol, Ty) -> Type where
  NameField :: String -> NameField field
```

A `NameField` is indexed by a `(Symbol, Ty)` pair but just holds a `String`. It
is the rendering analog of `EValueField`: where `EValueField` held a runtime
value for evaluation, `NameField` holds a display name for rendering.

We define name records for each state's scope:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L80-L115

envNames :: Rec NameField Env
envNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& RNil

scanningNames :: Rec NameField (Scope Scanning)
scanningNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "freq"
    :& NameField "dwell"
    :& RNil

lockedNames :: Rec NameField (Scope Locked)
lockedNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "freq"
    :& NameField "lockRssi"
    :& NameField "dwell"
    :& RNil

coolingDownNames :: Rec NameField (Scope CoolingDown)
coolingDownNames =
  NameField "rssi"
    :& NameField "noise"
    :& NameField "peakOffset"
    :& NameField "lastFreq"
    :& RNil
```

And a dispatch function that selects the right one given a state singleton:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L117-L122

stateNames :: SState s -> Rec NameField (Scope s)
stateNames = \case
  SIdle -> idleNames
  SScanning -> scanningNames
  SLocked -> lockedNames
  SCoolingDown -> coolingDownNames
```

Pattern matching on `SIdle` refines the return type to
`Rec NameField (Scope Idle)`, which is exactly what `renderExprDot` will expect
when rendering a guard from the `Idle` state. The refinement is automatic:
`stateNames SScanning` returns `Rec NameField (Scope Scanning)`.

The expression renderer itself is straightforward:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L408-L427

renderExprDot :: Rec NameField vs -> Expr vs t -> String
renderExprDot names = \case
  EPrim (PInt n) -> show n
  EPrim (PBool b) -> if b then "true" else "false"
  EPrim (PString s) -> show s
  EVar i -> case indexRec i names of
    NameField n -> n
  EOp o x y ->
    parens $
      renderExprDot names x
        ++ " "
        ++ renderOp o
        ++ " "
        ++ renderExprDot names y
  ERecord {} -> "<record>"
  EAccess {} -> "<field>"
  EChoice {} -> "<choice>"
  ECase {} -> "<case>"
  ELambda {} -> "<function>"
  EApply {} -> "<apply>"
```

`EVar i` looks up the name at index `i` in the names record. `EOp` renders infix
with parentheses. Primitives render as literals. The remaining cases (records,
sums, lambdas) are not used in our machine guards, so they render as placeholder
strings. In a production system you would handle them properly, but for SDR scan
guards we only need arithmetic and comparisons.

### The Transition Renderer

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L371-L406

compileDot :: [(String, Machine Env)] -> String
compileDot xs =
  block
    [ "digraph TypedSmLc {",
      "  rankdir=LR;",
      "  node [shape=box];",
      indent 2 (intercalate "\n" (map (uncurry renderMachineDot) xs)),
      "}"
    ]

renderMachineDot :: String -> Machine Env -> String
renderMachineDot name (Machine transitions) =
  block
    [ "subgraph cluster_" ++ dotId name ++ " {",
      "  label=" ++ jsString name ++ ";",
      indent 2 (intercalate "\n" (map (renderSomeTransitionDot name) transitions)),
      "}"
    ]

renderSomeTransitionDot :: String -> SomeTransition Env -> String
renderSomeTransitionDot name (SomeTransition t) = renderTransitionDot name t

renderTransitionDot :: String -> Transition Env from trigger to -> String
renderTransitionDot name (Transition from trigger to guard build) =
  dotNode name (stateName from)
    ++ " -> "
    ++ dotNode name (stateName to)
    ++ " [label="
    ++ jsString
      ( triggerName trigger
          ++ " ["
          ++ renderExprDot (stateNames from) guard
          ++ "] / "
          ++ buildName build
      )
    ++ "];"
```

The key function is `renderTransitionDot`: it pattern-matches on a
`Transition env from trigger to`, extracts the singleton state names via
`stateName from` and `stateName to`, and renders the guard expression using
`renderExprDot (stateNames from) guard`. The `stateNames from` call uses the
`from` singleton to select the correct name record, which is type-compatible
with the guard because the `Transition` type guarantees that the guard lives in
`env ++ NodeVars from`.

The edge labels in the graph come directly from the typed structure. If you
change a guard expression, the diagram updates. If you rename a state, the
labels update. There is no separate "documentation" to keep in sync with the
code, because the diagram *is* the code.

### The Output

Running the program with `--dot` gives us:

    $ runghc MachineStage5.hs --dot
    digraph TypedSmLc {
      rankdir=LR;
      node [shape=box];
      subgraph cluster_Quick_scan {
        label="Quick scan";
        Quick_scan_Idle -> Quick_scan_Scanning [label="Start [true] / StartScan"];
        Quick_scan_Scanning -> Quick_scan_Locked [label="Peak [((noise + 4) <= rssi)] / LockOnPeak"];
        Quick_scan_Scanning -> Quick_scan_CoolingDown [label="Timeout [(3 <= dwell)] / CoolScanner"];
        Quick_scan_Locked -> Quick_scan_Scanning [label="Drift [true] / TrackDrift"];
        Quick_scan_CoolingDown -> Quick_scan_Idle [label="Reset [true] / ResetToIdle"];
      }
      subgraph cluster_Dwell_scan {
        label="Dwell scan";
        Dwell_scan_Idle -> Dwell_scan_Scanning [label="Start [true] / StartScan"];
        Dwell_scan_Scanning -> Dwell_scan_Locked [label="Peak [((noise + 4) <= rssi)] / LockOnPeak"];
        Dwell_scan_Scanning -> Dwell_scan_CoolingDown [label="Timeout [(3 <= dwell)] / CoolScanner"];
        Dwell_scan_Locked -> Dwell_scan_Locked [label="Peak [((noise + 4) <= rssi)] / RefreshLock"];
        Dwell_scan_Locked -> Dwell_scan_Scanning [label="Drift [true] / TrackDrift"];
        Dwell_scan_Locked -> Dwell_scan_CoolingDown [label="Timeout [(3 <= dwell)] / CoolLock"];
        Dwell_scan_CoolingDown -> Dwell_scan_Idle [label="Reset [true] / ResetToIdle"];
      }
    }

That gives us a rendered Graphviz view of the two machines: ([full size
here](/img/entries/typed-sm-lc/machines.png))

![Graphviz rendering of the typed SDR scan
machines](/img/entries/typed-sm-lc/machines.png "Typed SDR scan machines")

The generated DOT file is available in the code samples:

The `Peak` trigger appears in both machines but with different guard conditions:
`quickScanMachine` only has `Peak` from `Scanning`, while `dwellScanMachine`
also has a `Peak` self-transition on `Locked` (the lock refresh). The guard
labels (`(noise + 4) <= rssi`) are generated directly by the expression
renderer, not hand-written strings. If we changed `threshold` from `4` to `6`,
the diagram would automatically read `(noise + 6) <= rssi` without touching any
rendering code.

### What Else Can We Do With This?

The same `Machine Env` value that produced a Graphviz diagram could also drive:

-   A Haskell simulation: pattern-match on transitions, `eval` the guards
    against concrete sensor values, and step the machine forward. Because `eval`
    from Part 1 is total, the simulation cannot crash on a well-typed guard.
-   Test-case generation: enumerate reachable states by trying all triggers at
    each state and evaluating guards symbolically.
-   Documentation tables: render each transition as a row with source, trigger,
    guard (pretty-printed), destination, and payload fields.
-   Other language backends: compile the guards and builders to JavaScript, C,
    PureScript, or anything else that can evaluate arithmetic and comparisons.

We will do that last one in Part 3: compile both machines to JavaScript and
embed the result as interactive state-machine simulators directly in the page.

## The Next Step

We set out to put our typed expression language inside a larger typed structure,
and we got a state-machine DSL where:

-   States are a closed kind, not strings. Misspellings are type errors.
-   Each state's payload is declared at the type level via `NodeVars`.
-   Guards are `Expr (Scope s) TBool`: they can only reference variables
    available in their source state, and they must produce a boolean.
-   Builders are `Rec (BuildField scope) (NodeVars to)`: they must produce
    exactly the fields the destination state requires, each with the correct
    type.
-   The Graphviz renderer walks the typed structure and produces labels directly
    from the expression pretty-printer, using `stateNames` to supply
    human-readable variable names at each position.

The expression language from Part 1 was useful on its own, but its real power is
as a building block. We threaded it through a larger typed structure and got a
machine description where GHC verifies internal consistency at every joint. The
guard references the right variables. The builder produces the right payload.
The destination exists. All from the same type indices that made `eval` total in
Part 1.

I think the thing that surprised me most, working with this pattern on real
projects, is how *boring* it becomes once you set it up. Adding a new transition
is mechanical: pick the source and destination singletons, write the guard in
the source scope, write the builder targeting the destination payload, and GHC
either accepts it or tells you exactly what is wrong. There are no surprises. No
"it compiles but does it actually work?" anxiety. The machine either typechecks
or it does not, and if it typechecks, you can be confident that the generated
artifacts (diagrams, JavaScript, whatever) will be consistent with each other.

That boringness is the point. The interesting work is in *designing* the state
space. Once the types are right, filling in the transitions is paint by numbers.
I would rather spend my creative energy on "what should the states be" than on
"did I remember to update the builder for the third transition out of Locked
after adding a field."

In [Part
3](/entry/extreme-haskell-typed-state-machines-javascript-compiler.html), we
compile both machines to JavaScript and embed them as live interactive demos.
The same typed expressions that produced the Graphviz labels will produce the
runtime guard and builder functions in the generated code. And because both
backends consume the same `Machine Env` value, they are guaranteed to agree with
each other. One source of truth, multiple outputs. That is the promise of this
whole exercise, and Part 3 is where it lands.

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

