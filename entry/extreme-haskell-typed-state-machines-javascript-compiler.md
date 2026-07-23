Extreme Haskell: Typed State Machines with Typed Lambda Calculus (Part 3)
=========================================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-haskell-typed-state-machines-javascript-compiler.html)

Welcome back! This is the final post of our journey through a typed state
machine DSL. In [Part
1](/entry/extreme-haskell-typed-state-machines-typed-expressions.html), we made
invalid expressions unrepresentable. In [Part
2](/entry/extreme-haskell-typed-state-machines.html), we used those expressions
to define two typed state machines.

This is where the machinery earns its keep: we compile both accepted machines to
JavaScript and embed the generated result in the page.

Again, all of the code here is [available
online](https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/flake.nix),
and the generated JavaScript in this page is produced by running the final
stage:

``` bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ runghc MachineStage5.hs
```

## The JavaScript Backend

At this point, the generated backend can be almost offensively simple. We are no
longer asking JavaScript to prove anything. We are only asking it to export a
state machine that Haskell already accepted.

### A Machine Is Just Data Now

> TODO: Start from the pressure point: we have two Haskell values that already
> passed the type checker. The JavaScript backend should not be an independent
> validator. It should be a boring consumer of accepted data.

> TODO: Show the smallest possible generated shape first: state names, trigger
> names, and transition records. Let the reader see the target before seeing the
> compiler.

### The Dumbest Possible Output

> TODO: Include a tiny generated JavaScript excerpt early. Not the full file,
> just enough to make the target concrete: a machine name, a state, a trigger,
> and one transition object.

### Backend Field Names

In [Part 1](/entry/extreme-haskell-typed-state-machines-typed-expressions.html),
we used typed records to line up field names and field types. For the JavaScript
backend, we use that same shape to associate each typed field with the
JavaScript name we want to emit:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L77-L122

data NameField :: (Symbol, Ty) -> Type where
  NameField :: String -> NameField field

stateNames :: SState s -> Rec NameField (Scope s)
stateNames = \case
  SIdle -> idleNames
  SScanning -> scanningNames
  SLocked -> lockedNames
  SCoolingDown -> coolingDownNames
```

> TODO: Explain that this is the bridge from type-level variable membership to
> mundane backend names. `Index` proves the field exists; `Rec NameField scope`
> lets the compiler recover the concrete string to print.

### One Guard Expression

Before compiling a whole machine, compile one expression. A guard expression is
the smallest useful example because it must become JavaScript code returning a
boolean:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L441-L467

renderExpr :: Rec NameField vs -> Expr vs t -> String
renderExpr names = \case
  EPrim (PInt n) -> show n
  EPrim (PBool b) -> if b then "true" else "false"
  EPrim (PString s) -> jsString s
  EVar i -> case indexRec i names of
    NameField n -> "scope." ++ n
  EOp o x y ->
    parens $
      renderExpr names x
        ++ " "
        ++ renderOp o
        ++ " "
        ++ renderExpr names y
  ERecord {} -> error "records are not part of this JavaScript demo backend"
  EAccess {} -> error "records are not part of this JavaScript demo backend"
  EChoice {} -> error "sums are not part of this JavaScript demo backend"
  ECase {} -> error "sums are not part of this JavaScript demo backend"
  ELambda {} -> error "functions are not part of this JavaScript demo backend"
  EApply {} -> error "functions are not part of this JavaScript demo backend"

renderOp :: Op a b c -> String
renderOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"
```

> TODO: Walk through each expression case: primitives, variables via
> `stateNames`, operators, and lambdas if we keep them in scope. Explain why
> variable rendering is driven by the typed environment instead of raw strings.

> TODO: Show one source expression and its emitted JavaScript. This should feel
> like the Part 1 pretty-printer, except the target language is JavaScript.

### One Destination Payload

> TODO: Explain the builder compiler as the point where the typed destination
> payload becomes an ordinary JavaScript object. This should be introduced only
> after the expression compiler, because builder fields contain expressions.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L514-L521

renderBuild :: Rec NameField scope -> Build scope to -> String
renderBuild names (Build _ fields) =
  "{" ++ intercalate ", " (renderBuildFields names fields) ++ "}"

renderBuildFields :: Rec NameField scope -> Rec (BuildField scope) fields -> [String]
renderBuildFields _ RNil = []
renderBuildFields names (BuildField field expr :& rest) =
  (field ++ ": " ++ renderExpr names expr) : renderBuildFields names rest
```

> TODO: Show one destination builder and the emitted object literal. The object
> is boring precisely because the type checking already happened.

### One Transition

> TODO: Show how states/triggers become strings, guards become JavaScript
> predicates, and destination builders become payload constructors. This should
> be the heart of the code-generation walkthrough.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L492-L512

renderSomeTransition :: SomeTransition Env -> String
renderSomeTransition (SomeTransition t) = renderTransition t

renderTransition :: Transition Env from trigger to -> String
renderTransition (Transition from trigger to guard build) =
  block
    [ "{",
      "  from: " ++ jsString (stateName from) ++ ",",
      "  trigger: " ++ jsString (triggerName trigger) ++ ",",
      "  to: " ++ jsString (stateName to) ++ ",",
      "  event: " ++ jsString (buildName build) ++ ",",
      "  guard: function (env, payload) {",
      "    var scope = Object.assign({}, env, payload);",
      "    return " ++ renderExpr (stateNames from) guard ++ ";",
      "  },",
      "  build: function (env, payload) {",
      "    var scope = Object.assign({}, env, payload);",
      "    return " ++ renderBuild (stateNames from) build ++ ";",
      "  }",
      "}"
    ]
```

> TODO: Show one generated transition. This is where the previous two pieces
> come together: a guard expression plus a destination payload builder.

### One Machine

Now we can compile a full machine:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L477-L490

renderMachine :: String -> Machine Env -> String
renderMachine name (Machine transitions) =
  block
    [ "{",
      "  name: " ++ jsString name ++ ",",
      "  initialState: \"Idle\",",
      "  initialEnv: { rssi: 9, noise: 4, peakOffset: 2 },",
      "  initialPayload: {},",
      "  triggers: [\"Start\", \"Peak\", \"Drift\", \"Timeout\", \"Reset\"],",
      "  transitions: [",
      indent 4 (intercalate ",\n" (map renderSomeTransition transitions)),
      "  ]",
      "}"
    ]
```

> TODO: Start with the smaller machine. Show enough generated output to connect
> states, triggers, guards, and destination builders to one coherent exported
> JavaScript value.

### Two Machines

And here is the final compiler boundary. It emits a JavaScript file exporting
both machines to the page:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/MachineStage5.hs#L469-L475

compileJs :: [(String, Machine Env)] -> String
compileJs xs =
  unlines
    [ "window.typedSmLcMachines = [",
      indent 2 (intercalate ",\n" (map (uncurry renderMachine) xs)),
      "];"
    ]
```

> TODO: Point out that `compileJs machines` is where the "two distinct machines"
> payoff becomes concrete. The compiler does not care whether the graph is the
> quick scanner or the dwell scanner.

### The Generated File

> TODO: Include a small excerpt of `/js/typed-sm-lc-machine.generated.js`, just
> enough to show that the generated output is ordinary JavaScript data and
> functions. Avoid dumping the whole file.

### Page Scaffolding

The page scaffolding is normal hand-written JavaScript. Its interesting
dependency is `/js/typed-sm-lc-machine.generated.js`; that file is the thing
emitted by the Haskell program above, and the hand-written scaffold only reads
the compiled machines it leaves on `window.typedSmLcMachines`.

> TODO: Describe the division of labor: generated file owns machine semantics;
> hand-written file owns DOM, buttons, history, and display state.

### The Embedded Demo

The JavaScript below has no idea what a GADT is. It does not need to. By the
time the generated JavaScript exists, the type checker has already ruled out the
invalid machines.

::: {#typed-sm-lc-demo}
:::

### What Failure Looks Like

> TODO: Add one intentionally invalid Haskell machine edit and describe the GHC
> error at a high level. The important point is that the failure happens before
> any generated JavaScript exists.

### Other Backends

> TODO: Sketch how the same `Machine Env` value could target Graphviz, a Haskell
> simulator, test-case generation, PureScript, Dhall, or a C-ish embedded DSL.
> This keeps the JavaScript backend from feeling like the only payoff.

### Was This Worth It?

> TODO: Close honestly. This is not a pattern for every CRUD form. It is a
> pattern for when one specification needs to drive multiple interpreters and
> backends, and when invalid states are expensive enough to make the type-level
> ceremony pay rent.

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

