---
title: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus (Part 3)"
categories: Haskell
series: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus"
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2026/02/07 12:32:55
identifier: typed-sm-lc-3
slug: extreme-haskell-typed-state-machines-javascript-compiler
script: /js/typed-sm-lc-machine.generated.js, /js/typed-sm-lc-demo.js
---

Welcome back! This is the final post of our journey through a typed state
machine DSL. In [Part 1][], we made invalid expressions unrepresentable. In
[Part 2][], we used those expressions to define two typed state machines.

This is where the machinery earns its keep: we compile both accepted machines
to JavaScript and embed the generated result in the page.

[Part 1]: /entry/extreme-haskell-typed-state-machines-typed-expressions.html
[Part 2]: /entry/extreme-haskell-typed-state-machines.html

Again, all of the code here is [available online][code samples], and the
generated JavaScript in this page is produced by running the final stage:

!!![code samples]:typed-sm-lc/flake.nix

```bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ runghc MachineStage5.hs
```

The JavaScript Backend
----------------------

At this point, the generated backend can be almost offensively simple. We are
no longer asking JavaScript to prove anything. We are only asking it to export a
state machine that Haskell already accepted.

### A Machine Is Just Data Now

> TODO: Start from the pressure point: we have two Haskell values that already
> passed the type checker. The JavaScript backend should not be an independent
> validator. It should be a boring consumer of accepted data.

> TODO: Show the smallest possible generated shape first: state names, trigger
> names, and transition records. Let the reader see the target before seeing
> the compiler.

### The Dumbest Possible Output

> TODO: Include a tiny generated JavaScript excerpt early. Not the full file,
> just enough to make the target concrete: a machine name, a state, a trigger,
> and one transition object.

### Backend Field Names

In [Part 1][], we used typed records to line up field names and field types.
For the JavaScript backend, we use that same shape to associate each typed
field with the JavaScript name we want to emit:

```haskell
!!!typed-sm-lc/MachineStage5.hs "data NameField" "stateNames ::"
```

> TODO: Explain that this is the bridge from type-level variable membership to
> mundane backend names. `Index` proves the field exists; `Rec NameField scope`
> lets the compiler recover the concrete string to print.

### One Guard Expression

Before compiling a whole machine, compile one expression. A guard expression is
the smallest useful example because it must become JavaScript code returning a
boolean:

```haskell
!!!typed-sm-lc/MachineStage5.hs "renderExpr ::" "renderOp ::"
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

```haskell
!!!typed-sm-lc/MachineStage5.hs "renderBuild ::" "renderBuildFields ::"
```

> TODO: Show one destination builder and the emitted object literal. The object
> is boring precisely because the type checking already happened.

### One Transition

> TODO: Show how states/triggers become strings, guards become JavaScript
> predicates, and destination builders become payload constructors. This should
> be the heart of the code-generation walkthrough.

```haskell
!!!typed-sm-lc/MachineStage5.hs "renderSomeTransition ::" "renderTransition ::"
```

> TODO: Show one generated transition. This is where the previous two pieces
> come together: a guard expression plus a destination payload builder.

### One Machine

Now we can compile a full machine:

```haskell
!!!typed-sm-lc/MachineStage5.hs "renderMachine ::"
```

> TODO: Start with the smaller machine. Show enough generated output to connect
> states, triggers, guards, and destination builders to one coherent exported
> JavaScript value.

### Two Machines

And here is the final compiler boundary. It emits a JavaScript file exporting
both machines to the page:

```haskell
!!!typed-sm-lc/MachineStage5.hs "compileJs ::"
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

<div id="typed-sm-lc-demo"></div>

### What Failure Looks Like

> TODO: Add one intentionally invalid Haskell machine edit and describe the GHC
> error at a high level. The important point is that the failure happens before
> any generated JavaScript exists.

### Other Backends

> TODO: Sketch how the same `Machine Env` value could target Graphviz, a
> Haskell simulator, test-case generation, PureScript, Dhall, or a C-ish
> embedded DSL. This keeps the JavaScript backend from feeling like the only
> payoff.

### Was This Worth It?

> TODO: Close honestly. This is not a pattern for every CRUD form. It is a
> pattern for when one specification needs to drive multiple interpreters and
> backends, and when invalid states are expensive enough to make the type-level
> ceremony pay rent.
