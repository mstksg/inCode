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

### Generating JavaScript

At this point, the generated backend can be almost offensively simple. We are
no longer asking JavaScript to prove anything. We are only asking it to export a
state machine that Haskell already accepted.

### The Backend Boundary

> TODO: Emphasize the central idea: the compiler does not need to re-check the
> program. The Haskell constructors and types already admitted only valid
> machine descriptions, so the backend can be mostly pretty-printing.

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

### Compiling Expressions

Here's the expression compiler for the JavaScript backend:

```haskell
!!!typed-sm-lc/MachineStage5.hs "renderExpr ::" "renderOp ::"
```

> TODO: Walk through each expression case: primitives, variables via
> `stateNames`, operators, and lambdas if we keep them in scope. Explain why
> variable rendering is driven by the typed environment instead of raw strings.

### Compiling Destination Payloads

> TODO: Explain the builder compiler as the point where the typed destination
> payload becomes an ordinary JavaScript object. The generated object is boring
> because the interesting thing happened at construction time.

### Compiling Transitions

> TODO: Show how states/triggers become strings, guards become JavaScript
> predicates, and destination builders become payload constructors. This should
> be the heart of the code-generation walkthrough.

### Compiling Whole Machines

And here is the state-machine compiler. It emits a JavaScript file exporting
both machines to the page:

```haskell
!!!typed-sm-lc/MachineStage5.hs "compileJs ::" "renderMachine ::" "renderSomeTransition ::" "renderTransition ::" "renderBuild ::" "renderBuildFields ::"
```

> TODO: Point out that `compileJs machines` is where the "two distinct machines"
> payoff becomes concrete. The compiler does not care whether the graph is the
> quick scanner or the dwell scanner.

### Generated JavaScript

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

> TODO: Sketch how the same `Machine Env` value could target Graphviz,
> PureScript, Dhall, a C-ish embedded DSL, or a Haskell simulator. This keeps
> the JavaScript backend from feeling like the only payoff.

### Was This Worth It?

> TODO: Close honestly. This is not a pattern for every CRUD form. It is a
> pattern for when one specification needs to drive multiple interpreters and
> backends, and when invalid states are expensive enough to make the type-level
> ceremony pay rent.
