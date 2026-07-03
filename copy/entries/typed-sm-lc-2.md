---
title: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus (Part 2)"
categories: Haskell
series: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus"
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2026/02/07 12:31:55
identifier: typed-sm-lc-2
slug: extreme-haskell-typed-state-machines
---

Welcome back to our journey through the limits of polite Haskell! In [Part
1][], we built a typed lambda calculus whose expressions track their free
variables and result types.

That is already useful, but the real payoff is that the expression language is
now a reusable typed component. In this post, we will put it inside a typed
state-machine description.

[Part 1]: /entry/extreme-haskell-typed-state-machines-typed-expressions.html

Again, all of the code here is [available online][code samples], and you can
load it all in ghci from the sample directory:

!!![code samples]:typed-sm-lc/flake.nix

```bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ ghci
ghci> :load MachineStage5.hs
```

The State Machine
-----------------

Okay, now here is where all of this finally starts paying rent.

The expression language we made is not merely something that evaluates to a
value in Haskell. It is a typed description of a computation. And if it is a
description, we can stick it inside a larger typed description: a state machine.

### The Version We Do Not Want

> TODO: Start with a deliberately stringly/untyped state-machine sketch:
> states as `String`, triggers as `String`, guards as ad-hoc Haskell functions,
> and updates as string-keyed maps. Show why it is pleasant to write and easy
> to break.

> TODO: Add two concrete failure examples: a transition to a misspelled state,
> and an update that writes a boolean into an integer field. This sets up the
> rest of the machinery.

### Promoted SDR States and Triggers

Let's make a small receive-only SDR scan controller with a few states and a
small trigger vocabulary:

```haskell
!!!typed-sm-lc/MachineStage5.hs "type data ScanState" "type data Trigger" "data SState" "data STrigger"
```

> TODO: Explain why we need both the promoted type-level states/triggers and
> the singleton runtime witnesses. The types give us static indexing; the
> singletons let us render names, inspect the graph, and compile later.

### Ambient Environment and Node Payloads

The machine reads an ambient environment, but each state owns its own local
payload:

```haskell
!!!typed-sm-lc/MachineStage5.hs "type family NodeVars" "type Env" "type Scope"
```

These are not JavaScript variables, and they are not strings we look up later.
They are variables in the typed expression language. The ambient environment
contains external observations like `rssi`, `noise`, and `peakOffset`; the
current node payload contains only the fields that make sense for that node.

> TODO: Make the connection to Part 1 explicit: `Scope s` is just the free
> variable environment for guards and builders that start in node `s`.

### Guards and Destination Builders

> TODO: Walk through a few of the named expressions (`strongEnoughScanning`,
> `scanNextFreq`, `lockedTooLong`) as plain machine rules before showing the
> transition type. This gives readers a semantic handle before the GADT wall
> appears.

So, a transition can demand a guard expression that actually returns a `Bool`,
and a destination builder can demand exactly the fields required by the node it
is entering:

```haskell
!!!typed-sm-lc/MachineStage5.hs "data BuildField" "data Build" "data Transition" "data SomeTransition" "newtype Machine"
```

### Heterogeneous Transitions

> TODO: Spend time on why the machine needs `SomeTransition`: every transition
> has different `from`, `trigger`, and `to` indices, so a normal homogeneous
> list would be too precise. The existential wrapper forgets exactly enough to
> store and inspect them together.

### A Small Machine

> TODO: Introduce `quickScanMachine` on its own first. Explain it as the sanity
> check machine: small enough to inspect, but already enough to prove the DSL
> can compile a complete graph.

### A Less Toy Machine

> TODO: Introduce `dwellScanMachine` as the example that justifies the ceremony:
> multiple transitions through the same nodes, repeated lock refreshes, timeout
> behavior, and payloads that change shape from node to node.

And now we can write two different machines against the same typed expression
language:

```haskell
!!!typed-sm-lc/MachineStage5.hs "quickScanMachine ::" "dwellScanMachine ::" "machines ::"
```

### Things GHC Will Not Let Us Say

> TODO: Add a few commented-out snippets or prose examples of rejected machine
> definitions: using `lockRssi` while still in `Scanning`, returning a
> `CoolingDown` payload without `lastFreq`, using an integer expression as a
> guard, or attempting to write ambient `rssi` as if it were node-local state.

The important thing here is not that this is ergonomic. It is absolutely not
ergonomic. The important thing is that the illegal versions are not values. If
either machine's `Peak` guard accidentally returned a `TInt`, or if a transition
to `Locked` forgot to construct `lockRssi`, there would be no JavaScript to
debug because the Haskell program that generates the JavaScript would not
compile.

### Why Two Machines Matter

> TODO: Make the user-facing thesis explicit: one generated machine is a demo;
> two generated machines show that we built a language. The same expression
> layer, transition representation, and backend can handle distinct graphs.

### Rendering the Graph

The first backend does not have to run the machine at all. Because the machine
is data, we can just render the control-flow graph:

```haskell
!!!typed-sm-lc/MachineStage5.hs "compileDot ::" "renderMachineDot ::" "renderSomeTransitionDot ::" "renderTransitionDot ::"
```

That gives us a rendered Graphviz view of the two machines: ([full size
here][fullsize-graph])

[fullsize-graph]: /img/entries/typed-sm-lc/machines.png

![Graphviz rendering of the typed SDR scan machines](/img/entries/typed-sm-lc/machines.png "Typed SDR scan machines")

The generated DOT file is available in the code samples:

!!![machine-dot]:typed-sm-lc/machines.dot.txt

> TODO: Talk through the graph output: the same `Peak` trigger has different
> structure in the two machines, guard labels come from the typed expression
> renderer, and event labels come from destination builders.

### Teaser: Other Interpreters

> TODO: Briefly mention other possible consumers of the same machine value:
> Haskell simulation, test-case generation, documentation tables,
> PureScript/Dhall/C backends. Save the actual compiler for Part 3.

At this point, we have two distinct state-machine specifications sharing the
same typed expression language. The next step is the real finale: in [Part 3][],
we compile both of them to JavaScript and embed the result in the page.

[Part 3]: /entry/extreme-haskell-typed-state-machines-javascript-compiler.html
