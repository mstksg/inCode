---
title: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus (Part 1)"
categories: Haskell
series: "Extreme Haskell: Typed State Machines with Typed Lambda Calculus"
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2026/02/07 12:30:55
identifier: typed-sm-lc-1
slug: extreme-haskell-typed-state-machines-typed-expressions
---

I always say, inside every Haskeller there are two wolves, living on both ends
of the Haskell Fancy Code Spectrum (HFCS). Are you going to write
"simple Haskell", using basic GHC 2010 tools and writing universal Haskell that
every introductory course offers, trying to keep the code as immediately
understandable and accessible? Or are you going to pile in all of the Haskell
type system and evaluation tricks you can find and turn on all the extensions,
and go full fancy?

In my [Seven Levels of Type Safety][levels] post, I described different
extremes of type safety and fancy code. I talked about how writing effective
code was finding the correct compromise for the level of communication and
safety you need.

[levels]: https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html

But this is not that kind of blog post. This series is about what happens
when you say "screw it, let's go full fancy"? Let's ignore the advice of the
great Kirk Lazarus. Let's go full fancy. Let's write code that is so
inscrutable, so much of a pain and torture to write, yet so _undeniably useful_
that you can't help but try to throw it in every single thing you write and
will feel a gnawing emptiness in your soul until you do.

Here's one example: let's write a type-safe method to specify your program as a
series of states, with triggered transitions between them. A Type-Safe state
machine graph using a type-safe lambda calculus. We want to specify this in a
way that we can write once and it will

1.  Be interpretable in a type-safe way within Haskell
2.  Be inspectable with visualizable control flow.
3.  Be compilable to multiple actual backends, letting you run the same
    function under multiple implementations.

Once you go down this road, everything you ever write will feel woefully unsafe
and limited. And everything you will want to write will be hopelessly
inscrutable by normal humans and borderline unusable. But such is the curse we
all bear. Turn around now, you have been warned.

This post (Part 1) will build up the typed expression language. Part 2 will use
that expression language to define typed state machines and visualize them, and
Part 3 will compile those machines to different languages and verify they
execute identically, with some live demos.

All of the code here is [available online][code samples], and if you check out
the repo and run `nix develop` you should be able to load it all in ghci:

!!![code samples]:typed-sm-lc/flake.nix

```bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ ghci
ghci> :load ExprStage1.hs
```

The Lambda Calculus
-------------------

### Why Reify Expressions?

> TODO: Motivate expression DSLs as portable descriptions of programs: run in
> Haskell, inspect, diagram, compile to other backends, and share semantics
> across implementations. Tie this back to real project use-cases without
> making the post feel like a library announcement.

> TODO: Set up the running example more concretely. The expression `(\x -> x *
> 3) 5` is intentionally tiny, but it should stand in for "a program fragment
> that we want to move around as data".

### A First Pass

Let's derive a way to express an algorithm or expression in Haskell that can be
reified and analyzed within Haskell, and eventually be a form we can compile to
different backends, interpret in Haskell, or generate Graphviz visualizations
in.

One basic thing we can do is start with:

```haskell
!!!typed-sm-lc/ExprStage1.hs "data Prim" "data Op" "data Expr"
```

And you can write `(\x -> x * 3) 5` as:


```haskell
!!!typed-sm-lc/ExprStage1.hs "fifteen ::"
```

Or, if we want a record projection and one labeled choice with a case analysis
over it:

```haskell
!!!typed-sm-lc/ExprStage1.hs "recordExample ::" "sumExample ::"
```

You can definitely easily render this in a graph, but what happens when you
write a Haskell interpreter? How can you "evaluate" this to 15, within Haskell?
What would the type even be? `eval :: Expr -> Maybe Prim`? Maybe just
`normalize :: Expr -> Expr` and hope that the result is `Prim`?

```haskell
!!!typed-sm-lc/ExprStage1.hs "normalize ::"
```

This would properly evaluate:

```haskell
ghci> normalize fifteen
EPrim (PInt 15)
```

Let's say this is 5% fancy. We used recursive types, used `Map` to look things
up efficiently.

### Runtime Safety is not the Same Thing

> TODO: Slow down here and spend a page on why `Maybe` feels responsible but
> does not actually give the caller the thing they want. The caller still has to
> handle failure in every backend, even when the expression was "obviously"
> constructed by trusted code.

But this isn't type-safe...we have undefined branches still. We could make the
entire thing monadic by returning `Maybe`:

```haskell
!!!typed-sm-lc/ExprStage2.hs "normalize ::"
```

This kind of works if you remember to thread everything through `Maybe` (or
`Either`) or what have you. But this is not ideal. You should be able to know,
at compile-time, that your `Expr` is valid. After all, you want to be able to
create one "valid" `Expr`, and run it at every context. It's useless to you if
every single time you used an `Expr`, you had to manually handle the `Nothing`
case. Your diagram generator, your Haskell runner, your code generator, will
always be in `Either` even though you know your `Expr` is valid, via tests or
something.

This is maybe 10% fancy. We used `Maybe`/`Either` to prevent runtime
exceptions, but didn't actually get rid of any runtime _errors_.

No, this is not okay and unacceptable. We should be able to verify in the types
if an `Expr` is valid.

### First Layer of Types

> TODO: Before showing the code, describe the obvious next thought: track the
> result type in the type parameter. This fixes one class of mistakes, but it
> does not know anything about variables yet.

The next step you'll see in posts online is to add a phantom index type to
`Expr`:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "type data Ty" "data Prim" "data Op" "data Expr"
```

This introduces several new language features, so bear with me as I break them
down.

First, we use `-XTypeData` to define a data kind, `Ty` is a kind with types
`TInt :: Ty`, `TBool :: Ty`, etc.

So now, for `Expr t`, `t` describes the result type. That result type is either
our domain's `Int`, our domain's `Bool`, or our domain's `String`. At least,
now, it is impossible to create an `Expr` that doesn't type check. Well, kind
of.

```haskell
!!!typed-sm-lc/ExprStage3a.hs "fifteen ::"
```

Because of `Ty`, we can also make a new indexed data type with phantoms of
"fully resolved" values:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "data EValue"
```

This is what we want to eventually `eval` into, as we can guarantee ourselves
to get a value of the correct type based on the `Ty`:

```haskell
eValueToInt :: EValue TInt -> Int
eValueToInt = \case
    EVInt x -> x
```

And GHC will verify this as a total pattern match because `EVInt` is the only
possible way to create an `EValue TInt`.

So, our `eval` function will be:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "data SomeValue" "eval ::"1
```

This does seem to work:

```haskell
ghci> for_ (eval M.empty fifteen) \case
    EVInt x -> print x      -- compiler-verified to always be EVInt
15
```

Our system also allows us to produce closures and functions as values:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "plusThree ::"
```

```haskell
ghci> for_ (eval M.empty plusThree) \case
    EVFun f -> for_ (f (EVInt 4)) \case
      EVInt x -> print x    -- compiler-verified to always be EVInt
7
```

We'll keep our bound variables stored as an ambient map of variable names
to their evaluated values for now. But, to do this, we need to turn the
heterogeneous `EValue t` into the homogeneous `Map String SomeValue` by
wrapping the type variable as an existential type.

You might notice we have a [singleton][] for our `Ty` type, `STy`, that pops up
in multiple situations.

[singleton]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

```haskell
!!!typed-sm-lc/ExprStage3a.hs "data STy"
```

Firstly, it might help to recognize the general pattern where `STy` (the
singleton) appears. It _usually_ pops up whenever we have existentially scoped
variables, like in `data SomeValue = forall t. SomeValue (STy t) (EValue t)`.
In this case, the `t` is completely lost to the outside world, and `STy t` is
used to allow us to recover a runtime witness to what `t` was, after pattern
matching on `STy`. This is the _dependent sum_ pattern, and is similar to how
typeclasses are used for *Data.Dynamic*.

Usually when you have a data kind, you can try to avoid singletons, but a lot
of times you're just delaying the inevitable. If you ever start hiding your
type variables inside existentials like here, you know that you're going to
have to start needing singletons or some similar mechanism soon.

In our case, we do need singletons to implement `eval`, because variables are
still stored ambiently in the environment and validated at runtime. The type
`Expr t` only specifies the type of the result, but the type information of the
ambient variables is not available. So, you can write `EVar STInt "myVar" ::
Expr TInt`, but:

*   `myVar` might not be a variable in scope at all, so `eval` will fail at
    runtime
*   `myVar` might be in scope, but might be a `TString` and not a `TInt`

The first case is easy enough to deal with (`M.lookup` returns `Nothing`), but
the second one is a little more subtle. Let's say we _do_ have a `SomeValue`
under our key `myVar`...how do we make sure it has the correct type?

We can do ad-hoc pattern matching on `EValue`, but that won't get us too far. Mostly
because some of the `EValue` constructors actually don't have enough
information for us to validate its actual type (try it! `EVFun` will give you a
lot of trouble). So, what we can do is write a function that takes two `STy` at
runtime and _unifies_ them conditionally if they are the same. We'll write a
function `sameTy :: STy a -> STy b -> Maybe (a :~: b)`, where

```haskell
data (:~:) :: k -> k -> Type where
    Refl :: a :~: a
```

_pattern matching_ on a `a :~: b` will reveal that `a` and `b` are the same
type variable, because the only way to construct it is with `Refl :: a :~: a`.

With that, we can write `sameTy`:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "sameTy ::"
```

There's a typeclass in _base_ (or rather, a "kindclass"), `TestEquality`,
that encapsulates this pattern:

```haskell
class TestEquality f where
    testEquality :: f a -> f b -> Maybe (a :~: b)
```

In fact, we can write our `sameTy` as an instance:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "instance TestEquality STy"
```

Overall, I'll say this is about 33% fancy. Anything with GADTs will be a
significant bump. But you might see the problem here: `EVar STInt "x"`. `x`
might not be defined, and it also might not have the correct type. Soooo yes,
we still have issues here.

But now at least we can write `eval`:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "eval ::"
```

We have a type-safe `eval` now that will create a value of the type we want.
But we still have the same errors when looking at variables: variables can
still not be defined, or be defined as the wrong type.

So, again, we cannot create a _fully_ type-checked `Expr`. We still have to
deal with _most_ of the same errors. This is noble, but clearly not good
enough. We have to go deeper.

### Records and Sums

Ah! You might have noticed that this past implementation dropped records and
sums. Before we move on, let's go ahead and add those. Introducing records and
sums at the same time as type-indexed `Expr` is a bit _too_ much of a jump to
fit into a single section.

Let's add sums and records, which can use pretty similar mechanisms (via
duality) for implementation.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "type data Ty"
```

`Ty` now includes `TRecord [(Symbol, Ty)]` and `TSum [(Symbol, Ty)]`, which
represents the field names and constructor payloads (`Symbol`
being a type-level string). So, for example, `TRecord ["value" ::: TInt,
"label" ::: TString]` would be the type of a record with ordered fields `value`
and `label` of integers and strings, respectively. `TSum ["Found" ::: TInt,
"Missing" ::: TString]` would be the type of a sum between `Found` containing
an integer and `Missing` containing a string. Note we take a page out of
[vinyl][] by defining the type alias `(:::) = '(,)` to make things
syntactically nicer.

[vinyl]: https://hackage.haskell.org/package/vinyl

We need the fields and types at the type level because we have to answer what
the `Expr` phantom type of field access is. If we had an
`x :: Expr (TRecord ["value" ::: TInt, "label" ::: TString])`,
we want the type of `x.value` to be `Expr TInt`.

To do this, we need to have a _value_ in our `Expr` for field access that can
"point" at a specific field in the type. One way to do that is to take a field
of type `Index`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "data Index"
```

You can read this as: "`IZ` is an index to the head of the type-level list, and
`IS n` is an index to the n-th item of the tail". So, `IS IZ` is an index into
the second element, `IS (IS IZ)` is an index into the third, etc.

If we have `["value" ::: TInt, "label" ::: TString]`, then we have values:

```haskell
IZ    :: Index ["value" ::: TInt, "label" ::: TString] ("value" ::: TInt)
IS IZ :: Index ["value" ::: TInt, "label" ::: TString] ("label" ::: TString)
```

Note that the way this is constructed, it's impossible for `IS (IS IZ) ::
Index ["value" ::: TInt, "label" ::: TString] _` to typecheck as anything!

In this way, we have a well-typed field accessor syntax, which takes an `Expr`
of a record of fields and indexes it to get an `Expr` of the type at that
index:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "EAccess ::"
```

Which is typed as:

```haskell
(`EAccess` IZ)    :: Expr (TRecord ["value" ::: TInt, "label" ::: TString]) -> Expr TInt
(`EAccess` IS IZ) :: Expr (TRecord ["value" ::: TInt, "label" ::: TString]) -> Expr TString
```

To _create_ an `Expr` of a record, we can use `Rec` from *[vinyl][]* or `NP`
from *[sop-core][]*: a heterogeneous list indexed by a type-level list.

[sop-core]: https://hackage.haskell.org/package/sop-core

```haskell
!!!typed-sm-lc/ExprStage3b.hs "data Rec" "ERecord ::" "recordExample ::"
```

We also need a type-level list witness for _sum_ types, because we need to be able
to implement the correct continuations for pattern matches: How do we know
_what_ thing to handle in each pattern match, unless the sum type has that
information in its type?

Luckily due to the magic of duality, we can use the same tools, for the
most part! We can inject into a sum with an `Index`, let's say for a `Expr
(TSum ["Found" ::: TInt, "Missing" ::: TString])`: sum type with `Found`
containing an integer and `Missing` containing a string:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "EChoice ::"
```

```haskell
EChoice IZ      :: Expr TInt -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
EChoice (IS IZ) :: Expr TString -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
```

And the case-statement is just a record of handler lambdas:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "data ExprHandler ::" "ECase ::" "sumExample ::"
```

Note that we're still using string binders, so there's still an element of
unsafety here... we give that the variable name is `"value"` and that it is a
`TInt`, but when we later refer to the variable with `EVar STInt "value"`, it
isn't type-checked that later references use the same type. The compiler would
be just as happy with `EVar STString "value"`. But that's the same issue as
before, at this stage.

One final complication is we need to update the `TestEquality` instance for
`STy`. Luckily, we have `SSymbol`, the singleton for `Symbol` that already has
a `TestEquality` instance, so writing `TestEquality` for `STy` is not too bad.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "instance TestEquality STy" "instance TestEquality STyField" "sameTy ::" "sameFields ::" "sameField ::"
```

With that, we can write our full `eval`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "traverseRec ::" "eval ::" "evalField ::"
```

#### Ergonomics of Records

Note that we could also choose to implement records and sums using row types
indexed by the name of the field itself, instead of an ordered list of tuples.
This would have the advantage of, ie, `{ value :: Int, label :: String}` being
the same type as `{ label :: String, value :: Int }`. However, for me, I really
do personally prefer the style of building things inductively (`:&`/`RNil` and
`IS`/`IZ`), it makes the type errors and constructions a lot easier to work
with and reason with.

However, we can get a little bit of the best of both worlds by using
typeclasses to auto-insert the `Index` witnesses into a list.

First, we can write a typeclass that searches a type-level list of fields and
produces the right `Index`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "class ListIx" "instance ListIx l" "instance {-# OVERLAPPABLE #-} ListIx l"
```

The FunDep `l xs -> a` lets us use this like a function: for a label `l` and a
list `xs`, we should be able to uniquely determine the `a` type it singles out,
if it exists. So we have an instance of `ListIx "value" ["value" ::: TInt,
"label" ::: TString] TInt`, where the label `"value"` and the list uniquely
determines the result type `TInt`.

We can now have a helper function that we can call like `eAccess "value"`
using GHC 9.10's `RequiredTypeArguments`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "eAccess ::"
```

That lets us write the field name directly, and the compiler will generate the
`Index` automatically for us:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "namedAccessExample ::"
```

Here, `eAccess "value" :: Expr (TRecord ["value" ::: TInt, "label" :::
TString]) -> Expr TInt`, its result type uniquely determined by the types in
the record fields.

The same trick works for sum injections, so we can write the constructor
name directly:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "eChoice ::" "namedChoiceExample ::"
```

Here, `eChoice "Missing" :: Expr TString -> Expr (TSum ["Found" ::: TInt,
"Missing" ::: TString])`, because the constructor name `"Missing"` uniquely
determines the payload type inside that sum.

### What Exactly is Still Wrong?

> TODO: Add a short "bad expression gallery": wrong variable name, right name
> with wrong type, lambda body using a variable outside of scope, operator with
> mismatched operands. This will make the typed-environment section feel
> necessary instead of decorative.

### Type-Safe Environments

In order to have `Var` be type-safe, the environment itself needs to be a part
of the `Expr` type, and you should only be able to use `Var` if the `Expr`
enforces it. `ELambda` would, therefore, introduce the new variable to the
environment.

We'll have:

```haskell
!!!typed-sm-lc/ExprStage4.hs "data Expr ::"1 "type (:::)"
```

So a value of type `Expr ["x" ::: TInt, "y" ::: TBool]` is an expression with
free variables `x` of type `Int` and a `y` of type `Bool`.

`ELambda` would therefore take a `Expr` with a free variable and turn it into
an `Expr` of a function type: (and `KnownSymbol` instance so that we can debug
print the variable name)

```haskell
!!!typed-sm-lc/ExprStage4.hs "ELambda ::"
```

So how do we implement `Var`? We have to gate it on whether or not the free
variable is available in the environment. For that, we can use `Index`:

```haskell
!!!typed-sm-lc/ExprStage4.hs "data Index ::"
```

You can reason about this like axioms: `Index as a` means that `a` is an item
in `as`, which is either `a` being at the start of the list (`IZ`) or `a` being
within the tail (`IS`).

For example, we have values `IZ :: Index [a,b,c] a`, `IS IZ :: Index [a,b,c]
b`, and `IS (IS IZ) :: Index [a,b,c] c`. So, if we require `Var` to take an
`Index`, we require it to indicate something that _is_ inside the `Expr`'s free
variable list and at that given index:

```haskell
!!!typed-sm-lc/ExprStage4.hs "EVar ::"
```

So it is legal to have `EVar IZ :: Expr ["x" ::: TInt, "y" ::: TBool] TInt`, and
also it is automatically inferred to be a `TInt`. But we could _not_ write
`EVar IZ :: Expr [] TInt`.

```haskell
!!!typed-sm-lc/ExprStage4.hs "data Expr ::" "eLambda ::" "fifteen ::"
```

In GHC 9.12 we can write `eLambda` using `RequiredTypeArguments` and so can
pass the type variable as a string literal, `eLambda "x"` but we can't yet put
this directly in `ELambda` for some reason.

Note that this is sometimes done using straight [De Bruijn indices][debruijn]:
`Expr :: [Ty] -> Type`, so we don't use any names but just the direct index,
but the point of this exercise is to be _borderline_ unbearable to write, and
_not_ to be _actually_ unbearable to write.

[debruijn]: https://en.wikipedia.org/wiki/De_Bruijn_index

To actually write _eval_ now, we need to have a type-safe environment to store
these variables. We can reuse the typed-record idea from the previous section,
but the important idea for now is that variables are no longer looked up by
hoping a string exists at runtime. They carry evidence that the field exists in
the environment.

```haskell
!!!typed-sm-lc/ExprStage4.hs "eval ::"
```

### What We Have Bought

> TODO: Summarize the guarantees in concrete terms: literals have domain types,
> operators enforce operand/result types, variables carry membership proofs,
> lambdas extend the environment, and evaluation no longer has to discover
> scope/type errors dynamically.

### What Still Hurts

> TODO: Be honest about ergonomics: `IZ`, `IS IZ`, singleton witnesses, noisy
> type signatures, and the general "I have become a theorem prover" feeling.
> This is a good place for the "extreme Haskell" tone.

By the end of this first part, we have a typed expression language where bad
variables and bad expression types are ruled out by construction. In [Part 2][],
we will use that language as the guard and update language for a typed state
machine.

[Part 2]: /entry/extreme-haskell-typed-state-machines.html
