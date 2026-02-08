---
title: "Extreme Haskell: Typed Lambda Calculus"
categories: Haskell
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2026/02/07 12:30:55
identifier: typed-lc-1
slug: extreme-haskell-typed-lambda-calculus
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

But this is not that kind of blog post. This blog post is about what happens
when you say "screw it, let's go full fancy"? Let's ignore the advice of the
great Kirk Lazarus. Let's go full fancy. Let's write code that is so
inscrutable, so much of a pain and torture to write, yet so _undeniably useful_
that you can't help but try to throw it in every single thing you write and
will feel a gnawing emptiness in your soul until you do.

Here is one example: a typed lambda calculus. Let's make one in Haskell where
you can write your expression once, and it will:

1.  Be interpretable in a type-safe way within Haskell
2.  Be inspectable with visualizable control flow.
3.  Be compilable to multiple actual backends, letting you run the same
    function under multiple implementations.

This is all stuff I have been using in real life in my personal projects, where
I've needed to write a specification of an algorithm that I can simulate in
Haskell, generate graphical visualizations of, and also convert to multiple
(purescript, dhall, C dialects) to unify algorithms and formulas across
back-ends without writing them from scratch every time.

Once you go down this road, everything you ever write will feel woefully unsafe
and limited. And everything you will want to write will be woefully inscrutable
by normal humans and borderline unusable. But such is the curse we all bear.
Turn around now, you have been warned.

As Adam Neely asked in his [AI Music Video Essay][neely], if you had trained
all of AI on pre-jazz music, could AI have invented jazz? If you trained it on
pre-80s hip-hop, could it have invented 80s hip-hop and its technological
breakthroughs? If you trained AI on safe Haskell code, could it invent the
monstrosity we are about to explore in this post?

[neely]: https://www.youtube.com/watch?v=U8dcFhF0Dlk

All of the code here is [available online][code samples], and if you check out
the repo and run `nix develop` you should be able to load it all in ghci:

!!![code samples]:typed-lc/flake.nix

```bash
$ cd code-samples/typed-lc
$ nix develop
$ ghci
ghci> :load Stage1.hs
```

The Outset
----------

Let's derive a way to express an algorithm or expression in Haskell that can be
reified and analyzed within Haskell, and eventually be a form we can compile to
different backends, interpret in Haskell, or generate Graphviz visualizations
in.

One basic thing we can do is start with:

```haskell
!!!typed-lc/Stage1.hs "data Prim" "data Op" "data Expr"
```

And you can write `(\x -> x * 3) 5` as:


```haskell
!!!typed-lc/Stage1.hs "fifteen ::"
```

You can definitely easily render this in a graph, but what happens when you
write a Haskell interpreter? How can you "evaluate" this to 15, within Haskell?
What would the type even be? `eval :: Expr -> Maybe Prim`? Maybe just
`normalize :: Expr -> Expr` and hope that the result is `Prim`?

```haskell
!!!typed-lc/Stage1.hs "normalize ::"
```

This would properly evaluate:

```haskell
ghci> normalize fifteen
EPrim (PInt 15)
```

Let's say this is 5% fancy. We used recursive types, used `Map` to look things
up efficiently.

But this isn't type-safe...we have undefined branches still. We could make the
entire thing monadic by returning `Maybe`:

```haskell
!!!typed-lc/Stage2.hs "normalize ::"
```

This kind of works if you remember to thread everything through `Maybe` (or
`Either`) or what have you. But this is not ideal. You should be able to know,
at compile-time, that your `Expr` is valid. After all, you want to be able to
create one "valid" `Expr`, and run it at every context. It's utterly useless to
you if every single time you used an `Expr`, you had to manually handle the
`Nothing` case. Your diagram generator, your Haskell runner, your code
generator, will always be in `Either` even though you know your `Expr` is valid,
via tests or something.

This is maybe 10% fancy. We used `Maybe`/`Either` to prevent runtime
exceptions, but didn't actually get rid of any runtime _errors_.

No, this is not okay and unacceptable. We should be able to verify in the types
if an `Expr` is valid.

First Layer of Types
--------------------

The next step you'll see in posts online is to add a phantom index type to
`Expr`:

```haskell
!!!typed-lc/Stage3.hs "type data Ty" "data STy" "data Prim" "data Op" "data Expr"
```

Here we use `-XTypeData` to define a data kind, `Ty` is a kind with types
`TInt :: Ty`, `TBool :: Ty`, etc.

So now `Expr a` evaluates to an `a`, which is either our domain's `Int`, our
domain's `Bool`, or our domain's `String`. At least, now, it is impossible to
create an `Expr` that doesn't type check:

```haskell
!!!typed-lc/Stage3.hs "fifteen ::"
```

We also need a [singleton][] for our `Ty` type, `STy`...this makes a whole lot
of things simpler. Usually when you have a data kind, you can try to avoid
singletons but a lot of times you're just delaying the inevitable. In this case
our lambda is "typed", `ELambda STInt "x"`, so it binds a variable of type
`Int` with name `x`.

[singleton]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

Overall, I'll say this is about 50% fancy. Anything with GADTs will be a
significant bump. But you might see the problem here: `EVar STInt "x"`. `x`
might not be defined, and it also might not have the correct type. Soooo yes,
we still have issues here.

But now at least we can write `eval`:

```haskell
!!!typed-lc/Stage3.hs "data EValue" "data SomeValue" "sameTy ::" "eval ::"
```

What did we gain here? We have a type-safe `eval` now that will create a
value of the type we want. But we still have the same errors when looking at
variables: variables can still not be defined, or be defined as the wrong type.

So, again, we cannot create an `Expr` that must be sensible and well-formed to
compile. We still have to deal with _most_ of the same errors. This is noble,
but clearly not good enough. We have to go deeper.

Type-Safe Environments
----------------------

In order to have `Var` be type-safe, the environment itself needs to be a part
of the `Expr` type, and you should only be able to use `Var` if the `Expr`
enforces it. `ELambda` would, therefore, introduce the new variable to the
environment.

We'll have:

```haskell
!!!typed-lc/Stage4.hs "data Expr ::"1 "type (:::)"
```

So a value of type `Expr '["x" ::: TInt, "y" ::: TBool]` is an expression with
free variables `x` of type `Int` and a `y` of type `Bool`.

`ELambda` would therefore take a `Expr` with a free variable and turn it into
an `Expr` of a function type: (and `KnownSymbol` instance so that we can debug
print the variable name)

```haskell
!!!typed-lc/Stage4.hs "ELambda ::"
```

So how do we implement `Var`? We have to gate it on whether or not the free
variable is available in the environment. For that, we can use `Index`:

```haskell
!!!typed-lc/Stage4.hs "data Index ::"
```

I have this in [functor-products][], but it's also `CoRec Proxy` from [vinyl][]
or `NP Proxy` from [sop-core][]. You can reason about this like axioms: `Index
as a` means that `a` is an item in `as`, which is either `a` being at the start
of the list (`IZ`) or `a` being within the tail (`IS`).

[functor-products][]: https://hackage.haskell.org/package/functor-products
[vinyl]: https://hackage.haskell.org/package/vinyl
[sop-core]: https://hackage.haskell.org/package/sop-core

For example, we have values `IZ :: Index '[a,b,c] a`, `IS IZ :: Index '[a,b,c]
b`, and `IS (IS IZ) :: Index '[a,b,c] c`. So, if we require `Var` to take an
`Index`, we require it to indicate something that _is_ inside the `Expr`'s free
variable list and at that given index:

```haskell
!!!typed-lc/Stage4.hs "EVar ::"
```

So it is legal to have `EVar IZ :: Expr '["x" ::: TInt, "y" ::: TBool] TInt`, and
also it is automatically inferred to be a `TInt`. But we could _not_ write
`EVar IZ :: Expr '[] TInt`.

```haskell
!!!typed-lc/Stage4.hs "data Expr ::" "eLambda ::" "fifteen ::"
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
these variables, and for this we can use `Rec` (from [vinyl][]) or `NP` from
[sop-core]:

```haskell
!!!typed-lc/Stage4.hs "data Rec" "indexRec ::" "eval ::"
```
