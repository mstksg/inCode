---
title: "Extreme Haskell: Typed Embedded Expression DSLs (Part 1)"
categories: Haskell
series: "Extreme Haskell: Typed Embedded Expression DSLs"
tags: functional programming, dependent types, haskell, singletons, types
create-time: 2026/02/07 12:30:55
identifier: typed-sm-lc-1
slug: extreme-haskell-typed-embedded-expression-dsls
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

But this is not that kind of blog post. This is the kind of blog post where we
celebrate terrifying type-safety, facetious fanciness, masochistic
meta-analysis. This series is about what happens when we dare to go full fancy.
Let's write code that is so inscrutable, so much of a pain and torture to
write, yet so _undeniably useful_ that you can't help but try to throw it into
every single thing you write and will feel a gnawing emptiness in your soul
until you do.

As our example, let's write a type-safe method to specify your program as a
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
that expression language to define typed state machines with embedded
predicates and visualize them, and Part 3 will compile those machines to
different languages and verify they execute identically, with some live demos.

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

Let's derive a way to express an algorithm or expression in Haskell that can be
reified and analyzed within Haskell, and eventually be a form we can compile to
different backends, interpret in Haskell, or generate Graphviz visualizations
in.

### A First Pass

One basic thing we can do is start with:

```haskell
!!!typed-sm-lc/ExprStage1.hs "data Prim" "data Op" "data Expr"
```

And you can write `(\x -> x * 3) 5` as:


```haskell
!!!typed-sm-lc/ExprStage1.hs "fifteen ::"
```

This is...pretty untyped. We could easily write something that is meaningless:

```haskell
!!!typed-sm-lc/ExprStage1.hs "badTypeExample ::"
```

Of course, GHC can typecheck our code if we literally write `\x -> x + 3` and
reject `1 && 2`. But we aren't trying to build opaque Haskell code here, we're
trying to represent our expression as an ADT that we can analyze _within_ the
language.

If we want a record projection and one labeled choice with a case analysis
over it:

```haskell
!!!typed-sm-lc/ExprStage1.hs "recordExample ::" "sumExample ::"
```

Now, for the entire point of `Expr`, we can write a function to pretty-print
it, using the *[prettyprinter][]* library:

[prettyprinter]: https://hackage.haskell.org/package/prettyprinter

```haskell
!!!typed-sm-lc/ExprStage1.hs "ppPrim ::" "ppOp ::" "ppExpr ::" "prettyExpr ::"
```

```haskell
ghci> prettyExpr fifteen
(\x -> x * 3) 5
ghci> prettyExpr badTypeExample
1 && 2
ghci> prettyExpr recordExample
{ label = "found", value = 7 }.value + 1
ghci> prettyExpr sumExample
case Found 7 of { Found value -> value + 1; Missing message -> 0 }
```

So, what happens when you write a Haskell interpreter? How can you "evaluate"
this to 15, within Haskell? What would the type even be? `eval :: Expr -> Maybe
Prim`? Maybe just `normalize :: Expr -> Expr` and hope that the result is
`Prim`?

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
exceptions, but didn't actually get rid of any runtime _errors_. We want GHC to
reject badly typed expressions.

No, this is not okay and unacceptable. We should be able to verify in the types
if an `Expr` is valid.

Type-Indexed Expressions
------------------------

### Just Add the Index

The next step you'll see in posts online is to add a phantom index type to
`Expr`:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "type data Ty" "data Prim" "data Op" "data Expr"
```

A phantom type is a type parameter that doesn't represent any actual _value_
"contained" inside the data type, but just serves to "tag" or distinguish
values for the compiler to reject or unify things in useful ways.

This introduces several new language features, so bear with me as I break them
down.

First, we use `-XTypeData` to define a data kind, `Ty` is a kind with types
`TInt :: Ty`, `TBool :: Ty`, etc. And in `Expr t`, we have an expression tagged
with `t`, which describes the result type.

For example, because we have `EPrim :: Prim t -> Expr t`, and `PInt 3 :: Prim
TInt`, we have `EPrim (PInt 3) :: Expr TInt`: a primitive 3 is an expression
describing an integer.

And because `EOp :: Op a b c -> Expr a -> Expr b -> Expr c`, and `OLte :: Op
TInt TInt TBool`, we have

```haskell
EOp OLte :: Expr TInt -> Expr TInt -> Expr TBool
```

So we can write an operation on two `Expr`s that typecheck how we'd expect:

```haskell
ghci> :t EOp OLte (EPrim (PInt 3)) (EPrim (PInt 4))
Expr TBool
```

We also have `EOp OAnd :: Expr TBool -> Expr TBool -> Expr TBool`, which means
the compiler will reject our previous `1 && 2` example:

```haskell
ghci> :t EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))
<interactive> error:
    • Couldn't match type ‘TInt’ with ‘TBool’
      Expected: Prim TBool
        Actual: Prim TInt
```

We can write lambdas in this system too:

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

### Evaluating Typed Values

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

### Singletons and Existentials

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

In our case, we _do_ need singletons to implement `eval`, because variables are
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

### Runtime Type Equality

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

### Pretty-Printing

One nice consequence of this type-index method is that if you choose to consume
them into an untyped target, you can do it more or less in the same way as the
non-indexed untyped data.

```haskell
!!!typed-sm-lc/ExprStage3a.hs "ppPrim ::" "ppOp ::" "ppExpr ::" "prettyExpr ::"
```

And they render the same way:

```haskell
ghci> prettyExpr fifteen
(\x -> x * 3) 5
ghci> prettyExpr plusThree
\x -> x + 3
ghci> prettyExpr badVariable
(\x -> x + 3) true
```

### Still Not Fully Verified

Implicit in the previous section was the admission of failure: this system lets
us use indexed types to help propagate unification (the result types of `OLte`,
`OAnd`, `OPlus`, etc.), but it can't prevent all ill-defined programs from
compiling.

The issue is `EVar`: its type `EVar :: STy t -> String -> Expr t` lets us bind
_any_ variable name as _any_ type, and it'll still typecheck. Even with the
help of everything we have, we can just straight-up declare a reference to an
unbound variable

```haskell
!!!typed-sm-lc/ExprStage3a.hs "unboundVariable ::"
```

This typechecks in GHC even though it's meaningless in the domain. We can also
reference the variable under any _type_:

```haskell
!!!typed-sm-lc/ExprStage3a.hs "badVariable ::"
```

That's because `EVar` can freely take any `STy` without any restriction, and no
association with the binder name, so there's no way for GHC to stop us.

So, again, we cannot create a _fully_ type-checked `Expr`. We still have to
deal with _most_ of the same errors. This is noble, but clearly not good
enough. We have to go deeper.

Typed Records and Sums
----------------------

A quick detour: you might have noticed that this past implementation dropped
records and sums. Before we move on, let's go ahead and add those. Introducing
records and sums at the same time as type-indexed `Expr` is a bit _too_ much of
a jump to fit into a single section.

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

### Record Access

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
!!!typed-sm-lc/ExprStage3b.hs "data Rec"
```

If you haven't seen `Rec` before, basically `Rec f [a,b,c]` is a tuple of `f
a`, `f b`, and `f c`. For example:

```haskell
ghci> :t Identity 3 :& Identity True :& Identity "hello" :& RNil
Rec Identity [Int, Bool, String]
ghci> :t Const "x" :& Const "y" :& RNil
Rec (Const String) [x1, x2]
```

Keeping `Rec f as` instead of a direct heterogeneous list of `a`s lets us store
more interesting things than just `Type`-kinded things. For example, since our
lists here are lists of `(Symbol, Ty)`, we can create a container to hold
fields:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "data ExprField"
```

The field constructor keeps a `KnownSymbol l` constraint, so the type-level
label is still available later when we need to render it:

```haskell
ghci> :t EField @"value" (EPrim (PInt 7))
            :& EField @"label" (EPrim (PString "found"))
            :& RNil
Rec ExprField ["value" ::: TInt, "label" ::: TString]
```

So, we can create `Expr (TRecord ["value" ::: TInt, "label" ::: TString])` by
taking a `Rec`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "ERecord"
```

We can make this a little more ergonomic by using `-XRequiredTypeArguments` (as
of GHC 9.10) to get rid of the `-XTypeApplication` ugliness:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "eField ::" "makeRecordExample ::" "recordExample ::"
```

### Sum Injection and Case Analysis

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
EChoice @"Found" IZ        :: Expr TInt -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
EChoice @"Missing" (IS IZ) :: Expr TString -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
```

```haskell
!!!typed-sm-lc/ExprStage3b.hs "makeSumExample ::"
```

And we can re-use `Rec` to define a type that can _handle_ a `["Found" :::
TInt, "Missing" ::: TString]` sum:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "data ExprHandler ::" "eHandler ::"
```

```haskell
ghci> :t eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
ExprHandler TInt ("Found" ::: TInt)
         -- ^ result           ^ payload
```

And we can put these into a `Rec` to handle each option in the list:

```haskell
ghci> :t eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
           :& eHandler "Missing" STString "message" (EPrim (PInt 0))
           :& RNil
Rec (ExprHandler TInt) ["Found" ::: TInt, "Missing" ::: TString]
```

And that's exactly what a case statement is:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "ECase ::" "sumExample ::"
```

Note that we're still using string binders, so there's still an element of
unsafety here... we say that the variable name is `"value"` and that it is a
`TInt`, but when we later refer to the variable with `EVar STInt "value"`, it
isn't type-checked that later references use the same type. The compiler would
be just as happy with `EVar STString "value"`.

Here's an example demonstrating both failure modes: the first handler
references a variable that doesn't exist, and the second handler references a
variable that does exist as an incorrect type! How unfortunate.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "badCaseBranchExample ::"
```

### Runtime Equality for Records and Sums

One complication is we need to update the `TestEquality` instance for `STy`.
The record and sum labels are type-level `Symbol`s, so we compare those with
the `sameSymbol` (kind of like `testEquality` for any `KnownSymbol` instance)
and then compare the payload types recursively.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "instance TestEquality STy" "instance TestEquality STyField" "sameTy ::" "sameFields ::" "sameField ::"
```

### The Full Eval

Before we write the final `eval`, let's practice using `Index` and `Rec`
together. If we have an index `Index as a` that picks out a value `a` in `as`,
then we can pick out the `f a` from a `Rec f as`:

```haskell
!!!typed-sm-lc/ExprStage3b.hs "indexRec ::"
```

We also can recursively iterate a function over each item, assuming the
function `forall x. f x -> g x`: that is, we can turn a `Rec f as` into a `Rec
g as` assuming our function is polymorphic over each `x`, and only depends on
the shape of `f`.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "traverseRec ::"
```

With that, we can write our full `eval`.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "eval ::"
```

### Ergonomics of Records and Sums

Note that we could also choose to implement records and sums using row types
indexed by the name of the field itself, instead of an ordered list of tuples.
This would have the advantage of, i.e., `{ value :: Int, label :: String}` being
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

### Pretty-Printing Records and Sums

To pretty-print, we finally use that `KnownSymbol` constraint we've been
tracking this entire time. We can use `symbolVal :: KnownSymbol s => p s ->
String` to get the string _value_ from the type-level string.

```haskell
!!!typed-sm-lc/ExprStage3b.hs "ppPrim ::" "ppOp ::" "ppFields ::" "ppHandlers ::" "ppExpr ::" "prettyExpr ::"
```

Capturing Variables
-------------------

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

Surprise! That small detour to add records and sums to our language actually
ended up being a smooth precursor to all of the techniques we will be using to
solve variable binders.

`ELambda` would therefore take a `Expr` with a free variable and turn it into
an `Expr` of a function type. We also keep a `KnownSymbol` constraint for the
name so that we can recover the name when we render the expression.

```haskell
!!!typed-sm-lc/ExprStage4.hs "ELambda ::"
```

`EVar` then becomes exactly like `EAccess`! We "index" into the environment of
the `Expr vs a` using `Index`:

```haskell
!!!typed-sm-lc/ExprStage4.hs "EVar ::"
```

So it is legal to have `EVar IZ :: Expr ["x" ::: TInt, "y" ::: TBool] TInt`, and
also it is automatically inferred to be a `TInt`. But we could _not_ write
`EVar IZ :: Expr [] TInt`.

And finally, our whole `Expr`:

```haskell
!!!typed-sm-lc/ExprStage4.hs "data Expr ::"
```

Just like with record access, we can use the `ListIx` class to write a named
helper:

```haskell
!!!typed-sm-lc/ExprStage4.hs "class ListIx" "instance ListIx l" "instance {-# OVERLAPPABLE #-} ListIx l" "eVar ::"
```

Adding in our other `-XRequiredTypeArguments` helpers:


```haskell
!!!typed-sm-lc/ExprStage4.hs "eLambda ::" "eField ::" "eAccess ::" "eChoice ::" "eHandler ::"
```

And we get something that is _truly_ type-safe: all expressions are well-typed,
and all variables are ensured to be bound!

```haskell
!!!typed-sm-lc/ExprStage4.hs "fifteen ::" "recordExample ::" "sumExample ::"
```

### What we gained

Now all of the previous examples "bad" examples we gave are finally
compiler-verified:

```haskell
ghci> :t (EVar IZ :: Expr '[] TInt)
<interactive> error:
    • Couldn't match type: (n0 ::: TInt) : xs0
                     with: '[]
      Expected: Index '[] (n0 ::: TInt)
        Actual: Index ((n0 ::: TInt) : xs0) (n0 ::: TInt)

ghci> :t (EVar IZ :: Expr '["x" ::: TString] TInt)
<interactive> error:
    • Couldn't match type ‘TString’ with ‘TInt’
      Expected: Index '["x" ::: TString] ("x" ::: TInt)
        Actual: Index '["x" ::: TString] ("x" ::: TString)

ghci> :t ECase
      (EChoice @"Found" IZ (EPrim (PInt 7)) :: Expr '[] (TSum '["Found" ::: TInt, "Missing" ::: TString]))
      ( EHandler @"value" @"Found" (EOp OPlus (EVar (IS IZ)) (EPrim (PInt 1)))
          :& EHandler @"message" @"Missing" (EOp OPlus (EVar IZ) (EPrim (PInt 1)))
          :& RNil
      )
<interactive> error:
    • Couldn't match type: (n0 ::: TInt) : xs0
                     with: '[]
<interactive> error:
    • Couldn't match type ‘TString’ with ‘TInt’
```

And using the `ListIx` helpers, those same failures show up as failed index
searches:

```haskell
ghci> :t (eVar "missing" :: Expr '[] TInt)
<interactive> error:
    • No instance for ‘ListIx "missing" '[] TInt’

ghci> :t (eVar "x" :: Expr '["x" ::: TString] TInt)
<interactive> error:
    • No instance for ‘ListIx "x" '[] TInt’

ghci> :t ECase
      (eChoice "Found" (EPrim (PInt 7)) :: Expr '[] (TSum '["Found" ::: TInt, "Missing" ::: TString]))
      ( eHandler "value" "Found" (EOp OPlus (eVar "missing") (EPrim (PInt 1)))
          :& eHandler "message" "Missing" (EOp OPlus (eVar "message") (EPrim (PInt 1)))
          :& RNil
      )
<interactive> error:
    • No instance for ‘ListIx "missing" '[] TInt’
<interactive> error:
    • No instance for ‘ListIx "message" '[] TInt’
```

Expressions that are not well-typed in our domain language are now rejected by
GHC![^bottom]

[^bottom]: Excluding `_|_` in Haskell-land (recursion or `undefined`),
unfortunately.

Note that this is sometimes done using straight [De Bruijn indices][debruijn]:
`Expr :: [Ty] -> Type`, so we don't use any names but just the direct index,
but the point of this exercise is to be _borderline_ unbearable to write, and
_not_ to be _actually_ unbearable to write.

[debruijn]: https://en.wikipedia.org/wiki/De_Bruijn_index

### Eval with a typed environment

To actually write _eval_ now, we need to have a type-safe environment to store
these variables. In order to `eval` a `Expr vs`, we need `EValue`s for each `v`
in `vs`. So for `Expr ["x" ::: TInt, "y" ::: TBool]`, we need to store a `TInt`
and a `TBool`. We can once again re-purpose `Rec`:

```haskell
!!!typed-sm-lc/ExprStage4.hs "data EValueField"
```

```haskell
ghci> :t (EVField (EVInt 3) :& EVField (EVBool True) :& RNil :: Rec EValueField '["x" ::: TInt, "y" ::: TBool])
Rec EValueField ["x" ::: TInt, "y" ::: TBool]
```

And so we can finally write:

```haskell
!!!typed-sm-lc/ExprStage4.hs "eval ::"
```

### Pretty-Printing Scoped Expressions

Now to get to the entire utility of this abstraction: inspecting and
consuming the structure. For our new structure, we took out the string name
from `EVar` in lieu of an index. This is intentional, so that we keep the
"responsibility" of storing the string name at the `ELambda` constructor and
not have `EVar` redundantly store it.

However, this means that for pretty-printing, we will need to track the
variable names in the environment as we descend into lambdas. This is done very
similar to how it was done in `eval`, but instead of tracking and indexing out
the evaluated `EValue`s, we track and index out the string names of each
variable instead.

```haskell
!!!typed-sm-lc/ExprStage4.hs "data NameField"
```

```haskell
ghci> :t NameField @"hello" :& NameField @"world" :& RNil
Rec NameField ["hello" ::: a, "world" ::: b]
```

```haskell
!!!typed-sm-lc/ExprStage4.hs "ppPrim ::" "ppOp ::" "ppFields ::" "ppHandlers ::" "ppExpr ::" "prettyExpr ::"
```

The Next Step
-------------

Well, we set out with a simple goal: an expression language that lives within
Haskell that we can inspect interrogate within the language, but where it was
impossible to construct (in Haskell) a term that did not type-check (in the
domain language).

There were some decisions that were made that could have gone either way, like
using row types for the record and sum type specification, but overall the goal
was to create a solid inductive system that can be easily pattern-matched and
prove with normal Haskell tools.

But, honestly, why does it matter that our expression language has to reject
invalid domain-level terms at the Haskell level? Why couldn't we go the way of
other expression DSLs in Haskell, where we settle with "untyped" terms being
expressible in Haskell, and validated using a separate `typeCheck` function to
validate terms at run-time?

I don't know. But really, maybe this is another way of taking the old [Parse,
Don't Validate][pdn] adage to the extreme. Why allow yourself to construct
invalid terms of your domain inside Haskell? Why use a "validate" function
(`isValid`) when you can just make invalid terms impossible to construct?

[pdn]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

Thinking about it again...no. There is no other choice. We CANNOT allow
invalid domain terms to be constructable. If we have the ability to do better,
we MUST. With great power comes great responsibility. And if we compromise
here, how can we trust ourselves not to compromise when it really matters?

One must imagine the stubborn typer happy.
