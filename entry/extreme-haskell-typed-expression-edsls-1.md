Extreme Haskell: Typed Expression EDSLs (Part 1)
================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on July 7, 2026.
> [Read online!](https://blog.jle.im/entry/extreme-haskell-typed-expression-edsls-1.html)

I always say, inside every Haskeller there are two wolves, living on opposite
ends of the Haskell Fancy Code Spectrum. Are you going to write "simple
Haskell", using basic GHC 2010 tools and writing universal Haskell that every
introductory course offers, trying to keep the code as immediately
understandable and accessible? Or are you going to pile in all of the Haskell
type system and evaluation tricks you can find and turn on all the extensions,
and go full fancy?

In my [Seven Levels of Type
Safety](https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html)
post, I described different extremes of type safety and fancy code. I talked
about how writing effective code was finding the correct compromise for the
level of communication and safety you need.

But this is not that kind of blog post. This is the kind of blog post where we
celebrate terrifying type-safety, facetious fanciness, and masochistic
meta-analysis. This series is about what happens when we dare to go full fancy.
Let's write code that is so inscrutable, so painful and torturous to write, yet
so *undeniably useful* that you can't help but try to throw it into every single
thing you write and will feel a gnawing emptiness in your soul until you do.

As our example, let's write a type-safe method to specify your program as a
series of states, with triggered transitions between them: a type-safe state
machine graph using a type-safe lambda calculus. We want to specify this in a
way that we can write once and then:

1.  be interpretable in a type-safe way within Haskell.
2.  be inspectable with visualizable control flow.
3.  be compilable to multiple actual back-ends, letting you run the same
    function under multiple implementations.

This exact thing is something I've needed and used multiple times now in
projects. I want to specify one program graph within Haskell, but in a way that
can compile both in C and javascript while also being visualizable and
interactively explorable.

Once you go down this road, everything you ever write will feel woefully unsafe
and limited. And everything you want to write will be hopelessly inscrutable by
normal humans and borderline unusable. But such is the curse we all bear. Turn
around now, you have been warned.

This post will build up the embedded typed expression language. Part 2 will use
that expression language to define typed state machines with embedded predicates
and visualize them, and Part 3 will compile those machines to different
languages and verify they execute identically, with some live demos.

All of the code here is [available
online](https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/flake.nix),
and if you check out the repo and run `nix develop` you should be able to load
it all in ghci:

``` bash
$ cd code-samples/typed-sm-lc
$ nix develop
$ ghci
ghci> :load ExprStage1.hs
```

## The Lambda Calculus

Let's derive a way to express an algorithm or expression in Haskell that can be
reified and analyzed within Haskell, and eventually be a form we can compile to
different backends, interpret in Haskell, or generate Graphviz visualizations
for.

### A First Pass

One basic thing we can do is start with:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L18-L34

data Prim = PInt Int | PBool Bool | PString String
  deriving (Eq, Show)

data Op = OPlus | OTimes | OLte | OAnd
  deriving (Eq, Show)

data Expr
  = EPrim Prim
  | EVar String
  | ELambda String Expr
  | EApply Expr Expr
  | EOp Op Expr Expr
  | ERecord (Map String Expr)
  | EAccess Expr String
  | EChoice String Expr
  | ECase Expr (Map String (String, Expr))
  deriving (Eq, Show)
```

And you can write `(\x -> x * 3) 5` as:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L36-L37

fifteen :: Expr
fifteen = ELambda "x" (EOp OTimes (EVar "x") (EPrim (PInt 3))) `EApply` EPrim (PInt 5)
```

The strings in `ELambda` introduce variables, and `EVar` refers to the bound
variable. As you can see, this is...pretty untyped. We could easily write
something that is meaningless:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L67-L69

badTypeExample :: Expr
badTypeExample =
  EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))
```

Of course, GHC can typecheck our code if we literally write `\x -> x + 3` and
reject `1 && 2`. But we aren't trying to build opaque Haskell code here, we're
trying to represent our expression as an ADT that we can analyze *within* the
language.

If we want a record projection and one labeled choice with a case analysis over
it:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L39-L62

recordExample :: Expr
recordExample =
  EOp
    OPlus
    ( EAccess
        ( ERecord $
            M.fromList
              [ ("value", EPrim (PInt 7)),
                ("label", EPrim (PString "found"))
              ]
        )
        "value"
    )
    (EPrim (PInt 1))

sumExample :: Expr
sumExample =
  ECase
    (EChoice "Found" (EPrim (PInt 7)))
    ( M.fromList
        [ ("Found", ("value", EOp OPlus (EVar "value") (EPrim (PInt 1)))),
          ("Missing", ("message", EPrim (PInt 0)))
        ]
    )
```

Now, for the entire point of `Expr`, we can write a function to pretty-print it,
using the *[prettyprinter](https://hackage.haskell.org/package/prettyprinter)*
library:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L253-L299

ppPrim :: Prim -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool True -> "true"
  PBool False -> "false"
  PString s -> PP.pretty (show s)

ppOp :: Op -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

ppExpr :: Bool -> Expr -> PP.Doc ann
ppExpr paren = \case
  EPrim p -> ppPrim p
  EVar v -> PP.pretty v
  ELambda n body ->
    wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
  EApply f x ->
    wrap $ ppExpr True f <+> ppExpr True x
  EOp o x y ->
    wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
  ERecord xs ->
    PP.encloseSep "{ " " }" ", " $
      [PP.pretty k <+> "=" <+> ppExpr False v | (k, v) <- M.toList xs]
  EAccess e k ->
    ppExpr True e <> "." <> PP.pretty k
  EChoice tag x ->
    wrap $ PP.pretty tag <+> ppExpr True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr False x <+> "of"
        , PP.encloseSep "{ " " }" "; " $
            [ PP.pretty tag <+> PP.pretty n <+> "->" <+> ppExpr False body
            | (tag, (n, body)) <- M.toList hs
            ]
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

prettyExpr :: Expr -> PP.Doc ann
prettyExpr = ppExpr False
```

``` haskell
ghci> prettyExpr fifteen
(\x -> x * 3) 5
ghci> prettyExpr badTypeExample
1 && 2
ghci> prettyExpr recordExample
{ label = "found", value = 7 }.value + 1
ghci> prettyExpr sumExample
case Found 7 of { Found value -> value + 1; Missing message -> 0 }
```

Now, we can write a quick typechecker for this using a greedy type-checking
algorithm ([written out
here](https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L77-L89)),
which is a fun exercise, but it's beyond the point of this post. For our
purposes, we're going to write the in-Haskell evaluator, which is one sure-fire
evidential/constructive way to prove an expression was valid after-the-fact.

So...how can you "evaluate" this to 15, within Haskell? What would the type even
be? The best we can do at this point is make the entire thing monadic by
returning `Maybe` or `Either`, and split out the expressions we write from the
values we can actually evaluate to:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L228-L330

data EValue
  = EVInt Int
  | EVBool Bool
  | EVString String
  | EVFun (EValue -> Maybe EValue)
  | EVRecord (Map String EValue)
  | EVChoice String EValue

eval :: Map String EValue -> Expr -> Maybe EValue
eval env = \case
  EPrim p -> evalPrim p
  EVar v -> M.lookup v env
  ELambda n body -> pure (EVFun (\x -> eval (M.insert n x env) body))
  EApply f x -> eval env f >>= \case
    EVFun f' -> eval env x >>= f'
    _ -> Nothing
  EOp o x y -> do
    u <- eval env x
    v <- eval env y
    case (u, v) of
      (EVInt a, EVInt b) -> case o of
        OPlus -> pure (EVInt (a + b))
        OTimes -> pure (EVInt (a * b))
        OLte -> pure (EVBool (a <= b))
        OAnd -> Nothing
      (EVBool a, EVBool b) -> case o of
        OAnd -> pure (EVBool (a && b))
        _ -> Nothing
      _ -> Nothing
  ERecord xs -> EVRecord <$> traverse (eval env) xs
  EAccess e k -> do
    EVRecord xs <- eval env e
    M.lookup k xs
  EChoice tag x -> EVChoice tag <$> eval env x
  ECase x hs -> do
    EVChoice tag payload <- eval env x
    (n, body) <- M.lookup tag hs
    eval (M.insert n payload env) body
```

This would properly evaluate:

``` haskell
ghci> eval M.empty fifteen
Just (EVInt 15)
```

We can also produce closures as values:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L64-L65

plusThree :: Expr
plusThree = ELambda "x" (EOp OPlus (EVar "x") (EPrim (PInt 3)))
```

``` haskell
ghci> for_ (eval M.empty plusThree) \case
    EVFun f -> print (f (EVInt 4))
    _ -> putStrLn "not a function"
Just (EVInt 7)
```

This kind of works if you remember to thread everything through `Maybe` (or
`Either`) or what have you. But this is not ideal. You should be able to know,
at compile-time, that your `Expr` is valid. After all, you want to be able to
create one "valid" `Expr`, and run it at every context. It's useless to you if
every single time you used an `Expr`, you had to manually handle the `Nothing`
case. Your diagram generator, your Haskell runner, your code generator, will
always be in `Either` even though you know your `Expr` is valid, via tests or
something. We want GHC to reject badly typed expressions, so we never need to
unwrap or handle a `Nothing` or `Left`!

No, no, this is not okay and not acceptable. We should be able to verify in the
types if an `Expr` is valid.

## Type-Indexed Expressions

### Just Add the Index

The next step you'll see in posts online is to add a phantom index type to
`Expr`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L19-L49

type data Ty
  = TInt
  | TBool
  | TString
  | Ty :-> Ty

data Prim :: Ty -> Type where
  PInt :: Int -> Prim TInt
  PBool :: Bool -> Prim TBool
  PString :: String -> Prim TString

data Op :: Ty -> Ty -> Ty -> Type where
  OPlus :: Op TInt TInt TInt
  OTimes :: Op TInt TInt TInt
  OLte :: Op TInt TInt TBool
  OAnd :: Op TBool TBool TBool

data Expr :: Ty -> Type where
  EPrim :: Prim t -> Expr t
  EVar :: STy t -> String -> Expr t
  ELambda :: STy a -> String -> Expr b -> Expr (a :-> b)
  EApply :: Expr (a :-> b) -> Expr a -> Expr b
  EOp :: Op a b c -> Expr a -> Expr b -> Expr c
```

(We'll explain each part of this declaration eventually)

A phantom type is a type parameter that doesn't represent any actual *value*
"contained" inside the data type, but just serves to "tag" or distinguish values
for the compiler to reject or unify things in useful ways.

This introduces several new language features, so bear with me as I break them
down.

First, we use `-XTypeData` to define a data kind: `Ty` is a kind with types
`TInt :: Ty`, `TBool :: Ty`, etc. And in `Expr t`, we have an expression tagged
with `t`, which describes the result type. (In fact, *all* data types are
automatically promoted to the type level with `-XDataKinds`. You might see the
`'Nothing` quote prefix syntax in cases where it's ambiguous if you're talking
about the data constructor or the type constructor, like `'[]` and `'(,)`)

For example, because we have `EPrim :: Prim t -> Expr t`, and
`PInt 3 :: Prim TInt`, we have `EPrim (PInt 3) :: Expr TInt`: a primitive 3 is
an expression describing an integer.

And because `EOp :: Op a b c -> Expr a -> Expr b -> Expr c`, and
`OLte :: Op TInt TInt TBool`, we have

``` haskell
EOp OLte :: Expr TInt -> Expr TInt -> Expr TBool
```

So we can write an operation on two `Expr`s that typecheck how we'd expect:

``` haskell
ghci> :t EOp OLte (EPrim (PInt 3)) (EPrim (PInt 4))
Expr TBool
```

We also have `EOp OAnd :: Expr TBool -> Expr TBool -> Expr TBool`, which means
the compiler will reject our previous `1 && 2` example:

``` haskell
ghci> :t EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))
<interactive> error:
    • Couldn't match type ‘TInt’ with ‘TBool’
      Expected: Prim TBool
        Actual: Prim TInt
```

We can write lambdas in this system too:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L51-L55

fifteen :: Expr TInt
fifteen =
  EApply
    (ELambda STInt "x" (EOp OTimes (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))
```

Because of `Ty`, we can also make a new indexed data type with phantoms of
"fully resolved" values:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L115-L119

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVFun :: (EValue a -> Maybe (EValue b)) -> EValue (a :-> b)
```

This is what we want to eventually `eval` into, as we can guarantee ourselves to
get a value of the correct type based on the `Ty`:

``` haskell
eValueToInt :: EValue TInt -> Int
eValueToInt = \case
    EVInt x -> x
```

And GHC will verify this as a total pattern match because `EVInt` is the only
possible way to create an `EValue TInt`.

### Singletons and Existentials

We'll keep our bound variables stored as an ambient map of variable names to
their evaluated values for now. But, to do this, we need to turn the
heterogeneous `EValue t` into the homogeneous `Map String SomeValue` by wrapping
the type variable as an existential type.

You might notice we have a
[singleton](https://blog.jle.im/entries/series/+introduction-to-singletons.html)
for our `Ty` type, `STy`, that pops up in multiple situations.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L27-L31

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
  STFun :: STy a -> STy b -> STy (a :-> b)
```

Firstly, it might help to recognize the general pattern where `STy` (the
singleton) appears. It *usually* pops up whenever we have existentially scoped
variables, like in `data SomeValue = forall t. SomeValue (STy t) (EValue t)`. In
this case, the `t` is completely lost to the outside world, and `STy t` is used
to allow us to recover a runtime witness to what `t` was, after pattern matching
on `STy`. This is the *dependent sum* pattern, and is similar to how `Typeable`
is used in *Data.Dynamic*.

In our case, because variables are still stored ambiently in the environment and
validated at runtime, we *do* need singletons to implement `eval` . The type
`Expr t` only specifies the type of the result, but the type information of the
ambient variables is not available. So, you can write
`EVar STInt "myVar" :: Expr TInt`, but:

-   `myVar` might not be a variable in scope at all, so `eval` will fail at
    runtime
-   `myVar` might be in scope, but might be a `TString` and not a `TInt`

The first case is easy enough to deal with (`M.lookup` returns `Nothing`), but
the second one is a little more subtle. Let's say we *do* have a `SomeValue`
under our key `myVar`...how do we make sure it has the correct type?

### Runtime Type Equality

We can do ad-hoc pattern matching on `EValue`, but that won't get us too far.
Mostly because some of the `EValue` constructors actually don't have enough
information for us to validate their actual type (try it! `EVFun` will give you
a lot of trouble). So, what we can do is write a function that takes two `STy`
at runtime and *unifies* them conditionally if they are the same. We'll write a
function `sameTy :: STy a -> STy b -> Maybe (a :~: b)`, where

``` haskell
data (:~:) :: k -> k -> Type where
    Refl :: a :~: a
```

*pattern matching* on a value of type `a :~: b` will reveal that `a` and `b` are
the same type variable, because the only way to construct it is with
`Refl :: a :~: a`.

With that, we can write `sameTy`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L133-L143

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
  STFun a b -> \case
    STFun c d -> do
      Refl <- sameTy a c
      Refl <- sameTy b d
      Just Refl
    _ -> Nothing
```

There's a typeclass in *base* (or rather, a "kindclass"), `TestEquality`, that
encapsulates this pattern:

``` haskell
class TestEquality f where
    testEquality :: f a -> f b -> Maybe (a :~: b)
```

In fact, we can write our `sameTy` as an instance:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L130-L131

instance TestEquality STy where
  testEquality = sameTy
```

We're now at a higher fanciness level than before. But you might see the problem
here: `EVar STInt "x"`. `x` might not be defined, and it also might not have the
correct type. Soooo yes, we still have issues here.

But now at least we can write `eval`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L145-L176

eval :: Map String SomeValue -> Expr t -> Maybe (EValue t)
eval env = \case
  EPrim (PInt n) -> pure (EVInt n)
  EPrim (PBool b) -> pure (EVBool b)
  EPrim (PString s) -> pure (EVString s)
  EVar t v -> do
    SomeValue t' v' <- M.lookup v env
    Refl <- sameTy t t'
    pure v'
  ELambda ta n body ->
    pure $ EVFun $ \x -> eval (M.insert n (SomeValue ta x) env) body
  EApply f x -> do
    EVFun g <- eval env f
    x' <- eval env x
    g x'
  EOp o x y -> case o of
    OPlus -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a + b))
    OTimes -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a * b))
    OLte -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVBool (a <= b))
    OAnd -> do
      EVBool a <- eval env x
      EVBool b <- eval env y
      pure (EVBool (a && b))
```

This does seem to work:

``` haskell
ghci> for_ (eval M.empty fifteen) \case
    EVInt x -> print x
15
```

Our system also allows us to produce closures and functions as values:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L63-L64

plusThree :: Expr (TInt :-> TInt)
plusThree = ELambda STInt "x" (EOp OPlus (EVar STInt "x") (EPrim (PInt 3)))
```

``` haskell
ghci> for_ (eval M.empty plusThree) \case
    EVFun f -> for_ (f (EVInt 4)) \case
      EVInt x -> print x    -- compiler-verified to always be EVInt
7
```

We have a type-safe `eval` now that will create a value of the type we want. But
we still have the same errors when looking at variables: variables can still not
be defined, or be defined as the wrong type.

### Pretty-Printing

One nice consequence of this type-index method is that if you choose to consume
them into an untyped target, you can do it more or less in the same way as the
non-indexed untyped data.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L87-L113

ppPrim :: Prim t -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool b -> if b then "true" else "false"
  PString s -> PP.pretty (show s)

ppOp :: Op a b c -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

ppExpr :: Bool -> Expr t -> PP.Doc ann
ppExpr paren = \case
  EPrim p -> ppPrim p
  EVar _ v -> PP.pretty v
  ELambda _ n body -> wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
  EApply f x -> wrap $ ppExpr True f <+> ppExpr True x
  EOp o x y -> wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
  where
    wrap
      | paren = PP.parens
      | otherwise = id

prettyExpr :: Expr t -> PP.Doc ann
prettyExpr = ppExpr False
```

And they render the same way:

``` haskell
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
*any* variable name as *any* type, and it'll still typecheck. Even with the help
of everything we have, we can just straight-up declare a reference to an unbound
variable

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L71-L72

unboundVariable :: Expr TInt
unboundVariable = EVar STInt "missing"
```

This typechecks in GHC even though it's meaningless in the domain. We can also
reference the variable under any *type*:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L57-L61

badVariable :: Expr TInt
badVariable =
  EApply
    (ELambda STBool "x" (EOp OPlus (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PBool True))
```

That's because `EVar` can freely take any `STy` without any restriction, and no
association with the binder name, so there's no way for GHC to stop us.

So, again, we cannot create a *fully* type-checked `Expr`. We still have to deal
with *most* of the same errors. This is noble, but clearly not good enough. We
have to go deeper.

## Typed Records and Sums

A quick detour: you might have noticed that this past implementation dropped
records and sums. Before we move on, let's go ahead and add those. Introducing
records and sums at the same time as type-indexed `Expr` is a bit *too* much of
a jump to fit into a single section.

Let's add sums and records, which can use pretty similar mechanisms (via
duality) for implementation.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L31-L79

type data Ty
  = TInt
  | TBool
  | TString
  | TRecord [(Symbol, Ty)]
  | TSum [(Symbol, Ty)]
  | Ty :-> Ty

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
  STRecord :: Rec STyField as -> STy (TRecord as)
  STSum :: Rec STyField as -> STy (TSum as)
  STFun :: STy a -> STy b -> STy (a :-> b)
```

`Ty` now includes `TRecord [(Symbol, Ty)]` and `TSum [(Symbol, Ty)]`, which
represent the field names and constructor payloads (`Symbol` being a type-level
string). So, for example, `TRecord ["value" ::: TInt, "label" ::: TString]`
would be the type of a record with ordered fields `value` and `label` of
integers and strings, respectively.
`TSum ["Found" ::: TInt, "Missing" ::: TString]` would be the type of a sum
between `Found` containing an integer and `Missing` containing a string. Note we
take a page out of [vinyl](https://hackage.haskell.org/package/vinyl) by
defining the type alias `(:::) = '(,)` to make things syntactically nicer.

### Record Access

We need the fields and types at the type level because we have to answer what
the `Expr` phantom type of field access is. If we had an
`x :: Expr (TRecord ["value" ::: TInt, "label" ::: TString])`, we want the type
of `x.value` to be `Expr TInt`.

To do this, we need to have a *value* in our `Expr` for field access that can
"point" at a specific field in the type. One way to do that is to take a field
of type `Index`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L47-L49

data Index :: [k] -> k -> Type where
  IZ :: Index (x : xs) x
  IS :: Index xs x -> Index (y : xs) x
```

You can read this as: "`IZ` is an index to the head of the type-level list, and
`IS n` is an index to the n-th item of the tail". So, `IS IZ` is an index into
the second element, `IS (IS IZ)` is an index into the third, etc.

If we have `["value" ::: TInt, "label" ::: TString]`, then we have values:

``` haskell
IZ    :: Index ["value" ::: TInt, "label" ::: TString] ("value" ::: TInt)
IS IZ :: Index ["value" ::: TInt, "label" ::: TString] ("label" ::: TString)
```

Note that the way this is constructed, it's impossible for
`IS (IS IZ) :: Index ["value" ::: TInt, "label" ::: TString] _` to typecheck as
anything!

In this way, we have a well-typed field accessor syntax, which takes an `Expr`
of a record of fields and indexes it to get an `Expr` of the type at that index:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L105-L105

  EAccess :: KnownSymbol l => Expr (TRecord as) -> Index as (l ::: a) -> Expr a
```

Which is typed as:

``` haskell
(`EAccess` IZ)    :: Expr (TRecord ["value" ::: TInt, "label" ::: TString]) -> Expr TInt
(`EAccess` IS IZ) :: Expr (TRecord ["value" ::: TInt, "label" ::: TString]) -> Expr TString
```

To *create* an `Expr` of a record, we can use `Rec` from
*[vinyl](https://hackage.haskell.org/package/vinyl)* or `NP` from
*[sop-core](https://hackage.haskell.org/package/sop-core)*: a heterogeneous list
indexed by a type-level list.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L43-L45

data Rec :: (k -> Type) -> [k] -> Type where
  RNil :: Rec f '[]
  (:&) :: f x -> Rec f xs -> Rec f (x : xs)
```

If you haven't seen `Rec` before, basically `Rec f [a,b,c]` is a tuple of `f a`,
`f b`, and `f c`. For example:

``` haskell
ghci> :t Identity 3 :& Identity True :& Identity "hello" :& RNil
Rec Identity [Int, Bool, String]
ghci> :t Const "x" :& Const "y" :& RNil
Rec (Const String) [x1, x2]
```

Keeping `Rec f as` instead of a direct heterogeneous list of `a`s lets us store
more interesting things than just `Type`-kinded things. For example, since our
lists here are lists of `(Symbol, Ty)`, we can create a container to hold
fields:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L109-L110

data ExprField :: (Symbol, Ty) -> Type where
  EField :: KnownSymbol l => Expr a -> ExprField (l ::: a)
```

The field constructor keeps a `KnownSymbol l` constraint, so the type-level
label is still available later when we need to render it:

``` haskell
ghci> :t EField @"value" (EPrim (PInt 7))
            :& EField @"label" (EPrim (PString "found"))
            :& RNil
Rec ExprField ["value" ::: TInt, "label" ::: TString]
```

So, we can create `Expr (TRecord ["value" ::: TInt, "label" ::: TString])` by
taking a `Rec`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L104-L104

  ERecord :: Rec ExprField as -> Expr (TRecord as)
```

We can make this a little more ergonomic by using `-XRequiredTypeArguments` (as
of GHC 9.10) to get rid of the `-XTypeApplication` ugliness:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L115-L150

eField :: forall l -> KnownSymbol l => Expr a -> ExprField (l ::: a)
eField l = EField @l

makeRecordExample :: Expr (TRecord ["value" ::: TInt, "label" ::: TString])
makeRecordExample =
  ERecord
    ( eField "value" (EPrim (PInt 7))
        :& eField "label" (EPrim (PString "found"))
        :& RNil
    )

recordExample :: Expr TInt
recordExample = EOp OPlus (EAccess @"value" makeRecordExample IZ) (EPrim (PInt 1))
```

### Sum Injection and Case Analysis

We also need a type-level list witness for *sum* types, because we need to be
able to implement the correct continuations for pattern matches: How do we know
*what* thing to handle in each pattern match, unless the sum type has that
information in its type?

Luckily due to the magic of duality, we can use the same tools, for the most
part! We can inject into a sum with an `Index`, let's say for a
`Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])`: sum type with `Found`
containing an integer and `Missing` containing a string:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L106-L106

  EChoice :: KnownSymbol l => Index as (l ::: a) -> Expr a -> Expr (TSum as)
```

``` haskell
EChoice @"Found" IZ        :: Expr TInt -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
EChoice @"Missing" (IS IZ) :: Expr TString -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L181-L182

makeSumExample :: Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
makeSumExample = EChoice @"Found" IZ (EPrim (PInt 7))
```

And we can re-use `Rec` to define a type that can *handle* a
`["Found" ::: TInt, "Missing" ::: TString]` sum:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L112-L133

data ExprHandler :: Ty -> (Symbol, Ty) -> Type where
  EHandler :: KnownSymbol l => STy a -> String -> Expr b -> ExprHandler b (l ::: a)

eHandler :: forall l -> KnownSymbol l => STy a -> String -> Expr b -> ExprHandler b (l ::: a)
eHandler l = EHandler @l
```

``` haskell
ghci> :t eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
ExprHandler TInt ("Found" ::: TInt)
         -- ^ result           ^ payload
```

And we can put these into a `Rec` to handle each option in the list:

``` haskell
ghci> :t eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
           :& eHandler "Missing" STString "message" (EPrim (PInt 0))
           :& RNil
Rec (ExprHandler TInt) ["Found" ::: TInt, "Missing" ::: TString]
```

And that's exactly what a case statement is:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L107-L107

  ECase :: Expr (TSum as) -> Rec (ExprHandler b) as -> Expr b
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L184-L191

sumExample :: Expr TInt
sumExample =
  ECase
    makeSumExample
    ( eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
        :& eHandler "Missing" STString "message" (EPrim (PInt 0))
        :& RNil
    )
```

Note that we're still using string binders, so there's still an element of
unsafety here... we say that the variable name is `"value"` and that it is a
`TInt`, but when we later refer to the variable with `EVar STInt "value"`, it
isn't type-checked that later references use the same type. The compiler would
be just as happy with `EVar STString "value"`.

Here's an example demonstrating both failure modes: the first handler references
a variable that doesn't exist, and the second handler references a variable that
does exist as an incorrect type! How unfortunate.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L202-L209

badCaseBranchExample :: Expr TInt
badCaseBranchExample =
  ECase
    (EChoice @"Found" IZ (EPrim (PInt 7)))
    ( eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "missing") (EPrim (PInt 1)))
        :& eHandler "Missing" STString "message" (EOp OPlus (EVar STInt "message") (EPrim (PInt 1)))
        :& RNil
    )
```

### Runtime Equality for Records and Sums

One complication is that we need to update the `TestEquality` instance for
`STy`. The record and sum labels are type-level `Symbol`s, so we compare those
with the `sameSymbol` (kind of like `testEquality` for any `KnownSymbol`
instance) and then compare the payload types recursively.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L284-L324

instance TestEquality STy where
    testEquality = sameTy

instance TestEquality STyField where
    testEquality = sameField

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
  STRecord as -> \case
    STRecord bs -> do
      Refl <- sameFields as bs
      Just Refl
    _ -> Nothing
  STSum as -> \case
    STSum bs -> do
      Refl <- sameFields as bs
      Just Refl
    _ -> Nothing
  STFun a b -> \case
    STFun c d -> do
      Refl <- sameTy a c
      Refl <- sameTy b d
      Just Refl
    _ -> Nothing

sameFields :: Rec STyField xs -> Rec STyField ys -> Maybe (xs :~: ys)
sameFields RNil RNil = Just Refl
sameFields (x :& xs) (y :& ys) = do
  Refl <- sameField x y
  Refl <- sameFields xs ys
  Just Refl
sameFields _ _ = Nothing

sameField :: STyField x -> STyField y -> Maybe (x :~: y)
sameField (STyField @l tx) (STyField @m ty) = do
    Refl <- sameSymbol (Proxy @l) (Proxy @m)
    Refl <- sameTy tx ty
    Just Refl
```

### The Full Eval

Before we write the final `eval`, let's practice using `Index` and `Rec`
together. If we have an index `Index as a` that picks out a value `a` in `as`,
then we can pick out the `f a` from a `Rec f as`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L60-L63

indexRec :: Index xs x -> Rec f xs -> f x
indexRec = \case
  IZ -> \(x :& _) -> x
  IS i -> \(_ :& xs) -> indexRec i xs
```

We also can recursively iterate a function over each item, assuming the function
`forall x. f x -> g x`: that is, we can turn a `Rec f as` into a `Rec g as`
assuming our function is polymorphic over each `x`, and only depends on the
shape of `f`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L65-L67

traverseRec :: Applicative m => (forall x. f x -> m (g x)) -> Rec f xs -> m (Rec g xs)
traverseRec _ RNil = pure RNil
traverseRec f (x :& xs) = (:&) <$> f x <*> traverseRec f xs
```

With that, we can write our full `eval`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L326-L369

eval :: Map String SomeValue -> Expr t -> Maybe (EValue t)
eval env = \case
  EPrim (PInt n) -> pure (EVInt n)
  EPrim (PBool b) -> pure (EVBool b)
  EPrim (PString s) -> pure (EVString s)
  EVar t v -> do
    SomeValue t' v' <- M.lookup v env
    Refl <- sameTy t t'
    pure v'
  ELambda ta n body ->
    pure $ EVFun $ \x -> eval (M.insert n (SomeValue ta x) env) body
  EApply f x -> do
    EVFun g <- eval env f
    x' <- eval env x
    g x'
  EOp o x y -> case o of
    OPlus -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a + b))
    OTimes -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a * b))
    OLte -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVBool (a <= b))
    OAnd -> do
      EVBool a <- eval env x
      EVBool b <- eval env y
      pure (EVBool (a && b))
  ERecord xs ->
    EVRecord <$> traverseRec (evalField env) xs
  EAccess e i -> do
    EVRecord xs <- eval env e
    case indexRec i xs of
      EVField v -> pure v
  EChoice i x ->
    EVSum i <$> eval env x
  ECase x hs -> do
    EVSum i v <- eval env x
    case indexRec i hs of
      EHandler t n body -> eval (M.insert n (SomeValue t v) env) body
```

### Ergonomics of Records and Sums

Note that we could also choose to implement records and sums using row types
indexed by the name of the field itself, instead of an ordered list of tuples.
This would have the advantage of making, for example,
`{ value :: Int, label :: String }` the same type as
`{ label :: String, value :: Int }`. However, I personally prefer the style of
building things inductively (`:&`/`RNil` and `IS`/`IZ`), it makes the type
errors and constructions a lot easier to work with and reason with.

However, we can get a little bit of the best of both worlds by using typeclasses
to auto-insert the `Index` witnesses into a list.

First, we can write a typeclass that searches a type-level list of fields and
produces the right `Index`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L51-L58

class ListIx (l :: Symbol) (xs :: [(Symbol, Ty)]) (a :: Ty) | l xs -> a where
  listIx :: Index xs (l ::: a)

instance ListIx l (l ::: a : xs) a where
  listIx = IZ

instance {-# OVERLAPPABLE #-} ListIx l xs a => ListIx l (m ::: b : xs) a where
  listIx = IS (listIx @l)
```

The FunDep `l xs -> a` lets us use this like a function: for a label `l` and a
list `xs`, we should be able to uniquely determine the `a` type it singles out,
if it exists. So we have an instance of
`ListIx "value" ["value" ::: TInt, "label" ::: TString] TInt`, where the label
`"value"` and the list uniquely determines the result type `TInt`.

We can now have a helper function that we can call like `eAccess "value"` using
GHC 9.10's `RequiredTypeArguments`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L118-L123

eAccess ::
  forall (l :: Symbol) ->
  (KnownSymbol l, ListIx l as a) =>
  Expr (TRecord as) ->
  Expr a
eAccess l e = EAccess @l e (listIx @l)
```

That lets us write the field name directly, and the compiler will generate the
`Index` automatically for us:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L167-L176

namedAccessExample :: Expr TString
namedAccessExample =
  eAccess
    "label"
    ( ERecord
        ( eField "value" (EPrim (PInt 7))
            :& eField "label" (EPrim (PString "found"))
            :& RNil
        )
    )
```

Here,
`eAccess "value" :: Expr (TRecord ["value" ::: TInt, "label" ::: TString]) -> Expr TInt`,
its result type uniquely determined by the types in the record fields.

The same trick works for sum injections, so we can write the constructor name
directly:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L125-L179

eChoice ::
  forall (l :: Symbol) ->
  (KnownSymbol l, ListIx l as a) =>
  Expr a ->
  Expr (TSum as)
eChoice l e = EChoice @l (listIx @l) e

namedChoiceExample :: Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])
namedChoiceExample = eChoice "Missing" (EPrim (PString "not here"))
```

Here,
`eChoice "Missing" :: Expr TString -> Expr (TSum ["Found" ::: TInt, "Missing" ::: TString])`,
because the constructor name `"Missing"` uniquely determines the payload type
inside that sum.

### Pretty-Printing Records and Sums

To pretty-print, we finally use that `KnownSymbol` constraint we've been
tracking this entire time. We can use
`symbolVal :: KnownSymbol s => p s -> String` to get the string *value* from the
type-level string.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L237-L282

ppPrim :: Prim t -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool b -> if b then "true" else "false"
  PString s -> PP.pretty (show s)

ppOp :: Op a b c -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

ppFields :: Rec ExprField xs -> [PP.Doc ann]
ppFields RNil = []
ppFields (EField @l x :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> "=" <+> ppExpr False x) : ppFields xs

ppHandlers :: Rec (ExprHandler b) xs -> [PP.Doc ann]
ppHandlers RNil = []
ppHandlers (EHandler @l _ n body :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> PP.pretty n <+> "->" <+> ppExpr False body) : ppHandlers xs

ppExpr :: Bool -> Expr t -> PP.Doc ann
ppExpr paren = \case
  EPrim p -> ppPrim p
  EVar _ v -> PP.pretty v
  ELambda _ n body -> wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
  EApply f x -> wrap $ ppExpr True f <+> ppExpr True x
  EOp o x y -> wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
  ERecord xs -> PP.encloseSep "{ " " }" ", " (ppFields xs)
  EAccess @l e _ -> ppExpr True e <> "." <> PP.pretty (symbolVal (Proxy @l))
  EChoice @l _ x -> wrap $ PP.pretty (symbolVal (Proxy @l)) <+> ppExpr True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr False x <+> "of"
        , PP.encloseSep "{ " " }" "; " (ppHandlers hs)
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

prettyExpr :: Expr t -> PP.Doc ann
prettyExpr = ppExpr False
```

## Capturing Variables

In order to have `EVar` be type-safe, the environment itself needs to be a part
of the `Expr` type, and you should only be able to use `EVar` if the `Expr`
enforces it. `ELambda` would, therefore, introduce the new variable to the
environment.

We'll have:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L80-L80

data Expr :: [(Symbol, Ty)] -> Ty -> Type where
```

So a value of type `Expr ["x" ::: TInt, "y" ::: TBool] t` is an expression with
free variables `x` of type `Int` and `y` of type `Bool`.

Surprise! That small detour to add records and sums to our language actually
ended up being a smooth precursor to all of the techniques we will be using to
solve for variable binders.

`ELambda` would therefore take an `Expr` with a free variable and turn it into
an `Expr` of a function type. We also keep a `KnownSymbol` constraint for the
name so that we can recover the name when we render the expression.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L83-L83

  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
```

`EVar` then becomes exactly like `EAccess`! We "index" into the environment of
the `Expr vs a` using `Index`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L82-L82

  EVar :: Index vs (n ::: t) -> Expr vs t
```

So it is legal to have `EVar IZ :: Expr ["x" ::: TInt, "y" ::: TBool] TInt`, and
also it is automatically inferred to be a `TInt`. But we could *not* write
`EVar IZ :: Expr [] TInt`.

And finally, our whole `Expr`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L80-L89

data Expr :: [(Symbol, Ty)] -> Ty -> Type where
  EPrim :: Prim t -> Expr vs t
  EVar :: Index vs (n ::: t) -> Expr vs t
  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
  EApply :: Expr vs (a :-> b) -> Expr vs a -> Expr vs b
  EOp :: Op a b c -> Expr vs a -> Expr vs b -> Expr vs c
  ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
  EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a
  EChoice :: KnownSymbol l => Index as (l ::: a) -> Expr vs a -> Expr vs (TSum as)
  ECase :: Expr vs (TSum as) -> Rec (ExprHandler vs b) as -> Expr vs b
```

Just like with record access, we can use the `ListIx` class to write a named
helper:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L47-L120

class ListIx (l :: Symbol) (xs :: [(Symbol, Ty)]) (a :: Ty) | l xs -> a where
  listIx :: Index xs (l ::: a)

instance ListIx l (l ::: a ': xs) a where
  listIx = IZ

instance {-# OVERLAPPABLE #-} ListIx l xs a => ListIx l (m ::: b ': xs) a where
  listIx = IS (listIx @l)

eVar :: forall n -> ListIx n vs a => Expr vs a
eVar n = EVar (listIx @n)
```

Adding in our other `-XRequiredTypeArguments` helpers:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L91-L117

eLambda :: forall n -> KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
eLambda n = ELambda @n

eField :: forall l -> KnownSymbol l => Expr vs a -> ExprField vs (l ::: a)
eField l = EField @l

eAccess ::
  forall l ->
  (KnownSymbol l, ListIx l as a) =>
  Expr vs (TRecord as) ->
  Expr vs a
eAccess l e = EAccess @l e (listIx @l)

eChoice ::
  forall l ->
  (KnownSymbol l, ListIx l as a) =>
  Expr vs a ->
  Expr vs (TSum as)
eChoice l e = EChoice @l (listIx @l) e

eHandler ::
  forall n ->
  forall l ->
  (KnownSymbol n, KnownSymbol l) =>
  Expr (n ::: a ': vs) b ->
  ExprHandler vs b (l ::: a)
eHandler n l = EHandler @n @l
```

And we get something that is *truly* type-safe: all expressions are well-typed,
and all variables are ensured to be bound!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L139-L171

fifteen :: Expr '[] TInt
fifteen =
  EApply
    (eLambda "x" (EOp OTimes (eVar "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))

recordExample :: Expr '[] TInt
recordExample =
  EOp
    OPlus
    ( eAccess
        "value"
        ( ERecord
            ( eField "value" (EPrim (PInt 7))
                :& eField "label" (EPrim (PString "found"))
                :& RNil
            )
        )
    )
    (EPrim (PInt 1))

sumExample :: Expr '[] TInt
sumExample =
  ECase
    (eChoice "Found" (EPrim (PInt 7)))
    ( eHandler "value" "Found" (EOp OPlus (eVar "value") (EPrim (PInt 1)))
        :& eHandler "message" "Missing" (EPrim (PInt 0))
        :& RNil
    )
```

### What we gained

Now all of the previous "bad" examples we gave are finally compiler-verified:

``` haskell
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

``` haskell
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
GHC![^1]

(As an exercise, can understand why those errors are what they are? Why they all
contain `ListIx _ '[]`? For more ergonomics there is stuff we can do with
`TypeError` machinery to make the messages a little prettier; the *vinyl*
library does a lot to make error messages a bit better, like in `HasField`
[instance](https://hackage.haskell.org/package/vinyl/docs/Data-Vinyl-Derived.html#t:FieldType))

Note that this is sometimes done using straight [De Bruijn
indices](https://en.wikipedia.org/wiki/De_Bruijn_index): `Expr :: [Ty] -> Type`,
so we don't use any names but just the direct index, but the point of this
exercise is to be *borderline* unbearable to write, and *not* to be *actually*
unbearable to write.

### Eval with a typed environment

To actually write *eval* now, we need to have a type-safe environment to store
these variables. In order to `eval` an `Expr vs`, we need `EValue`s for each `v`
in `vs`. So for `Expr ["x" ::: TInt, "y" ::: TBool]`, we need to store a `TInt`
and a `TBool`. We can once again re-purpose `Rec`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L130-L131

data EValueField :: (Symbol, Ty) -> Type where
  EVField :: EValue a -> EValueField '(l, a)
```

``` haskell
ghci> :t (EVField (EVInt 3) :& EVField (EVBool True) :& RNil :: Rec EValueField ["x" ::: TInt, "y" ::: TBool])
Rec EValueField ["x" ::: TInt, "y" ::: TBool]
```

And so we can finally write:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L122-L271

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Rec EValueField as -> EValue (TRecord as)
  EVSum :: Index as (l ::: a) -> EValue a -> EValue (TSum as)
  EVFun :: (EValue a -> EValue b) -> EValue (a :-> b)

eval :: Rec EValueField vs -> Expr vs t -> EValue t
eval env = \case
  EPrim (PInt n) -> EVInt n
  EPrim (PBool b) -> EVBool b
  EPrim (PString s) -> EVString s
  EVar i -> case indexRec i env of
    EVField v -> v
  ELambda body ->
    EVFun $ \x -> eval (EVField x :& env) body
  EApply f x -> case eval env f of
    EVFun g -> g (eval env x)
  EOp o x y -> case (o, eval env x, eval env y) of
    (OPlus, EVInt a, EVInt b) -> EVInt (a + b)
    (OTimes, EVInt a, EVInt b) -> EVInt (a * b)
    (OLte, EVInt a, EVInt b) -> EVBool (a <= b)
    (OAnd, EVBool a, EVBool b) -> EVBool (a && b)
  ERecord xs ->
    EVRecord $ mapRec (\(EField x) -> EVField (eval env x)) xs
  EAccess e i -> case eval env e of
    EVRecord xs -> case indexRec i xs of
      EVField v -> v
  EChoice i x -> EVSum i (eval env x)
  ECase x hs -> case eval env x of
    EVSum i y -> case indexRec i hs of
      EHandler h -> eval (EVField y :& env) h
```

At least, we are here. A type-safe EDSL where only AST's that can be validly
evaluated are legal to represent in Haskell. At least, we can embrace the
freedom of not having to carefully construct your terms. You can relax now. The
compiler and the types have your back.

Even `EVFun :: (EValue a -> EValue b) -> EValue (a :-> b)` has a total
`EValue a -> EValue b`, making the closure evaluation also fully total.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L145-L147

plusThree :: Expr '[] (TInt :-> TInt)
plusThree =
  eLambda "x" (EOp OPlus (eVar "x") (EPrim (PInt 3)))
```

``` haskell
ghci> case eval RNil plusThree of
    EVFun f -> case f (EVInt 4) of
      EVInt x -> print x
7
```

### Pretty-Printing Scoped Expressions

Now to get to the entire utility of this abstraction: inspecting and consuming
the structure. For our new structure, we took out the string name from `EVar` in
lieu of an index. This is intentional, so that we keep the "responsibility" of
storing the string name at the `ELambda` constructor and not have `EVar`
redundantly store it.

However, this means that for pretty-printing, we will need to track the variable
names in the environment as we descend into lambdas. This is done very similar
to how it was done in `eval`, but instead of tracking and indexing out the
evaluated `EValue`s, we track and index out the string names of each variable
instead.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L182-L183

data NameField :: (Symbol, Ty) -> Type where
  NameField :: KnownSymbol l => NameField (l ::: a)
```

``` haskell
ghci> :t NameField @"hello" :& NameField @"world" :& RNil
Rec NameField ["hello" ::: a, "world" ::: b]
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L185-L245

ppPrim :: Prim t -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool b -> if b then "true" else "false"
  PString s -> PP.pretty (show s)

ppOp :: Op a b c -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

ppFields :: Rec NameField vs -> Rec (ExprField vs) xs -> [PP.Doc ann]
ppFields _ RNil = []
ppFields names (EField @l x :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> "=" <+> ppExpr names False x) : ppFields names xs

ppHandlers :: Rec NameField vs -> Rec (ExprHandler vs b) xs -> [PP.Doc ann]
ppHandlers _ RNil = []
ppHandlers names (EHandler @n @l body :& xs) =
  ( PP.pretty (symbolVal (Proxy @l))
      <+> PP.pretty (symbolVal (Proxy @n))
      <+> "->"
      <+> ppExpr (NameField @n :& names) False body
  )
    : ppHandlers names xs

ppExpr :: Rec NameField vs -> Bool -> Expr vs t -> PP.Doc ann
ppExpr names paren = \case
  EPrim p -> ppPrim p
  EVar i -> case indexRec i names of
    NameField @n -> PP.pretty (symbolVal (Proxy @n))
  ELambda @n body ->
    wrap $
      "\\" <> PP.pretty (symbolVal (Proxy @n))
        <+> "->"
        <+> ppExpr (NameField @n :& names) False body
  EApply f x ->
    wrap $ ppExpr names True f <+> ppExpr names True x
  EOp o x y ->
    wrap $ ppExpr names True x <+> ppOp o <+> ppExpr names True y
  ERecord xs ->
    PP.encloseSep "{ " " }" ", " (ppFields names xs)
  EAccess @l e _ ->
    ppExpr names True e <> "." <> PP.pretty (symbolVal (Proxy @l))
  EChoice @l _ x ->
    wrap $ PP.pretty (symbolVal (Proxy @l)) <+> ppExpr names True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr names False x <+> "of"
        , PP.encloseSep "{ " " }" "; " (ppHandlers names hs)
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

prettyExpr :: Expr '[] t -> PP.Doc ann
prettyExpr = ppExpr RNil False
```

``` haskell
ghci> prettyExpr fifteen
(\x -> x * 3) 5
```

## The Next Step

Well, we set out with a simple goal: an expression language that lives within
Haskell that we can inspect and interrogate within the language, but where it
was impossible to construct (in Haskell) a term that did not type-check (in the
domain language).

But, honestly, why does it matter that our expression language has to reject
invalid domain-level terms at the Haskell level? Why couldn't we go the way of
other expression DSLs in Haskell, where we settle with "untyped" terms being
expressible in Haskell, and validated using a separate `typeCheck` function to
validate terms at runtime?

I don't know. But really, maybe this is another way of taking the old [Parse,
Don't
Validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
adage to the extreme. Why allow yourself to construct invalid terms of your
domain inside Haskell? Why use a "validate" function (`isValid`) when you can
just make invalid terms impossible to construct?

But...no. There is no other choice. We CANNOT allow invalid domain terms to be
constructible. If we have the ability to do better, we MUST. With great power
comes great responsibility. And if we compromise here, how can we trust
ourselves not to compromise when it really matters?

One must imagine the stubborn typer happy.

In Part 2, we'll use our new EDSL to specify visualizable state machines and
programs within Haskell that are type-checked to be correct, and what it looks
like to actually *use* these within Haskell. And in Part 3, we'll start
"compiling" them to different language targets and different backends, with the
assurance that our generated programs are all synchronized and self-consistent.

### A Note on AI Coding

I guess I'm going to have to start mentioning this in every post. *But*, I
really do feel like this "extreme type safety" approach is more critical than
ever, in the age of agentic coding and LLM. I've been using LLMs in my daily
coding for many months now at this point, and one common pattern I've noticed:
when I start with a design with very clear, very strict types, LLMs excel. They
make much fewer errors, and the type system provides more immediate feedback on
their progress, without needing hundreds of defensive `x != null`-style guard
pollution.

Once I can express what I want in the language of extreme "invalid states
unrepresentable" types, LLM agents no longer feel like agents of chaotic
spaghetti extruding unmaintainable code. Instead, it feels like...seeding a
crystal and watching it grow into a beautiful, shimmering lattice. It feels like
the language they yearn to speak. And the more expressive your types, the more
beautiful the crystalline structure in the end.

Honestly, humans might have problems writing and using this code, but LLMs
definitely don't, if properly scaffolded! Since early 2026, at least, for me.
We'll explore a bit more about this once we have more to work with in Part 2.

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

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

[^1]: Excluding `_|_` in Haskell-land (recursion or `undefined`), unfortunately.

