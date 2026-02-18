Extreme Haskell: Typed State Machines with Typed Lambda Calculus
================================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-haskell-typed-state-machines-with-typed-lambda-calculus.html)

I always say, inside every Haskeller there are two wolves, living on both ends
of the Haskell Fancy Code Spectrum (HFCS). Are you going to write "simple
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

But this is not that kind of blog post. This blog post is about what happens
when you say "screw it, let's go full fancy"? Let's ignore the advice of the
great Kirk Lazarus. Let's go full fancy. Let's write code that is so
inscrutable, so much of a pain and torture to write, yet so *undeniably useful*
that you can't help but try to throw it in every single thing you write and will
feel a gnawing emptiness in your soul until you do.

Here's one example: let's write a type-safe method to specify your program as a
series of states, with triggered transitions between them. A Type-Safe state
machine graph using a type-safe lambda calculus. We want to specify this in a
way that we can write once and it will

1.  Be interpretable in a type-safe way within Haskell
2.  Be inspectable with visualizable control flow.
3.  Be compilable to multiple actual backends, letting you run the same function
    under multiple implementations.

This is all stuff I have been using in real life in my personal projects, where
I've needed to write a specification of an algorithm that I can simulate in
Haskell, generate graphical visualizations of, and also convert to multiple
(purescript, dhall, C dialects) to unify algorithms and formulas across
back-ends without writing them from scratch every time.

Once you go down this road, everything you ever write will feel woefully unsafe
and limited. And everything you will want to write will be woefully inscrutable
by normal humans and borderline unusable. But such is the curse we all bear.
Turn around now, you have been warned.

As Adam Neely asked in his [AI Music Video
Essay](https://www.youtube.com/watch?v=U8dcFhF0Dlk), if you had trained all of
AI on pre-jazz music, could AI have invented jazz? If you trained it on pre-80s
hip-hop, could it have invented 80s hip-hop and its technological breakthroughs?
If you trained AI on safe Haskell code, could it invent the monstrosity we are
about to explore in this post?

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

### A First Pass

Let's derive a way to express an algorithm or expression in Haskell that can be
reified and analyzed within Haskell, and eventually be a form we can compile to
different backends, interpret in Haskell, or generate Graphviz visualizations
in.

One basic thing we can do is start with:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L8-L22

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
  deriving (Eq, Show)
```

And you can write `(\x -> x * 3) 5` as:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L24-L25

fifteen :: Expr
fifteen = ELambda "x" (EOp OTimes (EVar "x") (EPrim (PInt 3))) `EApply` EPrim (PInt 5)
```

You can definitely easily render this in a graph, but what happens when you
write a Haskell interpreter? How can you "evaluate" this to 15, within Haskell?
What would the type even be? `eval :: Expr -> Maybe Prim`? Maybe just
`normalize :: Expr -> Expr` and hope that the result is `Prim`?

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage1.hs#L27-L54

normalize :: Map String Expr -> Expr -> Expr
normalize env = \case
  EPrim p -> EPrim p
  EVar v -> case M.lookup v env of
    Nothing -> error "Variable not defined"
    Just x -> normalize env x
  ELambda n x -> ELambda n x
  EApply f x -> case normalize env f of
    ELambda n u -> normalize (M.insert n (normalize env x) env) u
    _ -> error "Type error"
  EOp o x y -> case (normalize env x, normalize env y) of
    (EPrim x', EPrim y') -> case (x', y') of
      (PInt a, PInt b) -> case o of
        OPlus -> EPrim (PInt (a + b))
        OTimes -> EPrim (PInt (a * b))
        OLte -> EPrim (PBool (a <= b))
        OAnd -> error "Type error"
      (PBool a, PBool b) -> case o of
        OAnd -> EPrim (PBool (a && b))
        _ -> error "Type error"
      _ -> error "Type error"
    (x', y') -> EOp o x' y'
  ERecord xs -> ERecord (M.map (normalize env) xs)
  EAccess e k -> case normalize env e of
    ERecord xs -> case M.lookup k xs of
      Just v -> normalize env v
      Nothing -> error "Field not found"
    _ -> error "Type error"
```

This would properly evaluate:

``` haskell
ghci> normalize fifteen
EPrim (PInt 15)
```

Let's say this is 5% fancy. We used recursive types, used `Map` to look things
up efficiently.

But this isn't type-safe...we have undefined branches still. We could make the
entire thing monadic by returning `Maybe`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage2.hs#L27-L55

normalize :: Map String Expr -> Expr -> Maybe Expr
normalize env = \case
  EPrim p -> pure (EPrim p)
  EVar v -> M.lookup v env >>= normalize env
  ELambda n x -> pure (ELambda n x)
  EApply f x -> normalize env f >>= \case
    ELambda n u -> do
      x' <- normalize env x
      normalize (M.insert n x' env) u
    f' -> EApply f' <$> normalize env x
  EOp o x y -> do
    u <- normalize env x
    v <- normalize env y
    case (u, v) of
      (EPrim x', EPrim y') -> case (x', y') of
        (PInt a, PInt b) -> case o of
          OPlus -> pure (EPrim (PInt (a + b)))
          OTimes -> pure (EPrim (PInt (a * b)))
          OLte -> pure (EPrim (PBool (a <= b)))
          OAnd -> Nothing
        (PBool a, PBool b) -> case o of
          OAnd -> pure (EPrim (PBool (a && b)))
          _ -> Nothing
        _ -> Nothing
      (x', y') -> pure $ EOp o x' y'
  ERecord xs -> ERecord <$> traverse (normalize env) xs
  EAccess e k -> do
    ERecord xs <- normalize env e
    M.lookup k xs
```

This kind of works if you remember to thread everything through `Maybe` (or
`Either`) or what have you. But this is not ideal. You should be able to know,
at compile-time, that your `Expr` is valid. After all, you want to be able to
create one "valid" `Expr`, and run it at every context. It's utterly useless to
you if every single time you used an `Expr`, you had to manually handle the
`Nothing` case. Your diagram generator, your Haskell runner, your code
generator, will always be in `Either` even though you know your `Expr` is valid,
via tests or something.

This is maybe 10% fancy. We used `Maybe`/`Either` to prevent runtime exceptions,
but didn't actually get rid of any runtime *errors*.

No, this is not okay and unacceptable. We should be able to verify in the types
if an `Expr` is valid.

### First Layer of Types

The next step you'll see in posts online is to add a phantom index type to
`Expr`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L14-L48

type data Ty
  = TInt
  | TBool
  | TString
  | TRecord
  | Ty :-> Ty

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
  STRecord :: STy TRecord
  STFun :: STy a -> STy b -> STy (a :-> b)

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
  ERecord :: Map String SomeExpr -> Expr TRecord
  EAccess :: STy t -> Expr TRecord -> String -> Expr t
```

Here we use `-XTypeData` to define a data kind, `Ty` is a kind with types
`TInt :: Ty`, `TBool :: Ty`, etc.

So now `Expr a` evaluates to an `a`, which is either our domain's `Int`, our
domain's `Bool`, or our domain's `String`. At least, now, it is impossible to
create an `Expr` that doesn't type check:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L50-L54

fifteen :: Expr TInt
fifteen =
  EApply
    (ELambda STInt "x" (EOp OTimes (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))
```

We also need a
[singleton](https://blog.jle.im/entries/series/+introduction-to-singletons.html)
for our `Ty` type, `STy`...this makes a whole lot of things simpler. Usually
when you have a data kind, you can try to avoid singletons but a lot of times
you're just delaying the inevitable. In this case our lambda is "typed",
`ELambda STInt "x"`, so it binds a variable of type `Int` with name `x`.

Overall, I'll say this is about 50% fancy. Anything with GADTs will be a
significant bump. But you might see the problem here: `EVar STInt "x"`. `x`
might not be defined, and it also might not have the correct type. Soooo yes, we
still have issues here.

But now at least we can write `eval`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage3.hs#L56-L132

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Map String SomeValue -> EValue TRecord
  EVFun :: (EValue a -> Maybe (EValue b)) -> EValue (a :-> b)

data SomeValue where
  SomeValue :: STy t -> EValue t -> SomeValue

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
  STRecord -> \case STRecord -> Just Refl; _ -> Nothing
  STFun a b -> \case
    STFun c d -> do
      Refl <- sameTy a c
      Refl <- sameTy b d
      Just Refl
    _ -> Nothing

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
    EVRecord <$> traverse evalField xs
  EAccess t e k -> do
    EVRecord xs <- eval env e
    SomeValue t' v' <- M.lookup k xs
    Refl <- sameTy t t'
    pure v'
  where
    evalField (SomeExpr t v) = do
      v' <- eval env v
      pure (SomeValue t v')
```

What did we gain here? We have a type-safe `eval` now that will create a value
of the type we want. But we still have the same errors when looking at
variables: variables can still not be defined, or be defined as the wrong type.

So, again, we cannot create an `Expr` that must be sensible and well-formed to
compile. We still have to deal with *most* of the same errors. This is noble,
but clearly not good enough. We have to go deeper.

### Type-Safe Environments

In order to have `Var` be type-safe, the environment itself needs to be a part
of the `Expr` type, and you should only be able to use `Var` if the `Expr`
enforces it. `ELambda` would, therefore, introduce the new variable to the
environment.

We'll have:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L44-L59

data Expr :: [(Symbol, Ty)] -> Ty -> Type where

type (:::) l a = '(l, a)
```

So a value of type `Expr '["x" ::: TInt, "y" ::: TBool]` is an expression with
free variables `x` of type `Int` and a `y` of type `Bool`.

`ELambda` would therefore take a `Expr` with a free variable and turn it into an
`Expr` of a function type: (and `KnownSymbol` instance so that we can debug
print the variable name)

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L62-L66

  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
  EApply :: Expr vs (a :-> b) -> Expr vs a -> Expr vs b
  EOp :: Op a b c -> Expr vs a -> Expr vs b -> Expr vs c
  ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
  EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a
```

So how do we implement `Var`? We have to gate it on whether or not the free
variable is available in the environment. For that, we can use `Index`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L31-L33

data Index :: [k] -> k -> Type where
  IZ :: Index (x ': xs) x
  IS :: Index xs x -> Index (y ': xs) x
```

I have this in \[functor-products\]\[\], but it's also `CoRec Proxy` from
\[vinyl\]\[\] or `NP Proxy` from \[sop-core\]\[\]. You can reason about this
like axioms: `Index as a` means that `a` is an item in `as`, which is either `a`
being at the start of the list (`IZ`) or `a` being within the tail (`IS`).

\[functor-products\]\[\]: https://hackage.haskell.org/package/functor-products
\[vinyl\]: https://hackage.haskell.org/package/vinyl \[sop-core\]:
https://hackage.haskell.org/package/sop-core

For example, we have values `IZ :: Index '[a,b,c] a`,
`IS IZ :: Index '[a,b,c] b`, and `IS (IS IZ) :: Index '[a,b,c] c`. So, if we
require `Var` to take an `Index`, we require it to indicate something that *is*
inside the `Expr`'s free variable list and at that given index:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L61-L66

  EVar :: Index vs (n ::: t) -> Expr vs t
  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
  EApply :: Expr vs (a :-> b) -> Expr vs a -> Expr vs b
  EOp :: Op a b c -> Expr vs a -> Expr vs b -> Expr vs c
  ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
  EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a
```

So it is legal to have `EVar IZ :: Expr '["x" ::: TInt, "y" ::: TBool] TInt`,
and also it is automatically inferred to be a `TInt`. But we could *not* write
`EVar IZ :: Expr '[] TInt`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L59-L88

data Expr :: [(Symbol, Ty)] -> Ty -> Type where
  EPrim :: Prim t -> Expr vs t
  EVar :: Index vs (n ::: t) -> Expr vs t
  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
  EApply :: Expr vs (a :-> b) -> Expr vs a -> Expr vs b
  EOp :: Op a b c -> Expr vs a -> Expr vs b -> Expr vs c
  ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
  EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a

eLambda :: forall n -> KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
eLambda n x = ELambda @n x

fifteen :: Expr '[] TInt
fifteen =
  EApply
    (eLambda "x" (EOp OTimes (EVar IZ) (EPrim (PInt 3))))
    (EPrim (PInt 5))
```

In GHC 9.12 we can write `eLambda` using `RequiredTypeArguments` and so can pass
the type variable as a string literal, `eLambda "x"` but we can't yet put this
directly in `ELambda` for some reason.

Note that this is sometimes done using straight [De Bruijn
indices](https://en.wikipedia.org/wiki/De_Bruijn_index): `Expr :: [Ty] -> Type`,
so we don't use any names but just the direct index, but the point of this
exercise is to be *borderline* unbearable to write, and *not* to be *actually*
unbearable to write.

To actually write *eval* now, we need to have a type-safe environment to store
these variables, and for this we can use `Rec` (from \[vinyl\]\[\]) or `NP` from
\[sop-core\]:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/typed-sm-lc/ExprStage4.hs#L27-L118

data Rec :: (k -> Type) -> [k] -> Type where
  RNil :: Rec f '[]
  (:&) :: f x -> Rec f xs -> Rec f (x ': xs)

indexRec :: Index xs x -> Rec f xs -> f x
indexRec = \case
  IZ -> \(x :& _) -> x
  IS i -> \(_ :& xs) -> indexRec i xs

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
```

## The State Machine

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

