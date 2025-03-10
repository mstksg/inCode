Verify your Typeclass Instances in Haskell Today!

==================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on April 1, 2017.
> [Read online!](https://blog.jle.im/entry/verified-instances-in-haskell.html)

One of the most common gripes people have when learning Haskell is the fact that
typeclass "laws" are only laws by convention, and aren't enforced by the
language and compiler. When asked why, the typical response is "Haskell can't do
that", followed by a well-intentioned redirection to quickcheck or some other
fuzzing library.

But, to any experienced Haskeller, "Haskell's type system can't express X" is
always interpreted as a (personal) challenge.

GHC Haskell's type system has been advanced enough to provide verified
typeclasses for a long time, since the introduction of data kinds and associated
types. And with the
*[singletons](http://hackage.haskell.org/package/singletons)* library, it's now
as easy as ever.

(The code for this post is available
[here](https://github.com/mstksg/inCode/tree/master/code-samples/verified-instances/VerifiedInstances.hs)
if you want to follow along!)

## Semigroups

Let's start simple -- everyone's favorite structural addition to magmas,
[semigroups](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Semigroup.html).
A semigroup is a type with an associative binary operation, `(<>)`:

``` haskell
class Semigroup a where
    (<>) :: a -> a -> a
```

Its one law is associativity:

``` haskell
(x <> y) <> z = x <> (y <> z)
```

But, this class stinks, because it's super easy to write bad instances:

``` haskell
data List a = Nil | Cons a (List a)
    deriving Show

infixr 5 `Cons`

instance Semigroup (List a) where
    Nil       <> ys = ys
    Cons x xs <> ys = Cons x (ys <> xs)
```

This instance isn't associative:

``` haskell
ghci> ((1 `Cons` 2 `Cons` Nil) <> (3 `Cons` 4 `Cons` Nil)) <> (5 `Cons` 6 `Cons` Nil)
1 `Cons` 5 `Cons` 3 `Cons` 6 `Cons` 2 `Cons` 4 `Cons` Nil
ghci> (1 `Cons` 2 `Cons` Nil) <> ((3 `Cons` 4 `Cons` Nil) <> (5 `Cons` 6 `Cons` Nil))
1 `Cons` 3 `Cons` 2 `Cons` 5 `Cons` 4 `Cons` 6 `Cons` Nil
```

But if you try to compile it, GHC doesn't complain at all. Is this an error on
the part of Haskell? Not quite; it's an error on the part of the `Semigroup`
typeclass not requiring proofs that the instance is indeed associative.

Let's try again.

### Verify me, Captain

We will now define `Semigroup` on the *kind* `List`, using `-XDataKinds`,
instead of the type.

``` haskell
class Semigroup a where
    type (x :: a) <> (y :: a) :: a

    (%<>) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x <> y)

    appendAssoc
        :: Sing (x :: a)
        -> Sing (y :: a)
        -> Sing (z :: a)
        -> ((x <> y) <> z) :~: (x <> (y <> z))
```

Now, `<>` exists not as a function on *values*, but as a function on *types*.
`%<>` is a function that performs `<>` at the value level, written to work with
singletons representing the input types, so that GHC can verify that it is
identical to the type family `<>`. (it's 100% boilerplate and should pretty much
exactly match the `<>` type family).[^1] Finally, `appendAssoc` is a proof that
the type family `<>` is associative, using `:~:` (type equality witness) from
`Data.Type.Equality`.

This means that, if a type is an instance of `Semigroup`, it not only has to
provide `<>`/`%<>`, but also a *proof that they are associative*. You can't
write the full instance without it!

`Semigroup` is a "kind-class", because it is a bunch of methods and types
associated with a certain kind. Which `<>` is dispatched when you do something
like `x <> y` depends on the *kind* of `x` and `y`. GHC does "kind inference"
and uses the `<>` corresponding to the kinds of `x` and `y`.

Using the `SingKind` typeclass from the singletons library, we can move back and
forth from `Sing x` and `x`, and get our original (value-level) `<>` back:

``` haskell
(<>)
    :: (SingKind m, Semigroup m)
    => Demote m
    -> Demote m
    -> Demote m
x <> y = withSomeSing x $ \sX ->
           withSomeSing y $ \sY ->
             fromSing (sX %<> sY)
```

Now, let's write the instance for `List`. First, we need to define the
singletons:

``` haskell
data instance Sing (xs :: List a) where
    SNil  :: Sing Nil
    SCons :: Sing x -> Sing xs -> Sing (Cons x xs)
```

Then, we can define the instance, using the traditional `(++)` appending that
lists famously have:

``` haskell
instance Semigroup (List a) where
    type Nil       <> ys = ys
    type Cons x xs <> ys = Cons x (xs <> ys)

    SNil       %<> ys = ys
    SCons x xs %<> ys = SCons x (xs %<> ys)

    appendAssoc = \case
      SNil       -> \_ _ -> Refl
      SCons x xs -> \ys zs ->
        case appendAssoc xs ys zs of
          Refl -> Refl
```

Like I promised, `%<>` is a boilerplate re-implementation of `<>`, to manipulate
value-level witnesses. `appendAssoc` is the interesting bit: It's our proof. It
reads like this:

1.  If the first list is `Nil`:

    ``` haskell
    -- left hand side
    (Nil <> ys) <> zs
      = ys <> zs        -- definition of `(Nil <>)`
    -- right hand side
    Nil <> (ys <> zs)
      = ys <> zs        -- definition of `(Nil <>)`
    ```

    So, no work needed. QED! (Or, as we say in Haskell, `Refl`!)

2.  If the first list is `Cons x xs`:

    ``` haskell
    -- left hand side
    (Cons x xs <> ys) <> zs
      = (Cons x (xs <> ys)) <> zs   -- definition of `(Cons x xs <>)`
      = Cons x ((xs <> ys) <> zs)   -- definition of `(Cons x xs <>)`
    -- right hand side
    Cons x xs <> (ys <> zs)
      = Cons x (xs <> (ys <> zs))   -- definition of `(Cons x xs <>)`
    ```

    So, the problem reduces to proving that `(xs <> ys) <> zs` is equal to
    `xs <> (ys <> zs)`. If we can do that, then we can prove that the whole
    things are equal. We generate that proof using `appendAssoc xs ys zs`, and,
    wit that proof in scope...QED!

And, we're done!

Note that if you had tried any *non-associative* implementation of `<>` (and
`%<>`), GHC would reject it because you wouldn't have been able to write the
proof!

#### Automatic Singletons

Deriving `Sing` and `SingKind` and both versions of `<>` is kind of tedious, so
it's useful to use template haskell to do it all for us:

``` haskell
$(singletons [d|
  data List a = Nil | Cons a (List a)
      deriving (Show)

  infixr 5 `Cons`

  appendList :: List a -> List a -> List a
  appendList Nil         ys = ys
  appendList (Cons x xs) ys = Cons x (appendList xs ys)
  |])

instance Semigroup (List a) where
    type xs <> ys = AppendList xs ys
    (%<>) = sAppendList

    appendAssoc = \case
      SNil       -> \_ _ -> Refl
      SCons _ xs -> \ys zs ->
        case appendAssoc xs ys zs of
          Refl -> Refl
```

The boilerplate of re-defining `<>` as `%<>` goes away!

And now, we we can do:

``` haskell
ghci> print $ ((1::Integer) `Cons` 2 `Cons` Nil) <> (3 `Cons` 4 `Cons` Nil)
1 `Cons` 2 `Cons` 3 `Cons` 4 `Cons` Nil
```

Ta dah!

### Naturally, Maybe

Now that we have our basic infrastructure, let's implement some other famous
semigroups:

First, the inductive nats, `data N = Z | S N:`

``` haskell
$(singletons [d|
  data N = Z | S N
    deriving (Show)

  plus :: N -> N -> N
  plus Z     y = y
  plus (S x) y = S (plus x y)
  |])

instance Semigroup N where
    type xs <> ys = Plus xs ys
    (%<>) = sPlus

    appendAssoc = \case
      SZ -> \_ _ -> Refl
      SS x -> \y z ->
        case appendAssoc x y z of
          Refl -> Refl
```

And the standard instance for `Maybe`, which lifts the underlying semigroup:

``` haskell
$(singletons [d|
  data Option a = None | Some a
      deriving (Show)
  |])

instance Semigroup a => Semigroup (Option a) where
    type None   <> y      = y
    type x      <> None   = x
    type Some x <> Some y = Some (x <> y)

    SNone   %<> y       = y
    x       %<> SNone   = x
    SSome x %<> SSome y = SSome (x %<> y)

    appendAssoc = \case
        SNone   -> \_ _ -> Refl
        SSome x -> \case
          SNone -> \_ -> Refl
          SSome y -> \case
            SNone -> Refl
            SSome z ->
              case appendAssoc x y z of
                Refl -> Refl
```

``` haskell
ghci> print $ S (S Z) <> S Z
S (S (S Z))
ghci> print $ Some (S Z) <> Some (S (S (S Z)))
Some (S (S (S (S Z))))
ghci> print $ None       <> Some (S (S (S Z)))
Some (S (S (S Z)))
```

## Going Monoidal

Of course, we can now introduce the `Monoid` typeclass, which introduces a new
element `empty`, along with the laws that appending with empty leaves things
unchanged:

``` haskell
class Semigroup a => Monoid a where
    type Empty a :: a

    sEmpty :: Sing (Empty a)

    emptyIdentLeft
        :: Sing x
        -> (Empty a <> x) :~: x

    emptyIdentRight
        :: Sing x
        -> (x <> Empty a) :~: x

empty
    :: (SingKind m, Monoid m)
    => Demote m
empty = fromSing sEmpty
```

Because working implicitly return-type polymorphism at the type level can be
annoying sometimes, we have `Empty` take the *kind* `a` as a parameter, instead
of having it be inferred through kind inference like we did for `<>`. That is,
`Empty (List a)` is `Empty` for the *kind* `List a`.

As usual in Haskell, the instances write themselves!

``` haskell
instance Monoid (List a) where
    type Empty (List a) = Nil

    sEmpty = SNil
    emptyIdentLeft _ = Refl
    emptyIdentRight  = \case
      SNil -> Refl
      SCons _ xs ->
        case emptyIdentRight xs of
          Refl -> Refl

instance Monoid N where
    type Empty N = Z

    sEmpty = SZ
    emptyIdentLeft _ = Refl
    emptyIdentRight  = \case
      SZ -> Refl
      SS x -> case emptyIdentRight x of
        Refl -> Refl

instance Semigroup a => Monoid (Option a) where
    type Empty (Option a) = None

    sEmpty = SNone
    emptyIdentLeft  _ = Refl
    emptyIdentRight _ = Refl
```

## Play that Funcy Music

How about some higher-kinded typeclasses?

``` haskell
class Functor f where
    type Fmap a b (g :: a ~> b) (x :: f a) :: f b

    sFmap
        :: Sing (g            :: a ~> b)
        -> Sing (x            :: f a   )
        -> Sing (Fmap a b g x :: f b   )

    -- | fmap id x == x
    fmapId
        :: Sing (x :: f a)
        -> Fmap a a IdSym0 x :~: x

    -- | fmap f (fmap g x) = fmap (f . g) x
    fmapCompose
        :: Sing (g :: b ~> c)
        -> Sing (h :: a ~> b)
        -> Sing (x :: f a   )
        -> Fmap b c g (Fmap a b h x) :~: Fmap a c (((:.$) @@ g) @@ h) x
```

`Fmap a b g x` maps the *type-level function* `g :: a ~> b` over `x :: f a`, and
returns a type of kind `f b`. Like with `Empty`, to help with kind inference, we
have `Fmap` explicitly require the *kinds* of the input and results of `g` (`a`
and `b`) so GHC doesn't have to struggle to infer it implicitly.

And, of course, along with `sFmap` (the singleton mirror of `Fmap`), we have our
laws: `fmap id x = x`, and `fmap g (fmap h) x = fmap (g . h) x`.

But, what are `a ~> b`, `IdSym0`, `:.$`, and `@@`? They're a part of the
*defunctionalization* system that the singletons library uses. A `g :: a ~> b`
means that `g` represents a type-level function taking a type of kind `a` to a
type of kind `b`, but, importantly, encodes it in a way that makes Haskell
happy. This hack is required because you can't partially apply type families in
Haskell. If `g` was a regular old `a -> b` type family, you wouldn't be able to
pass just `g` into `Fmap a b g` (because it'd be partially applied, and type
families always have to appear fully saturated).

You can convert a `g :: a ~> b` back into a regular old `g :: a -> b` using
`Apply`, or its convenient infix synonym `@@`, like `g @@ (x :: a) :: b`

The singletons library provides `type family Id a where Id a = a`, but we can't
pass in `Id` directly into `Fmap`. We have to pass in its "defunctionalized"
encoding, `IdSym0 :: a ~> a`.

For the composition law, we use `(:.$)` (which is a defunctionalized type-level
`.`) and apply it to `g` and `h` to get, essentially, `g :. h`, where `:.` is
type-level function composition.

Now we Haskell.

``` haskell
$(singletons [d|
  mapOption :: (a -> b) -> Option a -> Option b
  mapOption _ None     = None
  mapOption f (Some x) = Some (f x)

  mapList :: (a -> b) -> List a -> List b
  mapList _ Nil         = Nil
  mapList f (Cons x xs) = Cons (f x) (mapList f xs)
  |])

instance Functor Option where
    type Fmap a b g x = MapOption g x

    sFmap = sMapOption
    fmapId = \case
      SNone   -> Refl
      SSome _ -> Refl

    fmapCompose _ _ = \case
      SNone   -> Refl
      SSome _ -> Refl

instance Functor List where
    type Fmap a b g x = MapList g x

    sFmap = sMapList
    fmapId = \case
      SNil       -> Refl
      SCons _ xs ->
        case fmapId xs of
          Refl -> Refl

    fmapCompose g h = \case
      SNil -> Refl
      SCons _ xs ->
        case fmapCompose g h xs of
          Refl -> Refl
```

And there you have it. A verified `Functor` typeclass, ensuring that all
instances are lawful. Never tell me that Haskell's type system can't do anything
ever again!

Note that any mistakes in implementation (like, for example, having
`mapOption _ _ = None`) will cause a compile-time error now, because the proofs
are impossible to provide.

As a side note, I'm not quite sure how to implement the value-level `fmap` from
this, since I can't figure out how to promote functions nicely. Using `sFmap` is
the only way to work with this at the value level that I can see, but it's
probably because of my own lack of understanding. If anyone knows how to do
this, please let me know!

Anyway, what an exciting journey and a wonderful conclusion. I hope you enjoyed
this and will begin using this in your normal day-to-day Haskell. Goodbye, until
next time!

## Just one more

Hah! Of course we aren't done. I wouldn't let you down like that. I know that
you probably saw that the entire last section's only purpose was to build up to
the pièce de résistance: the crown jewel of every Haskell article, the Monad.

``` haskell
class Functor f => Monad f where
    type Return a   (x :: a)                   :: f a
    type Bind   a b (m :: f a) (g :: a ~> f b) :: f b

    sReturn
        :: Sing (x :: a)
        -> Sing (Return a x :: f a)

    sBind
        :: Sing (m :: f a)
        -> Sing (g :: a ~> f b)
        -> Sing (Bind a b m g)

    -- | (return x >>= f) == f x
    returnIdentLeft
        :: Sing (x :: a)
        -> Sing (g :: a ~> f b)
        -> Bind a b (Return a x) g :~: (g @@ x)

    -- | (m >>= return) == m
    returnIdentRight
        :: Sing (m :: f a)
        -> Bind a a m ReturnSym0 :~: m

    -- | m >>= (\x -> f x >>= h) == (m >>= f) >>= h
    bindCompose
        :: Sing (m :: f a)
        -> Sing (g :: a ~> f b)
        -> Sing (h :: b ~> f c)
        -> Bind a c m (KCompSym2 a b c g h) :~: Bind b c (Bind a b m g) h

data ReturnSym0 :: a ~> f a
type instance Apply (ReturnSym0 :: a ~> f a) (x :: a) = Return a x

type KComp a b c (g :: a ~> f b) (h :: b ~> f c) (x :: a) = Bind b c (g @@ x) h
data KCompSym2 a b c g h :: (a ~> f c)
type instance Apply (KCompSym2 a b c g h :: a ~> f c) (x :: a) = KComp a b c g h x

return
    :: (SingKind a, SingKind (f a), Monad f)
    => Demote a
    -> Demote (f a)
return x = withSomeSing x $ \sX ->
             fromSing (sReturn sX)
```

To help with kind inference, again, we provide explicit kind arguments for
`Return` (the kind of the thing that is being lifted) and `Bind` (the original
`a` and the resulting `b`).

Some boilerplate exists there at the bottom --- it's the plumbing for the
defunctionalization system. `returnIdentRight` requires a defunctionalized
version of `Return`, so we can provide that by defining `ReturnSym0`, and
writing an `Apply` instance for it (which "applies" it the parameter `x`).

We introduce `KComp` (kleisli composition) and its defunctionalized version in
order to express the third law, because we don't yet have type-level lambdas in
Haskell. The actual function it is expressing is `\x -> f x >>= g`, and that
definition is given on the `type KComp a b c ... = Bind ...` line. `KCompSym2`
is the defunctioanlized version, which is not a `a -> f c` but rather an
`a ~> f c`, which allows it to be partially applied (like we do for
`composeBind`). And, finally, to hook all of this up into the
defunctionalization system, we write an `Apply` instance yet again.

And, again, if anyone knows how I can write a value-level `Bind`, I'd definitely
appreciate hearing!

Let's see some sample implementations.

``` haskell
$(singletons [d|
  bindOption :: Option a -> (a -> Option b) -> Option b
  bindOption None     _ = None
  bindOption (Some x) f = f x

  concatMapList :: (a -> List b) -> List a -> List b
  concatMapList _ Nil         = Nil
  concatMapList f (Cons x xs) = f x `appendList` concatMapList f xs
  |])

instance Monad Option where
    type Return a   x   = Some x
    type Bind   a b m g = BindOption m g

    sReturn = SSome
    sBind   = sBindOption

    returnIdentLeft _ _ = Refl
    returnIdentRight = \case
      SNone   -> Refl
      SSome x -> case sReturn x of
        SSome _ -> Refl
    bindCompose = \case
      SNone   -> \_ _ -> Refl
      SSome _ -> \_ _ -> Refl

instance Monad List where
    type Return a   x   = PureList x
    type Bind   a b m g = ConcatMapList g m

    sReturn   = sPureList
    sBind x f = sConcatMapList f x

    returnIdentLeft x g = case sReturn x of
      SCons y SNil -> case emptyIdentRight (unSingFun1 g y) of
        Refl -> Refl

    returnIdentRight = \case
      SNil       -> Refl
      SCons _ xs -> case returnIdentRight xs of
        Refl -> Refl

    bindCompose = \case
      SNil       -> \_ _ -> Refl
      SCons x xs -> \g h -> case bindCompose xs g h of
        Refl -> case unSingFun1 g x of
          SNil       -> Refl
          SCons y ys ->
            let gxs  = sConcatMapList g xs
                hgxs = sConcatMapList h gxs
                hy   = unSingFun1 h y
                hys  = sConcatMapList h ys
            in  case distribConcatMap h ys gxs of
                  Refl -> case appendAssoc hy hys hgxs of
                    Refl -> Refl

-- | Proving that concatMap distributes over <>
distribConcatMap
    :: Sing (g :: a ~> List b)
    -> Sing (xs :: List a)
    -> Sing (ys :: List a)
    -> ConcatMapList g (xs <> ys) :~: (ConcatMapList g xs <> ConcatMapList g ys)
distribConcatMap g = \case
    SNil -> \_ -> Refl
    SCons x xs -> \ys ->
      case distribConcatMap g xs ys of
        Refl ->
          let gx    = unSingFun1 g x
              cmgxs = sConcatMapList g xs
              cmgys = sConcatMapList g ys
          in  case appendAssoc gx cmgxs cmgys of
                Refl -> Refl
```

Here we use `unSingFun1`, which converts a singleton of a type-level function
into a value-level function on singletons:

``` haskell
unSingFun1
    :: Sing  (f      :: a ~> b)
    -> Sing  (x      :: a)
    -> Sing  (f @@ x :: b)
```

The crux is that, given a `Sing (f :: a ~> b)` and a `Sing (x :: a)`, we can
"apply" them to get `Sing (f @@ x :: b)`

The proofs for the list instance is admittedly ugly to write, due to the fact
that `List` is a recursive type. It's also tricky because Haskell has poor to
little support for theorem proving and no real tools to help you write them
efficiently. But, the proofs for `Option` are really something, aren't they?
It's kind of amazing how much GHC can do on its own without requiring any manual
proving on the part of the user.

## Disclaimer

Don't do this in actual code, please
([why?](https://twitter.com/mstk/status/848677244478279680)). This post started
off as an April Fools joke that accidentally compiled correctly for reasons
which I cannot explain.

While I don't recommend that you do this in actual code, but definitely do
recommend that you do it for fun! The code in this post is available
[here](https://github.com/mstksg/inCode/tree/master/code-samples/verified-instances/VerifiedInstances.hs)
if you want to play around!

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

[^1]: In full *singletons* style, this should actually be expressed in terms of
    the the *partially applied* (defunctionalized) `<>`. However, I'm giving the
    non-defunctionalized versions here for clarity.

