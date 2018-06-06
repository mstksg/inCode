---
title: Lenses embody Products, Prisms embody Sums
categories: Haskell
tags: lenses, profunctors
create-time: 2018/05/22 23:29:16
identifier: lenses-and-prisms
slug: lenses-products-prisms-sums
patrons: Sam Stites
---

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics.  A big part of
this is because there are already so many great resources on lenses.

This post won't be a "lens tutorial", but rather a dive into an insightful
perspective on lenses and prisms that I've heard repeated many times (and
always credited to Edward Kmett), but not yet all compiled into a single place.
In particular, I'm going to talk about the perspective of lenses and prisms as
embodying the essences of products and sums (respectively), and how that
observation can help you with a more "practical" understanding of lenses and
prisms.

An Algebraic Recap
------------------

In Haskell, "products and sums" can roughly be said to correspond to "tuples
and `Either`".  If I have two types `A` and `B`, `(A, B)` is their "product"
type.  It's often called an "anonymous product", because we can make one
without having to give it a fancy name.  It's called a product type because `A`
has $n$ possible values and `B` has $m$ possible values, then `(A, B)` has
$n \times m$ possible values[^bottom].  And, `Either A B` is their (anonymous)
"sum" type.  It's called a sum type because `Either A B` has $n + m$ possible
values.  I won't go much deeper into this, but there are [many useful summaries
already online][adts] on this topic!

[^bottom]: All of this is disregarding the notorious "bottom" value that
inhabits every type.

[adts]: https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

Let's Get Productive!
---------------------

It's easy to recognize `(Int, Double)` as a product between `Int` and
`Bool`.  However, did you know that some types are secretly product types in
disguise?

For example, here's a classic example of a data type often used with *lens*:

```haskell
data Person = P { _pName :: String
                , _pAge  :: Int
                }
```

`Person` is an algebraic data type --- so-called because it is actually a
*product* between a `String` and `Int`.  `Person` is *isomorphic* to `(String,
Int)`.  I will be writing this as `Person <~> (String, Int)`.

By *isomorphic*, I mean that there are functions `split :: Person -> (String,
Int)` and `unsplit :: (String, Int) -> Person` where `unsplit . split = id` and
`split . unsplit = id`.  You can think of this property as stating formally
that you should be able to go from one type to the other without "losing any
information".

In our case, we have:

```haskell
split :: Person -> (String, Int)
split (P n a) = (n, a)

unsplit :: (String, Int) -> Person
unsplit (n, a) = P n a
```

And we can verify that `unsplit . split` is `id`:

```haskell
unsplit . split :: Person -> Person
unsplit . split
    = \x          -> unsplit (split x)        -- substitute definition of (.)
    = \case P n a -> unsplit (split (P n a))  -- expand patterns
    = \case P n a -> unsplit (n, a)           -- substitute definition of split
    = \case P n a -> P n a                    -- substitute definition of unsplit
    = \x      -> x                            -- condense patterns
    = id                                      -- definition of id
```

And verification of `split . unsplit = id` is left as an exercise.

There are some other interesting products in Haskell, too.  One such example is
`NonEmpty a` being a product between `a` (the head/first item) and `[a]` (the
tail/rest of the items).  This means that `NonEmpty a` is isomorphic to `(a,
[a])` --- we have `NonEmpty a <~> (a, [a])`!

Another curious product is the fact that every type `a` is a product between
*itself* and unit, `()`.  That is, every type `a` is isomorphic to `(a, ())`
(which follows from the algebraic property $x * 1 = x$).  Freaky, right?

```haskell
-- a <~> (a, ())

split :: a -> (a, ())
split x = (x, ())

unsplit :: (a, ()) -> a
unsplit (x, _) = x
```

One final interesting "product in disguise" is `Either a a`.  "But wait," you
say.  "That's a sum...right??"

Well, yeah.  But in addition, any `Either a a` is the product between `Bool`
and `a`.  That is, `Either a a` is isomorphic to `(Bool, a)`.  The `Bool` tells
you "left or right?" and the `a` is the contents!

```haskell
-- Either a a <~> (Bool, a)

split :: Either a a -> (Bool, a)
split (Left  x) = (False, x)
split (Right x) = (True , x)

unsplit :: (Bool, a) -> Either a a
unsplit (False, x) = Left  x
unsplit (True , x) = Right x
```

Proving that `unsplit . split = id`:

```haskell
unsplit . split :: Either a a -> Either a a
unsplit . split =
    = \x            -> unsplit (split x)          -- substitute definition of (.)
      -- trying case 1
    = \case Left  y -> unsplit (split (Left  y))  -- expand pattern for case 1
    = \case Left  y -> unsplit (False, y)         -- substitute definition of split
    = \case Left  y -> Left  y                    -- substitute definition of unsplit
    = \x            -> x                          -- condense pattern for case 1
    = id                                          -- definition of id
      -- trying case 2
    = \case Right y -> unsplit (split (Right y))  -- expand pattern for case 2
    = \case Right y -> unsplit (True , y)         -- substitute definition of split
    = \case Right y -> Right y                    -- substitute definition of unsplit
    = \x            -> x                          -- condense pattern for case 2
    = id                                          -- definition of id
```

And `split . unsplit = id` is again left as an exercise.

(`\case` here is from the *-XLambdaCase* extension)

### Lenses

So, how do lenses come into the picture?

Let's review a bit.  A `Lens' s a` is a way to "access" an `a` "inside" an `s`,
respecting some laws.

A `Lens' s a` is a data type with the following API:

```haskell
view :: Lens' s a -> (s -> a)                -- get the 'a' from an 's'
set  :: Lens' s a -> (a -> s -> s)           -- set the 'a' inside an 's'
```

respecting [some laws][lenslaws] --- get-put, put-get, and put-put.  Abstract
mathematical laws are great and all, but I'm going to tell you a secret that
gives a nice restatement of those laws.

[lenslaws]: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#the-lens-laws-

At first, you might naively implement lenses like:

```haskell
data Lens' s a = Lens' { view :: s -> a
                       , set  :: a -> s -> s
                       }
```

But this is bad bad bad.  That's because you can use this to represent lenses
that "break the laws".  This representation is, to use the technical term, "too
big".  It allows more more values than are actual lenses.

So, here's the secret: A `Lens' s a` means that *`s` is a product between
`a` and some type `q`*.

That means that if it is possible to represent `s` as some `(a, q)` (that is,
`s <~> (a, q)`), *then you have two lenses*!  Lenses are nothing more than
*descriptions of products*!  Another way to think of this is that if you are
able to "split" a type into two parts without losing any information, then each
part represents a lens.

A `Lens' s a` is nothing more than a witness for the fact that there is some
`q` where `s <~> (a, q)`.

With that in mind, let's re-visit a saner definition of lenses based on the
idea that lenses embody descriptions of products:

```haskell
data Lens' s a = forall q.
                 Lens' { split   :: s -> (a, q)
                       , unsplit :: (a, q) -> s
                       }    -- ^ s <~> (a, q)
```

(the `forall q.` is the *-XExistentialQuantification* extension, and allows us
to hide type variables in consructors)

Now, if `split` and `join` form an isomorphism, *this can only represent valid
lenses*![^big]

[^big]: This type is technically also "too big" (you can write a value where `split`
and `unsplit` do not form an isomorphism), but I think, to me, "`split` and
`join` must form an isomorphism" is a much clearer and natural law than
get-put/put-get/put-put.

We can implement our necessary lens API as so:

```haskell
view :: Lens' s a -> (s -> a)
view Lens'{..} = fst . split

set :: Lens' s a -> (a -> s -> s)
set Lens'{..} newVal x = case split x of
    (_, q) -> unsplit (newVal, q)      -- "replace" the `a`
```

(Using the *-XRecordWildcards* extension, where `Lens'{..}` binds `split` and
`unsplit` to the fields of the lens)

The implementation of the helper function `over` (which modifies the `a` with a
function) is also particularly elegant:

```haskell
over :: Lens' s a -> (a -> a) -> (s -> s)
over Lens'{..} f = unsplit . first f . split
```

The surprising result of this perspective is that **every product yields
lenses** (one for every item in the product), and **every lens witnesses one
side of a product**.

### Insights Gleamed

Let's take a look at our first product we talked about:

```haskell
data Person = P { _pName :: String
                , _pAge  :: Int
                }

split :: Person -> (String, Int)
split (P n a) = (n, a)

unsplit :: (String, Int) -> Person
unsplit (n, a) = P n a
```

Because `Person` is a product between `String` and `Int`, we get *two lenses*:
a `Lens' Person String` and `Lens' Person Int`.  *Every product* gives us a
lens for every item in the product.

```haskell
-- Person <~> (String, Int)

pName :: Lens' Person String
pName = Lens' { split   = \(P n a) -> (n, a)
              , unsplit = \(n, a)  -> P n a
              }

pAge :: Lens' Person Int
pAge = Lens' { split   = \(P n a) -> (a, n)
             , unsplit = \(a, n)  -> P n a
             }
```

These are actually the typical lenses associated with records!  You get exactly
these lenses if you use `makeLenses` from the *lens* package.

The inverse is true too.  **Every lens witnesses a product**.  The fact that we
have a lawful `pName :: Lens' Person String` means that a `Person` *must* be a
product between `String` and some other (hidden) type.

It can be insightful to look at products that we know and see what lenses those
correspond to.

For example, our `NonEmpty a <~> (a, [a])` product tells us that `NonEmpty a`
has at least two lenses: a "head" lens `Lens' (NonEmpty a) a` and a "tail" lens
`Lens' (NonEmpty a) [a]`.

Our `a <~> (a, ())` product gives some interesting insight.  This tells us that
we always have an "identity" lens `Lens' a a`, and a "unit" lens `Lens' a ()`,
for any `a`:

```haskell
-- a <~> (a, ())

identity :: Lens' a a
identity = Lens' { split   = \x      -> (x, ())
                 , unsplit = \(x, _) -> x
                 }

united :: Lens' a ()
united = Lens' { split   = \x       -> ((), x)
               , unsplit = \((), x) -> x
               }
```

In the language of lens, `identity :: Lens' a a` tells us that all `a`s have an
`a` "inside" them.  However, in the language of products, this just tells us
that `a` can be represented as `(a, ())`.  In the language of lens, `united ::
Lens' a ()` tells us that all `a`s have a `()` "inside" them.  In the language
of products, this just tells us that `a <~> (a, ())`.

What insight does our `Either a a <~> (Bool, a)` product perspective give us?
Well, let's write out their types and see what it might suggest:

```haskell
mysteryLens1 :: Lens' (Either a a) Bool
mysteryLens2 :: Lens' (Either a a) a
```

Looking at `mysteryLens1 :: Lens' (Either a a) Bool`, we are saying that every
`Either a a` has some `Bool` "inside" it.  From our knowledge of our
product, we know that this `Bool` is really a *flag* for left-ness or
right-ness.  Getting the `Bool` is finding out if we're in `Left` or `Right`,
and flipping the `Bool` "inside" is really just swapping from `Left` to
`Right`.

```haskell
flipEither :: Either a a -> Either a a
flipEither = over mysteryLens1 not

isLeft :: Either a a -> Bool
isLeft = view mysteryLens1
```

```haskell
ghci> flipEither (Left 'a')
Right 'a'
ghci> flipEither (Right 'a')
Left 'a'
ghci> isLeft (Left 'a')
True
ghci> isLeft (Right 'a')
False
```

Note that if we look at lenses as embodying "record fields" (things that give
you the ability to "get" a field, and "modify" a field --- corresponding with
`view` and `set`), we can think of `mysteryLens1` as an *abstract record field*
into the Leftness/Rightness of a value.  Thing of lenses as defining abstract
record fields is a [common tool for backwards compatiblity][lens-records].

[lens-records]: http://blog.ezyang.com/2016/12/a-tale-of-backwards-compatibility-in-asts/


Looking at `mysteryLens2 :: Lens' (Either a a) a`, we are saying that every
`Either a a` has some `a` "inside" it.  From what we know about the underlying
product, the `a` is just the "contained value", *ignoring* leftness or
rightness.  Getting the `a` is getting the contained value and losing
leftness/rightness, and re-setting the `a` inside is modifying the contained
value but preserving leftness/rightness.

```haskell
fromEither :: Either a a -> a
fromEither = view mysteryLens2

mapEither :: (a -> a) -> Either a a -> Either a a
mapEither = over mysteryLens2
```

```haskell
ghci> fromEither (Left 'a')
'a'
ghci> mapEither negate (Right 4)
Right (-4)
```

So that's really the essence of what a `Lens'` is.  A `Lens' s a` is the
embodiment of the fact that `s` can be represented as a product between `a` and
something else --- that `s <~> (a, q)`.  All of the lens laws just boil down to
this.  **Lenses embody products**.

### What Isn't a Lens?

This perspective also gives you some insight into when things *aren't* lenses.
For example, is it possible to make a lens that gives you the first item in a
list?

No, there isn't, because there isn't any type `q` that you could factor out
`[a]` into as `(a, q)`.  That would imply that `[a]` is always "an `a` with
something else"...but this isn't true with `[]`.

\"Sum-thing\" Interesting
-------------------------

It's easy to recognize `Either Int Bool` as a sum between `Int` and `Bool`.
However, did you know that some types are secretly sums in disguise?

For example, here's a data type you might encounter out there in the real
world:

```haskell
data Shape = Circle  Double           -- radius
           | RegPoly Natural Double   -- number of sides, length of sides
```

`Circle 2.9` represents a circle with radius 2.9, and `RegPoly 8 4.6`
represents a octagon (8-sided figure) whose sides all have length 4.6.

`Shape` is an algebraic data type --- so-called because it is actually a *sum*
between `Double` and `(Natural, Double)` (a `Natural` is the non-negative
`Integer` type).  `Shape` is *isomorphic* to `Either Double (Natural, Double)`.
To prove it, let's witness `Shape <~> Either Double (Natural, Double)` using
the functions `match` and `inject`:

```haskell
-- Shape <~> Either Double (Natural, Double)

match :: Shape -> Either Double (Natural, Double)
match (Circle  r  ) = Left r
match (RegPoly n s) = Right (n, s)

inject :: Either Double (Natural, Double) -> Shape
inject (Left   r    ) = Circle  r
inject (Right (n, s)) = RegPoly n s
```

Since `inject . match = id` and `match . inject = id`, this proves that `Shape`
is a sum in disguise.

Another interesting "hidden sum" is the fact that `[a]` in Haskell is actually
a sum between `()` and `(a, [a])`.  That's right --- it's a sum between `()`
and...itself?  Indeed it is pretty bizarre.

However, if we think of `()` as the possibility of an empty list, and `(a, [a])`
as the possibility of `NonEmpty a` (the "head" of a list consed with the rest of
the list), then saying that `[a]` is a sum between `()` and `NonEmpty a` is
saying that `[a]` is "either an empty list or a non-empty list".  Whoa.  Take
*that*, [LEM denialists][lem-denialists].[^lem]

[lem-denialists]: https://en.wikipedia.org/wiki/Constructivism_(mathematics)

[^lem]: Technically, LEM denialists and constructivists are somewhat vindicated
here, because it is not strictly true in Haskell that a list is either an empty
list or a non-empty list.  It can actually [be neither][bottom].

[bottom]: https://wiki.haskell.org/Bottom

```haskell
-- [a] <~> Either () (NonEmpty a)

match :: [a] -> Either () (NonEmpty a)
match []     = Left  ()
match (x:xs) = Right (x :| xs)

inject :: Either () (NonEmpty a) -> [a]
inject (Left   _       ) = []
inject (Right (x :| xs)) = x:xs
```

And, actually, there is another way to deconstruct `[a]` as a sum in Haskell.
You can treat it as a sum between `()` and `([a], a)` --- where the `()`
represents the empty list and the `([a], a)` represents an "all but the last
item" list and "the last item":

```haskell
-- [a] <~> Either () ([a], a)

match  :: [a] -> Either () ([a], a)
match xs
  | null xs   = Left  ()
  | otherwise = Right (init xs, last xs)

-- init gives you all but the last item:
-- > init [1,2,3] = [1,2]

inject :: Either () (a, [a]) -> [a]
inject (Left   _     ) = []
inject (Right (xs, x)) = xs ++ [x]
```

I just think it's interesting that the same type can be "decomposed" into a sum
of two different types in multiple ways.

Another curious sum: if we consider the "empty data type" `Void`, the type with
no inhabitants:

```haskell
data Void           -- no constructors, no valid inhabitants
```

then we have a curious sum: every type `a` is a sum between *itself* and
`Void`.  In other words, `a` is isomorphic to `Either a Void` (which follows
from the algebraic property $x + 0 = x$):

```haskell
-- a <~> Either a Void

-- | A useful helper function when working with `Void`
absurd :: Void -> a
absurd = \case -- empty case statement because we have
               -- no constructors of 'Void' we need to
               -- match on

match :: a -> Either a Void
match x = Left x

inject :: Either a Void -> a
inject (Left  x) = x
inject (Right v) = absurd v
```

Again, if you don't believe me, verify that `inject . match = id` and `match .
inject = id`!

One final example: earlier, we said that every type can be decomposed as a
*product* involving `()`.  Algebraically, finding that mystery type is easy ---
we solve $x = 1 * y$ for $y$ (since `()` is 1), and we see $y = x$.  This tells
us that every type is a product between `()` and itself (`a <~> ((), a)`).

However, can every type be decomposed as a *sum* involving `()`?

Algebraically, we need to find this mystery type by solving $x = 1 + y$ for
$y$, and the result is $y = x - 1$.  We can interpret $x - 1$ as "`a`, minus
one potential element".

This type isn't expressible in general in Haskell, so *no*, not *every* type
can be decomposed as a sum involving `()`.  The necessary and sufficient
condition is that there must exist some type that is the same as your original
type but with one missing element.

Oh, hey!  Remember our `[a] <~> Either () (NonEmpty a)` decomposition?  That's
exactly this!  We can `NonEmpty a` is our mystery type: it's exactly a list
`[a]` minus one potential element (the empty list).

There's another way to go about this: we can talk about $x - 1$ by specifying
one single "forbidden element".  This isn't explicitly possible in Haskell, but
we can simulate this by using an abstract type.  We have this ability using
"refinement types".  For example, using the [refined][] library, a `Refined
(NotEqualTo 4) Int` is a type that is the same as `Int`, except the `4` value
is forbidden.

[refined]: http://hackage.haskell.org/package/refined

We can use it to implement a `Int <~> Either () (Refined (NotEqualTo 4) Int)`
witness:

```haskell
-- | Like `Int`, but cannot be constructed if it is 4
type Not4 = Refined (NotEqualTo 4) Int

-- | Provided by the 'refined' library that lets us refine and unrefine a type
refineFail :: Int  -> Maybe Not4
unrefine   :: Not4 -> Int


-- | The "safe constructor"
match :: Int -> Either () Not4
match n = case refineFail n of
    Nothing -> Left ()          -- the value was 4, so we return `Left`
    Just x  -> Right x          -- value was succesfully refined

-- | The "safe extractor"
inject :: Either () Not4 -> Int
inject (Left  _) = 4
inject (Right x) = unrefine x
```

In fact, if we can parameterize an isomorphism on a specific value, *all* types
with at least one value can be expressed as a sum involving `()`!  It's always
`()` plus the type itself minus that given specific value.

### Through the Looking-Prism

Now let's bring prisms into the picture.  A `Prism' s a` also refers to some
`a` "inside" an `s`, with the following API: `preview` and `review`[^invent]

[^invent]: I didn't invent these names

```haskell
preview :: Prism' s a -> (s -> Maybe a)   -- get the 'a' in the 's' if it exists
review  :: Prism' s a -> (a -> s)         -- reconstruct the 's' from an 'a'
```

Naively you might implement a prism like this:

```haskell
data Prism' s a = Prism' { preview :: s -> Maybe a
                         , review  :: a -> s
                         }
```

But, again, this implementation space is too big.  There are way too many
values of this type that aren't *actual* "lawful" prisms.  And the laws are
kind of muddled here.

You might be able to guess where I'm going at this point.  Whereas a `Lens' s
a` is nothing more than a witness to the fact that `s` is a *product* `(a,
q)` ... a `Prism' s a` is nothing more than a witness to the fact that `s` is a
*sum* `Either a q`.  If it is possible to represent `s` as some `Either a
q`...then you have two prisms!  Prisms are nothing more than *descriptions of
sums*!

A `Prism' s a` is nothing more than a witness for an `exists q. s <~> Either a
q` isomorphism.

Under this interpretation, we can write a nice representation of `Prism'`:

```haskell
data Prism' s a = forall q.
                  Prism' { match  :: s -> Either a q
                         , inject :: Either a q -> s
                         }    -- ^ s <~> Either a q
```

Now, if `match` and `inject` form an isomorphism, *this can only represent
valid prisms*!

We can now implement the prism API:

```haskell
preview :: Prism' s a -> (s -> Maybe a)
preview Prism'{..} x = case match x of
    Left _  -> Nothing
    Right y -> Just y

review  :: Prism' s a -> (a -> s)
review Prism'{..} = inject . Left
```

Like for lenses, prisms also admit a particularly elegant formulation for
`over`, which maps a function over the `a` in the `s` if it exists:

```haskell
over :: Lens' s a  -> (a -> a) -> (s -> s)
over Lens'{..}  f = inject . first f . match    -- instance Bifunctor (,)

over :: Prism' s a -> (a -> a) -> (s -> s)
over Prism'{..} f = inject . first f . match    -- instance Bifunctor Either
```

Neat, they're actually exactly identical!  Who would have thought?

So we see now, similar to lenses, **every sum yields prisms**, and **every
prism witnesses one side of a sum**.

### Prism Wisd'm

Let's go back at our example prisms and see what sort of insight we can gain
from this perspective.

```haskell
data Shape = Circle  Double
           | RegPoly Natural Double

match :: Shape -> Either Double (Natural, Double)
match (Circle  r  ) = Left r
match (RegPoly n s) = Right (n, s)

inject :: Either Double (Natural, Double) -> Shape
inject (Left   r    ) = Circle  r
inject (Right (n, s)) = RegPoly n s
```

Because `Shape` is a sum between `Double` and `(Natural, Double)`, we get *two
prisms*:

```haskell
-- Shape <~> Either Natural (Natural, Double)

_Circle :: Prism' Shape Natural
_Circle = Prism' { match  = \case Circle  r    -> Left r
                                  RegPoly n s  -> Right (n, s)
                 , inject = \case Left   r     -> Circle r
                                  Right (n, s) -> RegPoly n s
                 }

_RegPoly :: Prism' Shape (Natural, Double)
_RegPoly = Prism' { match  = \case Circle  r    -> Right r
                                   RegPoly n s  -> Left (n, s)
                  , inject = \case Left  (n, s) -> RegPoly n s
                                   Right  r     -> Circle r
                  }
```

And these are actually the typical prisms associated with an ADT.  You actually
get exactly these if you use `makePrisms` from the *lens* package.

What can we get out of our decomposition of `[a]` as a sum between `()` and
`NonEmpty a`?  Let's look at them:

```haskell
-- [a] <~> Either () (NonEmpty a)

_Nil :: Prism' [a] ()
_Nil = Prism'
    { match  = \case []              -> Left ()
                     x:xs            -> Right (x :| xs)
    , inject = \case Left _          -> []
                     Right (x :| xs) -> x:xs
    }

_Cons :: Prism' [a] (a, [a])
_Cons = Prism'
    { match  = \case []              -> Right ()
                     x:xs            -> Left (x :| xs)
    , inject = \case Left  (x :| xs) -> x:xs
                     Right _         -> []
    }
```

We see a sort of pattern here.  And, if we look deeper, we will see that *all
prisms* correspond to some sort of "constructor".

After all, what do constructors give you?  Two things:

1.  The ability to "create" a value.  This corresponds to `review`, or `inject`
2.  The ability to do "case-analysis" or check if a value was created using
    that constructor.  This corresponds to `preview`, or `match`.

The API of a "constructor" is pretty much exactly the Prism API.  In fact, we
often use Prisms to simulate "abstract" constructors.

An *abstract constructor* is exactly what our *other* `[a]` sum decomposition
gives us!  If we look at that isomorphism `[a] <~> Either () ([a], a)` (the
"tail-and-last" breakdown) and write out the prisms, we see that they
correspond to the abstract constructors `_Nil` and `_Snoc`:

```haskell
-- [a] <~> Either () ([a], a)

_Nil :: Prism' [a] ()
_Nil = Prism' { match  = \xs -> if null xs
                                  then Left  ()
                                  else Right (init xs, last xs)
              , inject = \case Left _        -> []
                               Right (xs, x) -> xs ++ [x]
              }

_Snoc :: Prism' [a] ([a], a)
_Snoc = Prism' { match  = \xs -> if null xs
                                   then Right ()
                                   else Left  (init xs, last xs)
               , inject = \case Left  (xs, x) -> xs ++ [x]
                                Right _       -> []
               }
```

`_Snoc` is an "abstract constructor" for a list that lets us:

1.  "Construct" an `[a]` given an original list `[a]` and an item to add to the
    end, `a`
2.  "Deconstruct" an `[a]` into an initial run `[a]` and its last element `a`
    (as a pattern match that might "fail").

And, looking at `a <~> Either a Void`...what does that decomposition give us,
conceptually?

```haskell
-- a <~> Either a Void

identity :: Prism' a a
identity = Prism' { match = Left
                  , inject = \case
                      Left  x -> x
                      Right v -> absurd v
                  }


_Void :: Prism' a Void
_Void = Prism' { match = Right
               , inject = \case
                   Left  v -> absurd v
                   Right x -> x
               }
```

In lens-speak, `identity :: Prism' a a` tells us that all `a`s have an `a`
"inside" them (since `match` always matches) and that you can construct an `a`
with only an `a` (whoa).  In our "sum" perspective, however, it just witnesses
that an `a <~> Either a Void` sum.

In lens-speak, `_Void :: Prism' a Void` tells us that you can pattern match a
`Void` out of any `a`...but that that pattern match will never match.
Furthermore, it tells us that if you have a value of type `Void`, you can use
the `_Void` "constructor" to make a value of any type `a`!  We have `review
_Void :: Void -> a`!

However, in our "sum" perspective, it is nothing more than the witness of the
fact that `a` is the sum of `a` and `Void`.

And finally, let's look at our deconstruction of `Int` and `Reinfed (NotEqualTo
4) Int`.  What prisms does this yield, and what insight do we get?

```haskell
only4 :: Prism' Int ()
only4 = Prism'
    { match  = \n -> case refineFail n of
                       Nothing -> Left ()
                       Just x  -> Right x
    , inject = \case Left  _ -> 4
                     Right x -> unrefine x
    }

refined4 :: Prism' Int Not4
refined4 = Prism'
    { match  = \n -> case refineFail n of
                       Nothing -> Right ()
                       Just x  -> Left x
    , inject = \case Left  x -> unrefine x
                     Right _ -> 4
    }
```

The first prism, `only4`, is a prism that basically "only matches" on the `Int`
if it is `4`.  We can use it to implement "is equal to four", and "get a 4"

```haskell
-- | Checks if a value is 4
isEqualTo4 :: Int -> Bool
isEqualTo4 = isJust . preview only4

-- | Just is '4'
four :: Int
four = review ()
```

The name `only4` is inspired by the `only` combinator from the *lens* library,
which lets you provide a value you want to "restrict".

```haskell
-- | From the lens library; lets you provide the value you want to "restrict"
only :: Eq a => a -> Prism' a ()

only4 :: Prism' Int ()
only4 = only 4
```

The second prism, `refined4`, basically acts like a "abstract (smart)
constructor" for `Not4`, essentially `refineFail` and `unrefine`:

```haskell
makeNot4 :: Int -> Maybe Not4
makeNot4 = preview refined4

fromNot4 :: Not4 -> Int
fromNot4 = review refined4
```

### Prism or Not

To me, again, one of the most useful things about this prism perspective is
that it helps me see what *isn't* a prism.

For example, is it possible to have a prism into the *head* of a list?  That
is, is the following prism possible?

```haskell
_head :: Prism' [a] a           -- get the head of a list
```

If you think of a prism as just "a lens that might fail" (as it's often
taught), you might think yes.  If you think of a prism as just "a constructor
and deconstructor", you might also think yes, since you can construct an `[a]`
with only a single `a`.  You can definitely "implement" this prism using our
"too big" representation, in terms of `preview` and `review`.  Both of these
viewpoints of prisms will fail you and lead you astray.

[^headconst]: Although, upon further inspection, you might realize that
the constructor and deconstructor don't match

However, if you think of it as witnessing a sum, you might see that this prism
isn't possible.  There is no possible type `q` where `[a]` is a sum of `a` and
`q`.  The isomorphism `[a] <~> Either a q` cannot be made for *any* type `q`.
There is no way to express `[a]` as the sum of `a` and some other type.  Try
thinking of a type `q` --- it's just not possible!

The Path to Profunctors
-----------------------

As a finale, I'd like to show how these dual perspectives (lenses are witneses
to products and prisms are witnesses to sum) make their implementation in terms
of "profunctor optics" very natural.

First, some background -- a "profunctor optic" is a way of expressing things
like lenses and prisms in terms of "profunctor transformers".  Lenses, prisms,
etc. would not be record types, but rather functions that takes a profunctor
and return a new profunctor.

A profunctor `p` has values of type `p a b`, and you can roughly think of `p a
b` as "a relationship between `a` and `b`".

The `Profunctor` typeclass `p` gives us a function called `iso`, that lets us
transform a profunctor in terms of an isomorphism.

If type `s` is isomorphic to type `a` (`s <~> a`), then we can the function
`iso`, that the `Profunctor` class gives us:

```haskell
-- s <~> a

-- | The real `iso` is actually a little more polymorphic
class Profunctor p where
    iso :: Profunctor p
        => (s -> a)         -- ^ one half of the isomorphism
        -> (a -> s)         -- ^ the other half of the isomorphism
        -> p a a
        -> p s s
```

Given the `s -> a` and `a -> s` functions that witness `s <~> a`, the
`Profunctor` typeclass lets us transform a `p a a` into a `p s s` (a
relationship on `a`s to be a relationship on `s`).

(The `Profunctor` typeclass actually just provides `dimap`, but I am using the
common alias `iso = dimap` to enforce the connection)

### Profunctor Lens

A profunctor lens (one way of implementing) `Lens' s a` is a function:

```haskell
p a a -> p s s
```

You can think of it as taking a "relationship on `a`s" and turning it
into a "relationship on `s`s".

With a lens, we are saying that `s` is isomorphic to `(a, q)`.  That means that
we have, at our disposal:

```haskell
iso split unsplit
    :: Profunctor p
    => p (a, q) (a, q) -> p s s
```

In order to get a `p a a -> p s s`, we need a way to turn a `p a a` into a `p
(a, q) (a, q)`.  This says "take a relationship on `a`s and turn it into a
relationship on `(a, q)`, *ignoring* the `q`".

The typeclass `Strong` gives us just that!

```haskell
-- | The real `first'` is actually a little more polymorphic
class Profunctor p => Strong p where
    first'
        :: p a a                -- ^ relationship on part
        -> p (a, q) (a, q)      -- ^ relationship on whole
```

And so we now have a definition of a profunctor lens:

```haskell
makeLens
    :: Strong p
    => (s -> (a, q))        -- ^ split
    -> ((a, q) -> s)        -- ^ unsplit
    -> p a a                -- ^ relationship on a
    -> p s s                -- ^ relationship on s
makeLens split unsplit = iso split unsplit  -- ^ p (a, q) (a, q) -> p s s
                       . first'             -- ^ p a a -> p (a, q) (a, q)
```

Essentially, `iso split unsplit . first'` promotes a `p a a` to a `p s s`.  It
uses `first'` to turn the `p a a` into a `p (a, q) (a, q)`, turning a
relationship on the part to be a relationship on the whole.  Then we just apply
the essential `s <~> (a, q)` isomorphism that defines a lens.  And so `p a a ->
p s s`, going through the `s <~> (a, q)` isomorphism, is a lens!

### Profunctor Prisms

A profunctor prism (one way of implementing) `Prism' s a` is a function:

```haskell
p a a -> p s s
```

You can also think of this as taking a "relationship on `a`s" and turning it
into a "relationship on `s`s".

With a prism, we are saying that `s` is isomorphic to `Either a q`.  That means
that we have, at our disposal:

```haskell
iso match inject
    :: Profunctor p
    => p (Either a q) (Either a q)
    -> p s s
```

In order to get a `p a a -> p s s`, we need a way to turn a `p a a` into a `p
(Either a q) (Either a q)`.  This says "take a relationship on `a`s and turn it
into a relationship on `Either a q`, *ignoring* the `q`".

The typeclass `Choice` gives us just that:

```haskell
-- | The real `left'` is actually a little more polymorphic
class Profunctor p => Choice p where
    left'
        :: p a a                        -- ^ relationship on branch
        -> p (Either a q) (Either a q)  -- ^ relationship on all possibilities
```

And so we now have a definition of a profunctor prism:

```haskell
makePrism
    :: Choice p
    => (s -> (a, q))        -- ^ match
    -> ((a, q) -> s)        -- ^ inject
    -> p a a                -- ^ relationship on a
    -> p s s                -- ^ relationship on s
makePrism match inject =
    iso match inject   -- ^ p (Either a q) (Either a q) -> p s s
  . left'              -- ^ p a a -> p (Either a q) (Either a q)
```

Essentially, `iso match inject . left'` promotes a `p a a` to a `p s s`. It
uses `left'` to turn the `p a a` into a `p (Either a q) (Either a q)`, turning
a relationship on the part to be a relationship on the whole.  Then we just
apply the essential `s <~> Either a q` isomorphism that defines a prism.  And
so `p a a -> p s s`, going through the `s <~> Either a q` isomorphism, is a
prism!

### Recovering the Functionality

We can recover the original functionality by just picking specific values of
`p` that, when transformed, give us the operations we want.

For example, we want `view :: Lens' s a -> (s -> a)`, so we just make a
profunctor `p` where `p s s` contains an `s -> a`.

```haskell
-- | `(View a) s s` is just an `s -> a`
newtype View a s r = View { runView :: s -> a }

instance Profunctor (View a)
instance Strong (View a)
```

And when you give this to a lens (a "profunctor transformer"), you get a `(View
a) s s`, which is a newtype wrapper over an `s -> a`!

Note that you can't give this to a prism, since it is not possible to write a
`Choice` instance for `View a`.  Thus we naturally limit `view` to work only
for lenses (because they have `Strong`) and not for prisms (because prisms
require `Choice` to work).

For a more detailed look on implementing the entire lens and prism API in terms
of profunctors, check out Oleg Grenrus's amazing [Glassery][]!

[Glassery]: http://oleg.fi/gists/posts/2017-04-18-glassery.html

To me, this perspective makes it really clear to see "why" profunctor lenses
and profunctor prisms are implemented the way they are.  At first, the
profunctor optics definitions seemed really opaque and arbitrary to me, and I
had no idea why `Strong` and `Choice` corresponded to lenses and prisms.

But now, I know that lenses are prisms can be seen as just *profunctor
transformers* that *transform along the decomposition* that the lenses and
prisms represent.  For profunctor lenses, the profunctors get transformed to
"parts of a whole" profunctors, using `Strong`.  For profunctor prisms, the
profunctors get transformed to "branches of a possibility" profunctors, using
`Choice`.  Even their types clearly show what is going on:

```haskell
-- [Lens]  s <~> (a, q)
first' :: p a a -> p (a, q) (a, q)

-- [Prism] s <~> Either a q
left'  :: p a a -> p (Either a q) (Either a q)
```

In fact, `Strong` and `Choice` fit lenses and prisms like a glove so well that
sometimes I wonder if Edward Kmett just invented those typeclasses custom-made
to represent lenses and prisms.

Or...maybe lenses and prisms were invented custom-made based on `Strong` and
`Choice`?

Closing out
-----------

Hopefully this perspective --- that products yield lenses and sums yield prisms
--- helps you navigate how you discover lenses and prisms, and how you
interpret them when you see them.  I know for me, it has helped me understand
the odd lenses and prisms I often see, and also it helps me reason about when
it *doesn't* make sense to have a lens or prism.  It has also distilled the
lens and prism laws into something trivial that can be stated succinctly ("it
must be an isomorphism"), and also made the profunctor optics form seem
uncannily natural.

Some small notes need to be made to bridge this view with the way they are
actually implemented in practice:

1.  We have been dealing with `Lens' outer inner` and `Prism' outer inner`,
    which are known as "simple" optics.  In practice, this can be generalized
    by giving the "input" outer/inner values and "output" outer/inner values
    different type variables.

    For example, so far all our operations have basically been navigating
    between the isomorphisms that lenses and prisms represent:

    ```
    Lens' outer inner
    =================
     outer ---> (inner, q)
                   |
                   v
     outer <--- (inner, q)

    Prism' outer inner
    =================
     outer ---> Either inner q
                         |
                         v
     outer <--- Either inner q
    ```

    We can simply *re-label* the inputs and outputs to have different types,
    like so:

    ```
    Lens s t a b
    ============
       s   ---> (  a  , q)
                   |
                   |
                   v
       t   ---> (  b  , q)

    Prism s t a b
    =============
       s   ---> Either   a   q
                         |
                         |
                         v
       t   ---> Either   b   q
    ```

    Essentially, we're just deciding to give the inputs and outputs different
    type variables.  The main thing this helps is with is giving us the ability
    to distinguish inputs from outputs when we talk about these things.

    For example, before, with `Lens' outer inner`, if I say "the `outer`", you
    won't know if I mean the `outer` "before" we use the lens, or the `outer`
    *after* we use the lens.  However, with `Lens s t a b`, if I say "the `s`",
    you know that I just mean "the `outer` *before* we use the lens", and if I
    say "the `t`", you know that I mean "the `outer` *after* we use the lens".

    `Lens s t a b` (which is a version of `Lens' outer inner` where we relabel
    the type variables of the inputs and outputs) is called a [lens
    family][lens-family].  Be careful to never call it a "polymorphic lens".
    It **not** a polymorphic lens.  It is just a normal lens where we re-label
    the type variables of all of the involved pieces to aid in our discourse.

    [lens-family]: http://comonad.com/reader/2012/mirrored-lenses/

    ```haskell
    data Lens s t a b = forall q. Lens
        { split   :: s -> (a, q)        -- before (with s and a)
        , unsplit :: (b, q) -> t        -- after  (with t and b)
        }
    
    view :: Lens s t a b -> (s -> a)
    set  :: Lens s t a b -> (b -> s -> t)

    data Prism s t a b = forall q. Prism
        { match  :: s -> Either a q     -- before (with s and a)
        , inject :: Either b q -> t     -- after  (with t and b)
        }
    
    
    matching :: Prism s t a b -> (s -> Either t a)
    review   :: Prism s t a b -> (b -> t)
    ```

    We still require `unsplit . split = id`, `split . unsplit = id`, `inject .
    match = id`, and `match . inject = id`.  They're all still *isomorphisms*.
    We're just *relabeling our type variables* here to let us be more
    expressive with how we talk about all of the moving parts.

    Lens families can be used to implement "type changing lenses" where
    tweaking the inner type can cause the outer type to also change
    appropriately.  However, in this case, `unsplit . split = id` and `inject .
    match = id` should always still be enforced in the situation where the
    types do not change.

2.  In practice, the `q` to factor out your type into (in the `s <~> (a, q)`
    and `s <~> Either a q`) might not be an actual literal type.  In most
    cases, it's alright to treat it as a theoretical "abstract" type that
    follows the behavior you want given a restricted interface.  For example,
    the `only 'a' :: Prism' Char ()` prism matches only on `'a'`, and it is the
    sum of `Char` and a theoretical abstract `Char` type that excludes `'a'`.

    To formalize this, sometimes we can say that only "one direction" of the
    isomorphism has to be strictly true in practice.  If we only enforce that
    the round-trip of `unsplit . split = id` and `inject . match = id`, this
    enforces just the spirit of the hidden abstract type.


Exercises
---------

To help solidify your 


1.  Is (a, Void) a decomp
2.  what does the (Bool, a) <~> Either a a sum give us
3.  Write a lens composition

