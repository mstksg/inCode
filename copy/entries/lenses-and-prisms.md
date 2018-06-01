---
title: Lenses embody Products, Prisms embody Sums
categories: Haskell
tags: lenses, profunctors
create-time: 2018/05/22 23:29:16
identifier: lenses-and-prisms
slug: lenses-products-prisms-sums
---

I've written about a variety of topics on this blog, but one thing I haven't
touched in too much detail is the topic of lenses and optics.  A big part of
this is because there are already so many great resources on lenses, like the
famous (and my favorite) [lenses over tea][tea] series.

[tea]: https://artyom.me/lens-over-tea-1

This post won't be a "lens tutorial", but rather a dive into a (what I believe
is an) insightful perspective on lenses and prisms that I've heard repeated
many times, but not yet all gathered together into a single place.  In
particular, I'm going to talk about the perspective of lenses and prisms as
embodying the essences of products and sums (respectively), and how that
observation can help you with a more "practical" understanding of lenses and
prisms.

Products and Sums
-----------------

In Haskell, "products and sums" can roughly be said to correspond to "tuples
and `Either`".  If I have two types `A` and `B`, `(A, B)` is their "product"
type.  It's often called an "anonymous product", because we can make one
without having to give it a fancy name.  It's called a product type because `A`
has $n$ possible values and `B` has $m$ possible values, then `(A, B)` has
$n \times m$ possible values[^bottom].  And, `Either A B` is their (anonymous)
"sum" type.  It's called a sum type because `Either A B` has $n + m$ possible
values.  I won't go much deeper into this, but there are [many useful tutorials
already online][adts] on this topic!

[^bottom]: All of this is disregarding the notorious "bottom" value that
inhabits every type.

[adts]: https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types

Let's Get Productive!
---------------------

It's easy to recognize `(Int, Double)` as a product between `Int` and
`Bool`.  However, did you know that some types are secretly product types in
disguise?

For example, here's a classic example of a lensable data type

```haskell
data Person = P { _pName :: String
                , _pAge  :: Int
                }
```

`Person` is an algebraic data type --- so-called because it is actually a
*product* between a `String` and `Int`.  `Person` is *isomorphic* to `(String,
Int)`.  I will be writing this as `Person <~> (String, Int)`.

By *isomorphic*, I mean that there are functions `split :: Person -> (String, Int)`
and `unsplit :: (String, Int) -> Person` where `unsplit . split = id` and `split .
unsplit = id`.  You can think of this property as stating formally that you
should be able to go from one type to the other without "losing any
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

Another curious product is the fact that every type `a` is a product
between *itself* and unit, `()`.  That is, every type `a` is isomorphic to `(a,
())`.  Freaky, right?

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

respecting [some laws][lenslaws] --- get-put, put-get, and put-put.

[lenslaws]: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial#the-lens-laws-

Abstract mathematical laws are great and all, but I'm going to tell you a
secret that will render those laws obsolete.

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
`s <~> (a, q)`), *then you have two lenses*! Lenses are nothing more than
**descriptions of products**!

In other words, a `Lens' s a` is nothing more than a witness for an `exists q.
s <~> (a, q)` isomorphism.

With that in mind, let's re-visit a saner definition of lenses based on the
idea that lenses embody descriptions of products:

```haskell
data Lens' s a = forall q.
                 Lens' { split   :: s -> (a, q)
                       , unsplit :: (a, q) -> s
                       }    -- ^ s <~> (a, q)
```

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
pName :: Lens' Person String
pName = Lens' { split   = \(P n a) -> (n, a)
              , unsplit = \(n, a)  -> P n a
              }

pAge :: Lens' Person String
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
identity :: Lens' a a
identity = Lens' { split   = \x      -> (x, ())
                 , unsplit = \(x, _) -> x
                 }

unital :: Lens' a ()
unital = Lens' { split   = \x       -> ((), x)
               , unsplit = \((), x) -> x
               }
```

In the language of lens, `identity :: Lens' a a` tells us that all `a`s have an
`a` "inside" them.  However, in the language of products, this just tells us
that `a` can be represented as `(a, ())`.  In the language of lens, `unital ::
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

Looking at `mysteryLens2 :: Lens' (Either a a) a`, we are saying that every
`Either a a` has some `a` "inside" it.  From what we know about the underlying
product, the `a` is just the "contained value", *ignoring* leftness or
rightness.  Getting the `a` is getting the contained value and losing
leftness/rightness, and re-setting the `a` inside is modifying the contained
value but preserving leftness/rightness.

So that's really the essence of what a `Lens'` is.  A `Lens' s a` is the
embodiment of the fact that `s` can be represented as a product between `a` and
something else --- that `s <~> (a, q)`.  All of the lens laws just boil down to
this.  **Lenses embody products**.

There's Sum-thing about This...
-------------------------------

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
and...itself?  We can interpret this as `()` being one possibility, and `(a,
[a])` (head consed with another list) as the other:

```haskell
-- [a] <~> Either () (a, [a])

match :: [a] -> Either () (a, [a])
match []     = Left  ()
match (x:xs) = Right (x, xs)

inject :: Either () (a, [a]) -> [a]
inject (Left   _     ) = []
inject (Right (x, xs)) = x:xs
```

If you don't believe me, just verify that `inject . match = id` and `match .
inject = id` :)


Actually, however, there is another way to deconstruct `[a]` as a sum in
Haskell.  You can treat it as a sum between `()` and `([a], a)` --- where the
`()` represents the empty list and the `([a], a)` represents an "all but the
last item" list and "the last item":

```haskell
-- [a] <~> Either () ([a], a)

match  :: [a] -> Either () ([a], a)
match xs
  | null xs   = Left  ()
  | otherwise = Right (init xs, last xs)

inject :: Either () (a, [a]) -> [a]
inject (Left   _     ) = []
inject (Right (xs, x)) = xs ++ [x]
```

I just think it's interesting that the same type can be "decomposed" into a sum
of two different types in multiple ways.

(Fun haskell challenge: the version of `match` I wrote there is conceptually
simple, but very inefficient.  It traverses the input list three times, uses
two partial functions, and uses a `Bool`.  Can you write a `match` that does
the same thing while traversing the input list only once and using no partial
functions or `Bool`s?)

One final curious sum: if we consider the "empty data type" `Void`, the type
with no inhabitants:

```haskell
data Void           -- no constructors, no valid inhabitants
```

then we have a curious sum: every type `a` is a sum between *itself* and
`Void`.  In other words, `a` is isomorphic to `Either a Void`:

```haskell
-- a <~> Either a Void

match :: a -> Either a Void
match x = Left x

inject :: Either a Void -> a
inject (Left  x) = x
inject (Right v) = case v of
                    {}  -- empty case statement because we have
                        -- no constructors of 'v' we need to
                        -- match on
```

Again, if you don't believe me, verify that `inject . match = id` and `match .
inject = id`!

<!-- One last example -- one of my favorite sums from math is the fact that the -->
<!-- natural numbers are a sum between ... themselves and themselves.  `Natural <~> -->
<!-- Either Natural Natural`.  Sometimes you might hear this stated as $2 \mathbb{N} -->
<!-- \equiv \mathbb{N}$ (where $2 \mathbb{N}$ can be thought of as a fancy way of -->
<!-- writing $\mathbb{N} + \mathbb{N}$).  So, the sum of the naturals with -->
<!-- themselves is...exactly the naturals? -->

<!-- ```haskell -->
<!-- -- Natural <~> Either Natural Natural -->

<!-- match :: Natural -> Either Natural Natural -->
<!-- match n = case n `divMod` 2 of -->
<!--     (q, 0) -> Left  q       -- even number -->
<!--     (q, 1) -> Right 1       -- odd number -->

<!-- inject :: Either Natural Natural -> Natural -->
<!-- inject (Left  q) = 2 * q -->
<!-- inject (Right q) = 2 * q + 1 -->
<!-- ``` -->

<!-- Go figure! -->

### Through the Looking-Prism

Now let's bring prisms into the picture.  A `Prism' s a` also refers to some
`a` "inside" an `s`, with the following API: `preview` and `review`[^invent]

[^invent]: I didn't invent these names :)

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
q`...then you have two prisms!  Prisms are nothing more than **descriptions of
products**!

A `Prism' s a` is nothing more than a witness for an `exists q. s <~> Either a
q` isomorphism.

Under this interpretation, we can write a nice representation of `Prism'`:

```haskell
data Prism' s a = forall q.
                  Prism' { match  :: s -> Either a q
                         , inject :: Either a q -> s
                         }
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
`over`:

```haskell
over :: Lens' s a  -> (a -> a) -> (s -> s)
over Lens'{..}  f = inject . first f . match    -- instance Bifunctor (,)

over :: Prism' s a -> (a -> a) -> (s -> s)
over Prism'{..} f = inject . first f . match    -- instance Bifunctor Either
```

Neat, they're actually exactly identical!  Who would have thought?

So, again, **every sum yields prisms**, and **every prism witnesses one side of
a sum**.

### Prism Tour

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
`(a, [a])`?  Let's look at them:

```haskell
_Nil :: Prism' [a] ()
_Nil = Prism' { match  = \case []            -> Left ()
                               x:xs          -> Right (x, xs)
              , inject = \case Left _        -> []
                               Right (x, xs) -> x:xs
              }

_Cons :: Prism' [a] (a, [a])
_Cons = Prism' { match  = \case []            -> Right ()
                                x:xs          -> Left (x, xs)
               , inject = \case Left  (x, xs) -> x:xs
                                Right _       -> []
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


<!-- After all, what do constructors allow you -->


<!-- ### Type-Changing Lenses -->

<!-- In practice, we generalize the notion of a `Lens' s a` to be a "[lens -->
<!-- family][]".  We can diagram a `Lens' s a` in terms of the isomorphism it -->
<!-- witnesses: -->

<!-- [lens-family]: http://comonad.com/reader/2012/mirrored-lenses/ -->

<!-- ``` -->
<!-- Lens' s a -->
<!-- ========= -->

<!--  s   ->   (a,  q) -->
<!--            | -->
<!--            | -->
<!--            v -->
<!--  s   <-   (a,  q) -->
<!-- ``` -->

<!-- That is, we can "interrupt" the `s <~> (a, q)` isomorphism with a modifying -->
<!-- function to modify our `s`. -->

<!-- We can diagram a `Lens s t a b` (a so-called "polymorphic" lens) as a lens with -->
<!-- the ability to "change" the value in the product: -->

<!-- ``` -->
<!-- Lens s t a b -->
<!-- ============ -->

<!--  s   ->   (a,  q) -->
<!--            | -->
<!--            | -->
<!--            v -->
<!--  t   <-   (b,  q) -->
<!-- ``` -->

<!-- The isomorphism is still there, but it's hidden in a subtle way.  It helps if -->
<!-- you think of things in terms of an imaginary -->

<!-- Or, to put in terms of an abstract type family `Outer` and `Inner`. -->

<!-- ``` -->
<!-- Lens (Outer i) (Outer j) (Inner i) (Inner j) -->
<!-- ============================================ -->

<!--  Outer i -> (Inner i, q) -->
<!--                | -->
<!--                | -->
<!--                v -->
<!--  Outer j <- (Inner j, q) -->
<!-- ``` -->



<!-- Lens families (often incorrectly called "polymorphic lenses") are -->
<!-- somewhat of a complex topic to fully understand.  However, I think this "lenses -->
<!-- are products" perspective really makes lens families a little clearer. -->

<!-- Instead of `Lens' s a`, we can have instead (in pseudo-Haskell): -->

<!-- ```haskell -->
<!-- data LensFamily outer inner = forall q. -->
<!--     LF { split   :: forall i. outer i      -> (inner i, q) -->
<!--        , unsplit :: forall i. (inner i, q) -> outer i -->
<!--        } -->
<!-- ``` -->

<!-- LensFamily f` -->

<!-- Let's introduce another type synonym in pseudo-Haskell: -->

<!-- ```haskell -->
<!-- type LensFamily fam -->
<!-- ``` -->




<!-- Lens families (often incorrectly called "polymorphic lenses") are -->
<!-- somewhat of a complex topic to fully understand.  However, I think this "lenses -->
<!-- are products" perspective really makes lens families a little clearer. -->


<!-- To do this, instead of thinking of `Lens' s a` as indicating an -->
<!-- `exists q. s <~> (a, q)`, we can actually think of `s` as some function `F` -->
<!-- "parameterized" by `a`.  A `Lens' s a` -->

<!-- With a lens family, we generalize the idea of `Lens' s a` to *parameterize* `s` -->
<!-- in terms of the thing it "contains". -->


<!-- We write this typically as a `Lens s t a -->
<!-- b`, but we really mean (in pseudo-Haskell) `Lens (Contains a) (Contains b) a -->
<!-- b`, where `Contains` is some abstract relationship that tells us the type of -->
<!-- the thing that contains `a`. -->

<!-- For our `Person` lenses, we have: -->

<!-- ```haskell -->
<!-- pName :: Lens' Person String -->
<!-- pName = Lens' { split   = \(P n a) -> (n, a) -->
<!--               , unsplit = \(n, a)  -> P n a -->
<!--               } -->
<!-- ``` -->


<!-- We write a lens family as `Lens s t a b`, but we really *mean* some -->
<!-- `Lens (Constains a) (Contains b) a b`. -->

<!-- In practice, we can generalize the notion of a `Lens' s a` to be a `Lens s t a -->
<!-- b`.  One nice way to understand this is in terms of the type of `over`, and how -->
<!-- it's so similar to `fmap`: -->

<!-- ```haskell -->
<!-- over :: Lens s t a b -> (a -> b) -> (s   -> t  ) -->
<!-- fmap :: Functor f    => (a -> b) -> (f a -> f b) -->
<!-- ``` -->

<!-- `fmap` lets you apply an `a -> b` to values "inside" an `f a` to turn it into -->
<!-- an `f b`.  `over` for a `Lens s t a b` lets you apply an `a -> b` to values -->
<!-- "inside" an `s` to turn it into a `t`. -->

<!-- We can understand this in terms of the product formation by realizing that `s`, -->
<!-- `t`, `a`, and `b` cannot *really* vary independently. -->

<!-- it means you can "get" an `a` from an `s`, -->
<!-- and also that you can "set" the `a` inside the `s` to be a `b` modify the `a` with an `a -> b` to change the `s` into a -->
<!-- `t`. -->

<!-- If that's a little confusing, we can look at the type of the API of a full -->
<!-- `Lens`: -->

<!-- ```haskell -->
<!-- view :: Lens s t a b -> (s -> a) -->
<!-- set  :: Lens s t a b -> b -> s -> t -->
<!-- ``` -->

<!-- ### Strength -->

<!-- You might have heard that lenses aren't *actually* implemented as an ADT in -->
<!-- practice.  In most modern lens libraries, they are represented as higher-order -->
<!-- functions.  In my opinion, however, this is similarly boring :) -->

<!-- If we really do mean that a `Lens' s a` witnesses an isomorphism between `s` -->
<!-- and `(a, q)`...then we can represent that using a RankN function polymorphic -->
<!-- over a `Profunctor`. -->

<!-- If you are not familiar with profunctors, there's a [great -->
<!-- introduction][profunctors] on Oliver Charles's Blog written by Tom Ellis.  The -->
<!-- typeclass requires that we have ways to "re-map" both ends of the relationship: -->

<!-- [profunctors]: https://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html -->

<!-- ```haskell -->
<!-- lmap :: Profunctor p => (a' -> a)  -> p a b -> p a' b -->
<!-- rmap :: Profunctor p => (b  -> b') -> p a b -> p a  b' -->
<!-- ``` -->

<!-- `lmap` lets you "pre-apply" a function, essentially, to the "input" of a `p a -->
<!-- b`, and `rmap` lets you "post-apply" a function to the "output" of a `p a b`. -->
<!-- These definitions are "generic" and vague enough to allow for many different -->
<!-- colorful `Profunctor` instances that have all sorts of interesting -->
<!-- implementations of "pre-apply" and "post-apply". -->

<!-- However, for this post, I want to propose a specific interpretation of the -->
<!-- `Profunctor` typeclass: a `p a b` represents a *relationship* from `a` to `b`. -->
<!-- The `Profunctor` typeclass is delightfully vague enough that all of the many -->
<!-- different instances and choices of `p` all give very unique and interesting -->
<!-- perspectives on what a "relationship from `a` to `b`" even means. -->

<!-- What does this mean for lenses? -->

<!-- Well, a `Lens' s a` can be thought of as a way to "promote" a `p a a` to a `p s -->
<!-- s`.  It lets you promote some relationship on `a` (the internals) to become a -->
<!-- full-fledged relationship on `s`. -->

<!-- What does this mean from our "lenses are products" perspective?  It means that -->
<!-- if we can turn `p a a` into `p (a, q) (a, q)` that "ignores" the `q`...then we -->
<!-- can just pre-map and post-map `split` and `unsplit` to get a `p s s`: -->

<!-- ```haskell -->
<!-- rejoinProfunctor -->
<!--     :: Profunctor p -->
<!--     => p (String, Int) (String, Int) -->
<!--     -> p Person Person -->
<!-- rejoinProfunctor p = p'' -->
<!--   where -->
<!--     p'  :: p Person (String, Int) -->
<!--     p' = lmap split p         -- pre-map split -->
<!--     p'' :: p Person Person -->
<!--     p'' = rmap unsplit p          -- post-map unsplit -->
<!--     split   (P n a) = (n, a) -->
<!--     unsplit (n, a)  = P n a -->
<!-- ``` -->

<!-- We can make a typeclass for profunctors that have such a "promote to a -->
<!-- relationship over a tuple, ignoring the second field".  It's typically called -->
<!-- `Strong`: -->

<!-- ```haskell -->
<!-- class Profunctor p => Strong p where -->
<!--     first :: p a b -> p (a, q) (b, q) -->
<!-- ``` -->

<!-- `first` takes a `p a b` (some relationship from `a` to `b`) and promotes it to a `p -->
<!-- (a, q) (b, q)` (some relationship from `(a, q)` to `(b, q)` that *ignores* the -->
<!-- `q`). -->

<!-- With this in mind, we can now use any `Lens' s a` to transform a `p a a` into a `p -->
<!-- s s`: -->

<!-- ```haskell -->
<!-- fromLens' -->
<!--     :: Strong p -->
<!--     => Lens' s a -->
<!--     -> p a a -->
<!--     -> p s s -->
<!-- fromLens' (Lens' spl uns) p = rmap split (lmap unsplit (first p)) -->
<!-- ``` -->

