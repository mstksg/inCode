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

Be Productive!
--------------

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
unsplit = id`.

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
`a` and some type `x`*.

That means that if it is possible to represent `s` as some `(a, q)` (that is,
`s <~> (a, q)`, *then you have a lens*! Lenses are nothing more than
**descriptions of products**!

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

This type is technically also "too big" (you can write a value where `split`
and `unsplit` do not form an isomorphism), but I think, to me, "`split` and
`join` must form an isomorphism" is a much clearer and natural law than
get-put/put-get/put-put.

We can implement our necessary lens API as so:

```haskell
view :: Lens' s a -> (s -> a)
view (Lens' spl _) = fst . spl

set :: Lens' s a -> (a -> s -> s)
set (Lens' spl unspl) x y = let (_, q) = spl y
                            in  unspl (x, q)        -- "replace" the `a`
```

TODO: record wildcards?

The surprising result is that **every product yields lenses** (one for every
item in the product), and **every lens witnesses a product**.

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

### Strength

You might have heard that lenses aren't *actually* implemented as an ADT in
practice.  In most modern lens libraries, they are represented as higher-order
functions.  In my opinion, however, this is similarly boring :)

If we really do mean that a `Lens' s a` witnesses an isomorphism between `s`
and `(a, q)`...then we can represent that using a RankN function polymorphic
over a `Profunctor`. 

If you are not familiar with profunctors, there's a [great
introduction][profunctors] on Oliver Charles's Blog written by Tom Ellis.  The
typeclass requires that we have ways to "re-map" both ends of the relationship:

[profunctors]: https://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html

```haskell
lmap :: Profunctor p => (a' -> a)  -> p a b -> p a' b
rmap :: Profunctor p => (b  -> b') -> p a b -> p a  b'
```

`lmap` lets you "pre-apply" a function, essentially, to the "input" of a `p a
b`, and `rmap` lets you "post-apply" a function to the "output" of a `p a b`.
These definitions are "generic" and vague enough to allow for many different
colorful `Profunctor` instances that have all sorts of interesting
implementations of "pre-apply" and "post-apply".

However, for this post, I want to propose a specific interpretation of the
`Profunctor` typeclass: a `p a b` represents a *relationship* from `a` to `b`.
The `Profunctor` typeclass is delightfully vague enough that all of the many
different instances and choices of `p` all give very unique and interesting
perspectives on what a "relationship from `a` to `b`" even means.

What does this mean for lenses?

Well, a `Lens' s a` can be thought of as a way to "promote" a `p a a` to a `p s
s`.  It lets you promote some relationship on `a` (the internals) to become a
full-fledged relationship on `s`.

What does this mean from our "lenses are products" perspective?  It means that
if we can turn `p a a` into `p (a, q) (a, q)` that "ignores" the `q`...then we
can just pre-map and post-map `split` and `unsplit` to get a `p s s`:

```haskell
rejoinProfunctor
    :: Profunctor p
    => p (String, Int) (String, Int)
    -> p Person Person
rejoinProfunctor p = p''
  where
    p'  :: p Person (String, Int)
    p' = lmap split p         -- pre-map split
    p'' :: p Person Person
    p'' = rmap unsplit p          -- post-map unsplit
    split   (P n a) = (n, a)
    unsplit (n, a)  = P n a
```

We can make a typeclass for profunctors that have such a "promote to a
relationship over a tuple, ignoring the second field".  It's typically called
`Strong`:

```haskell
class Profunctor p => Strong p where
    first :: p a b -> p (a, q) (b, q)
```

`first` takes a `p a b` (some relationship from `a` to `b`) and promotes it to a `p
(a, q) (b, q)` (some relationship from `(a, q)` to `(b, q)` that *ignores* the
`q`).

With this in mind, we can now use any `Lens' s a` to transform a `p a a` into a `p
s s`:

```haskell
fromLens'
    :: Strong p
    => Lens' s a
    -> p a a
    -> p s s
fromLens' (Lens' spl uns) p = rmap split (lmap unsplit (first p))
```

