Fixed-Length Vector Types in Haskell, 2015
==========================================

Categories
:   Haskell
:   Tutorials
Tags
:   haskell
CreateTime
:   2015/04/27 21:56:11
PostDate
:   Never
Series
:   Beginner/Intermediate Haskell Projects
Identifier
:   fixvec

Fixed-length vector types (vector types that indicate the length of the vector
in the type itself) are one of the more straightforward applications of the
"super-Haskell" GHC type extensions.  There's a lot of magic you can do with
GHC's advanced type mechanisms, but I think fixed length vectors are a good
first step to beginning to understand several extensions, including
(potentially):

*   ConstraintKinds
*   DataKinds
*   GADTs
*   KindSignatures
*   TypeFamilies
*   TypeOperators

And using type system plugins.  (And of course the usual
`UndecidableInstances` etc.)  We'll be discussing two different ways to
implement this --- using type-level nats, and using the *GHC.TypeLits* model
to actually be able to use numeric literals in your types.

There are a few tutorials/writeups on this topic, but many of them are from
the time before we had some of these extensions, or only discuss a few.  I
hope to provide a nice comprehensive look about the tools available today to
really approach this topic.  That being said, I am no expert myself, so I
would appreciate any tips/edits/suggestions for things that I've missed or
done not-the-best :)

The Idea
--------

The basic idea is we'll have a type:

~~~haskell
Vec n a
~~~

Which is a vector with items of type `a`, whose length is somehow encoded in
the `n`.  We'll then discuss ways to do useful operations on this, as if it
were a list.

`n` can really only be a certain "kind" of thing --- a type that encodes a
length.  We can represent this by giving it a "kind signature":

~~~haskell
data Vec :: Nat -> * -> *
~~~

Which says that our `Vec` type constructor takes two arguments: something of
kind `Nat` (so it can't be any type...it has to be a type of kind `Nat`),
something of kind `*` (the "normal" kind, of things that have values, like
`Int`, `Maybe Bool`, etc.), and returns something of kind `*` (our vector
itself).

Using DataKinds for Type-Level Nats
-----------------------------------

There are a couple of ways to find something for that `n` `Nat` kind, and one
way is to use the simple inductive `Nat`:

~~~haskell
data Nat = Z | S Nat
~~~

You might have seen this type before...it gives us value-level natural
numbers, where `Z` is zero, `S Z` is one, `S (S Z)` is two, `S (S (S Z))` is
three, etc.  So if we had something of type `Nat`, it could represent any
natural number.  This declaration gives you:

*   A type `Nat`
*   A value constructor `Z :: Nat`
*   A value constructor `S :: Nat -> Nat`

However, with the `DataKinds` extension, when you define this, you also define
some extra fancy things.  You also define a *kind* `Nat`!  More specifically,
you get:

*   A kind `Nat`
*   A type `Z :: Nat` (`Z`, of *kind* `Nat`)
*   A type constructor `S :: Nat -> Nat` (`S`, which takes something of kind
    `Nat`, and returns a new thing of kind `Nat`)

We can check this out in GHCi:

~~~haskell
ghci> :set -XDataKinds
ghci> data Nat = Z | S Nat
ghci> :k Z
Nat
ghci> :k S Z
Nat
ghci> :k S (S Z)
Nat
~~~

So now we have a *type* that can encode numbers.  Something of type `Z`
represents zero...something of type `S Z` represents 1...something of type `S
(S Z)` represents two.

Note that you can't ever have anything like `S Bool`...that doesn't work,
because `Bool` is of kind `*`, but `S` expects only `Nat`s.

Now we can make our `Vec` data type, with something called `GADTs`, or
"generalized algebraic data types":

~~~haskell
data Vec :: Nat -> * -> * where
    Nil  :: Vec Z a
    (:#) :: a -> Vec n a -> Vec (S n) a

deriving instance Show a => Show (Vec n a)
~~~

If you've never seen `GADTs` before, think of it as a way of declaring a type
by giving the type of your constructors instead of just the normal boring
form.  It's nothing too crazy...it's basically like defining `Maybe` as:

~~~haskell
data Maybe :: * -> * where
    Nothing :: Maybe a
    Just    :: a -> Maybe a
~~~

instead of

~~~haskell
data Maybe a = Nothing | Just a
~~~

In both cases, they create constructors of type `Nothing :: Maybe a` and
`Just :: a -> Maybe a` anyway...so the GADT form just gives us a way to state
it explicitly.

Oh, we also used the `KindSignatures` extension to be able to give a
kind signature to `Vec`...this is important because we want to make sure the
first argument has to be a `Nat`.  That is, we can't have anything silly like
`Vec Bool Int`.  We also have to put a separate `StandaloneDeriving`-extension
standalone deriving clause instead of just having `deriving Show` because
`Vec` isn't a type that can be expressed in "normal `Haskell`".

Note that our type is basically like a list:

~~~haskell
data [] :: * -> * where
    []  :: [a]
    (:) :: a -> [a] -> [a]
~~~

Except now our type constructor actually has a new `Nat`

This means that, because of type erasure, everything "runtime" on our new type
is basically going to be identical to `[]` (not considering compiler tricks).
In-memory, this new type is essentially exactly `[]`, but its type has an
extra tag that is erased at compile-time.

<div class="note">
**Aside**

Things don't have to be this way, by the way.  We are basically "enhancing" a
list with type information.  We could have also done:

~~~haskell
newtype Vec :: Nat -> * -> * where
    VecList :: [a] -> Vec n a
~~~

And only offer "smart constructors", to construct things.  But you're going to
have to accept some runtime errors if you want to be able to convert arbitrary
lists to a target type.  You could also do

~~~haskell
newtype Vec :: Nat -> * -> * where
    VecVector :: Vector a -> Vec n a
~~~

Where a `Vec` really just wraps a `Vector` from the *vector* package, so now
we have $O(1)$ random access :)
</div>

Okay, let's define some useful methods:

~~~haskell
headV :: Vec (S n) a -> a
headV (x :# _)  = x

tailV :: Vec (S n) a -> Vec n a
tailV (_ :# xs) = xs
~~~

Ah, the classic `head`/`tail` trio from the days pre-dating Haskell.  `head`
and `tail` are somewhat of a sore spot or wart in Haskell's list API[^htgone],
because they're *partial functions*.  You tell people all about how Haskell is
great because it can prevent run-time errors by ensuring completeness and
having the type system enforce null-pointer checks...but then you go ahead and
put unsafe functions that throw errors for empty lists anyways in Prelude.

[htgone]: Can we get them out of Prelude?  Please? :)

But here...this will never happen!  We can only use `headV` and `tailV` on
non-empty lists...it won't typecheck for empty lists.  Do you see why?

It's because all empty lists are of type `Vec Z a`.  But `headV` and `tailV`
only take things of *type* `Vec (S n) a`, for any `Nat` `n`.  So, if you ever
try to use it on an empty list, it won't even compile!  No more pesky runtime
bugs.  `headV` and `tailV` are safe and will never crash at runtime!

Note that the return type of `tailV` is a vector of a length one less than the
given vector.  `tailV :: Vec (S Z) a -> Vec Z a`, for instance...or `tailV ::
Vec (S (S Z)) a -> Vec (S Z) a`.  Just like we want!

### Type families and appending

We can also "append" vectors.  But we need a way to add `Nat`s together first.
For that, we can use a type family, using the `TypeFamilies` extension (with
`TypeOperators`):

~~~haskell
type family x + y where
    Z   + y = y
    S x + y = S (x + y)
~~~

A "type family" is like a type level function.  Compare this to defining `(+)`
on the value level to the `Nat` *data* type:

~~~haskell
(+#) :: Nat -> Nat -> Nat       -- types!
Z   +# y = y
S x +# y = S (x +# y)
~~~

Basically, we're defining a new type-level function `(+)` on two types `x` and
`y`, both of kind `Nat`...and the result is their "sum".  Convince yourself
that this "addition" is actually addition.  Now, let's use it for `appendV`:

~~~haskell
appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV Nil       ys = ys
appendV (x :# xs) ys = x :# appendV xs ys
~~~

~~~haskell
ghci> let v1 = 1 :# 2 :# 3 :# Nil
ghci> let v2 = iterateV succ 0 :: Vec (S (S Z)) Int
ghci> v1 `appendV` v2
1 :# 2 :# 3 :# 0 :# 1 :# Nil
ghci> :t v1 `appendV` v2
v1 `appendV` v2 :: Vec (S (S (S (S (S Z))) Int
~~~

### Generating

It'd be nice to have type-safe methods of *generating* these things,
too...functions like `iterate`, or `enumFrom`.  One of the ways to do this is
by using a typeclass.

~~~haskell
class UnfoldV (n :: Nat) where
    unfoldV :: (b -> (a, b)) -> b -> Vec n a
~~~

An `UnfoldV` is a `Nat` where, given an "unfolding function", I can generate a
vector with the length encoded by that `Nat`.  The unfolding function is:
"Start with a `b`.  Run the function `b -> (a, b)` on that, and the first item
in the list is that `a`.  Then run the function again on the new `b`...and the
second item is that new `a`.  For lists, it'd look something like this:

~~~haskell
ghci> let unfoldL f x0 = let (y, x1) = f x0 in y : unfoldL f x1
ghci> :t unfoldL
unfoldL :: (b -> (a, b)) -> b -> [b]
ghci> take 5 $ unfoldL (\x -> (x `mod` 3 == 2, x^2 - 1)) 2
[True, False, True, False, True]
~~~


Let's write some instances:

~~~haskell
instance UnfoldV Z where
    unfoldV _ _ = Nil

instance UnfoldV n => UnfoldV (S n) where
    unfoldV f x0 = y :# unfoldV f x1
      where
        (y, x1) = f x0
~~~

Take a moment to think about what these instances are doing.  The instance for
`Z` is straightforward.  The instance is supposed to give a way to construct a
vector of length 0...and there is only one vector of length 0, `Nil`.  It has
to ignore the unfolding function.

For instances where you *don't* have `Vec Z a`...you have `Vec (S n) a`.
Well, you know the length is at least one.  And you know you can get the first
element, for sure (it's just the `fst` of `f x0`).  So we start with `y :#`
... but what comes next?  For what comes next, we need a `Vec n a`.  And...we
have a way to make a `Vec n a`!  We can use the instance of `UnfoldV` for `n`!

Of course, this only makes sense if `n` is an `UnfoldV`...so we have that as a
constraint on our typeclass.  In general, we can't assume that `n` is a
`UnfoldV` if `S n` is.  So we have to add that as a constraint.

Note that this style of declaration looks a lot like induction.  We define our
instance for zero...and then we say, "if `n` is an instance, then so is `S
n`".  Induction!

Let's see this in action.

~~~haskell
replicateV :: UnfoldV n => a -> Vec n a
replicateV = unfoldV (\x -> (x, x))

iterateV :: UnfoldV n => (a -> a) -> a -> Vec n a
iterateV f = unfoldV (\x -> (x, f x))

fromListMaybes :: UnfoldV n => [a] -> Vec n (Maybe a)
fromListMaybes = unfoldV $ \l -> case l of
                                   []   -> (Nothing, [])
                                   x:xs -> (Just x , xs)
~~~

~~~haskell
ghci> replicateV 'a'       :: Vec (S (S (S Z))) Char
'a' :# 'a' :# 'a' :# Nil
ghci> replicateV 'a'       :: Vec Z Char
Nil
ghci> iterateV succ 1      :: Vec (S (S (S (S Z)))) Int
1 :# 2 :# 3 :# 4 :# Nil
ghci> fromListMaybes [1,2] :: Vec (S (S (S Z))) (Maybe Int)
Just 1 :# Just 2 :# Nothing :# Nil
ghci> tailV (iterateV succ 1 :: Vec (S Z) Int)
Nil
~~~

Note that `replicateV` doesn't need to take in an `Int` parameter, like the on
in Prelude, to say how many items to have.  It just replicates enough to fill
the entire vector we want!

### Common Typeclasses

We can go in and implement common typeclasses, too.  All the ones you'd
expect.

For `Functor`, we can define one that works for everything:

~~~haskell
instance Functor (Vec n) where
    fmap _ Nil = Nil
    fmap f (x :# xs) = f x :# fmap f xs
~~~

For `Applicative`, it isn't so simple.  The Applicative instance is going to
be the "ZipList" instance...so we have to be able to make a `pure` that
depends on the type, and a `(<*>)` that depends on the type, too.

~~~haskell
instance Applicative (Vec Z) where
    pure _ = Nil
    Nil <*> _ = Nil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
    pure x = x :# pure x
    (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)
~~~

For `Vec Z`, it's just `Nil`.  For `Vec (S n)`...for pure, you need `x :#`
something...and that something has to be a `Vec n a`.  That's just `pure` for
`Vec n`!  Remember, we can't assume that `Vec n` is an `Applicative` just
because `Vec (S n)` is. So we need to add a constraint, that `Vec n` an
Applicative. Induction, again!

For `(<*>)`, we can get the first item easily, it's just `f x`.  But for the
next item, we need a `Vec n a`.  Luckily...we have exactly that with the
`(<*>)` for `Vec n`!

I'll leave the `Monad` instance as an exercise, but it's in the source files
for this post.  `join` for this instance should be a "diagonal" --- the first
item of the first vector, the second item of the second vector, the third item
of the third vector, etc.

We can define `Foldable` and `Traversable` the same way.

~~~hasell
instance Foldable (Vec Z) where
    foldMap _ Nil = mempty

instance Foldable (Vec n) => Foldable (Vec (S n)) where
    foldMap f (x :# xs) = f x <> foldMap f xs

instance Traversable (Vec Z) where
    traverse _ Nil = pure Nil

instance Traversable (Vec n) => Traversable (Vec (S n)) where
    traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)
~~~

Note that we can only use `foldMap f xs` on `xs :: Vec n a`, if `Vec n` is a
`Foldable`.  So that's why we add that constraint.

Again, `liftA2 (:#) :: Applicative f => f a -> f (Vec n a) -> f (Vec (S n)
a)`...so this only makes sense if `traverse f s` gives us a `Vec n a`.  So we
have to add that as a constraint.

TODO: ADD EXAMPLES OF THESE TYPECLASSES






