Inside My World
===============

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
:   haskell
CreateTime
:   2014/05/02 03:09:13
PostDate
:   Never
Identifier
:   inside

I like Haskell because it lets me live inside my world.

There are a lot of special worlds out there.

1.  The world of *Maybe*, in which things may or may not be there.
2.  The world of *List*, in which things may be many things.
3.  The world of *Reader r*, in which things are things that do not yet exist
    but are awaiting an *r* before they can come to be.
3.  The world of *State s*, in which things are things that are awaiting an
    *s* before they can come to be, but modify the *s* in the process.
4.  The world of *IO*, in which things are promised things that will be
    computed by a CPU, which can react to the outside world during that
    process.

And many more.

Haskell lets me stay in those worlds, and use all of the tools I normally have
when I'm not there.  I get to transform normal tools into tools that work in
my world.

Stuck in Maybe
--------------

In Haskell, we have a type called `Maybe a`:

~~~haskell
data Maybe a = Just a | Nothing
~~~

This says that `Maybe a` is like an Enum type of sorts...it can either be in
the form `Just x` --- something is there --- or `Nothing` --- nothing is
there.

If you are used to an OOP language with templates or generics, this is
similar to saying `Maybe<a>` -- `Maybe<a>` is a parameterized type over some
`a`.

This type is useful for functions that might fail:

~~~haskell
-- Takes two integers and returns -- possibly -- their integer quotient. It
-- succeeds if the denominator is not zero, and fails if it is.
divideMaybe :: Int -> Int -> Maybe Int
divideMaybe _ 0 = Nothing
divideMaybe x y = Just (x `div` y)

-- Takes a list and returns -- possibly -- its first element.  Fails if the
-- list is empty, and succeeds with the first element otherwise.
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

-- Takes an integer and returns -- possibly -- its half.  Fails if it is an
-- odd number.
halveMaybe :: Int -> Maybe Int
halveMaybe x | x `mod` 2 == 0 = Just (x `div` 2)
             | otherwise      = Nothing
~~~

When you want to return a value of type `Maybe a`, you can either return
`Just x` or `Nothing` --- they both are members of type `Maybe a`.  That's
what `Maybe Int` means --- an `Int` that might or might not be there!

If I gave you something of type `Maybe Int`, would you know for sure if that
`Int` was there or not?  You wouldn't!  You are living in the world of
uncertainties.

Welcome to the world of uncertainty.

### The Problem

Okay, well, I have a `Maybe Int`.  Which is nice and all...but...I want to do
normal inty-things with it.

That is...I have all these functions that work only on `Int`!

~~~haskell
addThree :: Int -> Int
addThree = (+ 3)

square :: Int -> Int
square = (^ 2)

showInt :: Int -> String
showInt = show
~~~

But...I can't do these things on `Maybe Int`!

~~~haskell
λ: addThree (Just 5)
<< SCARY ERROR! >>
<< addThree takes an `Int` but you gave it a `Maybe Int`.  What are you >>
<< trying to do anyway, wise guy. >>
~~~

In most other languages, to get around this, you would "exit" your uncertain
world.  That is, you would turn your uncertain 5 into either a certain 5 or an
error.  Or you would turn your uncertain 5 into either a certain 5 or some
"default" value.

That is, you would use functions like these to exit your world:

~~~haskell
certaintify :: Maybe a -> a
certaintify (Just x) = x
certaintify Nothing  = error "Nothing was there, you fool!"

certaintifyWithDefault :: a -> Maybe a -> a
certaintifyWithDefault _ (Just x) = x
certaintifyWithDefault d Nothing  = d
~~~

(In the standard libraries, these exist in the `Data.Maybe` module:
`certaintify` is `fromJust`, and `certaintifyWithDefault` is `fromMaybe`)

And then you can just willy-nilly use your normal `Int -> Int` functions on
what you pull out.

~~~haskell
λ: addThree (certaintify (headMaybe [1,2,3]))
3
λ: square (certaintify (halveMaybe 7))
*** Exception: Nothing was there, you fool!
λ: square (certaintifyWithDefault 0 (halveMaybe 7))
0
~~~

But...work with me here.  Let's say I want to live in my uncertain world.

There are a lot of reasons why one might want to do that.

Let's say you had a function that looked up a person from a database given
their ID number.  But not all ID numbers have a person attached, so the
function might fail and not lookup anyone.

~~~haskell
personFromId :: ID -> Maybe Person
~~~

And you also had a function that returned the age of a given person:

~~~haskell
age :: Person -> Int
~~~

What if you wanted to write a function that looked up *the age of the person
in that database with that ID*.  The result is going to also be in a  `Maybe`,
because the given ID might not correspond to anyone to have an age for.

~~~haskell
ageFromId :: ID -> Maybe Int
~~~

In this case, it would make no sense to "exit" the world of uncertainty as
soon as we get a `Maybe Person`, and then "re-enter" it somehow when you
return the `Maybe Int`.  Our entire answer is shrowded in uncertainty, so we
need to *stay inside this world* the entire time.

*This is the key*.  We want to find a way to deal with values inside a world
*without leaving it*.

So we have a function `Person -> Int`, and a `Maybe Person`...darnit.  How do
we use our `age` function, without leaving `Maybe`?  We certaintly want to
re-use the same function somehow, and not write it again from scratch!

### Can I have a lift?

So the problem:  I have a function `a -> b` that I want to be able to use
on a `Maybe a`...I want to stay in my `Maybe` world and use that function on
the uncertain value.

To phrase it in types, I want to turn an `a -> b` into a `Maybe a ->
Maybe b`.

I have a function on an `a` that returns a `b`...and so I want to turn it into
a function on an `a` that is possibly there/not there, to return --- well, a
`b` that is possibly there/not there!

I want to move my `a -> b` into *my world of uncertainty*.  So that I can use
*any* `a -> b` as if it were written for uncertain values this entire time.

If you look at this carefully, we want some sort of "function transformer".
Give our transfomer an `a -> b`, it'll output a new function `Maybe a -> Maybe
b`.

We want a function of type `(a -> b) -> (Maybe a -> Maybe b)`

Let's pretend we have one!  What could we do with it?

~~~haskell
inMaybe :: (a -> b) -> (Maybe a -> Maybe b)
~~~

~~~haskell
λ: let addThreeInMaybe = inMaybe addThree
λ: addThreeInMaybe (Just 7)
Just 10
λ: addThreeInMaybe Nothing
Nothing
λ: (inMaybe square) (Just 9)
Just 81
λ: (inMaybe showInt) Nothing
Nothing
λ: (inMaybe showInt) (Just 8)
Just "8"
~~~

Wow!  If I had this, I can now use normal functions and still stay inside my
uncertain world.

We can even write our `ageFromId`:

~~~haskell
ageFromId :: ID -> Maybe Int
ageFromId i = (inMaybe age) (personFromId i)
~~~

We can write out `inMaybe` ourselves:

~~~haskell
inMaybe :: (a -> b) -> (Maybe a -> Maybe b)
inMaybe f = go
  where
    go (Just x) = Just (f x)
    go Nothing  = Nothing
~~~

Now we are no longer afraid of dealing with uncertainty.  It's a scary realm,
but as long as we have `inMaybe`...all of our normal tools apply!

~~~haskell
λ: let x = headMaybe [2,3,4]        -- x = Just 2
λ: let y = (inMaybe square) x       -- y = Just 4
λ: let z = (inMaybe addThree) y     -- z = Just 7
λ: (inMaybe (> 5)) z
Just True
λ: let x' = halveMaybe 7            -- x' = Nothing
λ: let y' = (inMaybe square) x'     -- y' = Nothing
λ: let z' = (inMaybe addThree) y'   -- z' = Nothing
λ: (inMaybe (> 5)) z'
Nothing
~~~

### Functor

This concept of "bringing functions into worlds" is actually a useful and
generalizable concept.  In fact, in the standard libraryies, there's a
typeclass (which is like an interface, sorta, for you Java/OOP people) that
provides a common API/interface for "worlds that you can bring functions into."

We call it `Functor`:

~~~haskell
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)
~~~

If you haven't done much Haskell, this might seem scary, but don't worry.  All
it says that "if your world `f` is a `Functor`, then you have a function
`fmap` that turns any `a -> b` into an `f a -> f b`".

It should come as no surprise that `Maybe` is a `Functor`, so `fmap` *does*
take any function `a -> b` and "lifts" it into a the `Maybe` world, turning it
into a `Maybe a -> Maybe b`.

`fmap` for `Maybe` is incidentally exactly our `inMaybe`.

~~~haskell
λ: (fmap square) (headMaybe [4,5,6])
Just 16
λ: (fmap square) (halveMaybe 7)
Nothing
~~~

<aside>
    ###### Aside

Any "legitimate" instance of `Functor` should ideally satisfy a couple of
properties ---

1.  `fmap (f . g)` should equal `fmap f . fmap g`; that is, lifting composed
    functions be the same as composing lifted functions.
2.  `fmap id thing` should leave `thing` unchanged.
</aside>


Some notes about Functors before we move on!

First of all, even though we have been writing things like `(fmap f) x`, the
parentheses are actually unnecessary due to the way Haskell associates function
calls.  So `(fmap f) x` is the same as `fmap f x`, and we'll be writing it
that way from now on.

Secondly, an infix operator alias for `fmap` exists: `(<$>)`.  That way, you
can write `fmap f x` as `f <$> x`, which is like "applying" `f` "inside" `x`.
This might be more useful or expressive in some cases.

Merging Worlds
--------------

So with `Functor`, if we have a `Maybe a`, we can use an `a -> b` on it by
turning it into a `Maybe a -> Maybe b`.

That's neat and all...but, you might eventually realize that maybe you need a
little bit more some times.

Let's say that you have a matchmaking algorithm that calculates the
compatibility between two people.

~~~haskell
compatibility :: Person -> Person -> Double
~~~

~~~haskell
λ: compatibility John Sarah
82.3
~~~

That's fine and dandy if we are living in the world of normal functions and
normal types, but what if we were in the world of `Maybe`, the world of
uncertainty?

~~~haskell
λ: personFromId 144
Just John
λ: personFromId 14
Just Sarah
λ: compatibility (personFromId 144) (personFromId 14)
<< SCARY ERROR! >>
<< compatibility takes two Persons, but you gave two Maybe Persons. >>
<< have you no shame? >>
~~~

That didn't work, but what kind of answer would we have even expected?

Well, if we want to find the compatibility between two people that might not
even exist...the answer *should* be a compatibility that might not exist.

We want to move `compatibility :: Person -> Person -> Double`, and make it
work on two `Maybe Person`s; it'll work on two `Person`s inside the world and
create a `Double` inside the world.

We want some sort of "super `fmap`", that takes an `a -> b -> c` and turns it
into a `Maybe a -> Maybe b -> Maybe c`.

~~~haskell
inMaybes :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
~~~

~~~haskell
λ: (inMaybes compatibility) (personFromId 144) (personFromId 14)
Just 82.3
λ: personFromId 59
Nothing
λ: (inMaybes compatibility) (persomFromId 144) (personFromId 59)
Nothing
~~~

Note the last example --- you can't have a compatibility between a person that
exists and a person that doesn't exist!


### Apply yourself

As you probably could have guessed, this but this pattern is actually
generalizable to many different worlds, like `Functor` was.

We call it `Applicative`.  And because `Maybe` is an `Applicative`, we have
access to `liftA2`:

~~~haskell
liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
~~~

~~~haskell
λ: (liftA2 compatibility) (personFromId 144) (personFromId 14)
Just 82.3
~~~

What's the big deal about `Applicative`, anyway?  Do we need a separate, new
typeclass for just "`fmap` with two arguments"?

As it turns out, `Applicative` actually represents much more than just
`liftA2`.  `Applicative` *lets you combine values inside worlds*.

For example, in our previous example, we had `Maybe Person` and `Maybe
Person`, and we wanted to "combine" the *two* `Maybe`-things into *one*
`Maybe`-thing, a `Maybe Double`

We can actually

