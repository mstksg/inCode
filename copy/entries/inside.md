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
4.  The world of *IO*, in which things are things that will be computed by a
    CPU, which can react to the outside world during that process.

And many more.

Haskell lets me stay in those worlds, and use all of the tools I normally have
when I'm not there.  I get to transform normal tools into tools that work in
my world.

(This post is meant to be approachable by people unfamiliar with Haskell!
That being said, if there is a concept you don't understand, feel free to
leave a comment, [tweet][] me, stop by on irc at freenode's #haskell, or give
[Learn You a Haskell][LYAH] a quick read!)

[tweet]: https://twitter.com/mstk
[LYAH]: http://learnyouahaskell.com/

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

<aside>
    ###### Aside

Oh hi!

In this post, commands at the interactive Haskell interpreter (REPL) ghci are
prefaced with the prompt `λ:`.  If you see `λ:`, it means that this is
something you'd enter at ghci.  If not, it is normal Haskell source code!
</aside>


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

-- alternatively
ageFromId = inMaybe age . personFromId
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
can write `fmap f x` as `f <$> x`, which is meant to look similar to `f $ x`:

~~~haskell
λ: addThree $ 7
10
λ: addThree <$> Just 7
Just 10
λ: addThree <$> Nothing
Nothing
~~~

(If you had forgotten, `f $ x` = `f x`)



'Pre-lifting'
-------------

Okay, so we now can turn `a -> b` into `Maybe a -> Maybe b`.

That might be nice, but if you scroll up just a bit, you might see that there
are other functions that might be interesting to apply on a `Maybe a`.

What about `halveMaybe :: Int -> Maybe Int`?

Let's say I have a `Maybe Int` already...maybe something I got from using
`divideMaybe`.

Can I use `halveMaybe` on my `Maybe Int`?

~~~haskell
λ: let x = divideMaybe 12 3     -- x = Just 4 :: Maybe Int
λ: halveMaybe x
<< SCARY ERROR! >>
<< halveMaybe takes an Int but you gave it  >>
<< a Maybe Int.  Think about your life.     >>
~~~

Oh no!  Maybe we can't really stay inside our `Maybe` world after all!

This might be important!  Let's imagine this trip down our world of
uncertainty --- let's say we wanted a function `halfOfAge`

~~~haskell
halfOfAge :: ID -> Maybe Int
~~~

That returns (possibly), half of the age of the person corresponding to that
ID (and `Nothing` if the person looked up has an odd age.  Because odd ages
don't have halves, of course.).  Well, we already have `ageFromId :: ID ->
Maybe Int`, but we want to apply `halveMaybe` to that `Maybe Int`.  Bu we
can't!  Because `halveMaybe` only works on `Int`!

And we can't even use `fmap`, because:

~~~haskell
λ: :t fmap halveMaybe
fmap halveMaybe :: Maybe Int -> Maybe (Maybe Int)
~~~

Wrong wrong wrong!  We don't want a `Maybe Int -> Maybe (Maybe Int)`, we want
a `Maybe Int -> Maybe Int`!  `fmap` lifts "both sides" of the function, but we
only want, in this case, to "lift" the input.

This is a disaster!

But wait, calm down.  We have overcome similar things before.  With our recent
journey to `Functor` enlightenment in mind, let's try to look for a similar
path.

We had an `a -> b` that we wanted to apply to a `Maybe a`, we used `fmap` to
turn it into a `Maybe a -> Maybe b`.

So we have a `a -> Maybe b` here that we want to apply to a `Maybe a`.

The plan is simple!  We turn an `a -> Maybe b` into a `Maybe a -> Maybe b`.
Let's pretend we had such a function.

~~~haskell
preLift :: (a -> Maybe b) -> (Maybe a -> Maybe b)
~~~

How should we expect this to behave?

Well, let's think this through case-by-case.

If we want to apply `halveMaybe` to a number that isn't there...well...it
should also return a number that isn't there.  It should propagate the
not-thereness.

If we want to apply `halveMaybe` to a number that *is* there...well, just
apply it to that number, and take that result!  If the result is there, then
you have a result there.  If the result is not there, then you don't.

We have enough to write this out ourselves:

~~~haskell
preLift :: (a -> Maybe b) -> (Maybe a -> Maybe b)
preLift f = go
  where
    go Nothing  = Nothing
    go (Just x) = f x
~~~

~~~haskell
λ: :t preLift halveMaybe
Maybe Int -> Maybe Int
λ: let x = divideMaybe 12 3     -- x = Just 4 :: Maybe Int
λ: (preLift halveMaybe) x
Just 2
λ: let y = divideMaybe 12 0     -- y = Nothing :: Maybe Int
λ: (preLift halveMaybe) y
Nothing
~~~

Neat!  Now we don't have to fear `a -> Maybe b`'s...we can use them and *still
stay in our world*, without leaving our world of uncertainty!

### Monad

Like with `Functor` and `fmap`, this general pattern of turning an `a -> f b`
into an `f a -> f b` is also useful to generalize.

We say that if a world has such a way of "pre-lifting" a function (plus some
other requirements), it implements the `Monad` typeclass.

Now, you may or not have known this, but Monads have a...reputation.  You
might have heard that Monads were super scary and intimidating.  And you might
have tried to "get" Monads.  Well, search no more, it's that simple.

`Monad` is a typeclass (which is kinda like an interface), so that means that
if `Maybe` is a `Monad`, it "implements" that way to turn a `a -> Maybe b`
into a `Maybe a -> Maybe b`.

We call this `(a -> Maybe a) -> (Maybe a -> Maybe b)` function `bind`.

<aside>
    ###### Aside

The "other thing" that `Monad` has to have (the other thing that the
"interface" demands, besides `(=<<)`) is a way to "bring a value into your
world".

This function is called `return`.

For example, for `Maybe`, we need a way to take a normal value like an `Int`
and "bring it into" our world of uncertainty --- an `Int -> Maybe Int`.

For `Maybe`, semantically, to bring something like `7` into the world of
uncertainty...well, we already know the `7` is there.  So to bring a `7` into
`Maybe`, it's just `Just 7`
</aside>

Now, embarrassingly enough, `bind` actually isn't called `bind` in the standard
library...it actually only exists as an operator, `(=<<)`.

(Remember how there was an operator form of `fmap`?  We have both `fmap` and
`(<$>)`?  Well, in this case, we *only* have the operator form of `bind`,
`(=<<)`. Yeah, I know.  But we live with it just fine!).

`(=<<)` is exactly our `preLift` for `Maybe`.  Let's try it out:

~~~haskell
λ: :t (=<<) halveMaybe
Maybe Int -> Maybe Int
λ: let x = divideMaybe 12 3     -- x = Just 4 :: Maybe Int

-- use it as a prefix function
λ: (=<<) halveMaybe x
Just 2
λ: let y = divideMaybe 12 0     -- y = Nothing :: Maybe Int

-- use it as an infix operator
λ: halveMaybe =<< y
Nothing
~~~

And now maybe we can finally rest easy knowing that we can "stay inside
`Maybe`" and never have to leave it.

### Haskellers are weird

Now, for some strange reason, it is actually much more popular to use `(>>=)`
over `(=<<)`; `(>>=)` is just `(=<<)` backwards:

~~~haskell
λ: halveMaybe =<< Just 8
Just 4
λ: Just 4 >>= halveMaybe
Just 2
~~~

This is really weird!  I mean...really *really* weird!  Why would you ever put
the function you are applying *after* the value you are applying it to?
That's like having `x :: a` and `f :: a -> b`, and doing `x f` or something!

Why is this style the norm?  Who knows! [^whoknows]  People are just weird!

[^whoknows]: I know!  And I'm not telling!  Just kidding.  The main reason is
    that code ends up looking more "imperative"...imagine `divideMaybe 12 3
    >>= halveMaybe >>= halveMaybe` versus `halveMaybe =<< halveMaybe =<<
    divideMaybe 12 3`.  The former looks like you divide 12 by 3, then halve
    it, then halve it again...kind of like Unix pipes.  The latter looks like
    normal function application.

    Believe it or not, usage of Monads was originally motivated by structuring
    IO actions.  So, in that setting, it seemed natural to have an
    "imperative-y" feel.

    Also, in the popular "do notation" syntactical sugar, `(>>=)` is used in
    the desugaring and not `(=<<)`, so `(>>=)` pops out naturally when
    reasoning about Monads coming through do notation.

    Still, I personally feel like the prevalence of `(>>=)` over `(=<<)` is a
    rather unfortunate historical accident; almost all other code in Haskell
    is read "right to left" (like `f . g . h`), so having a "left to right"
    operator just seems to throw everything out of whack.

For the rest of this article, we will be using `(=<<)`; just be aware that you
might see `(>>=)` out in the wild more often!

Recap
-----

Thanks to `Functor` and `Monad`, we now have a way to confidently stay in our
world of uncertainty and still use normal functions on our uncertain values
--- we only have to use the right "lifters".

If you have an `x :: Maybe a` and you have a:

*   `f :: a -> b`, then use `fmap` or `(<$>)` --- `fmap f x` or `f <$> x`

*   `f :: a -> Maybe b`, then use `(=<<)` --- `f =<< x`

Armed with these two, you can comfortably stay in `Maybe` without ever having
to "get out of it"!

<aside>
    ###### Aside

Between `Functor` and `Monad`, there is another useful typeclass called
`Applicative` that lets you "merge" values in worlds together.

That is, if you had a matchmaking algorithm that calculates the compatibility
of two `Person`s, `compatibility :: Person -> Person -> Double`.

`compatiblity` can be described as a function that takes two `Person`s and
"squashes" them into one `Double`.

What if we wanted a `compatibiliy` that takes two `Maybe Person`s and
"squashes" them into one `Maybe Double`?

For that, we have the "squashing" typeclass, `Applicative`, which allows us
two arbitrarily combine two or three or as many `Maybe a`s as we want!
(Because `Maybe` implements/instances `Applicative`)

In our case, we can use `liftA2`, which does exactly what we wanted for
`compatibility`: `liftA2 :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)`.

~~~haskell
λ: personFromId 144
Just John
λ: personFromId 15
Just Sara
λ: personFromId 73
Nothing
λ: compatibility John Sara
82.3
λ: (liftA2 compatibility) (personFromId 144) (personFromId 15)
Just 82.3
λ: (liftA2 compatibility) (personFromId 144) (personFromId 73)
Nothing
~~~

In the last case, obviously there is no value of compatibility between a
person that exists and a person that doesn't exist.

The `Applicative` typeclass is a rather powerful and amazing typeclass, and
there is a [great tutorial on it here][adit] on all of its nuances.

[adit]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

The main *point* is that `Applicative` lets us take *two or more* "values" in
our worlds, and combine them into one value, all staying "inside" the world.
And staying "inside the world" is what this post is all about, isn't it?

For the purposes of this article, we will mostly be considering `liftA2` and
`liftA3`, to sort of "fill out" that list of normal functions that we have
ways to turn "into" our world-y functions: `a -> b`, `a -> Maybe b`, and now
`a -> b -> c` and `a -> b -> c -> d`!
</aside>

Other Worlds
------------



