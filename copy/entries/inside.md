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
3.  The world of *State*, in which things are things that will be computed in
    the future.
4.  The world of *IO*, in which things are things that will be computed by a
    CPU, which can react to the outside world during that process.

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
divideMaybe :: Int -> Int -> Maybe Int
divideMaybe _ 0 = Nothing
divideMaybe x y = Just (x `div` y)

headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

halveMaybe :: Int -> Maybe Int
halveMaybe x | x `mod` 2 == 0 = Just (x `div` 2)
             | otherwise      = Nothing
~~~

When you want to return a value of type `Maybe a`, you can either return `Just
x` or `Nothing` --- they both are members of type `Maybe a`.

`divideMaybe` is a function that takes two integers and returns --- maybe ---
their integer quotient.  If the second number is a zero, return `Nothing`.
Otherwise, return a `Just` containing the integer quotient.

`headMaybe` is a function that takes a list and returns --- maybe --- the
first element.  If given an empty list (`[]`), it returns `Nothing`.  If given
a list with a first element (the pattern `x:xs` matches `x` to the first
element and `xs` to the rest of the list, but we ignore `xs`), it returns a
`Just` containing the first element.

`halveMaybe` is a function that takes an integer and returns --- maybe ---
half of it.  If the integer is even, then it returns its half in a `Just`; if
it's not, it returns `Nothing`.

That's the type --- `halveMaybe :: Int -> Maybe Int`.  `halveMaybe` takes an
`Int` and returns a `Maybe Int` --- "an integer might possibly be there".

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
-- > addThree takes an `Int` but you gave it a `Maybe Int`.  What are you
-- >   trying to do anyway, wise guy.
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

There are a lot of reasons why.

Let's say you had a function that looked up a person from a database given
their ID number.  But not all ID numbers have a person attached, so the
function might fail and not lookup anyone.

~~~
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
soon as we get a `Maybe Person`.  Our entire answer is shrowded in
uncertainty, so we need to stay inside this world.

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
a function on an `a` that is possibly there/not there, to return --- well, a `b` that is
possibly there/not there!

Basically, I want to move my `a -> b` into *my world of uncertainty*.  So
that I can use *any* `a -> b` as if it were written for uncertain values
this entire time.

If you look at this carefully, we want some sort of "function transformer".
Give our transfomer an `a -> b`, it'll output a new function `Maybe a -> Maybe
b`.

We want a function of type `(a -> b) -> (Maybe a -> Maybe b)`

Let's pretend we have one!

~~~haskell
inMaybe :: (a -> b) -> (Maybe a -> Maybe b)
~~~

~~~haskell
λ: let addThreeInMaybe = inMaybe addThree
λ: addThreeInMaybe (Just 7)
Just 10
λ: addThreeInMaybe Nothing
Nothing
λ: (inMaybe square) (Just 9)    -- equivalent to inMaybe square (Just 9), the
                                -- parentheses are syntactically redundant
Just 81
λ: inMaybe showInt Nothing
Nothing
λ: inMaybe showInt (Just 8)
Just "8"
~~~

Wow!  If I had this, I can now use normal functions and still stay inside my
uncertain world.

We can even write our `ageFromId`:

~~~haskell
ageFromId :: ID -> Maybe Int
ageFromId i = inMaybe age (personFromId i)
~~~

We can write out `inMaybe` ourselves:

~~~haskell
inMaybe :: (a -> a) -> (Maybe a -> Maybe b)
inMaybe f = go
  where
    go (Just x) = Just (f x)
    go Nothing  = Nothing
~~~

Now we are no longer afraid of dealing with uncertainty.  It's a scary realm,
but as long as we have `inMaybe`...all of our normal tools apply!

~~~haskell
λ: let x = headMaybe [2,3,4]
λ: let y = inMaybe square x
λ: y
Just 4
λ: let z = inMaybe addThree y
λ: z
Just 7
λ: inMaybe (> 5) z
Just True
λ: let x' = halveMaybe 7
λ: let y' = inMaybe square x'
λ: let z' = inMaybe addThree y'
λ: inMaybe (> 5) z
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
  fmap :: (a -> b) -> f a -> f b
~~~

If you haven't done much Haskell, this might seem scary, but don't worry.  All
it says that "if your world `f` is a `Functor`, then you have a function
`fmap` that turns any `a -> b` into an `f a -> f b`".

It should come as no surprise that `Maybe` is a `Functor`, so `fmap` *does*
take any function `a -> b` and "lifts" it into a the `Maybe` world, turning it
into a `Maybe a -> Maybe b`.

`fmap` for `Maybe` is incidentally exactly our `inMaybe`.

~~~haskell
λ: fmap square (headMaybe [4,5,6])
Just 16
λ: fmap square (halveMaybe 7)
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

#### What does it "mean"?

We've been staying with `Maybe` so far, but we're going to jump head-first
into generalizing this to different worlds really fast, so it's important to
remember what `fmap` means and doesn't mean.

*All fmap does* is take a function and turn it into a function that works
"inside the Functor"; inside your world.  It takes an `a -> b` and turns it
into an `f a -> f b`.

What does this actually mean, "semantically"?

This is *completely up to the world/instance to decide*.

For `Maybe`, we used the *semantics* that "fmapping will apply the function to
the value if there is one."

For a different world...what does it mean to apply a function "inside that
world"?

It's up to that instance!  You get to pick whatever meaning/implementation
"makes the most sense" in your world.









[iopure]: http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity
