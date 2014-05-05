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
3.  The world of *State s*, in which things are things that will be computed
    in the future.
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

(in `Data.Maybe`, `certaintify` is `fromJust`, and `certaintifyWithDefault` is
`fromMaybe`)

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
soon as we get a `Maybe Person`, and then "re-enter" it somehow when you
return the `Maybe Int`.  Our entire answer is shrowded in uncertainty, so we
need to stay inside this world the entire time.

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
inMaybe :: (a -> a) -> (Maybe a -> Maybe b)
inMaybe f = go
  where
    go (Just x) = Just (f x)
    go Nothing  = Nothing
~~~

Now we are no longer afraid of dealing with uncertainty.  It's a scary realm,
but as long as we have `inMaybe`...all of our normal tools apply!

~~~haskell
λ: let x = headMaybe [2,3,4]    -- x = Just 2
λ: let y = inMaybe square x     -- y = Just 4
λ: let z = inMaybe addThree y   -- z = Just 7
λ: inMaybe (> 5) z
Just True
λ: let x' = halveMaybe 7        -- x' = Nothing
λ: let y' = inMaybe square x'   -- y' = Nothing
λ: let z' = inMaybe addThree y' -- z' = Nothing
λ: inMaybe (> 5) z'
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

Worlds of all sorts
-------------------

Let's look at a couple of other worlds we could exist in, and what it would
look like to move normal functions into them.

### Superpositions of possibilities

We looked at `Maybe`, which represents the return type of a function that
could possibly fail.  But let's look at the other end --- what about functions
that can possibly return multiple answers?

~~~haskell
evenUnder :: Int  -> [Int]
evenUnder n = filter even [1..(n-1)]

invSquare :: Double -> [Double]
invSquare x = [-sqrt x, sqrt x]

factorOf :: Int -> [Int]
factorOf n = filter ((== 0) . (n `mod`)) [1..n]
~~~

The idea is that when I call `evenUnder 8`, I get an answer that could validly
be 2, 4, or 6.  I enter a world of nondeterminism/superposition.  When I stay
in that world, I have to deal with the fact that my computation has multiple
possible answers.

~~~haskell
λ: evenUnder 8      -- what is an even number under 8?
[2,4,6]             -- (i'm giving you back a 2, 4, or 6 as possibilities)
~~~

It's easy to see how you can "exit" your world of superposition.  We already
made a funtion earliear that transported you from the world of superposition
to the world of uncertainty --- from there, we can exit completely.

~~~haskell
λ: headMaybe (factorOf 6)
Just 1
λ: certaintifyWithDefault 0 (headMaybe (factorOf 6))
1
~~~

(Interestingly enough, `headMaybe` is in the standard library in `Data.Maybe`
as `listToMaybe`)

But let's stay in this world.  Maybe in the end, we want to preserve the
nondeterminate properties of our computation.  For example...let's say we have
a function that looked up people by their favorite food, and that function
from person to age.  I want to write a function that, when given a food,
returns the age of every person who has it as their favorite food.

~~~haskell
isFavoriteFoodOf :: Food -> [Person]
age              :: Person -> Int
favoriteFoodAges :: Food -> [Age]
~~~

Clearly, we want to stay in nondeterminism/multiple possible answers the
entire time.

So, a type signature of `[a]` means "the result of my computation is many
possible answers."  What we need is a function that can take any ol' `a -> b`,
like `Person -> Int`, and make it work on `[Person]`.  That is, turn an `a ->
b` into a `[a] -> [b]`.

What would that even mean?

Well, if `invSquare 9` could possibly be -3 or 3, and we want to say "The
double of the inverse square"...that computation should yield -6 or 6.
Applying functions to a superposition values is like applying them to every
value.

So `[]` is a `Functor`, so that means that it has such an `fmap :: (a -> b) ->
([a] -> [b])`.

~~~haskell
λ: let doubleInList = fmap (*2)
λ: :t doubleInList
[Int] -> [Int]
λ: let x = invSquare 9      -- x = [-3,3]
λ: doubleInList x
[-6,6]
λ: fmap addThree x
[0,6]
~~~

And so we can say

~~~haskell
favoriteFoodAges :: Food -> [Age]
favoriteFoodAges food = (fmap age) (isFavoriteFoodOf food)
~~~

Where we take our previous `age` function and bring it into "the world of
multiple `People`".

### The world of awaiting

Here's an interesting one.

In Haskell, we have a `Reader s` world.  You can think of `(Reader s) a` as a
little machine that "waits" for something of type `s`, then *uses* it to make
an `a`.

~~~haskell
λ: :t lengthReader
lengthReader :: (Reader [x]) Int
λ: runReader lengthReader [1,2,3]
3
λ: :t oddReader
oddReader :: (Reader Int) Bool
λ: runReader oddReader 6
False
λ: runReader oddReader 5
True
~~~

So if I have a `(Reader Int) Bool`, I have a `Bool` that "lives" in the `Reader
Int` world --- it is an ephemereal `Bool` perpetually awaiting an
`Int` in order to be realized and found.  It's a `Bool` *waiting to be
produced* --- all it needs is some `Int`.

Back to our database analogy, we can have a `Reader ID` world, a world where
all of its values are not there yet...they are in the future; they are
*waiting* for an `ID` to be able to realize themselves.

Let's say I have `personReader :: (Reader ID) Person` --- a future Person living
in the world of awaiting --- but I want `ageReader :: (Reader ID) Int`.  I
want to give someone an *age* that is just waiting for an `ID`.

Well, no fear!  Because `Reader ID` is a `Functor`, we can move the same old
`age :: Person -> Int` function that we have been using all along, into our
world of awaiting!

We can turn a `Person -> Int` into a `(Reader ID) Person -> (Reader ID) Int`.

~~~haskell
λ: runReader personReader 108
Jason
λ: age Jason
37
λ: let ageReader = (fmap age) personReader
λ: :t ageReader
ageReader :: (Reader ID) Person
λ: runReader ageReader 108
37
~~~

The *semantics* of `fmap` for `Reader Int` is that it applies that function to
the future value, once it has been obtained.  That is, once you get a `Person`
from the `ID`, it applies the function `age` to that `Person` before finally
popping it out.

Now we can move all of our functions into the world of awaiting!

### The world of state changers

Now, the world of awaiting might seem a little boring or uninteresting.  (And
you might have guessed that `(Reader s) a` is just a fancy wrapper around a
function `s -> a`)

But here's a slight twist to it.  Let's have a little machine `(State s) a`,
where it *awaits* an `s` (like before) --- but when it receives it, it pops
out not just a resulting `a` but a *modified `s`*.

~~~haskell
λ: :t popList
popList :: (State [a]) a
   -- pass the state `[8,1,5]` to the `popList` machine
λ: let (result, newstate) = runState popList [8,1,5]
λ: result
8
λ: newstate
[1,5]
~~~

So we say this: `(State [Int]) Int` is a `Int` that lives in the world of
`State [Int]` --- it is awaiting a list of `Int`s before it can be fully
realized or known, but it also returns a new modified `Int` that is modified
as a result of its "processing".

For `popList`, it is basically an `a` that is waiting to be realized if just
given an `[a]`.  It removes the first element in the list, and that is the
result --- the list has been maimed.  In the process of "realizing" the `a`,
the list suffers a casualty/change.

The `State s` world is a world of values waiting to be produced by an `s`, but
alter the `s` in the process.

Let's say we want have our `popList :: (State [Int]) Int` (for `Int`s).  How can
we stay "inside our state world" and give a `popIsEven :: (State [Int]) Bool`,
where it takes the first item off of the list and then tells if it is even or
not?

You got it --- `fmap`.

~~~haskell
λ: let (x,newstate) = runState popList [3,8,10] -- x        = 3
                                                -- newstate = [8,10]
λ: let popIsEven = fmap even poopList
λ: :t popIsEven
popIsEven :: (State [Int]) Bool
λ: runState popIsEven newstate
(True, [10])
~~~

I think you get the picture at this point.  If I wanted to supply a `(State s)
b` (a machaine that popes out a `b`) but I only had a `(State s) a` (a machine
that pops out an `a`) and an `a -> b`, I could turn my `a -> b` into a `(State
s) a -> (State s) b` using `fmap`, because `State s` is a `Functor`!

I can apply normal functions and still remain in my
state-changing-result-realizing-machine world!

### I am IO

And then there is perhaps one of the more "infamous" worlds.  There is the
`IO` world.

But really, the `IO` world is a lot like the `Reader s` world!  Remember that
`Reader s Int` represents an `Int` living in a world that is waiting for an
`s` (the world of "awaiting").  `(Reader s) a` basically represents a
yet-to-be-realized `a` --- all you have to do is "run" it with that `s`.

In the same way, an `IO a` represents a yet-to-be-realized `a`.  It represents
*a computer routine that computes an `a` when run*.

An `IO a` is an object, like `(Reader s) a`, that, when compiled by the
appropriate compiler and then "run" by a computer, yields an `a`.  But only
when it is eventually run by the computer.

You can almost think of `IO a` as a literal chunk of machine code/C code that,
when run, will produce your `a`.

(Like for `(Reader s) a`, the `a` doesn't "exist" anywhere, *yet*.  But it
does after you run the `Reader`, in the future.  For `IO a`, the `a` doesn't
"exist" anywhere, yet.  The `a` is what that `IO` world promises to generate
when executed by a computer.)

This `IO` world --- world of things where you only describe *how* to produce
the thing (if you were a CPU) --- is unique among the ones we have looked at
before in that there is *no way to exit this world* in Haskell.

That is, where is no sort of meaningful function `IO a -> a`.  We had many
ways to "get out of" our previous worlds (`certaintify`, `runReader` and
`runState`, for example) and work with the naked values after they exit.


This kind of makes sense what you think about it.  If `IO a` is a little
chunk of machine code or an object containing instructions for some computer,
then the only thing that can really "get the `a`" is a computer --- and not a
Haskell program.

Because you can never even meaningfully "exit" the `IO` world if you tried
within a Haskell program, a function like `fmap` is *extremely* handy.

Let's say our database of persons exists on a text file somewhere.  Because
processors and computers are very good at reading text files, `IO` looks like
a pretty nice world to live in.  So let's say we had a function

~~~haskell
lookupPerson :: ID -> IO Person
~~~

that looks up a person from a text file on disk somewhere, by their ID.  And
we want to write a function

~~~hasell
lookupAge :: ID -> IO Age
~~~

Remember, `lookupPerson i` is an object describing a computation that will
return a `Person` when a computer eventually runs it.

We can do some shifting, like we did for `Reader s`, and think of `IO Person`
as "a `Person` that lives in a world of awaiting being computed by a
computer."

We already have our normal `age :: Person -> Int` function, but this is
apparently useless!

~~~haskell
λ: age (lookupPerson 10)
<< SCARY ERROR! >>
-- > age only works on Person, but you gave it an IO Person, a Person that
-- > lives in IO.  You have to rethink your life, buddy.
~~~

But we can turn a `Person -> Int` into an `IO Person -> IO Int`.  Riiight?
Because `IO` is a `Functor`, and we have `fmap`!


~~~haskell
lookupAge :: ID -> IO Age
lookupAge i = (fmap age) (lookupPerson i)
~~~

Hooray `fmap`!  Thanks you you, we can bring normal functions into any world
we like, and we can stay in that world and use normal functions!

Combining Worlds
----------------

Well, maybe.  After a while, you might notice that `fmap` and `Functor` has
some shortcomings.

Let's return to the world of `Maybe`, and our database of people.  Let's say
that you have a matchmaking algorithm that calculates the compatibility
between two people.

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
-- > compatibility takes two Persons, but you gave two Maybe Persons.
-- > do you have no shame?
~~~

That didn't work, but what kind of answer would we have even expected?

Well, if we want to find the compatibility between two people that might not
even exist...the answer *should* be a compatibility that might not exist.

That is:

~~~haskell
compatiblityInMaybes :: Maybe Person -> Maybe Person -> Maybe Double
~~~

~~~haskell
λ: compatibilityInMaybes (personFromId 144) (personFromId 14)
Just 82.3
λ: personFromId 59
Nothing
λ: compatibilityInMaybes (persomFromId 144) (personFromId 59)
Nothing
~~~

For the last answer, the compatibility between someone that does exist and
someone that doesn't exist ... well, doesn't exist!

### Apply Yourself

You might be guessing that, like `Functor` and `fmap`, this is a common design
pattern as well, that might be expanded to different worlds.

And you're right.  It's called `Applicative`.  And instead of `fmap`, we have
a new grab bag of functions that are aimed at *combining values inside
worlds*.

For `Functor`, we have functions aimed at *applying functions inside worlds*.
`Applicative` comes along and lets us *combine values inside worlds*.

For that, we have a handy function called `liftA2`.

~~~haskell
liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
~~~

And because `Maybe` is an `Applicative`, we can jump right away into turning
`compatibility :: Person -> Person -> Double` into a `Maybe Person -> Maybe
Person -> Maybe Double`

~~~haskell
λ: (liftA2 compatibility) (personFromId 144) (personFromId 14)
Just 82.3
λ: (liftA2 compatibility) (personFromId 144) (persomFromId 59)
Nothing
~~~

Using `Applicative`, we were able to combine two `Maybe a` values into one
`Maybe c`.  We were able to "combine" two values inside the same world.

But `Applicative` actually is a little "deeper" than just `liftA2`.  If you
peek into the definition of `Applicative`, you'll see that it doesn't even
mention `liftA2`worlds.:

~~~haskell
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
~~~

You probably could guess the semantics/meaning of `pure` and `(<*>)` when we
think about worlds:

*   `pure` lets us "enter" a world, for free.  For `Maybe`, it took here a
    certain 4, an `Int`, and pushed it into the world of uncertainty, a `Maybe
    Int`.

    The only meaningful implementation in `Maybe` is of course to just wrap
    the value in a `Just` --- a `Maybe` value that is *there*, obviously.

*   `(<*>)` applies the function inside the world on the left hand side to the
    value inside the world on the right hand side, all staying in the world
    the entire time.

Using this, we can write something like `liftA2 f x y` as

~~~haskell
liftA2 :: (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
~~~

Where `(<$>)` is `fmap`.  And we can write `liftA3` as

~~~haskell
liftA3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = f <$> x <*> y <*> z
~~~

<aside>
    ###### Aside

How does this work?  It's not magic.  If we add explicit parentheses, can see
that the above definition of `liftA2` is just

~~~haskell
(f <$> x) <*> y
~~~

And if `f :: a -> b -> c`, and `x :: Maybe a`, then `fmap f x` is `Maybe (b ->
c)` (because of currying).

So if `(f <$> x)` is `Maybe (b -> c)` --- that is, a function that might or
might not exist --- we can use `<*>` to apply that function to our `Maybe b`
to get a `Maybe c`.

If you don't understand this, don't worry; you can sort of just read `f <$> x
<*> y` as "apply `f` 'inside' `x` and `y`", and `f <$> x <*> y <*> z` as
"apply `f` 'inside' x, y, and z".
</aside>


#### Other worlds are Applicatives too

Other worlds are `Applicative`s too.  In fact, I sort of specifically picked
worlds that were both `Functor` and `Applicative`; how clever of me!

##### State Applicative

Combining two `State s` machines has interesting semantics.

What would it even mean to combine two machines that take in and modify a
state, and create a new machine that takes in and modifies a state?

Let's think about something like 

~~~haskell
pop2 :: (State [a]) (a,a)
pop2 = (liftA2 (,)) popList popList
~~~

where `(,)` is the tupling function `a -> b -> (a, b)`.

The type of `pop2` should tell you that it is some sort of state machine that
takes in an `[a]` as a state, and produces an `(a,a)` and pops out a modified
`[a]`.

The *semantics* of "combining" two `State s` machines is that when you
"combine" two machines, you create a new machine that feeds the input state into the
first machine, feeds the modified state of the first machine into the second
machine, and then pops out finally the modified state of the second machine.

<!-- TODO: Add a diagram here -->

So `pop2` should basically take something like `[1,2,3,4]`, give it to the first
`popList`, which would output `[2,3,4]`, which would then be fed into the
second `popList`, which would output `[3,4]`, which would be the final
modified state.

~~~haskell
λ: let (result, newstate) = runState pop2 [1,2,3,4]
λ: newstate
[3,4]
~~~

And what would `result` be?

Well, `popList` "contains" a to-be-realized `Int` (awaiting a state of `[Int]`
to realize it).  So to apply `(,)` "inside" the `State [Int]` world, we would
just take those two to-be-realized `Int`'s...and tuple them.  We create a
to-be-realized `(Int, Int)`!

And that's exactly what `(State [Int]) (Int, Int)` means!

So, `(liftA2 (,)) popList popList` snags up those two to-be-realized `Int`s,
and couples them up into a to-be-realized tuple, `(Int, Int)`.

The big picture --- we can stay in our "to-be-realized" world, and *still* use
our normal functions like `(,)`.  Not only that, we can combine the two
results "living" in the "to-be-realized" world, into one "to-be-realized"
world.

So back to what `result` must be --- the first `popList` pops an item off of
`[1,2,3,4]` and "realizes" a 1.  The second `popList` pops an item off of
`[2,3,4]` and "realizes" a 2.  So `pop2` on `[1,2,3,4]` should snag up those
two "realized" numbers 1 and 2 and realize a `(1, 2)`.

~~~haskell
λ: let (result, newstate) = runState pop2 [1,2,3,4]
λ: result
(1,2)
~~~

Neat.



----











------

[iopure]: http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity
[state]: http://blog.jle.im/entry/streaming-huffman-compression-in-haskell-part-1-trees#the-state-monad
