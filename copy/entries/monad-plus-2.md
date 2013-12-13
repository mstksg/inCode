Practical Fun with Monads --- The List MonadPlus
================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
Series
:   MonadPlus: Success/Failure Monads
:   Practical Monads
CreateTime
:   2013/12/11 21:29:37
PostDate
:   Never
Identifier
:   monad-plus-2

Part two of an exploration of a very useful design pattern in Haskell known as
MonadPlus, a part of an effort to make "practical" monads less of a mystery
and fun to the good peoples of this earth.

When we last left off on the [MonadPlus introduction][intro], we understood
that there are times when you want to chain functions on objects in a way that
"resembles" a failure/success process.  We did this by exploring the most
simple of all MonadPlus's: a simple "dumb" container for a value is either in
a success or a failure.  We looked at how the MonadPlus design pattern really
"behaved".

[intro]: http://blog.jle.im/entry/practical-fun-with-monads-introducing-monadplus

This time we're going to look at another MonadPlus --- the List.  By the end
of this series we're going to be using nothing but the list's MonadPlus
properties to solve this classic logic problem:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport
> across a river.  Unfortunately, his only boat can carry one thing at a time.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat.
> He can't leave the goat alone with the cabbage, or the goat will eat the
> cabbage.  How can he properly transport his belongings to the other side one
> at a time, without any disasters?

Let's get to it!

### MonadWhat? A review

Let's take a quick review!  Remember, a monad is just an object where you have
defined a way to chain functions inside it.  One type of object that is useful
to chain stuff onto are containers.  It's useful to repeatedly apply a
function (returning a container) to the thing inside the containers, instead
of on the container itself.

You'll find that you can be creative this "chaining" behavior, and for any
given type of object you can definitely define more than one way to "chain"
functions on that type of object.  One "design pattern" of chaining is
MonadPlus, where we use this chaining to model success/failure.

*   `mzero` means "failure", and chaining anything onto a failure will still be
   a failure.
*   `return x` means "succeed with `x`", and will return a "succesful" result
    with a value of `x`.

You can read through the [previous article][intro] for examples of seeing
these principles in action and in real code.


The List Monad
--------------

When I say "list monad", I mean "one way that you can implement chaining
operations on a list".  To be more precise, I should say "haskell's default
choice of chaining method on lists".  There is no "the list monad"...there is
"a way we can make *list* a monad".

And one way we can do it?  We saw it before --- yup!  We can model lists as a
MonadPlus --- a method of chaining that revolves around successes and
failures.

Don't believe me?  Let's take the exact same `halve` function...but instead of
returning a `Maybe Int`, we returned a list of `Int`s:

~~~haskell
halve :: Int -> [Int]
halve n = do
    x <- return n
    guard $ even x
    return $ x `div` 2
~~~

This is...the exact same function.  We didn't do anything but change the type
signature.  But because you believe me when I say that List is a
MonadPlus...this should work, right?  `guarad` should work for any MonadPlus.

How is list a meaningful MonadPlus?  Simple: a "failure" is an empty list.  A
"success" is a non-empty list.

Watch:

~~~haskell
λ: halve 8
[4]
λ: halve 7
[]
λ: halve 8 >>= halve
[4]
λ: halve 7 >>= halve
[]
λ: halve 32 >>= halve >>= halve >>= halve
[2]
λ: halve 32 >>= (\_ -> empty) >>= halve >>= halve >>= halve
[]
~~~

Oh my goodness.  `Nothing` is just `[]`...`Just a` is now just `[a]`.  It's al
so clear now.  Why does `Maybe` even exist?  What an outrage!  This whole
time!  It's all a lie!

In fact, if we generalize our type signature for `halve`, we can do some crazy
things...

~~~haskell
genericHalve :: MonadPlus m => Int -> m Int
genericHalve n = do
    x <- return n
    guard $ even x
    return x
~~~

~~~haskell
λ: genericHalve 8 :: Maybe Int
Just 4
λ: genericHalve 8 :: [Int]
[4]
λ: genericHalve 7 :: Maybe Int
Nothing
λ: genericHalve 7 :: [Int]
[]
~~~

<aside>
    ###### Aside

When we say something like `genericHalve 8 :: Maybe Int`, it means "I want
`genericHalve 8`...and I want the type to be `Maybe Int`."  This is necessary
here becuase in our `genericHalve` can be *any* MonadPlus, so we have to tell
ghci which MonadPlus we want.
</aside>

So there you have it. Maybe and lists are one and the same.  Lists *do* too
represent the concept of failure and success.  So...what's the difference?

### A List Apart

Lists can model failure the same way that Maybe can.  But it should be
apparent that list can model success...very interestingly.

Consider `[3, 5]`.  Clearly this is to represent some sort of "success".  But
what?

How about we look at it this way: `[3, 5]` represents two separate *paths* to
success.  When we look at a `Just 5`, we see a computation that succeeded with
a 5.  When we see a `[3, 5]`, we may interpret it as a computation that had
two possible succesful paths: one succeeding with a 3 and another with a 5.

You can also say that it represents a computaiton that *could have chosen* to
succeed in a 3, or a 5.  In this way, the list monad is often referred to as
the "choice" monad.

This view of a list as a collection of possible successes or choices of
successes is not the only way to think of a list as a monad...but it is the
way that the Haskell community has adopted as arguably the most useful.  (The
other main way is to approach it completely differently, making list not even
a MonadPlus and therefore not representing failure or success at all)

Think of it this way: A value goes through a long and arduous journey with
many choices and possible paths and forks.  At the end of it, you have the
result of every path that could have lead to a success.  Contrast this to the
`Maybe` monad, where a value goes through this arduous journey, but never has
any choice.  There is only one path --- succesful, or otherwise.  A `Maybe` is
deterministic...a list provides a choice in paths.

Let's take a simple example: `halveOrDouble`.  It provides two succesful paths
if you are even: halving and doubling.  It only provides one choice or
possible path to success if you are odd: doubling.  In this way it is slightly
racist.

~~~haskell
halveOrDouble :: Int -> [Int]
halveOrDouble n | even n    = [n `div` 2, n * 2]
                | otherwise = [n * 2]
~~~

~~~haskell
λ: halveOrDouble 6
[3, 6]
λ: halveOrDouble 7
[  14]
~~~

As you can see in the first case, with the 6, there are two paths to success:
the halve, and the double.  In the second case, with the 7, there is only one
--- the double.

How about we subject a number to this halving-or-doubling journey twice?  What
do we expect?

1.  The path of halve-halve only works if the number is divisible by two
    twice.  So this is only a succesful path if the number is divisible by
    four.
2.  The path of halve-double only works if the number is even.  So this is
    only a succesful path in that case.
3.  The path of double-halve will work in all cases!  It is a success always.
4.  The path of double-double will also work in all cases...it'll never fail
    for our sojourning number!

So...halving-or-doubling twice has two possible succesful paths for an odd
number, three succesful paths for a number divisible by two but not four, and
four succesful paths for a number divisible by four.

Let's try it out:

~~~haskell
λ: return 5 >>= halveOrDouble >>= halveOrDouble
[       5, 20]
λ: return 6 >>= halveOrDouble >>= halveOrDouble
[    6, 6, 24]
λ: return 8 >>= halveOrDouble >>= halveOrDouble
[ 2, 8, 8, 32]
~~~

The first list represents the results of all of the possible succesful paths 5
could have taken to "traverse" the dreaded `halveOrDouble` landscape twice ---
double-halve, or double-double.  The second, 6 could have emerged succesful
with halve-double, double-halve, or double-double.  8 had the succesful paths
it could have taken.

Let's look at this in the do notation form to offer some possible insight:

~~~haskell
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do
    x <- return n
    y <- halveOrDouble x
    z <- halveOrDouble y
    return z
~~~

Do notation describes **a single path of a value**.  This is slightly
confusing at first.  But look at it --- it has the exact same form as a
Maybe monad do block.

This thing describes, in general terms, the path of a single value.  `x`, `y`,
and `z` are not lists --- they represent a single value, in the middle of the
treacherous journey.

Here is an illustration, tracing out "individual paths":

~~~haskell
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do       -- halveOrDoubleTwice 6
    x <- return n               -- x =              Just 6
    y <- halveOrDouble x        -- y =      Just 3          Just 12
    z <- halveOrDouble y        -- z = Nothing  Just 6  Just 6  Just 24
    return z                    --     Nothing  Just 6  Just 6  Just 24
~~~

where you take the left path if you want to halve, and the right path if you
want to double.

Remember, just like in the Maybe monad, the `x`, `y`, and `z` represent the
*value* inside the object --- `x` represents the 6, `y` represents either the
3 or the 12, depending on what path you take.  This binding of `x`, `y`, or
`z` remains the same throughout the remainder of the path.

Here is the tricky part: the last line, `return z`, returns **what `z` is on
that path**.  In the halve-double path, `z` is 6.  In the `double-double`
path, `z` is 24.

What if we had typed `return y` instead of `return z`?

~~~haskell
halveOrDoubleDance :: Int -> [Int]
halveOrDoubleDance n = do       -- halveOrDoubleDance 6
    x <- return n               -- x <-             Just 6
    y <- halveOrDouble x        -- y <-     Just 3          Just 12
    z <- halveOrDouble y        -- z <- Nothing Just 6  Just 6  Just 24
    return z                    --      Nothing Just 3  Just 12 Just 12
~~~

~~~haskell
λ: halveOrDoubleDance 6
[    3,12,12]
λ: halveOrDoubleDance 7
[      14,14]
λ: halveOrDoubleDance 8
[ 4, 4,16,16]
~~~

![*halveOrDoubleDance 6*, all journeys illustrated](/img/entries/monad-plus/halvedouble.png "halveOrDoubleDance 6")

Huh.  What happened here?

Again, there are four possible paths/journies...only three of them end in
success.  In the halve-halve path...it fails.  Now let's see what happens in
the "halve-double" path.  In this case, it might be useful to look at the
corresponding Maybe do-block, and using the choices we make explicitly:

~~~haskell
halveOrDoubleDance' :: Int -> Maybe Int
halveOrDoubleDance' n = do      -- halveOrDoubleDance' 6
    x <- return n               -- Just 6
    y <- halve x                -- Just 3
    z <- double y               -- Just 6  (double n = Just n)
    return y                    -- Just 3
~~~

It is clear in this case that `return y` will give you the value of `y` **on
that path**.

In our halve-double path, the value of `y` (which is bound on the second line)
is 3.  That's why when we say `return y`, it is `[3]`.

Remember --- you have to treat everything as its own individual path.  In the
halve-double path, `y` is 3.  So `return y` returns 3.

### Solving real-ish problems

Okay, we are *almost* ready to finally implement our solution to the
Wolf/Goat/Cabbage puzzle.  Just one more demonstration.

Let's try this somewhat practical question:

"What operations on a number will make it a multiple of three?"

~~~haskell
isMultThree :: Int -> Bool                              -- 1
isMultThree a = a `mod` 3 == 0

testNumber :: Int -> [String]
testNumber n = do
    x <- return n                                       -- 2
    (f, fName)  <-  [ ((*2)         , "times two")      -- 3
                    , ((*3)         , "times three")
                    , ((+2)         , "plus two")
                    , ((+3)         , "plus three")
                    , ((^2)         , "square")
                    , ((+1).(^2)    , "square plus 1")
                    , ((+1).(^3)    , "cube plus 1")
                    , (id           , "stay the same")
                    ]
    let z = f x                                         -- 4

    guard $ isMultThree z                               -- 5
    return fName                                        -- 6
~~~

~~~haskell
λ: testNumber 4
["times three", "plus two"]
λ: testNumber 5
["times three", "cube plus 1"]
λ: testNumber 6
["times two", "times three", "plus three", "square", "stay the same"]
λ: testNumber 7
["times three", "plus two"]
λ: testNumber 8
["times three", "cube plus 1"]
~~~

Let's go over this step-by-step:

1.  First of all, define the utility function `isMultThree a`, which is true
    when `a` is a multiple of three and false when it isn't.
1.  In the block, `x` is set to be a choice in the journey.  This choice is
    always going to be `n`, but if we wanted to test multiple numbers, we
    could do something like `x <- [n, n+1, n+2]`.
2.  Now, the journey digerges.  `f` and `fName` is now a value that depends on
    the path we took.  If we took the first path, `f = (*2)` (the doubling
    function) and `fName = "times two"`.  On the second path, `f = (*3)` (the
    tripling function) and `fName = "times three"`, etc.
3.  We alias `z` to be the function we chose applied to `x`.  If we had chosen
    the path `f = (*2)`, `z` would be `(*2) x`, which is `x*2`.
4.  We check if `z` is a multiple of three.  If it isn't, the journey sadly
    ends here.  For example, if we called the function with `n = 4`, and we
    had chosen `f = (^2)` (the square function), this journey (involving the
    choice of `(^2)`) would meet its failure here...but the journey with the
    choice `f = (+2)` would not!
5.  At the end of the weary journey, we return the name of the function we
    chose.  This step is never reached for failed journeys.

Here is a diagram!

![*testNumber 5*, all journeys illustrated](/img/entries/monad-plus/testnumber.png "testNumber 5")
