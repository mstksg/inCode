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
defined a way to chain functions inside it.  You'll find that you can be
creative this "chaining" behavior, and for any given type of object you can
definitely define more than one way to "chain" functions on that type of
object.  One "design pattern" of chaining is MonadPlus, where we use this
chaining to model success/failure.

*   `mzero` means "failure", and chaining anything onto a failure will still be
   a failure.
*   `return x` means "succeed with `x`", and will return a "succesful" result
    with a value of `x`.

You can read through the [previous article][intro] for examples of seeing
these principles in action and in real code.

Without further ado, let us start on the list monad.

Starting the List Monad
-----------------------

Now, when I say "list monad", I mean "one way that you can implement chaining
operations on a list".  To be more precise, I should say "haskell's default
choice of chaining method on lists".  *There is no "the list monad"*...there
is "a way we can make the List data structure a monad".

And what's one way we can do this?  You could probably take a wild guess.
Yup, we can model lists as a MonadPlus --- we can model chaining in a way that
revolves around successes and failures.

So, how can a list mode success/failure?

Let's take a look at last article's `halve` function:

~~~haskell
halve :: Int -> Maybe Int
halve n = do
    guard $ even n
    return $ n `div` 2
~~~

~~~haskell
λ: halve 6
Just 3
λ: halve 7
Nothing
λ: halve 8 >>= halve
Just 2
λ: halve 7 >>= halve
Nothing
~~~

Here, our success/fail mechanism was built into the Maybe container. Remember,
first, it fails automatically if `n` is not even; then, it auto-succeeds with
``n `div` 2`` (which only works if it has not already failed).  But note that
we didn't actually really "need" Maybe here...we could have used anything that
had an `mzero` (insta-fail) and a `return` (auto-succeed).

Let's see what happens when we replace our Maybe container with a list:

~~~haskell
halve' :: Int -> [Int]
halve' n = do
    guard $ even n
    return $ n `div` 2
~~~

This is...the exact same function.  We didn't do anything but change the type
signature.  But because you believe me when I say that List is a
MonadPlus...this should work, right?  `guard` should work for any MonadPlus,
because every MonadPlus has an `mzero` (fail).  `return` should work for any
MonadPlus, too --- it wouldn't be a MonadPlus without `return` implemented!

So, how is list a meaningful MonadPlus?  Simple: a "failure" is an empty list.
A "success" is a non-empty list.

Watch:

~~~haskell
λ: halve' 6
[3]
λ: halve' 7
[]
λ: halve' 8 >>= halve'
[2]
λ: halve' 7 >>= halve'
[]
λ: halve' 32 >>= halve' >>= halve' >>= halve'
[2]
λ: halve' 32 >> mzero >>= halve' >>= halve' >>= halve'
[]
~~~

So there we have it.  `Nothing` is just like `[]`, `Just x` is just like
`[x]`.  This whole time.  It's all so clear now.  Why does `Maybe` even exist,
anyway, when we can just use `[]` and `[x]` for `Nothing` and `Just x` and be
none the wiser?

In fact, if we generalize our type signature for `halve`, we can do some crazy
things...

~~~haskell
genericHalve :: MonadPlus m => Int -> m Int
genericHalve n = do
    guard $ even n
    return $ n `div` 2
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

Hi again; remember, these asides are for people who might be unfamiliar with
Haskel syntax.  If you are comfortable already, feel free to ignore these.

No, when we say something like `genericHalve 8 :: Maybe Int`, it means "I want
`genericHalve 8`...and I want the type to be `Maybe Int`."  This is necessary
here becuase in our `genericHalve` can be *any* MonadPlus, so we have to tell
ghci which MonadPlus we want.
</aside>

So there you have it. Maybe and lists are one and the same.  Lists *do* too
represent the concept of failure and success.  So...what's the difference?

### A List Apart

Lists can model failure the same way that Maybe can.  But it should be
apparent that lists can do a little "more" than Maybe...

Consider `[3, 5]`.  Clearly this is to represent some sort of "success"
(because a failure would be an empty list).  But
what kind of "success" could it represent?

How about we look at it this way: `[3, 5]` represents two separate *paths* to
success.  When we look at a `Just 5`, we see a computation that succeeded with
a 5.  When we see a `[3, 5]`, we may interpret it as a computation that had
two possible succesful paths: one succeeding with a 3 and another with a 5.

You can also say that it represents a computation that *could have chosen* to
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
λ: halveOrDouble 5 >>= halveOrDouble
[       5, 20]
λ: halveOrDouble 6 >>= halveOrDouble
[    6, 6, 24]
λ: halveOrDouble 8 >>= halveOrDouble
[ 2, 8, 8, 32]
~~~

The first list represents the results of all of the possible succesful paths 5
could have taken to "traverse" the dreaded `halveOrDouble` landscape twice ---
double-halve, or double-double.  The second, 6 could have emerged succesful
with halve-double, double-halve, or double-double.  For 8, all paths are
succesful, incidentally.  He better check his privilege.

Let's look at this in the do notation form to offer some possible insight:

~~~haskell
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do
    x <- halveOrDouble n
    halveOrDouble x
~~~

Do notation describes **a single path of a value**.  This is slightly
confusing at first.  But look at it --- it has the *exact same form* as a
Maybe monad do block.

This thing describes, in general terms, the path of a **single value**.  `x`
is **not** a list --- it represents a single value, in the middle of its
treacherous journey.

Here is an illustration, tracing out "individual paths":

~~~haskell
halveOrDoubleTwice :: Int -> [Int]
halveOrDoubleTwice n = do       -- halveOrDoubleTwice 6
    x <- halveOrDouble n        -- x <-     Just 3          Just 12
    halveOrDouble x             --      Nothing  Just 6  Just 6  Just 24
~~~

where you take the left path if you want to halve, and the right path if you
want to double.

Remember, just like in the Maybe monad, the `x` represents the value *inside*
inside the object --- `x` represents **a** 3 or **a** 12 (but not "both" at
the same time), depending on what path you are taking/are "in".  That's why we
can call `halveOrDouble x`: `halveOrDouble` only takes `Int`s and `x` is *one*
`Int` along the path.

Note that once you bind a value to a variable (like `x`), then that is the
value for `x` for the entire rest of the journey.  In fact, let's see it in
action:

~~~haskell
hod2PlusOne :: Int -> [Int]
hod2PlusOne n = do              -- hod2plusOne 6
    x <- halveOrDouble n        -- x <-     Just 3          Just 12
    halveOrDouble x             --      Nothing  Just 6  Just 6  Just 24
    return $ x + 1              --      (skip)   Just 4  Just 13 Just 13
~~~

~~~haskell
λ: hod2PlusOne 6
[4, 13, 13]
~~~

Okay!  This is getting interesting now.  What's going on?  Well, there are
four possible "paths".

1.  In the half-half path, `x` (the result of the first halving) is 3.
    However, the half-half path is a failure --- 6 cannot be halved twice.
    Therefore, even though `x` is three, the path has already failed before we
    get to the `return (x + 1)`.  Just like in the case with Maybe, once
    something fails during the process of the journey, the entire journey is a
    failure.
2.  In the half-double path, `x` is also 3.  However, this journey doesn't
    fail.  It survives to the end.  After the doubling, the value of the
    journey at that point is "Just 6".  Afterwards, it "auto-succeeds" and
    replaces the current value with the value of `x` on that path (3) plus 1
    --- 4.  This is just like how in the Maybe monad, we return a new value
    after the guard.
3.  In the double-halve path, `x` (the result of the first operation, a
    double) is 12.  The second operation makes the value in the journey a 6.
    At the end of it all, we succeed with whatever the value of `x` is on that
    specific journey (12) is, plus one.  13.
4.  Same story here, but for double-double; `x` is 12.  At the end of it all,
    the journey never fails, so it succeeds with `x + 1`, or 13.






<!-- ![*halveOrDoubleDance 6*, all journeys illustrated](/img/entries/monad-plus/halvedouble.png "halveOrDoubleDance 6") -->

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
