Fun with the List Monad and MonadPlus: The Wolf, Goat and Cabbage Puzzle
==========================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
CreateTime
:   2013/12/04 21:43:34
PostDate
:   Never
Identifier
:   list-monad

Monads.  Haskell's famous for them, but they are one of the most
ill-understood concepts to the public.  They are mostly shrouded in mystery
because of their association with how Haskell models I/O.  This reputation is
undeserved.  Monads don't have anything to do with I/O.

This article is a part of a global effort to deshroud the mystery behind
monads and show that they are fun!  And exciting!  And really just a way of
chaining together functions that allow for new ways of approaching puzzles.

We are going to be solving the classic logic puzzle using the List monad:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport
> across a river.  Unfortunately, his only boat can carry one thing at a time.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat.
> He can't leave the goat alone with the cabbage, or the goat will eat the
> cabbage.  How can he properly transport his belongings to the other side one
> at a time, without any disasters?

Let us enter a brave new world!

Monads, Reviewed
----------------

As a Haskell blogger, I'm not allowed to write any straight-up monad
tutorials, but I don't need too --- there are a wealth of great ones.
[Adit provides a great concise one][adit], and, if you want,
[a more in depth one][wiki] is on the haskell.org wiki about using them in
real life.

[adit]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[wiki]: http://www.haskell.org/haskellwiki/All_About_Monads

However, here is a short description of "monadic style", as it applies to what
we are going to do today.

Remember --- different monads do not actually have any non-superficial
relationship to one another.  When we say monads, we just mean things that
chain together functions "inside" wrappers, containers, or contexts.  In our
case, containers.

### Maybe, maybe not

Let's look at the most obvious container -- a `Maybe a`.  A `Maybe a` is a
container that can either be `Just a` (representing a succesful result `a`) or
a `Nothing` (representing a failed result).

<aside>
    ###### Aside

Hi!  These asides are going to be for you readers that are unfamiliar with
Haskell syntax.  Feel free to ignore them if you already feel comfortable.

Anyways, if you've ever done any object-oriented programming, you might be
able to think of `Maybe a` as an abstract/virtual superclass with
templates/generics --- `Maybe<a>`, kinda.  And that superclass has two
subclasses: `Just<a>`, which has one public instance variable of type `a`, and
`Nothing`, which contains no instance variables.
</aside>

Often times you'll have functions that fail, and you want to chain them.  The
easiest way is that any function that is chained onto a failed value will
be skipped; a failure is the final result.

Consider the `halve` function, which returns ``Just (x `div` 2)`` on a
succesful halving, or `Nothing` on an unsuccesful halving:

~~~haskell
halve :: Int -> Maybe Int                       -- 1
halve x | even x    = Just (x `div` 2)          -- 2
        | otherwise = Nothing                   -- 3
~~~

<aside>
    ###### Aside

Hi again!  There are some quick syntax features here.

1.  This first line declares that the type of the function `halve` is `Int ->
    Maybe Int`, which means that it takes in an `Int` and returns a `Maybe
    Int` --- an integer wrapped in a "Maybe" container.
2.  This says that if x is even, then return a succesful ``Just (x `div` 2)``.
    ``x `div` 2`` is basically x divided by two, in case you couldn't guess
    already.
3.  Otherwise, return `Nothing` --- a failure.
</aside>

We can now chain `halve`s on results of other `halves`, and have any failures
automatically propagate to the end and short circuit your entire computation:

~~~haskell
λ: halve 8
Just 4
λ: halve 7
Nothing
λ: halve 8 >>= halve
Just 2
λ: halve 7 >>= halve
Nothing                         -- 1
λ: halve 6 >>= halve
Nothing
λ: halve 6 >>= halve >>= halve
Nothing                         -- 2
λ: halve 32 >>= (\_ -> Nothing)
Nothing                         -- 3
λ: halve 32 >>= halve >>= halve >>= halve
Just 2
λ: halve 32 >>= (\_ -> Nothing) >>= halve >>= halve >>= halve
Nothing                         -- 4
~~~

<aside>
    ###### Aside

In this article, code that begins with `λ: ` represents commands to be entered
at the interactive prompt, ghci.  Code that doesn't is actual source code.
</aside>

Some interesting points:

1.  Note that this command doesn't even bother with the second `halve`.  It
    knows that the end result will be `Nothing` no matter what (because `halve
    7` is `Nothing`), so it just skips right past the second `halve`.
2.  Same thing here; it just skips right past the third `halve`, becuase
    `halve 6 >>= halve` is `Nothing`.
3.  Somewhat interesting.  `(\_ -> Nothing)` is a function that returns
    `Nothing` no matter what.  So chaining that at the end of `halve 32` (a
    `Just 12`) sort of means instant failure no matter what.
4.  Disaterous!  Even though halving 32 four times usually is fine, giving
    `Just 2`, having just one failure along the way means that the entire
    thing is a failure.  Think of it as `Nothing >>= halve >>= halve >>=
    halve`.

### Do Notation

Haskell provides a convenient way of writing chained `>>=`'s called do
notation; here are a few samples matched with their equivalent `>>=` form:

~~~haskell
λ: half 8
Just 4
λ: do
 |     half 8
Just 4

λ: halve 8 >>= halve
Just 2
λ: do
 |     x <- halve 8
 |     halve x
Just 2

λ: halve 32 >>= halve >>= halve >>= halve
Just 2
λ: do
 |     x <- halve 32
 |     y <- halve x
 |     z <- halve y
 |     halve z
Just 2

λ: halve 32 >>= (\_ -> Nothing) >>= halve >>= halve
Nothing
λ: do
 |     x <- halve 32
 |     Nothing
 |     y <- halve x
 |     z <- halve y
 |     halve z
Nothing
~~~

It's kind of imperative-y --- "do `halve 32` and assign the result (16) to
`x`...do `halve x` and assign the result (8) to `y`..." --- but remember,
it's still just a bunch of chained `>>=`'s in the end.

### Failure is an option

The important thing to note here is that "do" notation basically builds up one
"giant" object.  Remember the last two examples --- the second to last one,
all of those lines were in an effort to build one giant `Just 2` value.  In
the last example, all of those lines were in an effort to build one giant
`Nothing` value.  That's why one `Nothing` "ruined" the whole thing.  The
entire computation is one big `Maybe a`.  If at any point in your attempt to
build that `Maybe a`, you fail, then you have `Nothing`.

This behavior of "fail anywhere, fail the entire thing" is a special subclass
of Monads.  Not all monads are like this.  This special subclass of monads, we
call "MonadPlus".  I know, the name isn't really that fancy and it's also
slightly embarassing.  But a MonadPlus is basically a monad that embodies this
ideal of _"I am building up either a success or a failure...and if at any
point I fail, the whole thing is a failure."_

What do I mean?

Well, "monads" really is just a way to allow things to return new objects
based on the contents of previous objects.  Given any object, there is
technically more than one way to do this, obviously...you can have the
"chaining" process be anything you want, arbitrarily.  Sometimes, it's useful
to think of this chaining as a failure/success process.  When we model
chaining as a failure/success process, we say that we model it as a MonadPlus.

When you're working with a MonadPlus, your failure case is called "empty".  In
fact, you can type in `empty` instead of `Nothing`, and Haskell will know you
mean `Nothing` --- it's like an alias.

Now let's revisit the last do block and make it more generic, and just
rephrase it in a form that we are going to be encountering more when we solve
our problem with the List monad:

~~~haskell
halveThriceOops :: Int -> Maybe Int
halveThriceOops n = do          -- call with n = 32
    x  <- Just n                -- Just 32              -- 1
    y  <- halve x               -- Just 16
    empty                       -- Nothing              -- 2
    z  <- halve x'              -- (skip)               -- 3
    zz <- halve x'              -- (skip)
    return zz                   -- (skip)               -- 4
~~~

Note that I've also included a line-by-line 'trace' of the do block with what
the monad "is" at that point.  It is what is calculated on that line, and it
would be the value returned if you just exited at that step.

1.  We're going to "initialize" by setting the whole result to be `Just n`, at
    first.  This is slightly redundant becuase this is sort of dummy step ---
    as soon as we put `n` in the `Just`, we "take it out" again and put it in
    `x`.  (The arrows mean "take the content inside of the `Maybe` and put it
    in this value")  So while this is sort of redudant, the reason for this
    will be clear later.  Also, it's nice to just sort of see a nice "Step 0"
2.  The failure.  Remember, `empty` means "fail here automatically", which, in
    a Maybe object, means `Nothing`.
3.  Now from here on, nothing else even matters...the entire block is a
    failure!
4.  `return :: a -> Maybe a` --- it basically "always succeeds" with the value
    given to it.  And if it wasn't for the fact that the block already failed
    before it could get to this line, it would succeed with `zz`.  This, too,
    is slightly redundant, but it'll also make sense later.  Remember, this is
    just a "succeed automatically with this value" --- if we had written
    `return 5`, the entire block would have succeeded with a 5 if it didn't
    already fail.  If we had written `return x`, it would have succeeded with
    the original unchanged value!

### Guards

Wouldn't it be handy to have a function that says "fail right...here.  if this
condition is not met"?  Sort of like a more judicious `empty`, which says
"fail here no matter what".

Luckily, Haskell gives us one in the standard library:

~~~haskell
guard :: MonadPlus m => Bool -> m ()        -- 1
guard True  = return ()
guard False = empty
~~~

<aside>
    ###### Aside

1.  This is a type signature, like before.  We say that `guard` is a function
    that takes a `Bool` and returns a `m ()` --- a monad containing `()`.  But
    we say that `m`, the monad, must be a MonadPlus.

    For example, if we applied this to `Maybe`, the concrete signature would
    be `guard :: Bool -> Maybe ()`
</aside>

So `guard` will make sure a condition is met, or else it fails the entire
thing.  If the condition is met, then it succeeds automatically and places a
`()` in the value.

We can use this to re-implement `halve`, using do notation:

~~~haskell
halve :: Int -> Maybe Int
halve n = do                -- n = 8        n = 7
    x <- return n           -- Just 8       Just 7
    guard $ even x          -- Just ()      Nothing
    return $ x `div` 2      -- Just 4       (skip)
~~~

<aside>
    ###### Aside

`guard $ even x` is no mystery...it is just shorthand for `guard (even x)`.
We just don't like writing all those parentheses out.
</aside>

So...halve puts `n` into a `Just` to get it in the context of `Maybe`.  Then,
if `x` (which is just `n`) is not even, it fails right there.  If not, it
auto-succeeds with ``x `div` 2``.

You can trust me when I say this works the exact same way!

As a friendly reminder, this entire block is "compiled"/desugared to:

~~~haskell
halve n :: Int -> Maybe Int
halve n = return n >>= (\x -> guard (even x)) >>= return (x `div` 2)
~~~

<aside>
    ###### Note

Some of this might seem a little convoluted...why didn't we just do:

~~~
halve :: Int -> Maybe Int
halve n = do
    guard $ even n
    return $ n `div` 2
~~~

The answer is that we could...but just hang on tight for a bit and see why I
didn't write it this way!
</aside>



### Lists

Well, this article is about the List monad and I have done very little to talk
about it at all.

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

How about we look at it this way:  `[3, 5]` represents a success of *either* 3
or 5...and we don't know which, it could be either!

This view of a list as a superposition of succesful states is not the only way
to think of a list as a monad...but it is the way that the Haskell community
has adopted as arguably the most useful.  (The other main way is to approach
it completely differently, making list not even a MonadPlus and therefore not
representing failure or success at all)

So let's say we have a function `doubleOrHalve` that succeeds by either
doubling or halving the number: (succeeds in multiple possible ways)

~~~haskell
doubleOrHalve :: Int -> [Int]
doubleOrHalve n | even n    = [n `div` 2, n * 2]
                | otherwise = [n * 2]
~~~

If n is even, it can succeed in both halving and doubling.  If n is odd, it
can only succeed in doubling.

Let's try chaining it!

~~~haskel
doubleOrHalveTwice :: Int -> [Int]
doubleOrHalveTwice n = do       -- n = 6
    x <- return n               -- [   6    ]
    y <- doubleOrHalve x        -- [3,  12  ]
    z <- doubleOrHalve y        -- [6, 6,24 ]
    return z
~~~

Oh boy.  How are we going to look at this?  How does this even make sense?

Our result at the end of `doubleAndHalveTwice 6` is `[6, 6, 24]`.

Let's reason this out in english:

1.  If we double or halve a 6, we get either 3 or 12.  We don't know which.
2.  What do we get if we double or halve twice?
3.  Well...if we had gotten 3 the first time, then the second time, we
    definitely get 6.
4.  If we had gotten 12 the first time, then the second time, we would either
    have a 6 or a 24.
5.  So, we either have a 6, or a 6 and a 24.  We either have 6, 6, or 24.  We
    don't know which.
6.  So, this `doubleAndHalveTwice` *suceeds*, definitely...but it can possibly
    succeed in three ways --- it can possibly succeed twice giving a 6, or
    partially succeed twice giving a 6, or partially succeed twice giving a
    24.

<!-- ### Maybe? -->

<!-- What do I mean? -->

<!-- Let's look at the most obvious container -- a `Maybe a`.  A `Maybe a` is a -->
<!-- container that can either be `Just a` (representing a succesful result `a`) or -->
<!-- a `Nothing` (representing a failed result). -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- Hi!  These asides are going to be for you readers that are unfamiliar with -->
<!-- Haskell syntax.  Feel free to ignore them if you already feel comfortable. -->

<!-- Anyways, if you've ever done any object-oriented programming, you might be -->
<!-- able to think of `Maybe a` as an abstract/virtual superclass with -->
<!-- templates/generics --- `Maybe<a>`, kinda.  And that superclass has two -->
<!-- subclasses: `Just<a>`, which has one public instance variable of type `a`, and -->
<!-- `Nothing`, which contains no instance variables. -->
<!-- </aside> -->

<!-- The Monad instance of Maybe is useful because it allows us to chain -->
<!-- failable-computations. -->

<!-- For example, the `halve` function, which returns ``Just (x `div` 2)`` on a -->
<!-- succesful halving, or `Nothing` on an unsuccesful halving: -->

<!-- ~~~haskell -->
<!-- halve :: Int -> Maybe Int                       -- 1 -->
<!-- halve x | even x    = Just (x `div` 2)          -- 2 -->
<!--         | otherwise = Nothing                   -- 3 -->
<!-- ~~~ -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- Hi again!  There are some quick syntax features here. -->

<!-- 1.  This first line declares that the type of the function `halve` is `Int -> -->
<!--     Maybe Int`, which means that it takes in an `Int` and returns a `Maybe -->
<!--     Int` --- an integer wrapped in a "Maybe" container. -->
<!-- 2.  This says that if x is even, then return a succesful ``Just (x `div` 2)``. -->
<!--     ``x `div` 2`` is basically x divided by two, in case you couldn't guess -->
<!--     already. -->
<!-- 3.  Otherwise, return `Nothing` --- a failure. -->
<!-- </aside> -->

<!-- ~~~haskell -->
<!-- λ: halve 6 -->
<!-- Just 3 -->
<!-- λ: halve 7 -->
<!-- Nothing -->
<!-- ~~~ -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- In this article, code that begins with `λ: ` represents commands to be entered -->
<!-- at the interactive prompt, ghci.  Code that doesn't is actual source code. -->
<!-- </aside> -->

<!-- How would we write `halveTwice`? -->

<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = -->
<!--     case halve x of                             -- 1 -->
<!--         Just x2 -> halve x2 -->
<!--         Nothing -> Nothing -->
<!-- ~~~ -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- 1.  Like a case/switch statement in any language, the path it takes depends on -->
<!--     what you give it.  In this case, it calculates `halve x`, and decides with -->
<!--     path depending on what `halve x` is. -->
<!-- </aside> -->

<!-- ~~~haskell -->
<!-- λ: halveTwice 6 -->
<!-- Nothing -->
<!-- λ: halveTwice 8 -->
<!-- Just 2 -->
<!-- ~~~ -->

<!-- Okay, this isn't too clean code.  What about `halveThrice`? -->

<!-- ~~~haskell -->
<!-- halveThrice :: Int -> Maybe Int -->
<!-- halveThrice x = -->
<!--     case halve x of -->
<!--         Just x2 -> -->
<!--             case halve x2 of -->
<!--                 Just x3     -> halve x3 -->
<!--                 Nothing     -> Nothing -->
<!--         Nothing     -> -->
<!--             Nothing -->
<!-- ~~~ -->

<!-- ~~~haskel -->
<!-- λ: halveThrice 4 -->
<!-- Nothing -->
<!-- λ: halveThrice 8 -->
<!-- Just 1 -->
<!-- ~~~ -->

<!-- Now that's just downright ugly. -->

<!-- What are we trying to do here, exactly? -->

<!-- Basically, we want to generate a new `Maybe` based on what a current `Maybe` -->
<!-- contains.  We want to chain these. -->

<!-- Monads to the rescue! -->

<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice = (halve x) >>= halve -->

<!-- halveThrice :: Int -> Maybe Int -->
<!-- halveThrice = (halve x) >>= halve >>= halve -->
<!-- ~~~ -->

<!-- And that's it! -->

<!-- `>>=` takes care of the plumbing (the ugly case statements) for us and -->
<!-- abstracts it away.  Want to know how?  Too bad!  This isn't a monad tutorial! -->
<!-- Read a real one :) -->

<!-- Anyways, the cool thing about monads in Haskell is that Haskell provides -->
<!-- convenient syntactic sugar for using `>>=`: -->

<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = do -->
<!--     x2  <- halve x -->
<!--     halve x2 -->
<!-- ~~~ -->

<!-- which is the same as: -->

<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = halve x >>= (\x2 -> halve x2)    -- 1 -->
<!-- ~~~ -->

<!-- which is -->

<!-- ~~~haskell -->
<!-- halveTwice :: Int -> Maybe Int -->
<!-- halveTwice x = halve x >>= halve -->
<!-- ~~~ -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- 1. Haskell has the construct `(\x -> f x)` which is basically a function that -->
<!--    takes `x` and returns `f x`.  So `(\x2 -> halve x2)` is a function that -->
<!--    takes an `x2` and returns `halve x2`.  This is exactly the same as the -->
<!--    function `halve` --- it takes an `x` and returns `halve x`. -->
<!-- </aside> -->

<!-- We can also do `halveFourTimes`: -->

<!-- ~~~haskell -->
<!-- halveFourTimes :: Int -> Maybe Int -->
<!-- halveFourTimes x = do -->
<!--     x2 <- halve x -->
<!--     x3 <- halve x2 -->
<!--     x4 <- halve x3 -->
<!--     halve x4 -->
<!-- ~~~ -->

<!-- Imagine having to do that manually! -->

<!-- ### The do block -->

<!-- Note one interesting thing about a `>>=` based definition of `halveFourTimes`: -->

<!-- ~~~haskell -->
<!-- halveFourTimes :: Int -> Maybe Int -->
<!-- halveFourTimes = halve x >>= halve >>= halve >>= halve -->
<!-- ~~~ -->

<!-- Note that at any time, if *any* of those `halve`s fail, the entire thing -->
<!-- fails. -->

<!-- This is how `Maybe` works --- if a computation fails, then all computations -->
<!-- deriving from that computation will also fail, necessarily. -->

<!-- Think about something like this: -->

<!-- ~~~haskell -->
<!-- halveFourTimesOops :: Int -> Maybe Int -->
<!-- halveFourTimesOops x = do -->
<!--     x2 <- halve x -->
<!--     x3 <- halve x2 -->
<!--     _  <- Nothing                       -- 1 -->
<!--     x4 <- halve x3 -->
<!--     halve x4 -->
<!-- ~~~ -->

<!-- <aside> -->
<!--     ###### Aside -->

<!-- 1.  An underscore `_` in Haskell is a wildcard; basically, it says "store this -->
<!--     value somewhere, but I don't need it again ever so I won't even bother -->
<!--     giving it a name." -->
<!-- </aside> -->

<!-- Now watch: -->

<!-- ~~~haskell -->
<!-- λ: halveFourTimes 32 -->
<!-- Just 2 -->
<!-- λ: halveFourTimesOops 32 -->
<!-- Nothing -->
<!-- ~~~ -->

<!-- That's what `Maybe` really is --- it chains together failable computations. -->
<!-- But if at any point in time, a computaion fails, then the entire full chained -->
<!-- computation is now a failure, no matter what. -->

<!-- To convince you, let's break down a simple `>>=` style chaining of -->
<!-- `halveTwiceOops`: -->

<!-- ~~~haskell -->
<!-- λ: halve 8 >>= (\_ -> Nothing) >>= halve -->
<!-- Nothing -->
<!-- ~~~ -->

<!-- What is `halve 8 >>= (\_ -> Nothing)`, anyway?  It is "ignore the result of -->
<!-- `halve 8` and return `Nothing` no matter what": -->

<!-- ~~~haskell -->
<!-- λ: halve 8 >>= (\_ -> Nothing) -->
<!-- Nothing -->
<!-- ~~~ -->

<!-- So obviously, we expect `Nothing >>= halve` to return `Nothing`...you can't -->
<!-- halve a failure! -->

<!-- ~~~haskell -->
<!-- λ: Nothing >>= halve -->
<!-- Nothing -->
<!-- ~~~ -->

<!-- So that's why, if at any point along the chain you have a failure, the entire -->
<!-- thing fails. -->

<!-- Okay, now let's get to the actual problem (finally!). -->















<!-- Okay, let's back up.  When we say "functor", we mean things that have the -->
<!-- ability to apply functions "inside" them. -->

<!-- ~~~haskell -->
<!-- -- Normal function application, with $ -->
<!-- λ: (*2) $ 3 -->
<!-- 6 -->
<!-- -- Function application inside a container, with <$> -->
<!-- λ: (*2) <$> [3] -->
<!-- [6] -->
<!-- λ: (*2) <$> Just 3 -->
<!-- -- "Just" is just a container that contains one value, the 3 -->
<!-- Just 6 -->
<!-- λ: (*2) <$> [3,4,5] -->
<!-- [6,8,10] -->
<!-- ~~~ -->

<!-- Note the last one...the List functor...we can say that the list -->
<!-- "simultaneously contains" 3, 4, and 5...just like a Maybe "contains" a 3.  So -->
<!-- when we apply the function `(*2)` (the doubling function) to what is "inside" -->
<!-- a List...we apply it to all things simultaneously inside the list (3, 4, and -->
<!-- 5). -->

<!-- Now, when we say "monad", we mean things that have the ability to create new -->
<!-- objects from the contents of the previous object. -->

<!-- For example, we want to create a `Just something` from the contents of -->
<!-- `Just 5`. So we write a function `f :: Int -> Just Int`, which takes an int -->
<!-- (in this case, the 5) and -->
