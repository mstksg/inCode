(Practical) Fun with Monads!  MonadPlus: The Success/Failure Monad
==================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
Series
:   MonadPlus: The Success/Failure Monad
:   Practical Monads
CreateTime
:   2013/12/04 21:43:34
PostDate
:   Never
Identifier
:   monad-plus-1

Monads.  Haskell's famous for them, but they are one of the most
ill-understood concepts to the public.  They are mostly shrouded in mystery
because of their association with how Haskell models I/O.  This reputation is
undeserved.  Monads don't have anything to do with I/O.

This series is a part of a global effort to deshroud the mystery behind monads
and show that they are fun!  And exciting!  And really just a way of chaining
together functions that allow for new ways of approaching puzzles.

The first sub-series will be on a certain way of implementing monads known as
*MonadPlus*.  At the end of it all, we are going to be solving the classic
logic puzzle, as old as time itself, using **only** the List monad, and no
other fancy data structures:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport
> across a river.  Unfortunately, his only boat can carry one thing at a time.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat.
> He can't leave the goat alone with the cabbage, or the goat will eat the
> cabbage.  How can he properly transport his belongings to the other side one
> at a time, without any disasters?

Let us enter a brave new world!

A Quick Review of Monads
------------------------

As a Haskell blogger, I'm not allowed to write any straight-up monad
tutorials, but I don't need too --- there are a wealth of great ones.
[Adit provides a great concise one][adit], and, if you want,
[a more in depth one][wiki] is on the haskell.org wiki about using them in
real life.

[adit]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[wiki]: http://www.haskell.org/haskellwiki/All_About_Monads

Remember --- different monads do not actually have any non-superficial
relationship to one another.  When we say monads, we just mean things that
chain together functions "inside" wrappers, containers, or contexts.  In our
case, containers.

This article attempts to explain as much special Haskell syntax as possible
and explain how monads are used "in real life", and how to use (mostly
idiomatic) monadic style to solve problems.  That being said, if you ever run
into anything you can't understand, feel free to read the articles above, give
[Learn You A Haskell][lyah] a quick read, or leave a comment.

[lyah]: http://learnyouahaskell.com/


Maybe, maybe not
----------------

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

Failure is an option
--------------------

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
didn't write it this way!  However, in real life, do it this way if you can.
Heh.
</aside>










