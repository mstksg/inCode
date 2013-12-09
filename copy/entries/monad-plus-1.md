Practical Fun with Monads --- Introducing: MonadPlus!
=====================================================

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

This series is a part of a global effort to pull away the shroud of mystery
behind monads and show that they are fun!  And exciting!  And really just a
way of chaining together functions that allow for new ways of approaching
puzzles.

The first sub-series (chapter?) will be on a specific class/family of monads
known as *MonadPlus*.  At the end of it all, we are going to be solving the 
classic logic puzzle, as old as time itself, using **only** the List monad
instance, and no loops, queues, or fancy stuff like that:

> A farmer has a wolf, a goat, and a cabbage that he wishes to transport
> across a river.  Unfortunately, his only boat can carry one thing at a time.
> He can't leave the wolf alone with the goat, or the wolf will eat the goat.
> He can't leave the goat alone with the cabbage, or the goat will eat the
> cabbage.  How can he properly transport his belongings to the other side one
> at a time, without any disasters?

Let us enter a brave new world!

### A quick review of monads

<aside>
    ###### Note

This article is written for both beginners --- people who have a fuzzy idea of
monads and a minimal understanding of functional programming principles, but
who have some experience in Object-Oriented Programming in a language like
Java or C++ --- and intermediate Haskell users --- people who have a somewhat
firm grasp on monads, but want to know about monads on a broader context (in
particular, the MonadPlus typeclass).

Intermediate Haskell users will most likely find this post to be review, and
I'll put a link in this paragraph when the next part is up so we can get to
"real" Haskelling.  However, this post might be beneficial if you read it
while asking, at every point, "How can this be abstracted and generalized?".
It's a fun exercise!

This article attempts to explain all Haskell syntax that might be foreign to
beginners.  That being said, if you ever run into anything you can't
understand, feel free to either read the articles above, give [Learn You A
Haskell][lyah] a quick read (you won't regret it!), or leave a comment --- I'd
love to answer your questions or hear your responses!

[lyah]: http://learnyouahaskell.com/

This first post will cover the basics of MonadPlus with the simplest MonadPlus
of all; the second part will explore the List MonadPlus, and the third will
finally tackle the Wolf/Goat/Cabbage puzzle with our combined knowledge.
</aside>

Okay, so as a Haskell blogger, I'm actually not allowed to write any monad
tutorials.  Luckily for you, however, I don't need too --- there are a wealth
of great ones. [Adit provides a great concise one][adit], and, if you want, [a
more in depth one][wiki] is on the haskell.org wiki about all sorts of monads
and using them in real life.

[adit]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[wiki]: http://www.haskell.org/haskellwiki/All_About_Monads

Remember --- different monads do not actually have any non-superficial
relationship to one another.  When we say monads, we just mean objects for
which we have defined a way to chain together functions "inside" wrappers,
containers, or contexts.


Maybe, maybe not
----------------

Monads are very useful when you are dealing with objects that are containers.
Let's look at the most obvious container -- a `Maybe a`.  A `Maybe a` is a
container that can either be `Just x` (representing a successful result `x` of
type `a`) or a `Nothing` (representing a failed result).

<aside>
    ###### Aside

Hi!  These asides are going to be for you readers that are unfamiliar with
Haskell syntax.  Feel free to ignore them if you already feel comfortable.

Anyways, if you've ever done any object-oriented programming, you might be
able to think of `Maybe a` as an abstract/virtual superclass with
templates/generics --- `Maybe<a>`, kinda.  And that superclass has two
subclasses: `Just<a>`, which has one public instance variable `x` of type `a`,
and `Nothing`, which contains no instance variables.
</aside>

Often times you'll have functions that fail, and you want to chain them.  The
easiest way is that any function that is chained onto a failed value will
be skipped; a failure is the final result.

Consider the `halve` function, which returns ``Just (x `div` 2)`` on a
successful halving, or `Nothing` on an unsuccessful halving:

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
2.  This says that if x is even, then return a successful ``Just (x `div` 2)``.
    ``x `div` 2`` is basically x divided by two, in case you couldn't guess
    already.
3.  Otherwise, return `Nothing` --- a failure.
</aside>

Because Maybe comes built-in as a monad, we can now chain `halve`s on results
of other `halves`, and have any failures automatically propagate to the end
and short circuit your entire computation:

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
Nothing
λ: halve 32 >> Nothing
Nothing                         -- 2
λ: halve 32 >>= halve >>= halve >>= halve
Just 2
λ: halve 32 >> Nothing >>= halve >>= halve >>= halve
Nothing                         -- 3
~~~

<aside>
    ###### Aside

In this article, code that begins with `λ: ` represents commands to be entered
at the interactive prompt, ghci.  Code that doesn't is actual source code.
</aside>

Remember, `>>=` means "use the results of the last thing to calculate this
next thing" --- it "chains" the functions.

How does this work, exactly?  That's not really in the scope of this article
(any monad tutorial will explain this in more detail).  But here are some
interesting points:

1.  Note that this command doesn't even bother with the second `halve`.  It
    knows that the end result will be `Nothing` no matter what (because `halve
    7` is `Nothing`), so it just skips right past the second `halve`.
2.  `>>` is a special variation of `>>=`.  `>>=` says "take the result of the
    last thing and use it on this", while `>>` says "ignore the result of the
    last thing and always return this".  So `>> Nothing` means "I don't care
    what the last thing succeeded with, I'm going to fail right here."
3.  Disastrous!  Even though halving 32 four times usually is fine (giving
    `Just 2`), having just one failure along the way means that the entire
    thing is a failure.  `halve 32 >> Nothing` is `Nothing`, so the whole
    thing is just `(Nothing) >>= halve >>= halve >>= halve`.

You can think of this failing phenomenon like this: At every step, Haskell
attempts to apply `halve` to the result of the previous step.  However, you
can't `halve` a `Nothing` because a `Nothing` has no value to halve!

### Do notation

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

λ: halve 32 >> Nothing >>= halve >>= halve
Nothing
λ: do
 |     x <- halve 32
 |     Nothing
 |     y <- halve x
 |     z <- halve y
 |     halve z
Nothing
~~~

In this notation, `x`, `y`, and `z`'s do not contain the `Just`/`Nothing`'s
--- they represent the actual **Ints inside them**, so we can so something
like `halve x` (where `halve` only takes Ints, not `Maybe Int`'s)

It kind of feels very imperative-y --- "do `halve 32` and assign the
result (16) to `x`...do `halve x` and assign the result (8) to `y`..." --- but
remember, it's still just a bunch of chained `>>=`s in the end.

Failure is an option
--------------------

The important thing to note here is that "do" notation basically builds up one
"giant" object.  Remember the last two examples --- the second to last one,
all of those lines were in an effort to build one giant `Just 2` value.  In
the last example, all of those lines were in an effort to build one giant
`Nothing` value.  That's why one `Nothing` "ruined" the whole thing.  The
entire computation is one big `Maybe a`.  If at any point in your attempt to
build that `Maybe a`, you fail, then you have `Nothing`.

Now, remember, saying "x is a monad" just means "we have defined a way of
chaining functions/operations on x".  Just like how we can now chain multiple
functions that return Maybe's (that don't take Maybe's as input).  However,
given any object, there is probably more than one way to meaningfully define
this "chaining".

Sometimes, it's useful to base your definition of chaining on the idea of a
failure/success process.  Sometimes it's useful to define chaining as "We are
building up either a success or a failure...and if at any point I fail, the
whole thing is a failure".

There is a special name for this design pattern.  In Haskell, we call
something like this a "MonadPlus".

I know, it's an embarrassingly bad name, and the reason it's like this is for
historical reasons.[^alternative]  But we're stuck with it for pretty much the
entire foreseeable future, and when you chose to adopt a success/failure model
for your chaining process, you have a MonadPlus.

[^alternative]: This issue actually stems from one of the more famous
embarassing mistakes in the design the Haskell standard library --- the
infamous [monad-applicative-functor hierarchy issue][maf].  As a result,
MonadPlus has to fulfill two roles, the other being unrelated to its
fail/success purpose. Ideally, we would have two classes: MonadFail and
Alternative.  In practice, however, the part of MonadPlus that does not
involve dealing with failure/success is rarely used, and we use Alternative
for that instead; the only real problem is therefore really just the
not-so-helpful naming.

[maf]: http://www.haskell.org/haskellwiki/Functor-Applicative-Monad_Proposal

There is a nice vocabulary we can use so we can talk about all MonadPlus's in
a general way:

-   We call a success a "return".  Yeah...the name is super confusing because
    of how the word "return" is used in almost every other context in computer
    science.  But hey.  Oh well.
-   We call a failure an "mzero".  Yes, this name is pretty lame too.

For Maybe, a "return" with the value `x` is `Just x`, and an "mzero" is a
`Nothing`.

Something cool about Haskell is that if we type `return x`, it'll interpret it
as an auto-success of value `x`.  If we type `mzero`, it'll be an "alias" of
whatever your failure is.

That means that for Maybe, `return x` is the same as `Just x`, and `mzero` is
basically an alias for `Nothing`.

<aside>
    ###### Aside

If you are familiar with object oriented languages like Java, MonadPlus is
really like an **interface**.  That is, if something is a MonadPlus, there is
a "guarantee" that that something will implement/define `return` and `mzero`
for that particular object.  In this way, `return` and `mzero` are
*polymorphic functions* that change their behavior based on what type you are
talking about, and you can write code that works with all MonadPlus's
generically without worrying about their actual type by using only `return`
and `mzero` (instead of say, `Just` and `Nothing`).

In Haskell, the term we use (instead of "interface") is "**typeclass**". There
are some subtle differences (including the fact that you can define default
functions, and you also have a bit more power in what you can specify), but it
doesn't hurt *too* much to make the analogy.
</aside>

### MonadPlus examples

To see this in action, let's revisit the last do block and make it more
generic, and just rephrase it in a form that we are going to be encountering
more when we solve our problem with the List monad (which is (spoilers) also a
MonadPlus):

~~~haskell
halveThriceOops :: Int -> Maybe Int
halveThriceOops n = do          -- call with n = 32
    x  <- return n              -- Just 32              -- 1
    y  <- halve x               -- Just 16
    mzero                       -- Nothing              -- 2
    z  <- halve y               -- (skip)               -- 3
    zz <- halve z               -- (skip)
    return zz                   -- (skip)               -- 4
~~~

Note that I've also included a line-by-line 'trace' of the do block with what
the monad "is" at that point.  It is what is calculated on that line, and it
would be the value returned if you just exited at that step.

1.  We're going to "initialize" by setting the whole result to be `Just n`, at
    first.  This is slightly redundant because this is sort of dummy step ---
    as soon as we put `n` in the `Just`, we "take it out" again and put it in
    `x`.  (The arrows mean "take the content inside of the Maybe and put it
    in this value")  So while this is sort of redundant, the reason for this
    will be clear in a later article.  Also, it's nice to just sort of see a
    nice "Step 0"
2.  The failure.  Remember, `mzero` means "fail here automatically", which, in
    a Maybe object, means `Nothing`.
3.  Now from here on, nothing else even matters...the entire block is a
    failure!
4.  Succeed with the value in `zz`.  This is supposed to be a `Just` with
    the value of `zz`.  Unfortunately, `zz` doesn't even have a value, because
    the entire block failed a long time ago.  So sad!


### Guards

It feels like just slapping in `mzero` willy-nilly is not that useful, because
then things just fail always no matter what.  Wouldn't it be handy to have a
function that says "fail right...here, if this condition is not met"?  Like
`mzero`, but instead of always failing, fails on certain conditions.

Luckily, Haskell gives us one in the standard library:

~~~haskell
guard :: MonadPlus m => Bool -> m ()        -- 1
guard True  = return ()
guard False = mzero
~~~

<aside>
    ###### Aside

1.  This is a type signature, like before.  We say that `guard` is a function
    that takes a `Bool` and returns a `m ()` --- a monad containing `()`.  But
    we say that `m`, the monad, must be a MonadPlus.

    For example, if we applied this to Maybe, the concrete signature would
    be `guard :: Bool -> Maybe ()`
</aside>

So `guard` will make sure a condition is met, or else it fails the entire
thing.  If the condition is met, then it succeeds automatically and places a
`()` in the value.

We can use this to re-implement `halve`, using do notation, aware of Maybe's
MonadPlus-ness:

~~~haskell
halve :: Int -> Maybe Int
halve n = do                -- <halve 8>   <halve 7>
    x <- return n           -- Just 8       Just 7
    guard $ even x          -- Just ()      Nothing
    return $ x `div` 2      -- Just 4       (skip)
~~~

<aside>
    ###### Aside

`guard $ even x` is no big mystery...it is just shorthand for `guard (even
x)`. We just don't like writing all those parentheses out. </aside>

So...halve first puts `n` into a `Just` to get it in the context of Maybe.
Then, if `x` (which is just `n`) is not even, it fails right there.  If not,
it auto-succeeds with ``x `div` 2``.

You can trust me when I say this works the exact same way!

As a friendly reminder, this entire block is "compiled"/desugared to:

~~~haskell
halve n :: Int -> Maybe Int
halve n = return n >>= (\x -> guard (even x)) >> return (x `div` 2)
~~~

<aside>
    ###### Note

Some of this might seem a little convoluted...why didn't we just do:

~~~haskell
halve :: Int -> Maybe Int
halve n = do
    guard $ even n
    return $ n `div` 2

-- or

halve :: Int -> Maybe Int
halve n = guard (even n) >> return (n `div` 2)
~~~

The answer is that we *could*...and admittedly, this is how you really should
write it in real life.  But just hang on until the next couple of chapters to
see why I didn't write it this way.
</aside>

A practical use
---------------

We aren't where we need to be to begin tackling that Wolf/Goat/Cabbage puzzle
yet...so to let this article not be a complete anticlimax (as a result of my
bad planning --- I had originally intended to do the entire three-part series
as one article), let's look at a practical problem that you can solve using
the Maybe monad.

The obvious examples are situations where it is useful to be able to chain
failable operations such as retrieving things from a database or a network
connection or applying partial functions (functions that only work on some
values, like our `halve`).

However, here is a neat one.

Let's say we are making a game where you can lose health by being hit or gain
health by picking up powerups.  We want to calculate the final health at the
end of the game.  It seems a bit easy: just add up all the losses and gains!
Unfortunately, it's not so simple --- it needs to be implemented such that if
your health ever dips below 0, you are dead.  Forever.  No powerups will ever
help you.

Think about how you would implement this normally.  You might have a state
object that stores the current health as well as a flag with the current
dead/alive state, and at every step, check if the health is 0 or lower; if it
is, swap the flag to be dead and ignore all other updates.

But let's try doing this instead with the Maybe monad:

~~~haskell
-- die or fail immediately
die :: Maybe Int
die = Nothing                       -- or die = mzero

-- if not dead, sets the health to the given level
setHealth :: Int -> Maybe Int
setHealth n = Just n                -- or setHealth n = return n

-- damage the player (from its previous health) and check for death
hit :: Int -> Maybe Int
hit currHealth = do
    let newHealth = currHealth - 1
    guard $ newHealth > 0           -- fail/die immediately unless newHealth
                                    --     is positive
    return newHealth                -- succeed with newHealth if not already
                                    --     dead

-- an alternative but identical definition of `hit`, using >>= and >>
hit' :: Int -> Maybe Int
hit' currHealth = guard (newHealth > 0) >> return newHealth
    where
        newHealth = currHealth - 1

-- increase the player's health from its previous health
powerup :: Int -> Maybe Int
powerup currHealth = Just $ currHealth + 1
~~~

~~~haskell
λ: setHealth 2 >>= hit >>= powerup >>= hit >>= powerup >>= powerup
Just 3
λ: setHealth 2 >>= hit >>= powerup >>= hit >>= hit >>= powerup
Nothing
λ: setHealth 10 >>= powerup >>= die >>= powerup >>= powerup 
Nothing
λ: do
 |     h0 <- setHealth 2        -- Just 2
 |     h1 <- hit h0             -- Just 1
 |     h2 <- powerup h1         -- Just 2
 |     h3 <- hit h2             -- Just 1
 |     h4 <- hit h3             -- Nothing
 |     h5 <- powerup h4         -- (skip)
 |     h6 <- powerup h5         -- (skip)
 |     return h6                -- (skip)
Nothing
~~~

And voilà!

You can think of the last do block conceptually this way: remember, `h3` does
not represent the `Just 1` value --- `h3` represents the number *inside* the
`Just 1` --- the 1.  So `h4` is supposed to represent the number inside its
value, too.  But because `hit h3` results in `Nothing`; `Nothing` has no value
"inside", so `h4` doesn't even have a value!  So obviously it doesn't even
make sense to call `powerup h4`...therefore `h5` has no value either! It's
therefore meaningless to call `powerup h5`, so meaningless to `return
h6`...the entire thing is a beautiful disaster.  A fiasco.  Mission
accomplished!

The whole thing works as expected; you can even die suddenly with `die`, which
ignores your current health.

Interestingly enough, we could actually eliminate all references to Maybe
altogether by always using `return` and `mzero` instead of `Just` and
`Nothing`.  And if we make our type signatures generic enough, we could use
this with *any* MonadPlus! But that is for another day.


Looking forward
---------------

Wow, who knew you could spend so much time talking about failure.  Anyways,
this is a good place to stop before we move onto how List is also a MonadPlus.
Okay, so what have we learned?

-   Monads are just a way of chaining functions on objects, and of course,
    every object's chaining process is different.  In fact there might be even
    more than one way to meaningfully chain functions on an object!

-   One useful "chaining approach" is to model things as success-failure chains,
    where you are building something from successes, but if you fail once in
    the process, the entire process fails.  An object that uses this
    approach/design pattern is called a MonadPlus.

-   The Maybe object is one such example.  We can define 'chaining' failable
    functions as functions that continue if the previous function succeeded,
    or propagate a failure if the previous function fails.  A failable
    function, for a Maybe object, is a function `:: a -> Maybe b` or even `::
    Maybe b`.

-   We can "forget" we are using Maybe and in fact talk about/write for
    "general" MonadPlus's, with `return x` meaning "succeed automatically with
    `x` (if we haven't failed already)", and `mzero` meaning "fail now".

For the mean time, think about how it might make sense to chain operations on
lists (ie, repeatedly applying functions `:: a -> [b]` to lists).

By this, I mean, given a function that turns a value into a list of values `f
:: a -> [b]`, find a way to meaningfully "chain" that function to a previous
list and get a new list:

~~~haskell
λ: oldList >>= f
newList             -- a new list based on old list; f "chained" to `oldList`.
~~~

Is there more than one way to think about chaining them, even?  And in what
ways we can define this "chaining" to represent success/failure?  Until next
time!



