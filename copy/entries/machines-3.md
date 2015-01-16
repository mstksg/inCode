Going Kleisli: Intro to Machines and Arrow Part 3
=================================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   arrows
:   netwire
CreateTime
:   2014/07/21 21:28:28
PostDate
:   Never
Series
:   Intro to Machines and Arrows
Identifier
:   machines-3

Hi!  I have to apologize a bit for the long delay; [starting grad
school][chapman] and things like that have made me have to scramble to adjust
to the new life.  But a couple of people have asked me to finish up and wrap
up this series, and I think I owe it to them :)

In the [last post][part2], we looked deeper into the Auto type, played
around with instancing it as familiar typeclasses, saw it as a member of the
powerful *Category* and *Arrow* typeclasses, and took advantage of this by
composing Autos both manually and using proc/do notation, and were freed from
the murk and mire of explicit recursion.  We observed the special nature of
this composition, and saw some neat properties, like local statefulness.

[part2]: http://blog.jle.im/entry/auto-as-category-applicative-arrow-intro-to-machines

Here we are going to push the abstraction further to see where it will go by
introducing the Kleisli category based on our `Auto` category in order to add
effects, making the plain ol' Auto type into something rich and featureful.
And finally, at the very end, we'll do a short case study on one motivating
example: arrowized FRP libraries!

As always, feel free to leave a comment if you have any questions, drop by
freenode's *#haskell*, or find me on [twitter][] :)

[twitter]: https://twitter.com/mstk "Twitter"

Composition is the key
----------------------

We're going to be seeing this pop a lot --- and you'll actually see this
pattern in haskell a lot, too.  Sometimes in Haskell, things gain their power,
their usefulness, not from the data type or concept itself, but from its
*composition*.

So let's find new and novel ways to compose different "varieties" of our
`Auto`, and maybe we can find some use for them :D

### On and Off

Often times in real-life implementations, we might have `Auto`s that maybe
"on" or "off".  If we imagine it as a stateful functions...at some times, the
function may be outputting, or it may be "closed off" for a while.

The important thing to keep in mind here is that while the function is not
outputting anything, its internal state is still ticking along.

We can pull in a data type from Haskell to represent these semantics: `Maybe`!

~~~haskell
!!!machines/Auto3.hs "onFor ::" machines
~~~

This will basically let a value "pass through" for `n` steps as Just, and then
stop forevermore.

~~~haskell
ghci> take 6 $ testAuto_ (onFor 3 . pure "hello") (repeat ())
[Just "hello", Just "hello", Just "hello", Nothing, Nothing, Nothing]
~~~

How about an Auto that lets items pass through until the first one that
matches a predicate?

~~~haskell
!!!machines/Auto3.hs "onUntil ::" machines
~~~

~~~haskell
ghci> take 6 $ testAuto_ (onUntil (> 4)) [1..]
[Just 1, Just 2, Just 3, Just 4, Nothing, Nothing]
~~~

You can think of these as "filters".  Stateful filters.  The first filters out
all but the first `n` elements, and the second lets everything through until
the first thing greater than four.

~~~haskell
onFor 3       :: Auto a   (Maybe a)
onUntil (> 4) :: Auto Int (Maybe Int)       -- specialized for Int for fun
~~~

So, how could you "compose" these?  Run a value through one filter, then the
other, so that only values that pass through both go through?

Naively, you might do something like this:

~~~haskell
til3or4 = onFor 3 . onUntil (> 4)
~~~

Except that doesn't work...`onFor 3` expects an `a`, but you're giving it a
`Maybe a`.

Perhaps we can imagine a new operator to compose them in a meaningful way,
such that the `Nothing`s "propagate" all the way down the chain...


~~~haskell
!!!machines/Auto3.hs "(.?) ::" machines
~~~

~~~haskell
ghci> let til3or4 = onFor 3 .? onUntil (> 4)
ghci> :t til3or4
til3or4 :: Auto Int (Maybe Int)             -- specialized to Int for fun
ghci> take 6 $ testAuto_ til3or4 [1..]
[Just 1, Just 2, Just 3, Nothing, Nothing, Nothing]
ghci> take 6 $ testAuto_ til3or4 [4..]
[Just 4, Nothing, Nothing, Nothing, Nothing, Nothing]
~~~

As we can see, the "composed" filters make the input values (ascending
integers) have to pass through both filters before making it through.  The
first one cuts out after three, and the second one cuts out after the first
value greater than 4.

Some domains actually want this behavior "built-in".  That is, they want *all*
`Auto`s to behave this way.  There are actually two routes we can take:

1.  We can define our new type as simply a newtype wrapper over such an
    `Auto`:

    ~~~haskell
    !!!machines/Auto3.hs "newtype AutoOn1" "instance Functor (AutoOn1 a)" "instance Category AutoOn1" "instance Arrow AutoOn1" machines
    ~~~


In this case, we can even define `Category` and
`Arrow` instances using a newtype wrapper:

<!-- ~~~haskell -->
<!-- newtype AutoOnOff a b = AutoOnOff (Auto a (Maybe b)) -->

<!-- instance Category AutoOnOff where -->
<!--     id  = AutoOnOff (arr Just) -->
<!--     (.) = (.?) -->

<!-- instance Arrow AutoOnOff where -->
<!--     arr   = AutoOnOff (fmap Just . arr) -->
<!--     first = --> 
<!-- ~~~ -->











<!-- We saw earlier --- sure, `Auto` is nice.  But when we define composition, we -->
<!-- changed the game up a lot.  Sure, `Maybe` is nice, but when we learn how to -->
<!-- compose `a -> Maybe b` functions, everything is changed.  `IO` as a data type -->
<!-- is a great idea.  But the ability to compose `a -> IO b`'s was what changed -->
<!-- the world.[^changedworld] -->

<!-- [^changedworld]: We're still waiting on this one, admittedly :) -->

<!-- If you've been using Haskell for even a little amount of time, you might -->
<!-- notice that often, the same "type" can be composed in different ways. -->

<!-- For example, how can you compose a function that returns `IO (Maybe b)`? -->

<!-- Well, you can imagine composing `Maybe a -> IO (Maybe b)`'s, which is what -->
<!-- `IO`'s `Monad` instance gives you.  Or, you can imagine composing `a -> IO -->
<!-- (Maybe b)`'s. -->




<!-- When -->
<!-- the composition operator is from a typeclass, we can use newt -->

<!-- Thanks to [abstractions like Functor, Monad, and Applicative][ode], we are -->
<!-- able to compose and manipulate "effectful functions".  It can be argued that -->
<!-- this ability (of composition and modification) really makes effects something -->
<!-- we can meaningfully work with in Haskell, and gives them their richness. -->

<!-- [ode]: http://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad -->

<!-- Indeed, many problems in Haskell can be solved not by asking, "how should my -->
<!-- function behave?", but "how should my functions *compose*"?  Indeed, the power -->
<!-- of `Maybe` is often revealed only in meaningful compositions of `a -> Maybe -->
<!-- b`'s. -->

<!-- Well hey, if `a -> Maybe b` is useful...hey, remember that we *are* in a -->
<!-- series about `Auto`, which is like "a generalization of `(->)`" in a way. -->
<!-- Let's see how we could compose `Auto a (Maybe b)` :D -->

<!-- Inhibition -->
<!-- ---------- -->

<!-- So, with -->

<!-- One of the most fascinating parts of Haskell is the mechanisms by which we -->
<!-- take ordinary functions and manipulate them into representing "effects". -->
<!-- Things like IO actions, stateful transitions, logging, failure, error -->
<!-- propagation.  Overall, they provide us with a much richer toolset for -->
<!-- describing functions and function composition. -->

<!-- The "magic", however, is in the *composition* of these functions.  And their -->
<!-- modification, combination, etc. with respect to these effects.  Haskell's -->
<!-- category-theory inspired abstractions give us a way to work with "effectful" -->
<!-- functions in a way that respect and preserve their semantic meaning. -->

<!-- For example, a commonly cited effect is "the ability to do IO".  In Haskell, -->
<!-- nothing does IO.  Instead, you have a data structure (no different than any -->
<!-- ADT you could cook up yourself) that represents computer instructions. -->

<!-- ~~~haskell -->
<!-- getLine  :: IO String -->
<!-- putStrLn :: String -> IO () -->
<!-- ~~~ -->

<!-- `getLine` is just a normal data structure, and so is `putStrLn`. -->

<!-- However, with Functor, Applicative, and Monad, we have the ability to "treat -->
<!-- an `IO String` like a `String`", and abstract away the effect.  If we wanted -->
<!-- to `getLength`, an IO action that gets input and returns the length, we can -->
<!-- use `fmap`: -->

<!-- ~~~haskell -->
<!-- getLength :: IO Int -->
<!-- getLength = fmap length getLine -->
<!-- ~~~ -->

<!-- If we wanted to put the string that we just retrieved, we can use `(=<<)`: -->

<!-- ~~~haskell -->
<!-- echo :: IO () -->
<!-- echo = putStrLn =<< getLine -->
<!-- ~~~ -->







<!-- Fancy Bells And Whistles -->
<!-- ------------------------ -->

<!-- ### Adding Inhibition -->

<!-- ### Inhibition with a value -->

<!-- ### Over a Monad -->

<!-- ### Samples -->

<!-- Functional Reactive Programming and Continuous Time -->
<!-- --------------------------------------------------- -->

<!-- ### Continuous Time -->

<!-- ### Denotative semantics -->

<!-- ### The Model -->

<!-- Adding Time -->
<!-- ----------- -->

<!-- ### Samples -->

<!-- ### Preserving continuous time -->

<!-- All together -->
<!-- ------------ -->

<!-- Looking forward -->
<!-- --------------- -->


