Intro to Machines & Arrows: the Category Typeclass (Part 2)
===================================================================

Categories
:   Haskell
:   Ramblings
Tags
:   haskell
:   functional reactive programming
:   machines
:   arrows
:   category
CreateTime
:   2014/02/04 21:18:53
PostDate
:   Never
Series
:   Intro to Machines and Arrows
Identifier
:   machines-2

In this post we're going to be continuing on with building up from simple
machines like Stream and Auto to a semantic picture of Arrowized Functional
Reactive Programming.

In [the last post][part1], we introduced two simple machines, the Stream and
the Auto.  In this post we're going to be focusing on Auto and recognizing it
as a member of a very powerful typeclass (or two!).  In the next, we bridge
the gap and introduce the final machine we'll be working with, the Wire, and
transition into the popular AFRP library [netwire][].  Join along if you wish!

[part1]: http://blog.jle.im/entry/intro-to-machines-arrows-part-1-stream-and
[netwire]: http://hackage.haskell.org/package/netwire

Recap
-----

In the last post, we introduced first a simple stream ---

~~~haskell
!!!machines/Stream.hs "newtype Stream"
~~~

Which we saw as a an infinitely long linked list, or an infinitely long stream
of values that had:

1.  An internal state that progresses deterministically as a function of the
    last.
2.  An output (a "head") of type `b`, a function of the internal state.

We saw this with a [simple stream][simplestream] that counts from 1 to
infinity, whose state was a number that counted up and whose output at every
step was just that same number.

!!![simplestream]:machines/Stream.hs "myStream:"

We saw that having a time-varying behavior that could not be affected by the
outside world was kinda limiting, so we then looked at Auto:

~~~haskell
!!!machines/Auto.hs "newtype Auto"
~~~

Which is a stream, but every time you "ask" for the next value, you give an
"influencing input".  Autos have:

1.  An influencing input of type `a` that is taken at every step
2.  An internal state that is a function of the previous state and the
    influencing input
3.  An output "head" of type `b`, a function of the internal state.

And we looked at [a simple auto][simpleauto] we which was like our stream, but
at every step could be "reset" with a value.

!!![simpleauto]:machines/Auto.hs "settableAuto:"

Then we took another approach to looking at this --- we thought about Autos as
functions "with state".  As in, `Auto a b` was like a function `a -> b`, but
which had an internal state that updated every time it was called.  In this
sense, `Stream b` was like a function `() -> b`, or a "constant"...yet the
constant possibly changed every time you asked for it.

We saw this in an auto that [returns the sum][summer] of everyhting you have
given it.

!!![summer]:machines/Auto.hs "summer:"

Autos are "function-like things"...they map or "morph" things of type `a` to
things of type `b` in some form, just like functions.  It looks like we
stumbled onto some sort of design pattern.  Wouldn't it be cool if we could
treat Autos the same way we treat functions?  And reason about them the same
way, think about them using the same logic?

What is the *essense* of function-like-ness?

The Essense of Function-like-ness
---------------------------------

Okay, so using "function-like-ness" more than a few times is slightly silly so
I'm going to introduce a word or two.  These things I will call *morphisms*.
Sometimes you'll see them called "arrows", but this is a slightly loaded word
as there is an Arrow typeclass in Haskell that doesn't exactly refer to the
same thing.

Here is my claim: the "essense" of this functionlikeness is their ability to
*compose* and "chain", and the *nature* of that composition process.

That is, if you have a morphism `f` from `a` to `b`, and a morphism `g` from
`b` to `c`, then you can "compose" or "chain" them to get a morphism from `a`
to `c`.

In Haskell we use the `(.)` operator for this --- to say more formally:

~~~haskell
f     :: Morphism a b
g     :: Morphism b c
g . f :: Morphism a c
~~~

Some important aspects of the nature of this composition is that it must
"associate".  That means:

~~~haskell
(h . g) . f == h . (g . f)
~~~

Composing the composition of `h` and `g` to `h` should be the same as
composing `h` with the composition of `g` and `f`.

The final feature is that there must exist some "identity" morphism that
leaves other morphisms unchanged under composition:

~~~haskell
id :: Morphism b b

id . f  == f
g  . id == g
~~~

It doesn't really matter what `id` literally "does" --- it only matters that
it leaves morphisms unchanged.

And...that's it!

### Functions are morphisms

We're just going to take a quick detour verify that normal functions satisfy
this new notion of "function-likeness"...so that we aren't crazy.

In Haskell, our functions are things of type `a -> b` --- a morphism from
`a` to `b`.

Our composition operator is the familiar `(.)` from Prelude.  You can prove
all of the above laws yourself using the definition of `(.)`:

~~~haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)
~~~

In practice, you can see associativity

~~~haskell
ghci> ((< 20) . (+4)) . (^2) $ 4
True
ghci> (< 20) . ((+4) . (^2)) $ 4
True
~~~

The identity is just Prelude's `id`:

~~~haskell
id :: a -> a
id x = x
~~~

~~~haskell
ghci>      (*3) $ 7
21
ghci> id . (*3) $ 7
21
ghci> (*3) . id $ 7
21
~~~

<div class="note">
**Aside**

I mean it, you can prove it yourself if you are bored some time :)  I'll start
you off with one of the identity laws:

~~~haskell
g . id = \x -> g (id x)
       = \x -> g x
       = g
~~~
</div>

So cool...this intuition applies to our actual idea of functions, so we are on
a sane track!

### Autos are morphism!

So we see that functions fit this idea.  Let's jump back to what we were
trying to show in the first place --- that Autos fit this "idea" of
function-like things, or morphisms.

Let's say I had an `f :: Auto a b` and a `g :: Auto b c`.  I want to "compose"
them --- `g . f`.  To get an `Auto a c`, somehow.  What would that even mean?

Well...if we think of `f` like a stateful function that takes in an `a` and
pops out a `b`...and `g` like a stateful function that takes in a `b` and pops
out a `c`...We can think of `g . f` as a stateful function that takes in an
`a`, feeds it to `f`, gets the `b` that `f` pops out, feeds that to `g`, and
gets the final `c` out at the end.

Also, Autos spit out both the result (the `c`) and the "updated Auto"...so the
updated Auto of the composition can just be the composition of the updated
Autos!

Enough talk, let's code!  We'll call our composition operator `(~.~)`.

~~~haskell
(~.~) :: Auto b c -> Auto a b -> Auto a c
g ~.~ f = ACons $ \x -> let (y, f') = runAuto f x
                            (z, g') = runAuto g y
                        in  (z, g' ~.~ f')
~~~

And...that should be it!  We run the input through first `f` then `g`,
collecting the "modified `f` and `g`", and returning both of those at the end,
composed.

Let's write a useful helper function so that we have more things to test this
out on:

~~~haskell
toAuto :: (a -> b) -> Auto a b
toAuto f = ACons \x -> (f x, toAuto f)
~~~

`toAuto` basically turns a function `a -> b` into a stateless `Auto a b`.

Time to test these out!

~~~haskell
ghci> let doubleA  = toAuto (*2)      :: Auto Int Int
ghci> let succA    = toAuto (+1)      :: Auto Int Int
ghci> let constA x = toAuto (const x) :: a -> Auto b a

ghci> testAuto_ doubleA [1..10]
[2,4,6,8,10,12,14,16,18,20]

ghci> testAuto_ (succA ~.~ doubleA) [1..10]
[3,5,7,9,11,13,15,17,19,21]

ghci> testAuto_ (succA ~.~ constA 20) [1,2,undefined]
[21,21,21]

ghci> testAuto_ summer [5,1,9,2,-3,4]
[5,6,15,17,14,18]

ghci> testAuto_ (double ~.~ summer) [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ settableAuto [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,2,-3,-2,-1]

ghci> testAuto_ summer [1,2,-3,-2,-1]
[1,3,0,-2,-3]

ghci> testAuto_ (summer ~.~ settableAuto)
    |     [Nothing,Nothing,Just (-3),Nothing,Nothing]
[1,3,0,-2,-3]

ghci> take 10 $ testAuto_ (settableAuto ~.~ constA Nothing) [(),()..]
[1,2,3,4,5,6,7,8,9,10]
~~~

And it looks like our Autos really can meaningfully compose!

Well, wait.  We need one last thing: the identity Auto:

~~~haskell
idA :: Auto a a
idA = ACons $ \x -> (x, idA)
~~~

~~~haskell
ghci> testAuto_ summer [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ (idA ~.~ summer) [5,1,9,2,-3,4]
[10,12,30,34,28,39]

ghci> testAuto_ (summer ~.~ idA) [5,1,9,2,-3,4]
[10,12,30,34,28,39]
~~~

### Category

These concepts are actually formalized in the mathematical concept of a
"category" --- things with objects and morphisms between them, following
certain properties (like the ones I mentioned earlier).

In Haskell, we often consider our objects as Haskell types; our usual
morphisms is the function arrow, `(->)`[^func] --- but in this case, it might be
interesting to consider a different category --- the category of Haskell types
and morphisms `Auto`.

[^func]: Remember, we can write `a -> b` as `(->) a b`; like other operators,
`(->)` can be used both infix and prefix.

In Haskell, we have a typeclass that allows us to do generic operations on all
Categories --- so now we can basically treat `Auto`s "as if" they were `(->)`.
We can literally "abstract over" the idea of a function.  Neat, huh?

~~~haskell
class Category r where
    id  :: r a a
    (.) :: r b c -> r a b -> r a c
~~~

Basically, with Category, we can "abstract over" function composition and
`id`.  That is, insteaed of `(.)` being only for composing normal
functions...we can use to compose morphisms in any Category, like Auto!  We
can also write "generic code" that works on *all* morphisms --- not just
`(->)`!  This is like having functions `mapM` and `sequence` --- which work
for *all* Monads, not just IO or Maybe or something.  We can reason about
Monads as things on their own, instead of just as isolated instances.

First, let's write the `(->)` Category instance:

~~~haskell
instance Category (->) where
    id    = \x -> x
    g . f = \x -> g (f x)
~~~

And then our `Auto` Category instance:

~~~haskell
instance Category Auto where
    id    = ACons $ \x -> (x, id)
    g . f = ACons $ \x -> let (y, f') = runAuto f x
                              (z, g') = runAuto g y
                          in  (z, g' . f')
~~~

And now... we can work with both `(->)` and `Auto` as if they were the "same
thing" :)

~~~haskell
doTwice :: Category r => r a a -> r a a
doTwice f = f . f
~~~

~~~haskell
ghci> doTwice (*2) 5
20
ghci> testAuto_ (doTwice (toAuto (*2))) [5]
[20]
ghci> testAuto_ (doTwice summer) [5,1,9,2,-3,4]
[5,11,26,43,57,61]
ghci> take 6 $ testAuto_ (doTwice summer) (repeat 1)
[1,3,6,10,15,21]
~~~

The main cool thing here is that we can now abstract over the "idea" of `id`
and `(.)`, and now our Autos have basically not only captured the idea of
function-ness, but can now literally act like normal functions in our code.  I
mentioned something similar in an earlier post [in MonadPlus][monadplus] ---
the ability to have a "common language" to talk and abstract over many things
is powerful not only for expressiveness but for reasoning and maintainability.

[monadplus]: http://blog.jle.im/entry/the-list-monadplus-practical-fun-with-monads-part


<!-- More Typeclasses! -->
<!-- ----------------- -->

<!-- So because we now love typeclasses so much, let's see what other useful -->
<!-- abstractions we can apply to Auto. -->

<!-- This section is just going to be a whirlwind tour of instancing Auto as -->
<!-- various useful typeclasses --- mostly typeclassess that we'll be using later. -->

<!-- ### Functor -->

<!-- Of course we already spent a lot of time talking about Functor, so we might as -->
<!-- well start here. -->

<!-- What is a functor?  It represents something that can be mapped over.  More -->
<!-- pedantically, it is something that implements a lawful `fmap`. -->

<!-- ~~~haskell -->
<!-- class Functor f where -->
<!--     fmap :: (a -> b) -> f a -> f b -->
<!-- ~~~ -->

<!-- Can we make `Auto` a Functor? -->

<!-- Actually...well...not really.  It really doesn't make sense, if you think -->
<!-- about it.  If you look at the type signature for `fmap` and substitute `f ~ -->
<!-- Auto`, you'll immediately see why: -->

<!-- ~~~haskell -->
<!-- fmap :: (a -> b) -> Auto a -> Auto b -->
<!-- ~~~ -->

<!-- You can't have a function `Auto a -> Auto b`...it doesn't really make sense -->
<!-- because `Auto a` isn't even the type of a value, a concrete type. -->

<!-- Conceptually, you can think of this as asking "What am I even mapping over?", -->
<!-- and see that you can't really "map over" `a b` with one function. -->

<!-- *BUT*, it *does* make sense for `Auto i` to be a Functor: -->

<!-- ~~~haskell -->
<!-- fmap :: (a -> b) -> (Auto i) a -> (Auto i) b -->
<!-- ~~~ -->

<!-- Okay, what would this even mean? -->

<!-- `Auto i` is something that takes an `i` as an input.  `Auto i a` is something -->
<!-- that outputs an `a`.  So if `Auto i` is a functor...it means that I can turn -->
<!-- `Auto i a` into `Auto i b` with a function `a -> b`.  I can turn an Auto -->
<!-- taking an `i` and outputting an `b` into an Auto taking an `i` and outputting -->
<!-- a `b`.  I "map over" the *output*. -->

<!-- So `Auto i` is a functor where you can `map` the output.  If I was going to -->
<!-- output a `5`, if I `fmap (+1)`, I'd actually output a `6`. -->

<!-- ~~~haskell -->
<!-- instance Functor (Auto i) where -->
<!--     fmap f a =  ACons $ \x -> -->
<!--                   let (y  , a') = runAuto a x -->
<!--                   in  (f y, fmap f a') -->
<!-- ~~~ -->

<!-- ~~~haskell -->
<!-- λ: testAuto_ settableAuto -->
<!--   |  [Nothing,Nothing,Just (-3),Nothing,Nothing] -->
<!-- [1,2,-3,-2,-1] -->

<!-- λ: testAuto_ (fmap (*2) settableAuto) -->
<!--   |  [Nothing,Nothing,Just (-3),Nothing,Nothing] -->
<!-- [2,4,-6,-4,-2] -->
<!-- ~~~ -->

<!-- Functor...check!  What next? -->

<!-- ### Applicative -->

<!-- Everyone knows that the "cool", *hip* typeclasses are the classic trio, -->
<!-- [Functor, Applicative, Monad][fam].  Let's just move on right along and go for -->
<!-- Applicative. -->

<!-- [fam]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html -->

<!-- What is an Applicative functor, anyway?  It really has two things: the ability -->
<!-- to apply functions "inside" containers to values "inside" containers, and the -->
<!-- ability to wrap a value in a default context/container (all following the laws -->
<!-- of course).  The second part is going to be more immediately useful to -->
<!-- us.[^pointed] -->

<!-- [^pointed]: Actually this "wrapping" property really was kind of jammed into -->
<!-- Applicative, it technically belongs [Pointed][] typeclass, and Applicative is -->
<!-- technically only supposed to mean the function wrapping ability, under some -->
<!-- interpretations.  But splitting up typeclasses to such a fine degree isn't -->
<!-- quite practical. -->

<!-- [Pointed]: http://hackage.haskell.org/package/pointed-4.0/docs/Data-Pointed.html -->

<!-- ~~~haskell -->
<!-- class Functor f => Applicative f where -->
<!--     pure  :: a -> f a -->
<!--     (<*>) :: f (a -> b) -> f a -> f b -->
<!-- ~~~ -->

<!-- Again, we see that it doesn't make too much sense for `Auto` to be an -->
<!-- Applicative...but `Auto i` definitely can be. -->

<!-- ~~~haskell -->
<!-- pure :: a -> (Auto i) a -->
<!-- ~~~ -->

<!-- Can you think of a function that has this type signature? -->

<!-- As it turns out, due to [parametricity][], there is actually only exactly one -->
<!-- meaningful function that has this type signature. -->

<!-- [parametricity]: http://en.wikipedia.org/wiki/Parametricity -->

<!-- What must that `Auto i a` be?  Well, it clearly must output an `a`.  Can it -->
<!-- possibly incorporate `i` in any way?  It can't!  Because it can't really -->
<!-- "make" any `a`s besides the one given to it in `pure k`.  So that `pure k :: -->
<!-- Auto i a` must be an Auto that ignores its input `i` and always returns `k` -->
<!-- every time. -->

<!-- It must be the "constant" arrow. -->

<!-- ~~~haskell -->
<!-- instance Applicative (Auto i) where -->
<!--     pure k = ACons $ \_ -> (k, pure k) -->
<!--     (<*>)  = undefined      -- for now -->
<!-- ~~~ -->

<!-- ~~~haskell -->
<!-- λ: testAuto_ (pure 5) [1..10] -->
<!-- [5,5,5,5,5,5,5,5,5,5] -->

<!-- λ: testAuto_ (summer . pure 5) [1..10] -->
<!-- [5,10,15,20,25,30,35,40,45,50] -->

<!-- λ: testAuto_ (pure 5 . summer) [1..10] -->
<!-- [5,5,5,5,5,5,5,5,5,5] -->
<!-- ~~~ -->

<!-- As it turns out, `pure k` is the same as the `constant k` that we defined -->
<!-- earlier.  Now we just have a more semantically meaningful way of constructing -->
<!-- it instead of using `autoize` -->

<!-- I'll leave the implementation of `(<*>)` as an exercise, partially because -->
<!-- it's not too surprising and would be a fun thing to work out on your own, and -->
<!-- partially because `pure` is more useful for the time being. -->

<!-- ### Monad -->

<!-- So a Monad, conceptually, is just a functor we can "squish".  Basically, we -->
<!-- need the function: -->

<!-- ~~~haskell -->
<!-- join :: (Auto i) ((Auto i) a) -> (Auto i) a -->
<!-- ~~~~ -->

<!-- Basically, we must turn an Auto returning an Auto into just an Auto returning -->
<!-- a value. -->

<!-- We must turn a morphism returning a morphism and turn it into a morphism -->
<!-- returning a value. -->

<!-- Typically, this is done by feeding your `i` into the outside Auto to get a -->
<!-- returned Auto, and feeding that *same* `i` into the returned Auto, and getting -->
<!-- the result of that. -->

<!-- This actually gives you a *Reader-like* monad behavior, for Auto --- the -->
<!-- ability to chain together multiple Auto's all together with a "common" -->
<!-- input/environment. -->

<!-- However, even though this is interesting, we actually won't be using the Monad -->
<!-- instance of `Auto` all too much for now either.  I'll again leave this as an -->
<!-- exercise.  The solutions for all of these exercises are available in the -->
<!-- sample code for the article. -->

<!-- Arrow -->
<!-- ----- -->

<!-- Now, one final typeclass: Arrow. -->

<!-- As it turns out, `Category` by itself is nice, but for the games we will -->
<!-- eventually be playing with function composition, it doesn't offer too much in -->
<!-- terms of combinators. -->

<!-- There is a neat Haskell extension that provides syntactic sugar for complex, -->
<!-- multi-way, side-chained compositions, called "proc notation". Proc notation -->
<!-- will prove invaluable to us eventually, but it requires some more Category -->
<!-- combinators to work. -->

<!-- As it turns out, the `Arrow` typeclass exists as a general grab-bag of -->
<!-- combinators to make life a lot easier for us. -->

<!-- ~~~haskell -->
<!-- class Category r => Arrow r where -->
<!--     arr    :: (a -> b) -> r a b -->
<!--     first  :: r a b -> r (a,c) (b,c) -->
<!--     second :: r a b -> r (c,a) (c,b) -->
<!--     (***)  :: r a b -> r c d -> r (a,c) (b,d) -->
<!--     (&&&)  :: r a b -> r a c -> r a (b,c) -->
<!-- ~~~ -->

<!-- ### instance Arrow -->

<!-- Let's write an Arrow instance for Auto. -->

<!-- `arr` takes a normal function and turns it into a pure Auto --- we wrote this -->
<!-- before, it's just `functionToAuto` -->

<!-- `first` takes an Auto and turns it into an Auto that only operates on the -->
<!-- first part of a tuple.  `second` is the same, but for the second part. -->

<!-- `(***)` takes two Autos and makes an Auto that applies them "in parallel" to -->
<!-- two parts of a tuple.  `(&&&)` takes two Autos that both take the same type, -->
<!-- and makes an Auto that applies both Autos to the same value "in parallel" and -->
<!-- returns the results as a tuple. -->

<!-- ~~~haskell -->
<!-- instance Arrow Auto where -->
<!--     arr f     = ACons $ \x -> (f x, arr f) -->
<!--     first a   = ACons $ \(x,y) -> -->
<!--                   let (x', a') = runAuto a x -->
<!--                   in  ((x', y), first a') -->
<!--     second a  = ACons $ \(x,y) -> -->
<!--                   let (y', a') = runAuto a y -->
<!--                   in  ((x, y'), second a') -->
<!--     a1 *** a2 = ACons $ \(x,y) -> -->
<!--                   let (x', a1') = runAuto a1 x -->
<!--                       (y', a2') = runAuto a2 y -->
<!--                   in  ((x',y'), a1' *** a2') -->
<!--     a1 &&& a2 = ACons $ \x -> -->
<!--                   let (y1, a1') = runAuto a1 x -->
<!--                       (y2, a2') = runAuto a2 x -->
<!--                   in  ((y1,y2), a1' &&& a2') -->
<!-- ~~~ -->

<!-- Don't be too mystified by the Arrow typeclass.  Really, Arrows are just -->
<!-- Categories for which we have defined ways to chain compositions side-by-side. -->
<!-- Things like `(***)` and `(&&&)` are pretty useful if we want to be able to -->
<!-- compose multiple functions in fork-like ways, or to compose two functions -->
<!-- side-by-side with another function.  We will end up doing this a lot when we -->
<!-- work with AFRP, so this is pretty handy to have. -->

<!-- As it turns out, there are actually a lot of specialized Arrow typeclasses, -->
<!-- too, which are the same sort of "grab bag" of combinators, except for -->
<!-- different purposes. -->

<!-- The more useful ones for our purpose will be ArrowChoice and ArrowLoop, and -->
<!-- you can read into these.  It actually isn't all too important right now to -->
<!-- understand how these are implemented --- just know that ArrowChoice gives you -->
<!-- combinators for forking compositions and ArrowLoop gives you combinators for -->
<!-- *recursive* compositions. -->

<!-- ### Proc Notation -->

<!-- The *main* purpose for Arrow (in our situation) is that now that we have -->
<!-- instanced Arrow for our Autos, we can now use it in proc notation. -->

<!-- This is similar to how once we instance something as a Monad, we can use it in -->
<!-- "do" notation. -->

<!-- Proc notation is basically "do notation for Arrows", and is just syntactical -->
<!-- sugar for the various Arrow combinators we described before. -->

<!-- Proc notation consists of lines of cute little ASCII "arrows": -->

<!-- ~~~haskell -->
<!-- output <- arrow -< input -->
<!-- ~~~ -->

<!-- where `arrow` is the Arrow, `input` is the "input" fed into the Arrow, and -->
<!-- "output" binds the result to the name "output".  We can omit "output" and -->
<!-- we will "forget" the output. -->

<!-- Cute, right? -->

<!-- For example, to write our `succ . double` composition we wrote earlier: -->

<!-- ~~~haskell -->
<!-- doubleSucc1 :: Auto Int Int -->
<!-- doubleSucc1 = succ . double -->

<!-- doubleSucc2 :: Auto Int Int -->
<!-- doubleSucc2 = proc n -> do -->
<!--   doubled <- double -< n -->
<!--   succ -< doubled -->
<!-- ~~~ -->

<!-- What if we wanted the arrow to return a tuple with the result of doubling, and -->
<!-- also the result of doubling then succing?? -->

<!-- ~~~haskell -->
<!-- doubleSucc'1 :: Auto Int (Int,Int) -->
<!-- doubleSucc'1 = (id *** succ) . (double &&& id) -->

<!-- doubleSucc'2 :: Auto Int (Int,Int) -->
<!-- doubleSucc'2 = proc n -> do -->
<!--   doubled      <- double -< n -->
<!--   doubleSucced <- succ   -< doubled -->
<!--   returnA -< (doubled, doubleSucced) -->
<!-- ~~~ -->

<!-- `returnA` is just `id` (for Category Auto), the identity Arrow.  But we call -->
<!-- it `returnA` to draw an analogy between `return` for Monads.  They both serve -->
<!-- the same purpose --- they take normal values and turn them into something you -->
<!-- can use in a do/proc block. -->

<!-- So basically, every line in a proc block must look like: -->

<!-- ~~~haskell -->
<!-- arrow -< input -->
<!-- ~~~ -->

<!-- Or, if you want to name the result for later use, -->

<!-- ~~~haskell -->
<!-- output <- arrow -< input -->
<!-- ~~~ -->

<!-- Just like how "do" blocks compose several monad values into one giant monad -->
<!-- value, "proc" blocks compose several morphisms/arrows into one giant arrow. -->

<!-- Remember that proc blocks don't actually "do" anything.  You aren't sequencing -->
<!-- actions.  You basically are creating a *dependency* graph --- saying which -->
<!-- arrows depend on the output of which arrows, and how they all twist and -->
<!-- combine together. -->

<!-- In `doubleSucc'2`, we are saying this: -->

<!-- "If you want to run this arrow, and you give us a value `n`, then the result -->
<!-- is a tuple `(doubled, doubleSucced)`, where `doubled` is the result of running -->
<!-- that `n` through `double`, and `doubleSucced` is the result of running -->
<!-- `doubled` through `succ`." -->

<!-- So when we eventually use proc notation with our Auto, it describes one giant -->
<!-- "tick" of the big function. -->

<!-- #### proc rec -->

<!-- Finally, we often will need to have Autos that "depend" on eachother in a -->
<!-- cyclic way.  For example, in a harmonic oscillator system, the position -->
<!-- depends on the force applied to the object, but the force applied depends on -->
<!-- the position. -->

<!-- These "recursive" bindings come from combinators provided by the ArrowLoop -->
<!-- typeclass: -->

<!-- ~~~haskell -->
<!-- object :: Auto () Int -->
<!-- object = proc _ -> do -->
<!--   rec -->
<!--     let acc = -1 * pos -->
<!--     vel <- summer -< acc -->
<!--     pos <- summer -< vel -->
<!--   returnA -< pos -->
<!-- ~~~ -->

<!-- So we can have `pos` depend on `acc`, and `acc` depend on `pos`. -->

<!-- Hopefully our Autos im -->
