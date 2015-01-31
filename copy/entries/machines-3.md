Effectful Arrows: Intro to Machines and Arrow Part 3
====================================================

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

[chapman]:http://blog.jle.im/entry/looking-forward-a-doctorate-program

In the [last post][part2], we looked deeper into the Auto type, played
around with instancing it as familiar typeclasses, saw it as a member of the
powerful *Category* and *Arrow* typeclasses, and took advantage of this by
composing Autos both manually and using proc/do notation, and were freed from
the murk and mire of explicit recursion.  We observed the special nature of
this composition, and saw some neat properties, like local statefulness.

[part2]: http://blog.jle.im/entry/auto-as-category-applicative-arrow-intro-to-machines

Here we are going to push the abstraction further to see where it will go by
introducing mechanisms for adding effects, making the plain ol' Auto type into
something rich and featureful. And finally, at the very end, we'll do a short
case study on one motivating example: arrowized FRP libraries!

As always, feel free to leave a comment if you have any questions, drop by
freenode's *#haskell*, or find me on [twitter][] :)

[twitter]: https://twitter.com/mstk "Twitter"

Effectful Stepping
------------------

Recall our original definition of `Auto a b` as a newtype wrapper over a
function:

~~~haskell
a -> (b, Auto a b)
~~~

This can be read as saying, "feed the `Auto` an `a`, and (purely) get a
resulting `b`, and a 'next stepper'" --- the `b` is the result, and the `Auto
a b` contains the information on how to proceed from then on.

Well, if you've been doing Haskell for any decent amount of time, you can
probably guess what's going to happen next.  It's, incidentally, one of the
things that Haskellers love doing the most, and it's been engrained deeply
into the Haskell culture.

Instead of "purely" creating a naked result and a "next step"...we're going to
return it in a context.

~~~haskell
a -> f (b, Auto a b)
~~~

Whaat, you say?  What good does that do?

Well, what does returning things in a context ever lets you do?

In Haskell, contexts like these are usually meant to be able to defer the
process of "getting the value" until the end, after you've built up your
contextual computation.  This process can be complicated, or simple, or
trivial.

For example, a function like:

~~~haskell
a -> b
~~~

means that it simply creates a `b` from an `a`.  But a function like:

~~~haskell
a -> Maybe b
~~~

Means...it *might* give you a `b` from an `a`?  Or it might not?  You won't
really know until you inspect the result later.  If you eventually use
`fromMaybe`, then the resulting `Maybe b` can *control*, using its Nothingness
or its Justness, the final `b` that you get.

A function like:

~~~haskell
a -> State s b
~~~

Means that, given an `a`, you get a state machine that can *create a `b`*
using a stateful process, once given an initial state.  The `b` doesn't
"exist" yet; all you've given is instructions for creating that `b`...and the
`b` that is eventually created will in general depend on whatever initial `s`
you give the state machine.

A function like:

~~~haskell
a -> IO b
~~~

Means that, given an `a`, you're given *a computer program* that, when
executed by a computer, will generate a `b`.  The `b` doesn't "exist" yet;
depending on how the world is and how IO processes interact, how you are
feeling that day...the `b` generated will be different.  The process of IO
execution has the ability to *choose* the `b`.

So how about something like:

~~~haskell
a -> State s (b, Auto a b)
~~~

This means that, given `a`, "running" the `Auto` with an `a` will give you *a
state machine* that gives you, using a stateful process, both the *result* and
the *next step*.  The crazy thing is that now you are given the state machine
*the ability to decide the next `Auto`*, the next "step".


Something like:

~~~haskell
a -> IO (b, Auto a b)
~~~

means that your new `Auto`-running function will give you a result and a "next
step" that is going to be dependent on IO actions.

Let's jump straight to abstracting over this and explore a new type, shall we?

### Monadic Auto

~~~haskell
!!!machines/Auto3.hs "newtype AutoM" machines
~~~

We already explained earlier the new power of this type.  Let's see if we can
write our favorite instances with it.  First of all, what would a `Category`
instance even do?

Recall that the previous `Category` instance "ticked" each `Auto` one after
the other and gave the final results, and then the "next Auto" was the
compositions of the ticked autos.

In our new type, the "ticking" happens *in a context*.  And we need to tick
twice; and the second one is dependent on the result of the first.  This means
that your context has to be *monadic*, in order to allow you to do this.

The neat thing is that Haskell's built-in syntax for handling monadic
sequencing is nice, so you might be surprised when you write the `Category`
instance:

~~~haskell
!!!machines/Auto3.hs "instance Monad m => Category (AutoM m)" machines
~~~

Does it look familiar?

It should!  Remember the logic from the `Auto` Category instance?

~~~haskell
!!!machines/Auto2.hs "instance Category Auto" machines
~~~









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


