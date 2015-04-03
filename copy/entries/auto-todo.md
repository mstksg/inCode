Auto: A Todo GUI application with Auto and GHCJS
================================================

Categories
:   Haskell
:   Auto
:   Tutorials
Tags
:   auto
:   haskell
:   ghcjs
CreateTime
:   2015/03/26 00:15:21
PostDate
:   Never
Series
:   All About Auto
:   Beginner/Intermediate Haskell Projects
Identifier
:   auto-todo

Continuing along with [All About Auto][series], let's look at another exciting
and useful application of the [*auto*][] library: GUI's.  We're going to look
at the canonical "hello world" of GUI apps these days --- the todo app.  We're
going to be using the specs of [todoMVC][] to build a todoMVC "candidate" that
follows the specs...and along the way see what *auto* offers in its tools of
managing isolated state components and modeling GUI logic.  We're really going
to be focusing on application logic --- "control" and "model" --- and not
looking too close on "views", which *auto* doesn't quite try to offer and
where you can really pick your own view rendering system, making this
adaptable to really any platform --- javascript/web, desktop, command line,
etc.; in particular, we're going to be using the low-level *[ghcjs-dom][]*
library to render our front end for the purpose of this tutorial.

[series]: http://blog.jle.im/entries/series/+all-about-auto
[auto]: http://hackage.haskell.org/package/auto
[todoMVC]: http://todomvc.com/
[ghcjs-dom]: http://hackage.haskell.org/package/ghcjs-dom

A live version of our end-product [is hosted and online][demo].

[demo]: https://mstksg.github.com/auto-examples/todo

This post does assume *some* concepts from the [tutorial][]...if not all, then
at least those in the [introductory post][intro] or the [README][].  If you
ever find yourself thinking that these concepts are completely new and crazy,
you might want to try looking through the [tutorial][] or [docs][auto] to
refresh your mind.  As always, comments are welcome, and I'm also usually on
*#haskell-auto* as *jle`*, and also on [twitter][]

[tutorial]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md
[intro]: http://blog.jle.im/entry/introducing-the-auto-library
[README]: https://github.com/mstksg/auto/blob/master/README.md
[twitter]: https://twitter.com/mstk "Twitter"

This post is also a 

<!-- (Fair warning...this is not quite a "ghcjs tutorial", if that's what you're -->
<!-- looking for; it's an auto tutorial that uses some rudimentary ghcjs. -->
<!-- Hopefully you can learn from that too!) -->

Overall Layout
--------------

At the highest level, *auto* is a library that provides us tools to build and
worth with stream transformers on streams of values.  Transform a stream of
input values to a stream of output values.  So, let's try to phrase our Todo
app problem in that perspective.  What are our inputs, and what are our
outputs?

For a Todo app, the outputs are probably going to be a *todo list* itself.  If
we're building a GUI, then having the todo list itself is going to be enough
to build our front-end display.  The stream of *inputs* is a little less
obvious, but, well, what does an app really take as inputs?  Commands!  Our
stream of inputs will be commands sent by a GUI or by whatever front-end we
choose.  Our todo app then is a transformer of a stream of commands to a
stream of todo lists...where the todo list we get changes as we process more
commands.

So the "overall loop" will be:

1.  A front-end rendered by *ghcjs-dom* (or whatever) with event handlers that
    drop commands into a concurrent `Chan` queue.  This just handles
    rendering.
2.  Our `Auto` launched with `runOnChan`, which waits on the `Chan` queue,
    runs the inputs through the `Auto`, and renders the result.  This handles
    all of the logic.

We like types in Haskell, so let's begin by laying out our types!

~~~haskell
data TodoInp = IAdd  String
             | ITask TaskID TaskCmd
             | IAll TaskCmd
             deriving Show

data TaskCmd = CDelete
             | CPrune
             | CComplete Bool
             | CModify String
             | CNop
             deriving Show

type TaskMap = IntMap Task
type TaskID  = Int

data Task = Task { taskDescr     :: String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

-- from Data.Serialize, from the cereal library
instance Serialize Task
~~~

We have a type to represent our inputs, `TodoInp`, which can be an
"add" command with a `String`, a "task" command with a `TaskId` (`Int`) and a
`TaskCmd`, and an "all" command with a `TaskCmd` that is supposed to represent
sending that command to all tasks.

Our `TaskCmd` represents commands we can send to individual tasks -- we can
delete, prune (delete if completed), set the "completed" flag, or modify the
description.

We're going to represent our task list, `TaskMap`, as not a `[]` list, but as
an `IntMap` from *containers*, which associates an `Int` to a `Task` that we
can look up with the `IntMap` API.  What would a `TaskMap` store other than a
bunch of `Task`s, which we are defining as jus a tupling of a `String`
description and a `Bool` completed/uncompleted status.

### The Todo Auto

Time to go over the logic portion!  The part that *auto* is meant for!  We're
going to structure the logic of our app (also known as the "model") by using
principles of local statefulness to avoid ever working with a "global state",
and working in a declarative, high-level manner.

### Tasks

It's clear that the core of our entire thing is going to be the "task list"
construct itself...something that can dynamically add or remove tasks.

In *auto*, there is a construct created just for this kind of situation:
dynamic collections indexed by a key (a "task id"), where you can add or
subtract `Auto`s from dynamically --- they are `dynMap` and `dynMapF` from
*[Control.Auto.Collection][cacol]*.  We'll be using `dynMapF` because it's
serializable, and we don't need the extra power that `dynMap` offers.

[cacol]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Collection.html

~~~haskell
dynMapF :: (k -> Interval m a b)    -- ^ function to initialize new `Auto`s
        -> a                        -- ^ default inputs
        -> Auto m ( IntMap a        -- ^ input for each internal `Auto`, indexed by key
                  , Blip [k]        -- ^ blip stream to initialize new `Auto`s
                  )
                  (IntMap b)        -- ^ `Auto` outputs, by key
~~~

`dynMapF` keeps a "dynamic collection" of `Interval m a b`s, indexed by an
`Int` key, or an "ID".  It takes as input a stream of `IntMap a`...basically a
bunch of `(Int, a)` pairs.  `dynMapF` routes each input to the `Interval` at
that ID/address (with a suitable "default" `a` if none was sent in), and then
outputs all of the results as an `IntMap b` --- a bunch of `(Int, b)` pairs,
each output with the address of the `Auto` that made it.

For example, `IntMap.singleton 5 True` would send `True` to the `Auto` stored
at `5`.  It'll then output something that includes `(5, "received True!")` ---
the output of the `Auto` at slot 5.

Whenever an `Interval` turns "off" (is `Nothing`), it is removed from the
collection.  In this way we can have `Auto`s "turn themselves off".

It also takes as input a blip stream of `[k]`s.  We use each emitted `k` to
"initialize a new `Interval`" and throw it into the collection, creating a new
unique key for it.  Every time a new `Auto` is initialized, `dynMapF` creates
a new key for it.

Read over the [tutorial section on blip streams and `Interval`s][blipint] if
you are still unfamiliar with them.

[blipint]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md#semantic-tools

This pretty much fits exactly what we want for our task collection.  If we
imagined that we had our `Task` as an `Auto`:

~~~haskell
initTask :: Monad m => String -> Interval m TaskCmd Task
~~~

`initTask` takes a string (a starting description) and initializes an
`Interval` that takes in a stream of task commands, and has a stream of new,
updated `Task`s as its output stream.  At every step, it processes the command
and outputs the new appropriate `Task`.

We can then use this as our "initializer" for `dynMapF`...and now we have a
dynamic collection of tasks!

~~~haskell
taskCollection :: Auto (IntMap TaskCmd, Blip [String]) (IntMap Task)
taskCollection = dynMapF initTask CNop
~~~

If we wanted to send in the command `CModify "hey!"` to the task whose
id/key/address is `12`, I'd feed in `IntMap.singleton 12 (CModify "hey!")`.
The output would then contain the output of feeding that `CModify` to the
`Auto` at that slot 12, associated with slot 12 on the output `IntMap`.

Writing `initTask` and the task `Auto` is straightforward with `accum`, which
is basically like `foldl` on the inputs and a "current state".  (The current
state is of course the `Task`).

~~~haskell
initTask :: Monad m => String -> Interval m TaskCmd Task
initTask descr = accum f (Just (Task descr False))
  where
    f (Just t) tc = case tc of
                      CDelete                  -> Nothing
                      CPrune | taskCompleted t -> Nothing
                             | otherwise       -> Just t
                      CComplete s              -> Just t { taskCompleted = s }
                      CModify descr            -> Just t { taskDescr = descr }
                      CNop                     -> Just t
    f Nothing _   = Nothing
~~~

We normally like to avoid using `accum` in case it drives us towards imperative
code.  But here, it is an expressive and meaningful way to state something
simple.

See that our `Auto` "turns off" by outputting `Nothing`.  That's interval
semantics, and it's what `dynMapF` relies on for its internal `Auto`s!


<!-- ### Todo -->

<!-- With `taskMap`, we really have exactly all we need for an output --- output an -->
<!-- `IntMap` of tasks associated with ID's.  Now just to determine the input -->
<!-- streams --- the `IntMap TaskCmd` with the command to give to every ID, and the -->
<!-- `Blip [String]` which emits whenever we want to pop in a new task. -->




<!-- Now, at this point, we really could build everything as a giant `accum` that -->
<!-- takes in inputs and processes the change they do to our task list.  But that -->
<!-- would probably be a pain to write and read...and reading it would not really -->
<!-- tell you anything about the algorithm's structure itself.  It doesn't convey -->
<!-- programmer intent.  And also, we're pretty much shoe-horning in an imperative -->
<!-- algorithm.  That's not what *auto* is about.  *auto* is about giving you tools -->
<!-- to define complex streams transformations as compositions and combinations of -->
<!-- primitive, simple, semantically meaningful stream tranformations.  Define -->
<!-- relationships between quantities that hold "forever", using simple -->
<!-- building-block relationships. -->




