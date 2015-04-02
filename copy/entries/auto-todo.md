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

With *auto* and with Haskell in general, we have the choice to either start
from the top down or the bottom up.  For the purpose of this tutorial, let's
explore a bottom-up approach.

The smallest meaningful "stream transformer" at the very bottom of the core of
the app is going to be a single `Task`'s `Auto`.  `TaskCmd`s come in, and
`Task`s come out.

This is really best expressed with `accum`, folding up the input commands.
Normally we try to avoid `accum` when possible, because it leads to
non-composable and possibly imperative-style code, but in this case, we can
consider this as a "primitive action" by which we build up the rest of our
program.

~~~haskell
taskAuto :: Monad m => String -> Interval m TaskCmd Task
taskAuto descr = accum f (Just (Task descr False))
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

`accum` will basically "fold up" input commands on a held `Task`.  Every step,
it looks at the input and the task and outputs the task that results from that
applying that command.  We give it an initial `Task`, with the input
description and a "false" completed status.

Note that we opt for an `Interval` instead of an `Auto`.  (Remember that an
`Interval m a b` is just a type synonym for `Auto m a (Maybe b)`).  This is
because we want to convey the semantic idea that the `Task` can be "on"
(active) or "off" (deleted).  This "on"/"offness" --- combined with the notion
that the `Task` will be on for a period of time, then off for another ---
means that it fits well with the spirit/semantics of `Interval`.  The type
synonym helps us convey what the `Maybe` "means".

Now, we're going to be managing a dynamic *collection* of
`taskAuto`s...something that we can add and remove dynamically from.  Working
with collections like these is the job of the [Control.Auto.Collection][cacol]
module.  Looking through these, we see that what we need --- a dynamic
collection of `Auto`s that can be added, removed, etc., indexed by a key ---
is exactly `dynMap` and `dynMapF`.  We chose `dynMapF` because it's
serializable and slightly less powerful, so easier to work with.

[cacol]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Collection.html

~~~haskell
dynMapF :: (k -> Interval m a b) -> a -> Auto m (IntMap a, Blip [k]) (IntMap b)
~~~

`dynMapF` is a very commonly used tool that shows up often when we have
dynamic collections, so it might be helpful to get to know it.

`dynMapF` keeps a dynamic collection of `Interval m a b`s, each stored at an
`Int` key.  At every step, it takes in an `IntMap` associating `Int` keys to
an inputs `a`, "feeds" it into the `Interval` associated with that key, and
outputs an `IntMap` of the results.  It also takes in a blip stream of
`[k]`...whenever it emits, it "adds a new `Interval`" to the collection, using
the "initialization function" `k -> Interval m a b`.  It also takes an `a`
"default input", if a given `Auto` has no input in the input `IntMap`.

Read over the [tutorial section on blip streams and `Interval`s][blipint] if
you are still unfamiliar with them.

[blipint]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md#semantic-tools

For our purposes, `dynMapF` will be storing `taskAuto`s.  This works out well,
because it'll automatically remove `taskAuto`s that are *off* (`Nothing`).
Perfect!

~~~haskell
taskMap :: Monad m => Auto m (IntMap TaskCmd, Blip [String]) (IntMap Task)
taskMap = dynMapF taskAuto CNop
~~~

So, when the input blip stream emits any `String`, it'll "initialize" a new
`taskAuto` with that given string as a description.  It'll also take an input
`IntMap` associating a command with a key, feed the command to the `taskAuto`
stored at that key, and output all of the results of the updates.

### Todo

With `taskMap`, we really have exactly all we need for an output --- output an
`IntMap` of tasks associated with ID's.  Now just to determine the input
streams --- the `IntMap TaskCmd` with the command to give to every ID, and the
`Blip [String]` which emits whenever we want to pop in a new task.




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




