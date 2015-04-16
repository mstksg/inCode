Auto: A Todo GUI application with Auto and GHCJS + ghcjs-vdom
=============================================================

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
etc.; in particular, we're going to be using *[ghcjs-vdom][]*
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

This post is also a short tutorial

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
!!!auto/Todo.hs "import " "data TodoInp" "data TaskCmd" "type TaskMap" "data Task " "instance Serialize Task"
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

#### Tasks

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

For example, `IM.singleton 5 True` would send `True` to the `Auto` stored
at `5`.  It'll then output something that includes `(5, "received True!")` ---
the output of the `Auto` at slot 5.

Whenever an `Interval` turns "off" (is `Nothing`), it is removed from the
collection.  In this way we can have `Auto`s "remove themselves".

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
!!!auto/Todo.hs "taskCollection ::"
~~~

If we wanted to send in the command `CModify "hey!"` to the task whose
id/key/address is `12`, I'd feed in `IntMap.singleton 12 (CModify "hey!")`.
The output would then contain the output of feeding that `CModify` to the
`Auto` at that slot 12, associated with slot 12 on the output `IntMap`.

Writing `initTask` and the task `Auto` is straightforward with `accum`, which
is basically like `foldl` on the inputs and a "current state".  (The current
state is of course the `Task`).

~~~haskell
!!!auto/Todo.hs "initTask ::"
~~~

See that our `Auto` "turns off" by outputting `Nothing`.  That's interval
semantics, and it's what `dynMapF` relies on for its internal `Auto`s!

#### Routing the inputs

The only thing left, then, is just to route our input stream to send
everything to the correct `Auto` in `taskCollection`.

Our input stream is going to be a stream of `TodoInp`, which can be "add",
"send command to a single task", or "send command to all tasks".  Really,
though, you can think of it three separate streams all "jammed" into one
stream.

This is a common pattern that we can use *blip streams* for.  Instead of
working with one big fatty stream, we can work with several blip streams that
only emit when the input that we care about comes in.

Typically, we'd do this with `emitJusts`:

~~~haskell
emitJusts :: (a -> Maybe b) -> Auto m a (Blip b)
~~~

You can imagine `emitJusts` is a "siphon" off of the input stream of
`a`s...and pulling out only the values that we care about, as a blip stream of
`b`'s.

We can build our "siphoners":

~~~haskell
!!!auto/Todo.hs "getAddEvts ::" "getModEvts ::" "getMassEvts ::"
~~~

`getAddEvts`, when used with `emitJusts`, will siphon off all `IAdd` commands
as a blip stream of `[String]`s, emitting descriptions of new tasks to add.

`getModEvts`, when used with `emitJusts`, will siphon off all `ITask` commands
as a blip stream of `IntMap TaskCmd`, which will be fed into `taskCollection`
and `dynMapF`.

`getMassEvts` is pretty much the same thing...siphoning off all `IAll`
commands as a blip stream of `IntMap TaskCmd`.  It needs a list of all
`TaskID`s though, to do its job...because it needs to make an `IntMap`
targeting all of the current tasks.

Remember, we interace with tasks through an `IntMap TaskCmd`...which is a map
of task id-task command pairs.  The `TaskCmd` stored at key `1` will be the
command we want to send to task id 1.

Let's see it all work together!

~~~haskell
!!!auto/Todo.hs "todoApp ::"
~~~

To read the proc block, it does help to sort of see all of the lines as
english statements of what things "are".

1.  `allIds` is a list of keys (id's) currently in the task map `taskMap`.
    All of the id's of the tasks currently alive.

2.  Now, we fork into blip streams:

    *   `newTaskB` is a blip stream that emits with task description whenever
        `inpEvt` calls for one.
    *   `modTaskB` is a blip stream that emits with a command to a specific task
        whenever `inpEvt` calls for one.
    *   `masstaskB` is a blip stream that emits commands to every single task in
        `allIds` whenever `inpEvt` calls for it.
    *   `allInpB` is a blip stream with addressed commands whenever either
        `modTaskB` or `massTaskB` emits.

6.  `taskCommands` is a map of addressed commands for each task.  It's
    whatever `allInpB` emits, when it does emit...or just `IM.empty` (an empty
    map) when it doesn't.

7.  `taskMap` is the map of tasks that we get from our `taskCollection`
    updater, which manages a collection of tasks.  `taskCollection` needs the
    commands for each task and the new tasks we want to do its job.

We state things as an interplay of streams.  And in the end, the result is
what we want --- an indexed list of tasks.

Note that we needed the `rec` block because we referred to `taskMap` at the
beginning (to get `allIds`), but we don't define `taskMap` until the end.

We use `arrD` here to "close the loop".  `arrD IM.keys []` means, "apply
`IM.keys` to the[^delayed] input, but...the very first result, just output
`[]`.".  So, the very first time we ask for anything, it'll just output `[]`.
This means we don't run into any recursive loops (if we ask for `taskMap`, but
`taskMap` doesn't even exist yet, how does that work?  in this way, we don't
even need `taskMap` at first.  After the second, third steps, etc, we already
have `taskMap`, so it's no problem).

[^delayed]: (delayed)

