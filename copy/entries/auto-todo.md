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
adaptable to really any platform --- javascript/web, desktop, command line...

[series]: http://blog.jle.im/entries/series/+all-about-auto
[auto]: http://hackage.haskell.org/package/auto
[todoMVC]: http://todomvc.com/

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

Our overall architecture will therefore be a front-end of things that trigger
"events" that will drop commands into a concurrent `Chan` queue...with
something like `runOnChan` watching the queue, waiting for input commands,
feeding the commands through the `Auto` when they get them, and rendering the
output that pops out to the webpage/front-end display.  We write the logic in
*auto*, and hook up our event triggers and renderers to be the inputs and
outputs.

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

Now, at this point, we really could build everything as a giant `accum` that
takes in inputs and processes the change they do to our task list.  But that
would probably be a pain to write and read...and reading it would not really
tell you anything about the algorithm's structure itself.  It doesn't convey
programmer intent.  And also, we're pretty much shoe-horning in an imperative
algorithm.  That's not what *auto* is about.  *auto* is about giving you tools
to define complex streams transformations as compositions and combinations of
primitive, simple, semantically meaningful stream tranformations.  Define
relationships between quantities that hold "forever", using simple
building-block relationships.


