Auto: A Todo GUI application with Auto (on GHCJS, etc.)

========================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on April 23, 2015.
> [Read online!](https://blog.jle.im/entry/auto-a-todo-gui-application-with-auto-on.html)

Continuing along with [All About
Auto](http://blog.jle.im/entries/series/+all-about-auto), let's look at another
exciting and useful application of the
*[auto](http://hackage.haskell.org/package/auto)* library: GUI's. We're going to
look at the canonical "hello world" of GUI apps these days --- the todo app.
We're going to be using the specs of [todoMVC](http://todomvc.com/) to build a
todoMVC "candidate" that follows the specs...and along the way see what *auto*
offers in its tools of managing isolated state components and modeling GUI
logic. We're really going to be focusing on application logic --- "control" and
"model" --- and not looking too close on "views", which *auto* doesn't quite try
to offer and where you can really pick your own view rendering system, making
this adaptable to really any platform --- javascript/web, desktop, command line,
etc.

A live version of our end-product [is hosted and
online](https://mstksg.github.com/auto-examples/todo).

This post does assume *some* concepts from the
[tutorial](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md)...if
not all, then at least those in the [introductory
post](http://blog.jle.im/entry/introducing-the-auto-library) or the
[README](https://github.com/mstksg/auto/blob/master/README.md). If you ever find
yourself thinking that these concepts are completely new and crazy, you might
want to try looking through the
[tutorial](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md) or
[docs](http://hackage.haskell.org/package/auto) to refresh your mind. As always,
comments are welcome, and I'm also usually on *#haskell-auto* as *jle\`*, and
also on [twitter](https://twitter.com/mstk "Twitter")

(Fair warning...this is not quite a "ghcjs tutorial", if that's what you're
looking for; it's an auto tutorial that uses some rudimentary ghcjs. Hopefully
you can learn from that too!)

## Overall Layout

At the highest level, *auto* is a library that provides us tools to build and
work with stream transformers on streams of values. Transform a stream of input
values to a stream of output values. So, let's try to phrase our Todo app
problem in that perspective. What are our inputs, and what are our outputs?

For a Todo app, the outputs are probably going to be a *todo list* itself. If
we're building a GUI, then having the todo list itself is going to be enough to
build our front-end display. The stream of *inputs* is a little less obvious,
but, well, what does an app really take as inputs? Commands! Our stream of
inputs will be commands sent by a GUI or by whatever front-end we choose. Our
todo app then is a transformer of a stream of commands to a stream of todo
lists...where the todo list we get changes as we process more commands.

So the "overall loop" will be:

1.  A front-end rendered by *ghcjs-dom* (or whatever) with event handlers that
    drop commands into a concurrent `Chan` queue. This just handles rendering.
2.  Our `Auto` launched with `runOnChan`, which waits on the `Chan` queue, runs
    the inputs through the `Auto`, and renders the result. This handles all of
    the logic.

We like types in Haskell, so let's begin by laying out our types!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/Todo.hs#L19-L46

import Control.Auto
import Control.Auto.Collection
import Control.Monad.Fix
import Data.IntMap             (IntMap)
import Data.Serialize
import GHC.Generics
import Prelude hiding          ((.), id)
import qualified Data.IntMap   as IM

data TodoInp = IAdd  String           -- new task with description
             | ITask TaskID TaskCmd   -- send command to task by ID
             | IAll TaskCmd           -- send command to all tasks
             deriving Show

data TaskCmd = CDelete          -- delete
             | CPrune           -- delete if completed
             | CComplete Bool   -- set completed status
             | CModify String   -- modify description
             | CNop             -- do nothing
             deriving Show

data Task = Task { taskDescr     :: String
                 , taskCompleted :: Bool
                 } deriving (Show, Generic)

instance Serialize Task -- from Data.Serialize, from the cereal library
```

We have a type to represent our inputs, `TodoInp`, which can be an "add" command
with a `String`, a "task" command with a `TaskId` (`Int`) and a `TaskCmd`, and
an "all" command with a `TaskCmd` that is supposed to represent sending that
command to all tasks.

Our `TaskCmd` represents commands we can send to individual tasks -- we can
delete, prune (delete if completed), set the "completed" flag, or modify the
description.

We're going to represent our task list, `TaskMap`, as not a `[]` list, but as an
`IntMap` from *containers*, which associates an `Int` to a `Task` that we can
look up with the `IntMap` API. What would a `TaskMap` store other than a bunch
of `Task`s, which we are defining as jus a tupling of a `String` description and
a `Bool` completed/uncompleted status.

## The Todo Auto

Time to go over the logic portion! The part that *auto* is meant for! We're
going to structure the logic of our app (also known as the "model") by using
principles of local statefulness to avoid ever working with a "global state",
and working in a declarative, high-level manner.

### Tasks

It's clear that the core of our entire thing is going to be the "task list"
construct itself...something that can dynamically add or remove tasks.

In *auto*, there is a construct created just for this kind of situation: dynamic
collections indexed by a key (a "task id"), where you can add or subtract
`Auto`s from dynamically --- they are `dynMap` and `dynMapF` from
*[Control.Auto.Collection](http://hackage.haskell.org/package/auto/docs/Control-Auto-Collection.html)*.
We'll be using `dynMapF` because it's serializable, and we don't need the extra
power that `dynMap` offers.

``` haskell
dynMapF :: (k -> Interval m a b)    -- ^ function to initialize new `Auto`s
        -> a                        -- ^ default inputs
        -> Auto m ( IntMap a        -- ^ input for each internal `Auto`, indexed by key
                  , Blip [k]        -- ^ blip stream to initialize new `Auto`s
                  )
                  (IntMap b)        -- ^ `Auto` outputs, by key
```

`dynMapF` keeps a "dynamic collection" of `Interval m a b`s, indexed by an `Int`
key, or an "ID". It takes as input a stream of `IntMap a`...basically a bunch of
`(Int, a)` pairs. `dynMapF` routes each input to the `Interval` at that
ID/address (with a suitable "default" `a` if none was sent in), and then outputs
all of the results as an `IntMap b` --- a bunch of `(Int, b)` pairs, each output
with the address of the `Auto` that made it.

For example, `IM.singleton 5 True` would send `True` to the `Auto` stored at
`5`. It'll then output something that includes `(5, "received True!")` --- the
output of the `Auto` at slot 5.

Whenever an `Interval` turns "off" (is `Nothing`), it is removed from the
collection. In this way we can have `Auto`s "remove themselves".

It also takes as input a blip stream of `[k]`s. We use each emitted `k` to
"initialize a new `Interval`" and throw it into the collection, creating a new
unique key for it. Every time a new `Auto` is initialized, `dynMapF` creates a
new key for it.

Read over the [tutorial section on blip streams and
`Interval`s](https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md#semantic-tools)
if you are still unfamiliar with them.

This pretty much fits exactly what we want for our task collection. If we
imagined that we had our `Task` as an `Auto`:

``` haskell
initTask :: Monad m => String -> Interval m TaskCmd Task
```

`initTask` takes a string (a starting description) and initializes an `Interval`
that takes in a stream of task commands, and has a stream of new, updated
`Task`s as its output stream. At every step, it processes the command and
outputs the new appropriate `Task`.

We can then use this as our "initializer" for `dynMapF`...and now we have a
dynamic collection of tasks!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/Todo.hs#L48-L50

taskCollection :: Monad m
               => Auto m (IntMap TaskCmd, Blip [String]) (IntMap Task)
taskCollection = dynMapF initTask CNop
```

If we wanted to send in the command `CModify "hey!"` to the task whose
id/key/address is `12`, I'd feed in `IM.singleton 12 (CModify "hey!")`. The
output would then contain the output of feeding that `CModify` to the `Auto` at
that slot 12, associated with slot 12 on the output `IntMap`.

Writing `initTask` and the task `Auto` is straightforward with `accum`, which is
basically like `foldl` on the inputs and a "current state". (The current state
is of course the `Task`).

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/Todo.hs#L52-L62

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
```

See that our `Auto` "turns off" by outputting `Nothing`. That's interval
semantics, and it's what `dynMapF` relies on for its internal `Auto`s!

### Routing the inputs

The only thing left, then, is just to route our input stream to send everything
to the correct `Auto` in `taskCollection`.

Our input stream is going to be a stream of `TodoInp`, which can be "add", "send
command to a single task", or "send command to all tasks". Really, though, you
can think of it three separate streams all "jammed" into one stream.

This is a common pattern that we can use *blip streams* for. Instead of working
with one big fatty stream, we can work with several blip streams that only emit
when the input that we care about comes in.

Typically, we'd do this with `emitJusts`:

``` haskell
emitJusts :: (a -> Maybe b) -> Auto m a (Blip b)
```

You can imagine `emitJusts` is a "siphon" off of the input stream of `a`s...and
pulling out only the values that we care about, as a blip stream of `b`'s.

We can build our "siphoners":

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/Todo.hs#L95-L105

getAddEvts :: TodoInp -> Maybe [String]
getAddEvts (IAdd descr) = Just [descr]
getAddEvts _            = Nothing

getModEvts :: TodoInp -> Maybe (IntMap TaskCmd)
getModEvts (ITask n te) = Just $ IM.singleton n te
getModEvts _            = Nothing

getMassEvts :: ([TaskID], TodoInp) -> Maybe (IntMap TaskCmd)
getMassEvts (allIds, IAll te) = Just $ IM.fromList (map (,te) allIds)
getMassEvts _                 = Nothing
```

`getAddEvts`, when used with `emitJusts`, will siphon off all `IAdd` commands as
a blip stream of `[String]`s, emitting descriptions of new tasks to add.

`getModEvts`, when used with `emitJusts`, will siphon off all `ITask` commands
as a blip stream of `IntMap TaskCmd`, which will be fed into `taskCollection`
and `dynMapF`.

`getMassEvts` is pretty much the same thing...siphoning off all `IAll` commands
as a blip stream of `IntMap TaskCmd`. It needs a list of all `TaskID`s though,
to do its job...because it needs to make an `IntMap` targeting all of the
current tasks.

Remember, we interace with tasks through an `IntMap TaskCmd`...which is a map of
task id-task command pairs. The `TaskCmd` stored at key `1` will be the command
we want to send to task id 1.

Let's see it all work together!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/Todo.hs#L64-L93

todoApp :: MonadFix m => Auto m TodoInp (IntMap Task)
todoApp = proc inpEvt -> do

    rec -- all id's currently alive
        allIds <- arrD IM.keys [] -< taskMap

        -- "forking" `inpEvt` into three blip streams:
        -- newTaskB :: Blip [String]
        newTaskB  <- emitJusts getAddEvts  -< inpEvt
        -- modTaskB :: Blip (IntMap TaskCmd)
        modTaskB  <- emitJusts getModEvts  -< inpEvt
        -- massTaskB :: Blip (IntMap TaskCmd)
        massTaskB <- emitJusts getMassEvts -< (allIds, inpEvt)

        -- merge the two streams together to get "all" inputs, single and
        -- mass.
        let allInpB :: Blip (IntMap TaskCmd)
            allInpB = modTaskB <> massTaskB

        -- from a blip stream to an `IntMap` stream that is empty when the
        -- stream doesn't emit
        -- taskCommands :: IntMap TaskCmd
        taskCommands <- fromBlips IM.empty -< allInpB

        -- feed the commands and the new tasks to `taskMap`...the result is
        -- the `IntMap` of tasks.
        -- taskMap :: IntMap Task
        taskMap <- taskCollection -< (taskCommands, newTaskB)

    id -< taskMap
```

To read the proc block, it does help to sort of see all of the lines as english
statements of what things "are".

1.  `allIds` is a list of keys (id's) currently in the task map `taskMap`. All
    of the id's of the tasks currently alive.

2.  Now, we fork into blip streams:

    -   `newTaskB` is a blip stream that emits with task descriptions whenever
        `inpEvt` calls for one.
    -   `modTaskB` is a blip stream that emits with a command to a specific task
        whenever `inpEvt` calls for one.
    -   `massTaskB` is a blip stream that emits commands to every single task in
        `allIds` whenever `inpEvt` calls for it.
    -   `allInpB` is a blip stream with addressed commands whenever either
        `modTaskB` or `massTaskB` emits.

3.  `taskCommands` is a map of addressed commands for each task. It's whatever
    `allInpB` emits, when it does emit...or just `IM.empty` (an empty map) when
    it doesn't.

4.  `taskMap` is the map of tasks that we get from our `taskCollection` updater,
    which manages a collection of tasks. `taskCollection` needs the commands for
    each task and the new tasks we want to do its job.

We state things as an interplay of streams. And in the end, the result is what
we want --- an indexed list of tasks.

Note that we needed the `rec` block because we referred to `taskMap` at the
beginning (to get `allIds`), but we don't define `taskMap` until the end.

Note that we use `arrD` for `allIds`. What we really "meant" was something like:

``` haskell
allIds <- arr IM.keys -< taskMap
```

But...this doesn't really work out, because when the whole thing "starts", we
don't know what `taskMap` is. We need to know `massTaskB` to know `taskMap`, and
we need to know `allIds` to know `massTaskB`, and...recursive dependency!

We can use `arrD` to specify an "initial output" to "close the loop" (in
technical terms). We want `allIds` to initially be `[]` (we can assume we start
with no task id's), so instead of

``` haskell
allIds <- arr IM.keys -< taskMap
```

we have

``` haskell
allIds <- arrD IM.keys [] -< taskMap
```

Where `[]` is the "initial output", so when we first try to do anything, we
don't need `taskMap` --- we just pop out `[]`!

This is just a small thing to worry about whenever you have recursive bindings.
There is a small cognitive price to pay, but in return, you have something that
really just looks like laying out relationships between different quantities :)

## Interfacing with the world

Our application logic is done; let's explore ways to interface with it!

### Testing/command line

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/auto/todo-cmd.hs#L25-L62

parseInp :: String -> Maybe TodoInp
parseInp = p . words
  where
    p ("A":xs)   = Just (IAdd (unwords xs))
    p ("D":n:_)  = onId n CDelete
    p ("C":n:_)  = onId n (CComplete True)
    p ("U":n:_)  = onId n (CComplete False)
    p ("P":n:_)  = onId n CPrune
    p ("M":n:xs) = onId n (CModify (unwords xs))
    p _          = Nothing

    onId :: String -> TaskCmd -> Maybe TodoInp
    onId "*" te = Just (IAll te)
    onId n   te = (`ITask` te) <$> readMaybe n

formatTodo :: IntMap Task -> String
formatTodo = unlines . map format . IM.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , desc
                                         ]

main :: IO ()
main = do
    putStrLn "Enter command! 'A descr' or '[D/C/U/P/M] [id/*]'"
    void . interactAuto $ -- interactAuto takes an Interval; `toOn` gives
                          --   one that runs forever
                          toOn
                          -- default output value on bad command
                        . fromBlips "Bad command!"
                          -- run `formatTodo <$> todoApp` on emitted commands
                        . perBlip (formatTodo <$> todoApp)
                          -- emit when input is parseable
                        . emitJusts parseInp
```

`interactAuto` runs an `Interval` by feeding it in strings from stdin printing
the output to stdout, until the output is "off"/`Nothing` --- then stops. Here
we use `parseInp` to emit input events whenever there is a parse, run `todoApp`
(formatted) on the emitted events, and then condense it all with `fromBlips` and
wrap it in an "always on" `toOn`.

    $ cabal sandbox init
    $ cabal install auto
    $ cabal exec runghc todo-cmd.hs
    Enter command! 'A descr' or '[D/C/U/P/M] [id/*]'
    > A take out the trash
    0. [ ] take out the trash

    > A do the dishes
    0. [ ] take out the trash
    1. [ ] do the dishes

    > C 1
    0. [ ] take out the trash
    1. [X] do the dishes

    > U 1
    0. [ ] take out the trash
    1. [ ] do the dishes

    > C 0
    0. [X] take out the trash
    1. [ ] do the dishes

    > P *
    1. [ ] do the dishes

You can [download and run this
yourself](https://github.com/mstksg/inCode/tree/master/code-samples/auto/todo-cmd.hs)!

Looks like the logic works! Time to take it to GUI!

### As a GUI

To build a GUI, we must build an `Auto` that takes in inputs from events and
output everything the front-end renderer needs to render the interface.

For a typical todomvc gui, we need to be able to filter and select things. So
that means we need to be extend our input type with filtering and selecting
events. And our output has to also indicate the current filter selected, and the
current task selected, as well.

``` haskell
data GUIOpts = GUI { _currFilter   :: Filter        -- currently applied filter
                   , _currSelected :: Maybe TaskID  -- currently selected task
                   }

data GUIInp = GIFilter Filter
            | GISelect (Maybe TaskID)

data Filter = All | Active | Completed
            deriving (Show, Generic, Enum, Eq)

instance Serialize Filter
```

Instead of defining a new input mega-type with all input events and the todo map
with the options, we can use good ol' fashioned `Either` and `(,)`. So now,
instead of:

``` haskell
todoApp :: Auto m TodoInp (IntMap Task)
```

We have:

``` haskell
todoAppGUI :: Auto m (Either TodoInp GUIInp) (IntMap Task, GUIOpts)
```

Now we take *either* `TodoInp` or `GUIInp` and then return *both* `IntMap Task`
*and* `GUIOpts`.

``` haskell
todoAppGUI :: Auto' (Either TodoInp GUIInp) (IntMap Task, GUIOpts)
todoAppGUI = proc inp -> do
    filt  <- holdWith All                      . emitJusts filtInps -< inp
    selc  <- holdWith Nothing                  . emitJusts selcInps -< inp
    tasks <- holdWith mempty . perBlip todoApp . emitJusts todoInps -< inp

    id -< (tasks, GUI filt selc)
  where
    todoInps :: Either TodoInp GUIInp -> Maybe TodoInp
    todoInps (Left ti) = Just ti
    todoInps _         = Nothing
    filtInps :: Either TodoInp GUIInp -> Maybe Filter
    filtInps (Right (GIFilter filt)) = Just filt
    filtInps _                       = Nothing
    selcInps :: Either TodoInp GUIInp -> Maybe (Maybe TaskID)
    selcInps (Right (GISelect sec))  = Just selc
    selcInps _                       = Nothing
```

Here we have the same idea as before. One input stream of
`Either TodoInp GUIInp` comes through, and we fork it into three blip streams
that each do what we want. `holdWith x :: Auto m (Blip b) b` is always the value
of the last emitted item...but starts off as `x` first.

By the way, the above code is much more succinct if you are willing to use
*[lens](http://lens.github.com)*...

``` haskell
todoAppGUI :: Auto' (Either TodoInp GUIInp) (IntMap Task, GUIOpts)
todoAppGUI = proc inp -> do
    filt  <- holdWith All
           . emitJusts (preview (_Right . _GIFilter)) -< inp
    selc  <- holdWith Nothing
           . emitJusts (preview (_Right . _GISelect)) -< inp
    tasks <- holdWith mempty . perBlip todoApp
           . emitJusts (preview _Left)                -< inp

    id -< (tasks, GUI filt selc)
```

(assuming we defined the prisms for `GUIInp` or used `''mkPrisms`)

Neat, right? In a way, you can say that `emitJusts` and `Prisms`/lens was a
match made in heaven :)

### Giving it life

The last step is to hook everything up together ---

1.  Setting up events in our GUI to feed inputs to a queue
2.  Setting up the queue to wait on inputs, and output the task map/gui status
    on every one using `todoAppGUI`
3.  Rendering the output into the GUI framework of your choice

The second step in particular can be handled with good ol' `[runOnChan][]`:

``` haskell
runOnChan :: (b -> IO Bool) -> Chan a -> Auto' a b -> IO (Auto' a b)
```

We know and love `runOnChan` from when we used it to make our
[chatbot](http://blog.jle.im/entry/auto-building-a-declarative-chatbot-with-implicit-serialization#irc-backend-the-ugly-part).
It runs an `Auto' a b` "on a `Chan`" (concurrent queue). The first argument is
an "output hander" --- it handles the `b`s that the `Auto'` pops out. It decides
whether to stop the whole thing or keep on listening based on the `Bool` result
of the handler. The second argument is the `Chan a` to listen for inputs on.
Whenever something is dropped into that `Chan`, it runs the `Auto'` with the `a`
and processes the output `b` with the handler.

Our final runner is then just:

``` haskell
runOnChan renderGUI inputChan todoAppGUI
```

where

``` haskell
renderGUI :: (IntMap Task, GUIOpts) -> IO Bool
inputChan :: Chan (Either TodoInp GUIInp)
```

The rendering is done with `renderGUI`...and it really depends on your framework
here. That's #3 from the list above.

All you need after that is just to have your GUI hook up event handlers to drop
the appropriate `Either TodoInp GUIInp` into `inputChan`...and you're golden!

## Seeing it in action

We've reached the end of our tutorial --- the parts about `auto`. It is my hope
that whatever GUI front-end you want to work with, it'll be simple enough to
"plug in" our `Auto` logic.

A [live demo](https://mstksg.github.com/auto-examples/todo) is online too; you
can see [the source of the front-end
bindings](https://github.com/mstksg/auto-examples/blob/master/src/TodoJS.hs)

This is a bare-bons *ghcjs* implementation using *ghcjs-dom*, which uses direct
dom manipulation.

User [eryx67](https://github.com/eryx67) has been kind enough to provide [an
implementation in
*ghcjs*](https://github.com/eryx67/auto-examples/blob/master/src/TodoJS.hs) with
the *[virtual-dom](https://github.com/ocharles/virtual-dom)* library
([side-by-side
comparison](https://github.com/mstksg/auto-examples/commit/246133a89fbca6a2ec7ea276d8536701f6ab8d2c?diff=split)),
so there is a slightly less uglier implementation with abstraction :)

As always, feel free to ask questions in the comments, hop over to
*#haskell-game* or *#haskell-auto* on freenode, or send me a
[tweet](https://twitter.com/mstk "Twitter")! And look forward to more tutorials
as the [All About Auto](http://blog.jle.im/entries/series/+all-about-auto)
series progresses!

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

