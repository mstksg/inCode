Auto: Building a Declarative Chatbot with Implicit Serialization
================================================================

Categories
:   Haskell
:   Auto
:   Tutorials
Tags
:   auto
:   haskell
CreateTime
:   2015/03/24 18:02:54
PostDate
:   2015/03/25 10:20:17
Series
:   All About Auto
:   Beginner/Intermediate Haskell Projects
Identifier
:   auto-chatbot

Today we're going to continue along with the [All About Auto][series]
and principles of architecture that you can apply to your own projects.
denotational components from the [auto][] library that is modular and has
implicit serialization.  Most importantly, we'll look at the "design process",
introduction series and look at building a declarative chatbot using the

[auto]: http://hackage.haskell.org/package/auto

This post assumes *some* concepts from the [tutorial][], or at least my [last
post][intro] or the [README][].  If some of these ideas seem completely new,
than looking through the [tutorial][] or the [docs][auto] might refresh your
mind...feel free to also leave a comment, stop by *#haskell-auto* on freenode
where I go by *jle`*, or [tweet me][twitter]

[tutorial]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md
[intro]: http://blog.jle.im/entry/introducing-the-auto-library
[README]: https://github.com/mstksg/auto/blob/master/README.md

All of the code in this tutorial can be [downloaded and run][source] using
`runghc` (with the appropriate dependencies installed).  Feel free to play
along!

!!![source]:auto/chatbot.hs

Overall Layout
--------------

*auto* is a library that at the highest level gives you a stream transformer.
Transform a stream of inputs to a stream of outputs.  So when we approach a
chat bot, we have to think --- what are the inputs, and what are the outputs?

The choice should be pretty straightforward -- our input stream is a stream of
input messages from the irc server, and our output stream is a stream of
messages to send to the server.  In haskell we like types, so let's make some
types.

~~~haskell
-- first, our imports
!!!auto/chatbot.hs "import Control.Auto"
~~~

~~~haskell
!!!auto/chatbot.hs "type Nick" "data InMessage" "newtype OutMessages" "instance Monoid"
~~~

We make some type aliases to make things a bit clearer.  Our inputs are going
to be a data type/"struct" with a nick, a body, a source, and a time.  Our
outputs are going to be a `Data.Map.Map` from *containers* associating
channels with messages to send.  I'm just adding here a `Monoid` instance in
case we want to combine `OutMessages` maps.

The type for a chat bot over a monad `m` would then be:

~~~haskell
!!!auto/chatbot.hs "type ChatBot"1
~~~

A `ChatBot` takes a stream of `InMessage`s and returns a stream of
`OutMessages`s...and might have effects in `m` as it does so.

Note that we get a free instance of `Monoid` on `ChatBot m`:

~~~haskell
mappend :: ChatBot m -> ChatBot m -> ChatBot m
~~~

That takes two `ChatBot`s and creates a new `ChatBot` that forks the input
stream (sends all `InMessage`s) to both of the original ones, and `mappend`s
the results.  So the new `ChatBot` will send message to both original ones and
return a "combined" `OutMessages`.

However, not all modules really have to "care" about the room of the
outputs...they might just always reply directly to the room they received the
message on.  So it'll help us to also make another sort of `Auto`:

~~~haskell
!!!auto/chatbot.hs "type RoomBot"1
~~~

A `RoomBot` doesn't care where its messages go...it just replies to the same
room it got its input from.  It outputs a blip stream of message lists; when
it doesn't want to send messages out, it doesn't emit.  When it does, it
*does* emit, with the list of messages.

(Remember, a *blip stream* is just like a normal stream of values, except it
only actually *has* a value every once in a while, when it "emits".  A `Blip
Bool` is a stream that sometimes, occasionally emits with a `Bool`.  We work
with them using combinators and `Auto`s from [`Control.Auto.Blip`][cablip])

[cablip]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Blip.html

### Converting

We can write a quick helper function to convert a `RoomBot` into a full-on
`ChatBot`, so we can merge them together with `mappend`/`(<>)`:

~~~haskell
!!!auto/chatbot.hs "perRoom ::"
~~~

(This example uses proc notation; see this [proc notation primer][proc] for a
quick run-down of the relevant aspects)

[proc]: https://github.com/mstksg/auto/blob/master/tutorial/tutorial.md#brief-primer-on-proc-notation

We say that `messages` is just the output of `rb` fed with the input, except
it "collapses" the blip stream into a normal stream by substituting in `[]`
whenever the stream doesn't emit.  So `messages` is `[]` when `rb` doesn't
emit (it doesn't want to send anything), and `messages` is `[message1,
message2 ...]`, with the emitted contents, when it *does*.

The "output" will be a singleton map with the source of the input and the
messages to send to that source.

So now if we have a `RoomBot m`, we can convert it up into a `ChatBot m`, and
combine it/merge it with other `ChatBot m`s.


### The whole deal

We have enough now then to imagine our entire program architecture:

*   Write a bunch of separate modules, as `ChatBot m`s or `RoomBot m`s, which
    ever one is more convenient.  The beauty is that we can merge them all
    together in the end with our promoter.
*   Combine all of our modules with `mconcat` --- that is, something like
    `chatBot = mconcat [module1, module2, module3, module 4]`.  And that's it,
    that's our entire chat bot!
*   Having an overall `chatBot :: ChatBot m`, we can use something like
    `runOnChan` from `Control.Auto.Run` to have it exist on a concurrent
    thread and watch a channel for input, and perform an action on output.
*   Find an out-of-the-box irc library that can trigger adding something to a
    concurrent queue when it receives a message, and where you can send
    messages to rooms.

And...that's it.  Program logic in our `ChatBot m`s, and handling the
"view"/input with our backend.

#### Free Serialization

Remember that *auto* gives us the ability to serialize and resume our `Auto`s
for free...so we can at any time save the state of our chat bot to disk, and
resume it when we re-load.  We don't have to worry about manually gathering
our state between each `Auto` and writing serialization code.

There's a "convenience combinator" called `serializing'` in
`Control.Auto.Serialize` (it's one of many different ones that can do
something like this; [check out the module][caser] to see other ways of
varying disciplined-ness!).  It'll take any `Auto` and turn it into an `Auto`
that "self-serializes" --- when you begin running it, it automatically loads
its previous state if it exists, and as you run it, it automatically maintains
an updated "resume state" on disk.

[caser]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Serialize.html

~~~haskell
serializing' :: MonadIO m => FilePath -> ChatBot m -> ChatBot m
~~~

Note that `serializing' fp :: MonadIO m => ChatBot m -> ChatBot m`.  It looks
a lot like an "identity-ish" sort of function, right?  That's because it is
meant to behave *like* `id`...the returned `ChatBot` behaves identical to the
previous one...except it splices in the serializing action in-between. (We are
in `MonadIO` now, because the `Auto` has to access `IO` in order to serialize
itself between steps).

So, instead of

~~~haskell
chatBot :: Monad m => ChatBot m
chatBot = mconcat [module1, module2, module3]
~~~

We can do:

~~~haskell
chatBot :: MonadIO m => ChatBot m
chatBot = serializing' "state.dat" $ mconcat [module1, module2, module3]
~~~

And now our `chatBot` will automatically resume itself on program startup, and
keep its state backed up on disk at `state.dat`.  We get this for free,
without doing anything extra in the composition of our modules.

Note that in practice, with a bot you are actively developing, this might not
be the best idea.  `serializing'` *analyzes* your `Auto`s to determine a
serialization and reloading strategy, and applies that to do its job.
However, if you, for example, add a new module to your chat bot...the
serialization strategy will change, and your new bot won't be able to resume
old save files.

One solution at this point is just to serialize individual modules that you do
not see yourself changing...or even just serializing parts of the modules you
don't see yourself changing.  Then you can change each portion separately and
not worry about migration issues.

~~~haskell
chatBot :: MonadIO m => ChatBot m
chatBot = mconcat [ serializing' "m1.dat" module1
                  , module2
                  , serializing' "m3.dat" module3
                  ]
~~~

We're not all-or-nothing now here, either!  So, `module1` gets serialized and
auto-resumed from `m1.dat`, `module2` is not serialized at all, and `module3`
now gets serialized and auto-resumed from `m3.dat`.


IRC Backend (the ugly part)
---------------------------

Before we get started on our actual modules, let's just write out the
backend/interface between our `ChatBot` and irc to get it out of the way.
This will vary based on what library you use; I'm going to use the
[simpleirc-0.3.0][simpleirc], but feel free to use any interface/library you
want.

[simpleirc]: http://hackage.haskell.org/package/simpleirc-0.3.0

~~~haskell
!!!auto/chatbot.hs "withIrcConf ::" "channels ::" "conf ::" "main ::"
~~~

That should be it...don't worry if you don't understand all of it, most of it
is just implementation details from `simpleirc`.  The overall loop is
`runOnChanM` waits on a separate thread for `inputChan`...when it gets
input, it runs it through `ChatBot` and sends the outputs through
*simpleirc*'s interface.  Meanwhile, `onMessage` is triggered whenever
*simpleirc* receives a message, where it prepares an `InMessage` and drops it
off at `inputChan`.

~~~haskell
runOnChanM :: Monad m
           => (forall c. m c -> IO c)   -- convert `m` to `IO`
           -> (b -> IO Bool)            -- handle output
           -> Chan a                    -- chan to await input on
           -> Auto m a b                -- `Auto` to run
           -> IO (Auto m a b)
~~~

`runOnChanM` runs any `Auto m a b`, as long as there's a way to convert it to
`Auto IO a b` (we can use a `ChatBot IO`, so we just put `id` there).  You
give it a "handler" `b -> IO Bool` that it run whenever it outputs; if the
handler returns `False`, then the whole thing stops.  You give it the `Chan a`
to await for input `a`s on, and it takes care of the rest.  It blocks until
the handler returns `False`, where it'll return the "updated" `Auto m a b`
with updated state after running through all of those inputs.

Phew.  With that out of the way, let's get right on to the fun part ---
building our chat bot modules.

The Modules
-----------

### seenBot

What's a common module?  Well, we can write a module that keeps track of the
last time any user was "seen" (sent a message), and then respond when there is
a query.

There are two components here...the part that keeps track of the last seen
time, and the part that responds to queries.

Keeping track of our last seen time sounds like a job that takes in a stream
of `(Nick, UTCTime)` pairs and outputs a stream of `Map Nick UTCTime`, where
we could look up the last seen time for a nick by looking up the nick in the
map.

Logically, this is pretty straightforward, and anything other than `accum`
(which is like `foldl'`) would really be a bit overkill; every input would
just update the output map.

~~~haskell
trackSeens :: Monad m => Auto m (Nick, UTCTime) (Map Nick UTCTime)
trackSeens = accum (\mp (nick, time) -> M.insert nick time mp) M.empty
~~~

`accum` takes the same thing that `foldl` takes:

~~~haskell
foldl ::            (b -> a -> b) -> b -> [a] -> b
accum :: Monad m => (b -> a -> b) -> b -> Auto m a b
~~~

So it basically "folds up" the entire history of inputs, with a starting
value.  Every time an input comes, the output is the new folded history of
inputs.  You can sort of think of it as it applying the function to any
incoming values to an internal accumulator and updating it at every step.

The next component is just to respond to requests.  We want to do something on
some "triggering" input.  Every once in a while, some input will come that
will "trigger" some special response.  This is a sign that we can use *blip
streams*.

~~~haskell
queryBlips :: Auto m Message (Blip Nick)
queryBlips = emitJusts (getRequest . words)
  where
    getRequest ("@seen":nick:_) = Just nick
    getRequest _                = Nothing
~~~

`queryBlips` takes an input stream of strings and turns it into an output
*blip stream* that emits with a `Nick` whenever the input stream contains a
request in the form of `"@seen [nick]"`.

With these simple blocks, we can build our `seenBot`:

~~~haskell
-- seenBot :: Monad m => Auto m InMessage (Blip [Message])
!!!auto/chatbot.hs "seenBot ::"
~~~

Here we define `respond` as a function that takes a `Nick` and returns the
output `[Message]`.  We could have also defined it outside as a helper
function `respond :: Map Nick UTCTime -> Nick -> [Message]`...but `seens` is
already in scope, so we might as well just do it there.

For our output, we use the `Functor` instance of blip streams.  `respond <$>
queryB` is a blip stream that emits whenever `queryB` emits (so, whenever
there is a query input), but replaces the emitted value with the result of the
function on the value.  So whenever `queryB` emits, this whole thing emits
with `respond` applied to whatever `Nick` was emitted --- in this case, our
`[Message]`.

Short, sweet, simple.  In fact, `trackSeens` and `queryBlips` are small enough
that their definition could really have been inlined.  Breaking them down just
allowed us to look at them individually for this tutorial.

So that's it for that; also, if we wanted `seenBot` to serialize and persist
across sessions, all we have to do is use:

~~~haskell
serializing' "seenbot.dat" seenBot :: MonadIO m => RoomBot m
~~~

Neat, right?

If we forsee ourselves adding more features to `seenBot`, we can future-proof
our `seenBot` for now by only serializing `trackSeens`, meaning replacing that
line with:

~~~haskell
    seens <- serializing' "seen.dat" trackSeens -< (nick, time)
~~~

Remember, `serializing' fp` acts as a sort of "identity", so you can drop it
in anywhere and you'd expect it to behave the same.

### repBot

Another common bot is a "reputation bot", which allows users to increment or
decrement another user's reputation scores, and look up a user's total score.

Again there are two components --- keeping track of the scores of all of the
users, and responding to requests.

This time though, our "score updates" only happen every once in a while,
triggered by certain words in the message.  Again, this pattern calls for a
blip stream:

~~~haskell
updateBlips :: Auto m (Nick, Message) (Blip (Nick, Int))
updateBlips = emitJusts getUpdateCommand
  where
    -- updater is the person triggering the update blip
    getUpdateCommand (updater, msg) =
      case words msg of
        "@addRep":nick:_ | nick /= updater -> Just (nick, 1)
        "@subRep":nick:_                   -> Just (nick, -1)
        _                                  -> Nothing
~~~

`updateBlips` takes in a stream of `(Nick, Message)`, with the person who is
sending the message and their message, and outputs a blip stream that and
emits with a `(Nick, Int)` whenever the message is a command.  The emitted
`(Nick, Int)` has the person to adjust, and the amount to adjust by.  Note
that we ignore commands where the person is trying to increase their own
reputation because that's just lame.

We probably want to keep track of the scores as a `Map Nick Int`, so we can do
that with something like `accum` again.  However, `accum` takes a stream of
normal values, but we have a *blip stream*, so we can use `scanB` instead.
`scanB` is pretty much the same thing, but it collapses a blip stream into a
value stream by holding the "current result" of the fold.[^accumB]

[^accumB]: `scanB f x0 :: Auto m (Blip a) b`, but there's also `accumB f x0 ::
Auto m a (Blip a) (Blip b)`, which emits whenever the input emits only.

~~~haskell
trackReps :: Monad m => Auto m (Blip (Nick, Int)) (Map Nick Int)
trackReps = scanB (\mp (nick, change) -> M.insertWith (+) nick change mp) M.empty
~~~

And finally, the "response" portion --- we want to be able to respond to
commands and look up the result.  We basically had this identical pattern for
`seenBot`:

~~~haskell
queryBlips :: Auto m Message (Blip Nick)
queryBlips = emitJusts (getRequest . words)
  where
    getRequest ("@rep":nick:_) = Just nick
    getRequest _                = Nothing
~~~

And...now we can wrap it all together with a nice proc block:

~~~haskell
-- repBot :: Monad m => Auto m InMessage (Blip [Message])
!!!auto/chatbot.hs "repBot ::"
~~~

Again note that we take advantage of the `Functor` instance of blip streams to
create a new blip stream (`lookupRep <$> queryB`) that emits whenever `queryB`
emits, but replaces the value with `lookupRep` applied to whatever `Nick` was
in the query blip.  We also take advantage that `reps` is in scope and define
`lookupRep` right there in the block.

### announceBot

Let's just go over one more module...and I think you'll be able to use your
imagination to think of and implement your own from here.

Let's make an "announceBot", that listens for "announcement" messages from
anyone (even in private messages) and broadcasts them to all of the channels
in the provided list.  It rate-limits the announcements, though, so that a
user is only limited to three announcements per day.

We can start with our typical "blip stream that emits on a certain command" to
start off everything:

~~~haskell
announceBlips :: Monad m => Auto m (Nick, Message) (Blip [Message])
announceBlips = emitJusts getAnnounces
  where
    getAnnounces (nick, msg) =
      case words msg of
        "@ann":ann -> Just [nick ++ " says \"" ++ unwords ann ++ "\"."]
        _          -> Nothing
~~~

`announceBlips` takes in a nick-message pair and emits an announcement
`[Message]` whenever the incoming message is an announcement command.

Next, we'd like to keep track of how many times a user has made an
announcement today.  This is pretty much just `scanB` again like with
`repBot`:

~~~haskell
trackAnns :: Monad m => Auto m (Blip Nick) (Map Nick Int)
trackAnns = scanB (\mp nick -> M.insertWith (+) nick 1 mp) M.empty
~~~

However, we'd like to be able to "reset" this map whenever a new day arrives.
For that, we can use `resetOn` from [`Control.Auto.Switch`][caswitch], which
takes an `Auto` and gives it a "reset channel" input blip stream, that resets
the whole thing whenever the blip stream emits:

[caswitch]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Switch.html

~~~haskell
resetOn :: Monad m => Auto m a b -> Auto m (a        , Blip c) b
resetOn trackAnns :: Monad m =>     Auto m (Blip Nick, Blip c) (Map Nick Int)
~~~

(It doesn't care about the actual value emitted, so we can leave it as a type
variable `c` conceptually.)

Now the only thing we need is a blip stream that emits whenever there is a new
day.  For that, we can use `onChange` from [`Control.Auto.Blip`][cablip]:

[cablip]: http://hackage.haskell.org/package/auto/docs/Control-Auto-Blip.html

~~~haskell
newDayBlips :: Monad m => Auto m Day (Blip Day)
newDayBlips = onChange
~~~

`newDayBlips` takes in a stream of `Day`s (from `Data.Time`) that we get from
the `InMessage` and outputs a blip stream that emits whenever the day changes.
It emits with the new `Day`...but we don't really care about the emitted
value, we're just using it to trigger `resetOn trackAnns`.

Finally, let's wrap it all together!

Remember, `announceBot` is a full on `ChatBot m`, and not a `RoomBot m`
anymore, so it has to say where it wants to send its messages.

~~~haskell
-- announceBot :: Monad m => [Channel] -> Auto m InMessage OutMessages
!!!auto/chatbot.hs "announceBot ::"
~~~

Only slightly more involved, but still pretty readable, right?  We find out if
things have flooded, and our target channels will be just the original source
if true (a message as a reprimand); otherwise, all the channels in `chans`.
If they have flooded, then our `outB` (blip stream of `[Message]` to send to
each room) will just be `["No flooding!"]` if yes, or the actual announcement
otherwise.

Finally, our `Blip OutMessages` will be the `OutMessage` formed by associating
all of the channels in `targetChans` with the message in `outB`...emitting
whenever `outB` emits.

Note here that we use `(<$)` from the `Functor` instance of blip streams.  `x
<$ fooB` is a new blip stream that emits whenever `fooB` emits...but
instead *replaces the emitted value*.  So for `4 <$ fooB`, if `fooB`
emits with `"hello"`, `4 <$ fooB` emits with `4`.  "Emit at the same time, but
pry out the value and put in your own."

Finally we use `fromBlips`, which we met before in the definition of
`perRoom`: the output is the `OutMessage` in `outMsgsB` whenever `outMsgsB`
*does* emit...or it's `mempty` (the empty map) when it doesn't.

Wrapping it all up
------------------

We have three nice modules now.  Now let's wrap it all together.

First, if you've been following around, you might have noticed that we needed
`Serialize` instances (from the *cereal* library) for `UTCTime` and `Day` in
order for `trackSeens` and `newDayBlips` to serialize properly.  We can just
write really rough versions of them for now for demonstration purposes:

~~~haskell
!!!auto/chatbot.hs "instance Serialize UTCTime" "instance Serialize Day"
~~~

And, writing `chatBot`:

~~~haskell
!!!auto/chatbot.hs "chatBot ::"
~~~

Or, to future-proof, in case we foresee adding new modules:

~~~haskell
!!!auto/chatbot.hs "chatBot' ::"
~~~

And...that's it!

Feel free to [download and run this all yourself][source] using `runghc`!
(provided you have the appropriate libraries installed)

<div class="note">

**Aside**

This is a quick diversion!  It's slightly more advanced, so don't worry if you
don't understand it immediately.

Note that the `perRoom` upgrade has the same `RoomBot m` watch *all* of the
channels and send any replies back to the channel that it just received from.
Every channel is really interacting with the *same* `RoomBot` instance, with
one shared state.  So `perRoom repBot` keeps track of reputations between
rooms --- asking for someone's reputation in one room will be the same as
asking for it in another room.

Another way we could "upgrade" a `RoomBot` is to give each channel its own
little copy, with separate state.  We can do this using `mux`:

~~~haskell
!!!auto/chatbot.hs "isolatedRooms ::"
~~~

`mux` is an "`Auto` multiplexer":

~~~haskell
mux :: (k -> Auto m a b) -> Auto m (k, a) b
~~~

`mux f` associates a separate/different `Auto`, with its own isolated state,
with every key `k`.  It takes in a key-input pair `(k, a)` and feeds the `a`
into the `Auto` it has associated with that key `k`.  The function `f` is what
`Auto` initialize if the `k` has not yet been seen before.

So we feed it a `(Channel, InMessage)`, and it feeds in that `InMessage` to
the `RoomBot m` associated with that `Channel`...and the output is the `Blip
[Message]` blip stream that the `RoomBot` at that `Channel` popped out.

Our "auto initialization function" is `const rb`, because no matter what
channel we're in, we always want to initialize with the same `rb`.

So, for example, if we had `isolatedRooms repBot`, if a message came from
channel *#foo* saying `"@rep john"`, *only the `repBot` associated with #foo*
would get the message, and only that `repBot`'s output will be displayed.  If
here is not yet a `repBot` instance associated with *#foo*, then a new one
will be created by calling `const repBot` on `"#foo"`...initializing a new
`repBot` that only knows about *#foo* messages.

</div>


Fin
---

Hopefully, going over this project, you're starting to see some
common and powerful idioms and tools.  I hope that a clear picture of how to
approach and finish a program with the *auto* library looks...and how
beneficial the platform and what it offers is to streamlining the development
process.

Also, hopefully the "declarative" nature of everything is apparent.
Especially for *proc* blocks...everything just "looks like" a graph of
relationships.  This quantity is related to this quantity in this way, this
quantity is related to that in that way, etc.  It looks like you're just
specifying a graph of relationships, which is really what the core of *auto*
is all about.  We assemble complex relationships by putting together small,
simple relationships.

Another interesting thing is that we never really explicitly managed any sort
of state or state type.  All of our `Auto`s handled their state on their own.
We didn't need to make a giant massive aggregate bulky "global state"
type...and we can add new "aspects" of state (new modules) without ever having
to change any data type.  The state all manages itself!

And yeah, we didn't just implement "easy" modules/components...these are
actual working components that you might see in real bots, and not just toy
ones.

Where can we go from here?  Well, you might actually want to maybe write
"subscription" `Auto`s that are updated every minute or so:

~~~haskell
type ChronBot m = Auto m UTCTime OutMessages
~~~

You feed them inputs every minute with the time, and it's allowed to react
with the time and output an `OutMessages`.  You can use this bot to implement
things like rss feed watchers/subscribers, for instance.

So, instead of using an input channel waiting for `InMessage`, you might wait
for `Either InMessage UTCTime`...and drop in `Left im` whenever you get a
message, and `Right time` from a thread that just waits a minute and
repeatedly throws in times.

We can do this with minimal extra work by using the `(|||)` combinator from
`Control.Auto`:

~~~haskell
(|||) :: Auto m a c -> Auto m b c -> Auto m (Either a         b      ) c
(|||) :: ChatBot m  -> ChronBot m -> Auto m (Either InMessage UTCTime) OutMessages
~~~

And...you get it all for free!  No extra work.  Now both the `ChatBot` and the
`ChronBot` will wait on the input stream, and the `Left`s will be fed to the
`ChatBot` and the `Right`s will be fed to the `ChronBot`.

Anyway, this post is long enough.  Have fun exploring *auto* on your own;
expect more tutorials soon as I continue the [All About Auto][series] series,
too.  I'm always happy to hear about any project you might be working on!  You
can find me on twitter as [mstk][twitter].  If you have any questions or
comments/suggestions, feel free to leave a comment down below or drop by
freenode's *#haskell-auto* or *#haskell-game*, where I go by *jle`*!  And, as
always, happy Haskelling!

[series]: http://blog.jle.im/entries/series/+all-about-auto
[twitter]: https://twitter.com/mstk "Twitter"
