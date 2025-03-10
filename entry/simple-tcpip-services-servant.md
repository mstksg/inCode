Dead-simple TCP/IP services using servant

==========================================

> Originally posted by [Justin Le](https://blog.jle.im/) on August 5, 2019.
> [Read online!](https://blog.jle.im/entry/simple-tcpip-services-servant.html)

In my time I've written a lot of throwaway binary TCP/IP services (servers and
services you can interact with over an internet connection, through command line
interface or GUI). For me, this involves designing a protocol from scratch every
time with varying levels of hand-rolled authentication and error detection (Send
this byte for this command, this byte for this other command, etc.). Once I
design the protocol, I then have to write both the command line client and the
server --- something I usually do from scratch over the raw TCP streams.

This process was fun (and informative) the first few times I did it, but
spinning it up from scratch again every time discouraged me from doing it very
often. However, thankfully, with the
*[servant](https://hackage.haskell.org/package/servant)* haskell library (and
*[servant-cli](https://hackage.haskell.org/package/servant-cli)*, for command
line clients), writing a TCP server/client pair for a TCP service (using HTTP
under the hood) becomes dead-simple --- the barrier for creating one fades away
that designing/writing a service becomes a tool that I reach for immediately in
a lot of cases without second thought.

*servant* is usually advertised as a tool for writing web servers, web
applications, and REST APIs, but it's easily adapted to write non-web things as
well. Let's dive in and write a simple TCP/IP service (a todo list manager) to
see how straightforward the process is!

To goal of this article is to take service/program that you already have planned
out, and *easily provide* it with a networked API that can be used over any
TCP/IP connection (over the internet, or even locally). This won't teach you
*how* to write a todo app, but rather how to *hook up* a todo app over a TCP/IP
connection quickly, with a command line client --- and in such a simple way that
you wouldn't give a second thought based on complexity issues.

This post can also serve as a stepping-stone to a "microservices architecture",
if you intend to build towards one (this is explored deeper by
[k-bx](https://github.com/k-bx/owlcloud))...but really it's more focused for
standalone user-facing applications. How you apply these techniques is up to you
:)

All of the code in this article is available online, and the server and client
are available as "stack executables": if you download them all, and set the
permissions properly (`chmod u+x`), you can directly run them to launch the
server and client (if they are all download to the same directory).

-   [API](https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/Api.hs)
-   [Server](https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/server.hs)
-   [Client](https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/client.hs)

## Todo API

As an example, we'll work through building one of my favorite self-contained
mini-app projects, a [Todo list manager a la todo-mvc](http://todomvc.com/). Our
service will provide functionality for:

1.  Viewing all tasks and their status
2.  Adding a new task
3.  Setting a task's completion status
4.  Deleting a task
5.  Pruning all completed tasks

To facilitate doing this over an API, we'll assign each task a task ID when it
comes in, and so commands 3 and 4 will require a task ID.

To formally specify our API:

1.  `list`: View all tasks by their ID, status, and description. Optionally be
    able to filter for only incomplete tasks.
2.  `add`: Given a new task description, insert a new uncompleted task. Return
    the ID of the new task.
3.  `set`: Given a task ID and an updated status, update the task's status.
4.  `delete`: Given a task ID, delete the task.
5.  `prune`: Remove all completed tasks. Returns all the task IDs that where
    deleted.

We can state this using servant's type level DSL, using an `IntMap` (from
*containers*) to represent the current tasks and an `IntSet` to represent a set
of task IDs.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/Api.hs

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall      #-}

module Api where

import           Data.Aeson
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Proxy
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API

data Task = Task
    { taskStatus :: Bool
    , taskDesc   :: Text
    }
  deriving (Show, Generic)
instance ToJSON   Task
instance FromJSON Task

type TodoApi = "list"   :> QueryFlag "filtered"
                        :> Get  '[JSON] (IntMap Task)
          :<|> "add"    :> QueryParam' '[Required] "desc" Text
                        :> Post '[JSON] Int
          :<|> "set"    :> Capture "id" Int
                        :> QueryParam "completed" Bool
                        :> Post '[JSON] ()
          :<|> "delete" :> Capture "id" Int
                        :> Post '[JSON] ()
          :<|> "prune"  :> Post '[JSON] IntSet

todoApi :: Proxy TodoApi
todoApi = Proxy
```

(This is how you specify an API (via a type) using *servant*, with their
provided `:<|>` and `:>` operators --- `:<|>` combines routes, `:>` combines
path components, and `QueryParam`, `Capture`, etc. all add parts to components
and routes.)

We have five routes, which more or less mirror exactly the five bullet points
listed above, with some minor implementation choices:

-   For `list`, we take "filtered or not filtered" as a query flag, and return
    an `IntMap` of a `Task` data type (status, description) under their integer
    ID key.
-   For `add`, we take the task description as a query parameter, and return the
    new ID.
-   For `set`, we take the task ID as a capture (path component) and an optional
    boolean query parameter. If the parameter is not given, it will be taken as
    a toggle; otherwise, it will be taken as a setting of the completion status.
-   For `delete`, we also take the task ID as a capture.
-   For `prune`, we return the deleted IDs as an `IntSet` (also from
    *containers*).

"Query flag", "query parameter", "capture" are all a part of the language of
HTTP and W3C. In our case, since we aren't ever directly programming against the
actual protocol-HTTP (it's only used under the hood) or pretending to write an
actual web-interfacing server, we don't really need to care too much to
distinguish them. However, it can be useful to pick meaningful choices if we
ever do want to expose this API as a web service.

## Todo Service Server

The logic to implement a todo server is pretty straightforward, which is why we
chose it as an example project. It only really needs one state: the `IntMap` of
current tasks.

To write a servant server with
*[servant-server](https://hackage.haskell.org/package/servant-server)*, I
usually like to just set up a skeleton with each route:

``` haskell
serveTodoApi :: IORef (IntMap Task) -> Server TodoApi
serveTodoApi taskRef = serveList
                  :<|> serveAdd
                  :<|> serveSet
                  :<|> serveDelete
                  :<|> servePrune
```

The corresponding GHC error tells us everything we need:

    server.hs:15:24: error:
        Variable not in scope: serveList :: Bool -> Handler (IntMap Task)
       |
    15 | serveTodoApi taskRef = serveList
       |                        ^^^^^^^^^

    server.hs:16:24: error:
        Variable not in scope: serveAdd :: Text -> Handler Int
       |
    16 |                   :<|> serveAdd
       |                        ^^^^^^^^

    server.hs:17:24: error:
        Variable not in scope: serveSet :: Int -> Maybe Bool -> Handler ()
       |
    17 |                   :<|> serveSet
       |                        ^^^^^^^^

    server.hs:18:24: error:
        Variable not in scope: serveDelete :: Int -> Handler ()
       |
    18 |                   :<|> serveDelete
       |                        ^^^^^^^^^^^

    server.hs:19:24: error:
        Variable not in scope: servePrune :: Handler IntSet
       |
    19 |                   :<|> servePrune
       |                        ^^^^^^^^^^

It tells us exactly the types of each handler we need.

Because `Handler` has a `MonadIO` instance, we can now directly just write every
handler in terms of how it manipulates the `IntMap` in the `IORef`, and use
`liftIO`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/server.hs#L16-L46

serveTodoApi :: IORef (IntMap Task) -> Server TodoApi
serveTodoApi taskRef = serveList
                  :<|> serveAdd
                  :<|> serveSet
                  :<|> serveDelete
                  :<|> servePrune
  where
    serveList :: Bool -> Handler (IntMap Task)
    serveList filt = filtFunction <$> liftIO (readIORef taskRef)
      where
        filtFunction
          | filt      = IM.filter (not . taskStatus)
          | otherwise = id
    serveAdd :: Text -> Handler Int
    serveAdd t = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      let newKey = maybe 0 ((+ 1) . fst) (IM.lookupMax ts)
      in  ( IM.insert newKey (Task False t) ts, newKey )
    serveSet :: Int -> Maybe Bool -> Handler ()
    serveSet tid s = liftIO $ atomicModifyIORef' taskRef $ \ts ->
        ( IM.adjust adjuster tid ts, () )
      where
        adjuster (Task c d) = case s of
          Nothing -> Task (not c) d
          Just c' -> Task c'      d
    serveDelete :: Int -> Handler ()
    serveDelete tid = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      ( IM.delete tid ts, () )
    servePrune :: Handler IntSet
    servePrune = liftIO $ atomicModifyIORef' taskRef $ \ts ->
      let (compl,incompl) = IM.partition taskStatus ts
      in  (incompl, IM.keysSet compl)
```

And that's it!

To run our server, we can use
*[warp](https://hackage.haskell.org/package/warp)*'s `run` with
*servant-server*'s `serve`, after initializing the `IORef` that our server will
use with an empty map:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/server.hs#L48-L53

main :: IO ()
main = do
    taskRef <- newIORef IM.empty
    putStrLn "Launching server..."
    run 3434 $
      serve todoApi (serveTodoApi taskRef)
```

We now have a todo TCP/IP service running on port 3434!

## Todo Service Client

To write a client, we have a couple of options.

You *could* hand-write a command-line client using either
*[optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)*
(or your favorite command line args parser) for an options-and-arguments style
interface or a readline library like
*[haskeline](https://hackage.haskell.org/package/haskeline)* for an interactive
interface.

Hand-writing one is made pretty simple with
*[servant-client](https://hackage.haskell.org/package/servant-client)*, which
allows you to generate all of the HTTP calls using the `client` function:

``` haskell
list :<|> add :<|> set :<|> delete :<|> prune = client todoApi
```

This will give you the functions `list :: Bool -> ClientM (IntMap Task)`,
`add :: Text -> ClientM Int`, `set :: Int -> Maybe Bool -> ClientM ()`, etc.,
that you can now run whenever you want to dispatch a command or make a fetch
according to your hand-rolled needs.

However, this blog post is about "dead-simple" setups that you can roll out
within minutes. For *that*, you can use the library
*[servant-cli](https://hackage.haskell.org/package/servant-cli)* to
automatically generate an *optparse-applicative*-based command line interface
that allows you to directly specify your commands based on command line
arguments

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/client.hs#L34-L60

main :: IO ()
main = do
    c <- parseHandleClient todoApi (Proxy :: Proxy ClientM)
        ( header "todo" <> progDesc "Todo TCP/IP service client" )
        ( displayList
     :<|> (\i -> "Added with ID " ++ show i)
     :<|> const "Set!"
     :<|> const "Deleted!"
     :<|> (\ts -> "Cleared items: " ++ intercalate ", " (map show (IS.toList ts)))
        )

    res <- newManager defaultManagerSettings >>= \mgr ->
      runClientM c $
        mkClientEnv mgr (BaseUrl Http "localhost" 3434 "")

    case res of
      Left  e -> throwIO e
      Right r -> putStrLn r

displayList :: IntMap Task -> String
displayList = unlines
            . map (\(k, t) -> printf "%d) %s" k (displayTask t))
            . IM.toList
  where
    displayTask (Task c t)
      | c         = "[x] " ++ T.unpack t
      | otherwise = "[ ] " ++ T.unpack t
```

The main thing that does the work is `parseHandleClient`, which takes (after
some proxies specifying the API and client type):

1.  Extra arguments modifying the command line help messages

2.  A way to "handle" a response for every command.

    -   For `list`, we display it using a nice pretty-printer
    -   For `add`, we show the new ID number.
    -   For `set` and `delete`, we just display the fact that it was successful
        (remember, these routes returned `()`)
    -   For `prune`, we pretty-print the deleted items.

    We choose to handle each command as a `String`, but we can choose to handle
    them each into any type we want (even something involving`IO`) as long as
    each handler returns something of the same type.

    The handler is run and is returned as the value in `Right` when used with
    `runClientM`.

Again, a nice way to "write" our `parseHandleClient` function with its handlers
is by writing a skeleton definition and letting GHC tell us what goes in each
hole:

``` haskell
main :: IO ()
main = do
    c <- parseHandleClient todoApi (Proxy :: Proxy ClientM)
        ( header "todo" <> progDesc "Todo TCP/IP service client" )
        ( handleList
     :<|> handleAdd
     :<|> handleSet
     :<|> handleDelete
     :<|> handlePrune
        )
    pure ()
```

    client.hs:36:11: error:
        • Variable not in scope: handleList :: IntMap Task -> [Char]
       |
    36 |         ( handleList
       |           ^^^^^^^^^^

    client.hs:37:11: error:
        Variable not in scope: handleAdd :: Int -> [Char]
       |
    37 |      :<|> handleAdd
       |           ^^^^^^^^^

    client.hs:38:11: error:
        Variable not in scope: handleSet :: () -> [Char]
       |
    38 |      :<|> handleSet
       |           ^^^^^^^^^

    client.hs:39:11: error:
        Variable not in scope: handleDelete :: () -> [Char]
       |
    39 |      :<|> handleDelete
       |           ^^^^^^^^^^^^

    client.hs:40:11: error:
        Variable not in scope: handlePrune :: IS.IntSet -> [Char]
       |
    40 |      :<|> handlePrune
       |           ^^^^^^^^^^^

One last thing --- *servant-cli* requires some instances to provide "help"
documentation for the command line interfaces:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/servant-services/client.hs#L25-L32

instance ToParam (QueryFlag "filtered") where
    toParam _ = DocQueryParam "filtered" [] "Whether or not to filter completed items" Flag
instance ToParam (QueryParam' '[Required] "desc" Text) where
    toParam _ = DocQueryParam "desc" [] "Task description" Normal
instance ToParam (QueryParam "completed" Bool) where
    toParam _ = DocQueryParam "completed" ["True","False"] "Set status to (leave out for toggle)" Normal
instance ToCapture (Capture "id" Int) where
    toCapture _ = DocCapture "id" "ID number of task"
```

And we now get a fully-featured client for our service!

    # make sure you run ./server.hs in a separate window
    $ ./client.hs --help
    todo

    Usage: client.hs COMPONENT
      Todo TCP/IP service client

    Available options:
      -h,--help                Show this help text

    Path components:
      add
      delete
      list
      prune
      set

    $ ./client.hs add --help
    add

    Usage: client.hs add --desc TEXT


    Available options:
      --desc TEXT              Task description (Text)
      -h,--help                Show this help text

    $ ./client.hs list --help
    list

    Usage: client.hs list [--filtered]


    Available options:
      --filtered               Whether or not to filter completed items
      -h,--help                Show this help text

    $ ./client.hs set --help
    set

    Usage: client.hs set <id> [--completed BOOL]


    Available options:
      <id>                     ID number of task (Int)
      --completed BOOL         Set status to (leave out for toggle) (options: True,
                               False)
      -h,--help                Show this help text

## Conclusion

One major thing I like about this method is that it's very type-safe and allows
for types to *drive* your development. Note how all of the messiness of a binary
protocol like TCP/IP are abstracted away, and you only ever deal with `IntMap`s,
`Text`, `Bool`, `Int`s. And also note how in every step of the way, we use types
to guide us: in writing our server, we first used "blanks" to ask GHC what the
type of each of the handlers needs to be, which helps us plan our server
implementation and ensures that it handles everything properly. In writing our
client, we also used "blanks" to ask GHC what the type of each of our response
handlers needs to be, which allows us to quickly and effectively drill down
every option. These are things that *all* servant-based projects get to benefit
from.

Hopefully this post serves as a good introduction to the *servant*,
*servant-server*, and *servant-cli* libraries, and additionally shows how easy
it is to give any application you want a TCP/IP interface to be usable as a
TCP/IP service. In the real world, your applications might be a little more
complex (and you might even require authentication), but hopefully after reading
this article, the "network-facing" part of your service or application becomes a
lot less intimidating :)

I know for me, the main benefit has been to tear down the "barrier of
entry"/mental startup cost, and I've started writing little services and clients
like this as a first-step in a lot of cases, instead of as something I dread and
only end up adding to a few programs.

Also, fair disclosure: the author of *servant-cli* is me! I'm not *super* happy
with the user experience story at the moment, but it has been usable for me so
far. If you have any suggestions or ideas for improving *servant-cli*, I'd [love
to hear](https://github.com/mstksg/servant-cli) (and look at any PRs!)

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

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

