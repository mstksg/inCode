---
title: "Dead-simple TCP/IP services using servant"
categories: Haskell, Tutorials
tags: functional programming, haskell, servant
create-time: 2019/07/31 14:03:23
identifier: servant-services
slug: simple-tcpip-services-servant
---

In my time I've written a lot of throwaway binary TCP/IP services (servers and
services you can interact with over an internet connection, through command
line interface or GUI). For me, this involves designing a protocol from scratch
every time with varying levels of hand-rolled authentication and error
detection (Send this byte for this command, this byte for this other command,
etc.). Once I design the protocol, I then have to write both the client and the
server --- something I usually do from scratch over the raw TCP streams.

This process was fun (and informative) the first few times I did it, but
spinning it up from scratch again every time discouraged me from doing it very
often.  However, thankfully, with the *[servant][]* haskell library, writing a
TCP server/client pair for a TCP service becomes dead-simple --- the barrier
for creating one fades away that designing/writing a service becomes a tool
that I reach for immediately in a lot of cases without second thought.

[servant]: https://hackage.haskell.org/package/servant

*servant* is usually advertised as a tool for writing web servers, web
applications, and REST APIs, but it's easily adapted to write non-web things as
well (especially with the help of *[servant-client][]* and *[servant-cli][]*).
Let's dive in and write a simple TCP/IP service (a todo list manager) to see
how straightforward the process is!

[servant-client]: https://hackage.haskell.org/package/servant-client
[servant-cli]: https://hackage.haskell.org/package/servant-cli

To goal of this article is to take service/program that you already have
planned out, and *easily provide* it with a networked API that can be used over
any TCP/IP connection (even locally, if you are into that sort of thing).  We
aren't going to teach you *how* to write a todo app, but rather how to hook up
a todo app over a TCP/IP connection quickly.  This post can also serve as a
stepping-stone to a "microservices architecture", if you intend to build
towards one (this is explored deeper by [k-bx][owlcloud])...but really it's
more focused for standalone user-facing applications.  How you apply these
techniques is up to you :)

[owlclod]: https://github.com/k-bx/owlcloud


This article is written for the late beginner to intermediate haskeller, who
knows how to "do" what they want the server to do, but only just needs a simple
way to hook it up over a TCP/IP connection.

Todo API
--------

As an example, we'll work through building one of my favorite self-contained
mini-app projects, a [Todo list manager a la todo-mvc][todo-mvc].  Our service
will provide functionality for:

[todo-mvc]: http://todomvc.com/

1.  Viewing all tasks and their status
2.  Adding a new task
3.  Setting a task's completion status
4.  Deleting a task
5.  Pruning all completed tasks

To facilitate doing this over an API, we'll assign each task a task ID when it
comes in, and so commands 3 and 4 will require a task ID.

To formally specify our API:

1.  `list`: View all tasks by their ID, status, and description.  Optionally be
    able to filter for only incomplete tasks.
2.  `add`: Given a new task description, insert a new uncompleted task.  Return
    the ID of the new task.
3.  `set`: Given a task ID and an updated status, update the task's status.
4.  `delete`: Given a task ID, delete the task.
5.  `prune`: Remove all completed tasks.  Returns all the task IDs that where
    deleted.

We can state this using servant's type level DSL, using an `IntMap` (from
*containers*) to represent the current tasks and an `IntSet` to represent a set
of task IDs.

```haskell
!!!servant-services/Api.hs
```

We have five routes, which more or less mirror exactly the five bullet points
listed above, with some minor implementation choices:

*   For `list`, we take "filtered or not filtered" as a query flag, and return
    an `IntMap` of a `Task` data type (status, description) under their integer
    ID key.
*   For `add`, we take the task description as a query parameter, and return
    the new ID.
*   For `set`, we take the task ID as a capture (path component) and an
    optional boolean query parameter.  If the parameter is not given, it will
    be taken as a toggle; otherwise, it will be taken as a setting of the
    completion status.
*   For `delete`, we also take the task ID as a capture.
*   For `prune`, we return the deleted IDs as an `IntSet` (also from
    *containers*).

"Query flag", "query parameter", "capture" are all a part of the language of
HTTP and W3C.  In our case, since we aren't ever directly programming against
the actual protocol-HTTP (it's only used under the hood) or pretending to write
an actual web-interfacing server, we don't really need to care too much to
distinguish them.  However, it can be useful to pick meaningful choices if we
ever do want to expose this API as a web service.

Todo Service Server
-------------------

The logic to implement a todo server is pretty straightforward, which is why we
chose it as an example project.  It only really needs one state: the `IntMap`
of current tasks.

To write a servant server with *[servant-server][]*, I usually like to just set up a skeleton with each
route:

[servant-server]: https://hackage.haskell.org/package/servant-server

```haskell
serveTodoApi :: IORef (IntMap Task) -> Server TodoApi
serveTodoApi taskRef = serveList
                  :<|> serveAdd
                  :<|> serveSet
                  :<|> serveDelete
                  :<|> servePrune
```

The corresponding GHC error tells us everything we need:

```
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
```

It tells us exactly the types of each handler we need.

Knowing that `Handler` is a `MonadIO`, we can now directly just write
every handler in terms of how it manipulates the `IntMap` in the `IORef`:

```haskell
!!!servant-services/server.hs "serveTodoApi ::"
```

And that's it!

To run our server, we can use *[warp][]*'s `run` with *servant-server*'s
`serve`, after initializing the `IORef` that our server will use with an empty
map:

```haskell
!!!servant-services/server.hs "main ::"
```

[warp]: https://hackage.haskell.org/package/warp

We now have a todo TCP/IP service running on port 3434!

Todo Service Client
-------------------

To write a client, we have a couple of options.

You *could* hand-write a command-line client using either
*[optparse-applicative][]* (or your favorite command line args parser) for an
options-and-arguments style interface or a readline library like
*[haskeline][]* for an interactive interface.

[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[haskeline]: https://hackage.haskell.org/package/haskeline

Hand-writing one is made pretty simple with *[servant-client][]*, which allows
you to generate all of the HTTP calls using the `client` function:

```haskell
list :<|> add :<|> set :<|> delete :<|> prune = client todoApi
```

This will give you the functions `list :: Bool -> ClientM (IntMap Task)`,
`add :: Text -> ClientM Int`, `set :: Int -> Maybe Bool -> ClientM ()`, etc.,
that you can now run whenever you want to dispatch a command or make a fetch
according to your hand-rolled needs.

However, this blog post is about "dead-simple" setups that you can roll out
within minutes.  For *that*, you can use the library *[client-cli][]* to
automatically generate an *optparse-applicative*-based command line interface
that allows you to directly specify your commands based on command line
arguments

```haskell
!!!servant-services/client.hs "main ::" "displayList ::"
```

The main thing that does the work is `parseHandleClient`, which takes (after
some proxies specifying the API and client type):

1.  Extra arguments modifying the command line help messages
2.  A way to "handle" a response for every command.

    *   For `list`, we display it using a nice pretty-printer
    *   For `add`, we show the new ID number.
    *   For `set` and `delete`, we just display the fact that it was successful
        (remember, these routes returned `()`)
    *   For `prune`, we pretty-print the deleted items.

    We choose to handle each command as a `String`, but we can choose to handle
    them each into any type we want (even `IO`) as long as each handler returns
    something of the same type.

    The handler is run and is returned as the value in `Right` when used with
    `runClientM`.

Again, a nice way to "write" our `parseHandleCleint` function with its handlers
is by writing a skeleton definition and letting GHC tell us what goes in each
hole:

```haskell
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
```

One last thing --- *servant-cli* requires some instances to provide
"help" documentation for the command line interfaces:

```haskell
!!!servant-services/client.hs "instance ToParam"
```
And we now get a fully-featured client for our service!

```
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
```

Conclusion
----------

One major thing I like about this method is that it's very type-safe and allows
for types to *drive* your development.  Note how all of the messiness of a
binary protocol like TCP/IP are abstracted away, and you only ever deal with
`IntMap`s, `Text`, `Bool`, `Int`s.  And also note how in every step of the way,
we use types to guide us: in writing our server, we first used "blanks" to ask
GHC what the type of each of the handlers needs to be, which helps us plan our
server implementation and ensures that it handles everything properly.  In
writing our client, we also used "blanks" to ask GHC the type of each of our
response handlers needs to be, which allows us to quickly and effectively drill
down every option.

Hopefully this post serves as a good introduction to the *servant*,
*servant-server*, and *servant-cli* libraries, and additionally shows how easy
it is to give any application you want a TCP/IP interface to be usable as a
TCP/IP service.  In the real world, your applications might be a little more
complex (and you might even require authentication), but hopefully after
reading this article, the "network-facing" part of your service or application
becomes a lot less intimidating :)

I know for me, the main benefit has been to tear down the "barrier of
entry"/mental startup cost, and I've started writing little services and
clients like this as a first-step in a lot of cases, instead of as something I
dread and only end up adding to a few programs.

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts.  Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
