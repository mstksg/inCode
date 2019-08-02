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

We can state this using servant's type level DSL, using an `IntMap` to
represent the current tasks and an `IntSet` to represent a set of task IDs.

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

The corresponding ghc error tells us everything we need:

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

Too easy
