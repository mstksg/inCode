---
title: "Setting up a dead-simple TCP/IP service using servant"
categories: Haskell, Tutorials
tags: functional programming, haskell, servant
create-time: 2019/07/31 14:03:23
identifier: servant-services
slug: servant-services
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

*servant* is usually advertised as a tool for writing web servers and web
applications, but it's easily adapted to write non-web things as well
(especially with the help of *[servant-cli][]*).  Let's dive in and write a
simple TCP/IP service (a todo list manager) to see how straightforward the
process is!

[servant-cli]: https://hackage.haskell.org/package/servant-cli
