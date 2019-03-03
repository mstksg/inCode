---
title: "dhall-typed: Exploring an implementation of a typed lambda calculus"
categories: Haskell
tags: haskell, dependent types
create-time: 2019/03/02 17:20:21
identifier: typed-lc-1
slug: exploring-typed-lambda-calculus
---

I've been wrapped up on a side-project for a while that has been an interesting
deep dive into the limits of type-level Haskell.  I've been stuck many times,
but I've often come back later with new ideas to go around certain problems.
Now at another stuck point, I just wanted to take some time to put my thoughts
out into a blog post for those interested to see what it feels like to be down
in these miry pits, and also for those more skilled than me to be able to maybe
help :)

Keep in mind that the struggles contained in this post are:

*   Not representative of most day-to-day practical type-level Haskell, a la
    *servant* and other type-level user-friendly libraries.
*   More likely than not to be very trivial in a language with actual dependent
    types, like Idris.

The motivation
--------------

Here I summarize the motivation behind this project.  Feel free to skip!

One of my favorite Haskell projects in the past year or so has been [dhall][],
a typed and functional turing-incomplete strongly normalizing configuration
language.  I honestly believe that dhall is the solution to many of the
problems of configuration languages (w.r.t json, yaml, xml) in modern software
development, and I hope to see it gain widespread adoption some day as a
"killer app" of functional turing-incomplete programming.

[dhall]: https://dhall-lang.org/

One day I came across a post on the Haskell subreddit asking if there was a
nice way to allow user scripts within a Haskell application, and I suggested
maybe to allow users to provide scripts as dhall programs that are interpreted
by Haskell applications. This idea interested me; I tried out a
proof-of-concept with monoidal actions, and it wasn't too bad.  But to allow
for more expressive scripts, I tried implementing a framework with *monadic*
actions.  I tried implementing a general framework using church-encoded free
monads, and (aside from the lack of type inference) it wasn't altogether too
awful.

However, writing the Haskell code to *import* these monadic actions into
Haskell wasn't very fun.  The only way I knew how was basically to interpret an
untyped Dhall AST (parsed into Haskell) into something you could run within
Haskell.

Interpreting an untyped Dhall AST within Haskell is as much fun as you'd think
it would be (that is, not very).  It's essentially the same thing as writing a
`FromJSON` instance --- something you want to avoid doing by hand whenever
possible.

This is where I got the idea: instead of interpreting an untyped Dhall
AST...why don't we interpret a *typed* Dhall AST?  That is, an AST where the
*type* of the Dhall expression is directly a part of the type of the AST value.
In my mind, a `DTerm a` would be a dhall term that represents something of type
`a`.  From there, you can then convert any typed Dhall AST term into a Haskell
value representing a type you can know statically.

Instead of parsing a `Expr` into your ADT in a way that might fail (because of
a bad `Expr`), you can parse an *explicitly typed* `Expr` into your ADT, in a
way that you ensure all of the fields exist, etc., in a *total* way.

And, in the context of dhall scripting, you could actually just explicitly
"run" the typed ADT directly within Haskell to produce something of an expected
type (like a free monad instantiation).

So, that was the dream.  How hard could it be?

Dhall Calculus
--------------

The first step in my journey was to understand the formal system that Dhall is
based on.  According to the documentation, dhall has roots in [System Fω][].

[System Fω]: https://en.wikipedia.org/wiki/System_F#System_F.CF.89

After doing some heavy digging and reading, this is what I gathered:

### The Hierarchy

We have a hierarchy of values and their meta-level "types".  At the bottom, we
have terms (`1`, `True`, `"hello"`, etc.).  You can write functions,
applications, etc. with terms.  To help us reason with terms, all terms are
categorized into *types* (`Bool`, `Natural`, etc.).

However, types are also their own level of values, in that we can have
type-level functions and things like that.  In order to allow us to reason with
these type-level constructs, we categories types into *kinds* (`Type`, `Type ->
Type`), and say that the kind of types that can categorize values is `Type`.

In Dhall, kinds are also their own level of values.  We can have kind-level
functions and function application, and all of those complex structures.  To
help us reason with kinds, we categorize kinds into *sorts* (`Kind`, `Kind ->
Kind`, etc.), and say that the sort of kinds that can categorize types is
`Kind`.

In Dhall, the level of sorts is where things essentially end.  While Dhall
sorts are also their own level of values in a sense, there is a limitation: we
cannot make *sort functions*, and we cannot have *sort variables*.  They are
(perhaps superficially) forbidden in Dhall.  This effectively stops the
hierarchy, because there is no need to consider the meta-level "type" of a
sort.  By forbidding sort variables and sort functions and similar
abstractions, we basically make "order" (the "type" of a sort) a meaningless
construct.  We make `Sort` the "order" of sorts that can categorize kinds, but
this fundamental limitation means that `Sort` is the only order that exists.

### Within a Level

Alright, so we've established *why* the hierarchy exists, what gives rise to
them, and the mechanism that essentially cuts it off at *sort*.

So, what actually *lives* at each of these levels?  What sort of structures do
we allow that exist completely *within* a level (discounting things that span
multiple levels)?

#### Primitives

At each level exists the primitives of that level that are provided by the
language.  Think of these as the "built-in terms", "built-in types", "built-in
kinds", etc.  Without primitives, we'd just be manipulating lambdas all day
(which is, incidentally, what [morte][] is.  In a way, Dhall can be thought of
as just a fork of morte that adds primitives).

[morte]: http://hackage.haskell.org/package/morte

At the *term* level, we have primitives like:

```dhall
1
"hello"
-8
List/fold
Natural/even
```

And also more complex primitive "constructors", that combine sub-terms

```dhall
x + y                       -- operators in Dhall
[x,y,z]                     -- list literals in Dhall
{ foo = x, bar = y }        -- record literals in Dhall
x ∧ y                       -- record merging in Dhall
< Left = x | Right : y >    -- union literals in Dhall; x is a term, y is a type
```

At the *type* level, we have primitives like:

```dhall
Bool
Text
List
```

And primitive "constructors:"

```dhall
{ foo : x, bar : y }       -- record types in Dhall; x and y are types of kind Type
x ⩓ y                      -- merging record types in Dhall
{ foo = x, bar = y }       -- type-level record literals in Dhall; x and y are types
< Left : x | Right : y >   -- union types in Dhall; x and y are types of kind Type
< Left = x | Right : y >   -- type-level union literals in Dhall; x is a type, y is a kind
```

At the *kind* level, we only have the kinds of type-level records and unions
and also kind-level records and unions (and the operations on them) as
primitives.  At the *sort* level, we only have the sorts of kind-level records
and unions, and also sort-level records and unions (and operations on them).

#### Function abstraction

At all levels (except for *sort*), we have the *function abstraction*, that looks
like:

```dhall
\(x : blah) -> thing
```

Here are examples at the term, type, and kind levels:

```dhall
-- Term-level
\(x : Natural) -> x + 2
-- Type-level
\(x : Type) -> List x
-- Kind-level
\(x : Kind -> Kind) -> x Type
```

Disallowing sort-level function abstractions is a mechanism of artificially
"cutting off" the effective hierarchy at sort.  In theory, we could go on
forever; but what ever level we disallow lambdas at is the level that our
hierarchy stops at.

An important point here is that these are all lambdas "within" a single level.
The *input* and the *output* are all within the same level: term to term, type
to type, kind to kind, etc.

As a consequence of "having function abstractions", we also gain a couple of
other constructs:

*   **Variables**: At all levels with function abstractions, we can talk about
    *variables* at that level. A variable is *bound* if it's inside the body of
    a lambda that introduces it, and is *free* otherwise.
*   **Function Application**: At all levels with function abstractions, we also
    have *function applications*, which is the application of a function
    abstraction to a value:

    ```dhall
    -- Term level
    (\(x : Natural) -> x + 2) 4
    -- Type level
    (\(x : Type) -> List x) Bool
    ```

    *Technically*, we can actually have function application on levels without
    function applications, of that level has the apropriate primitives (like
    `List` on the type level, `Natural/show` at the value level). However, in
    Dhall, this doesn't happen, so we're going to address this as essentially
    paired with function abstraction.
