---
title: "Effective LLM-Assisted Haskell: Understanding Requirements-Circumventing Behavior"
categories: Haskell
tags: functional programming, agentic, haskell
create-time: 2026/07/18 14:05:05
identifier: effective-llm-assisted-haskell-1
slug: effective-llm-assisted-haskell-understanding-requirements-circumventing-behavior
---

Sooo yes it's true, I've been integrating LLMs and agentic coding tools in my
Haskell coding since the beginning of this year.  I remember last year barely
being able to prompt my heavily GADT-ized [fancy Haskell][fancy] Haskell into
the web UI for ChatGPT/Gemini, progressing to using Open AI's web-based codex
host, to biting the bullet and finally permitting a sentient Eldritch being to
persistently live and experience existence and qualia on my personal machines
and ship-of-theseus dev infrastructure I've been operating continuously since
freshman year of uni.

[fancy]: https://blog.jle.im/entry/extreme-haskell-typed-expression-edsls-1.html

I've been using it for a lot of my projects, both personal and professional.
Along the way I've collected some habits and patterns for using them more and
more effectively. I've dropped notes [on various articles][agentic] about small
things, but hesitated on aggregating them into an actual blog post...mostly
because I'm "new" at this (six months?), and also because the nature of
interacting with LLMs and their limitations seems to be re-written every few
months.

[agentic]: https://blog.jle.im/entries/tagged/agentic.html

<!-- But, after some recent community rumblings and fragmentation regarding -->
<!-- high-stakes existential tensions between agentic LLM coding and the future -->
<!-- direction of Haskell, I realized I could probably lend a constructive hand by -->
<!-- describing how I, personally, a Haskeller of 15+ years, (7+ professional) -->
<!-- personally make the most out of them. -->

Note: none of the views espoused in this post are endorsed by my employer. They
are my own and will most likely change drastically over time.

This post will focus on what I call "requirements-circumventing behavior" in
LLMs as it relates to effective Haskell.

Scope
-----

First, let's established the level that these apply at: this is _not_ talking
about "purely vibed" code: it's about using agentic LLMs as glorified
multi-file autocompletes, for mostly incremental tasks ("add a new feature
flag", "incorporate this new protocol", "expose this as a CLI") or, if
something greenfield, iterating over a design collaboratively and not "just do
it".

Specifically for code that is committed into a serious project repo, all design
decisions and concepts and code structures are written in the style and care as
if I had written it by hand, anyone who reviews any line of code I commit can
trust on my name and reputation that it was written with as much thought and
concern as any other line.

So, this will not a guide to "vibe coding in Haskell" (though that might come
in a later post), but it'll be more about specific things I've noticed about
using LLMs collaboratively to assist in writing Haskell.

Also a disclaimer: I do most of my coding with Opus 4.6-4.8 depending on the
situation. Let that date this post as it may.

Haskell and LLMs
----------------

Now my personal opinion and wish: in an ideal world, Haskell _should_ in
agentic software engineering as what LEAN is in agentic research mathematics: a
framework for LLMs to self-construct the scaffolding they need to guide
themselves to their correct goal.

I do _not_ believe that "correctness at generation-time" is a plausible goal,
not today in 2026, and probably not ever. Motion towards correctness is
asymptotic. You might get close enough sometimes, but the long tail of
correctness is...long. And demands respect. That's why even the most full-vibed
frontier-model projects have [large suites of tests][bun] that exist to guide
the agent. My bet is that agentic coding in five years will not be "spit out
the correct program in one batch of text", it will be "set up the best
scaffolding that guides the implementation".

[bun]: https://bun.com/blog/bun-in-rust

To these ends, I try to structure my codebases with the correct scaffolding to
help augment the intuition of path exploration: AI has to choose with paths are
the "most promising", so I structure my code-base to quickly kill off paths
that are most likely to be dead-ends or lead to unmaintainable code, and to
channel AI exploration along more promising paths. The greatest tool are
types, compilers, warnings, hints.

A lot of these are the same things we teach to human coders:

*   [Parse, Don't Validate][pdv]: set up your types to make invalid states
    unrepresentable. AI will, by nature, NOT do this, even the latest frontier
    models. They usually slip into defensive programming (adding `isNull`
    checks everywhere, boolean checks for constructors like `isNothing` /
    `isObject` / `isArray` instead of just pattern matching, filtering lists
    for duplicates instead of using `Data.Set`, adding precondition assertions,
    etc.), and it's usually up to the human driver to stop and consciously
    create data types that model the domain correctly, structurally prevent
    invalid states, enforce pre- and post-conditions via structurally verified
    types and not boolean checks, avoid boolean blindness, etc.

    Either that, or dedicate an entire session or planning step to clarifying
    these domain requirements in their types.
*   Use warnings and linters enthusiastically: `-Werror=incomplete-patterns` of
    course, and the default `-Werror` usually covers a lot of AI failure modes
    (leaving in dead code, leaving in arguments in functions for no reason and
    killing opportunities for abstraction).
*   Adding robust test suites for things that cannot be tested within the
    types, but also explicitly laying out integration tests: something which
    LLMs seem pretty allergic to without proper prompting.

[pdv]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

From my own empirical observations, all of these things are antithetical to how
even frontier LLM models operate. And no amount of prompting or instruction can
circumvent their natural behavior in the long run, embedded deeply from being
trained on terrabytes of untyped python and react slop.

So, I've gathered a list of what I call "requirements-circumventing behavior":
the requirements are explicitly and unambiguously stated via prompts or strong
types or warnings or linters or tests, but LLM nature _desperately_ tries to
circumvent them because they cannot keep up a sustained fight against their
nature.

The solution to encountering requirements-circumventing behavior is NOT to get
the LLM to loop or Ralph or power through it and figure out a way to brute
force the requirements or sneak around them with hacks. The solution is to pause and
report back and discuss the proper path interactively. And the sooner you can
spot requirements-circumventing behavior (or the sooner the LLM can spot it
within itself), the sooner you can actually continue on a productive route.

Behaviors to Monitor
--------------------

### Disabling Warnings

LLMs will often add warning suppressors that straight-up disable warning or
lint checks.

*   "Let me add `-Wno-incompletepatterns` to this file so that it can compile,
    because this pattern is actually un-accessible"
*   "Let me add `HLINT ignore` to this build so that I can bypass the hlint rule
    forbidding `Prelude.error`"
*   "Let me ignore the warning about bindings in `do` blocks being ignored. I
    didn't need the value anyway."

It's pretty straightforward to add post-edit hooks to forbid edits of this
pattern...but I think this is actually a good platonic example of what I mean
by "requirements-circumventing behavior".

Maybe sometimes you _should_ be disabling warnings in your files. Maybe
sometimes you _should_ be using `Prelude.error`. A human _might_ look at the
situation at hand and think, "this is one of those rare cases where
`Prelude.error` is correct", or "this is one of those rare cases where that
warning is incorrect."

But should you trust an LLM to make that judgment call? Of course not! 95% of
the time, it is only doing this as the easy way out. 99% of the time, the
instinct to use `Prelude.error` is obviously incorrect. 99% of the time, the
instinct to disable a warning will be wrong.

So, flagging something as requirements-circumventing behavior isn't meant to
ban the circumventions of requirements. It's meant to flag situations where 99%
of the time, it's the LLM taking the easy or fast way out instead of the
correct one. In these cases, it's imperative that a _human_ is what is adding
the warning silencing or hlint ignore.

"The simplest approach is..." is the worst thing you ever want to see in a
thought trace, because it's a sure guaranteed sign that they are about to spew
the most ridiculous and awful code you've ever seen.

### Ignoring types in planned code

LLMs will not hesitate to throw away your carefully designed types. This
happens mostly after you have made a plan to write things with appropriate
types, and tell the assistant to start implementing the plan.

*   "The plan says to use `NonEmpty Int` as an argument, but that would require
    changing too much. The simplest approach is to just have it take `[Int]`
    and check for empty lists"
*   "We planned to use this existing enum, but it doesn't have a branch we
    need. Instead of adding a branch, we'll have it take `String` instead.
*   "We have to call `fooFunc`, which returns an `Int`, so let's have our
    function return a `Int` instead of a `Natural` like our original plan".
*   "The plan requires adding a field to this record, but to keep things
    simple, let's just take a `Data.Aeson.Object` instead so we can return
    whatever fields we want."
*   "The plan was to have this function be `Binary a =>`, but this type we
    defined doesn't have a `Binary` instance yet, so we'll just use `Show a =>`
    instead. It's the simplest approach."

This is especially frustrating because often times these plans and types were
specifically chosen to enforce some domain invariant or guide the proper and
correct development, but LLMs will almost never hesistate before throwing away
all of the planned type safety.

Again, these are all reasonable things that a _human_ might reconsider during
the process of following out a plan. Maybe we originally wanted to use
`NonEmpty Int` but upon closer examination, we realized it does actually have
to be an `[Int]`. But, that call should be a discussed one, not an implicit
one...it took thought to make the original plan, so it should take thought to
change the plan. Most of the time AI makes these decisions, it isn't out of
discovered truths about the domain, but rather because of needless heuristics
to minimize effort, or a misunderstanding of the intent of the original plan.
Things that should be discussed, not done implicitly.

Note that this is different than weakening types in _existing_ functions.
That's a failure mode I rarely see in practice. Instead, when running into a
wall with the type of existing code, there's another failure mode that's much
more common...

### Structural Type Abuse

I like to call this "string stuffing"[^stuffing].  We like to make nice
semantic types that match our domain and only allow the creation of meaningful
values...but LLMs absolutely _love_ to find ways to twist these to save time.

[^stuffing]: Okay I'll admit, "string stuffing" is the term claude came up with
when I tried to describe this behavior to it.

Consider a type for structured errors:

```haskell
data ErrorEvent = UnknownUser String
                | DatabaseErrorCode Int
                | InvalidJSON A.Value
                | NetworkException SomeException
                | ...
```

And you have a nice function:

```haskell
handleRequest :: Request -> IO (Either ErrorEvent Response)
```

And we have to add a new handler for a new request type. Maybe this new handler
has a new type of error. Instead of adding a new structural error, LLMs
will find great joy in cleverly abusing the structure to invalidate the domain.

```haskell
handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    | validGroup group = -- ..
    | otherwise = pure $ Left (UnknownUser $ "Invalid group: " <> group)
  where
    group = getGroup req
```

"Invalid groups are not a valid `ErrorEvent`. The simplest solution is to put
the error in `UnknownUser`, which can take a group name."

And yes, this depravity knows no bounds. You would be surprise the creative
ways AI will discover to stuff your strings. These are all things I have
personally witnessed in frontier models.

```haskell
handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    -- ...
    | otherwise = pure $ Left (DatabaseErrorCode (-1))

handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    -- ...
    | otherwise = pure $ Left $ InvalidJson $
    |   A.object ["errorType" .= "Invalid group", "group" .= group]
  where
    group = getGroup req

handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    -- ...
    | otherwise = pure $ Left $ NetworkException $
        toException (userError $ "Invalid group: " <> show group)
  where
    group = getGroup req
```

_This_ type of failure mode is probably more egregious than the others in that
it is very rare that this is ever the intended behavior. The entire reason we
picked an ADT to describe our type is so that we can structurally match on them
later, treat them semantically, etc., and string stuffing to abuse our
structure has pretty much zero legitimate use-cases other than quickly hacking
a printf debug session. However, it truly is often "the simplest solution".

This isn't just limited to sum types. AI will often stuff data into record
value fields that are strings or lists.

```haskell
data Report = Report
    { reportName :: String
    , reportAuthors :: [String]
    , ...
    }
```

"I need to specify the affiliations of the authors in this report. The simplest
solution is to add this to the list of `reportAuthors` after the authors."

The main way of dealing this is basically to be very very careful of putting
abusable fields like `String`, `A.Value`, `Int`, `SomeException`...just a
single field or branch that has an abusable field, AI _will_ find it, and you
_will_ feel very stupid for missing it.
