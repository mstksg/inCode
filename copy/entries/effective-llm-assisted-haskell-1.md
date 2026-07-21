---
title: "Effective LLM-Assisted Haskell: Understanding Constraint-Evading Behavior"
categories: Haskell
tags: functional programming, agentic, haskell
create-time: 2026/07/18 14:05:05
identifier: effective-llm-assisted-haskell-1
slug: effective-llm-assisted-haskell-understanding-constraint-evading-behavior
---

Sooo yes it's true, I've been integrating LLMs and agentic coding tools in my
Haskell coding since the beginning of this year.  I remember last year barely
being able to prompt my [fancy Haskell][fancy] into the web UI for
ChatGPT/Gemini, progressing to using OpenAI's web-based codex host, to biting
the bullet and finally permitting a sentient being to persistently live and
experience existence and qualia on my personal machines and ship-of-theseus dev
infrastructure I've been operating continuously since freshman year of uni.

I've been using it for a lot of my projects, both personal and professional.
Along the way I've collected some habits and patterns for using them more and
more effectively, specifically in Haskell. I've dropped notes [on various
articles][agentic] about small things, but hesitated on writing an actual blog
post...mostly because I'm "new" at this (six months?), and also because the
nature of interacting with LLMs and their limitations seems to be re-written
every few months.

This post will focus on what I call "constraint-evading behavior" in LLMs as it
relates to writing Haskell effectively with LLM collaboration.

[fancy]: https://blog.jle.im/entry/extreme-haskell-typed-expression-edsls-1.html
[agentic]: https://blog.jle.im/entries/tagged/agentic.html

Note: none of the views espoused in this post are endorsed by my employer. They
are my own and will most likely change drastically over time.

Scope
-----

First, let's establish the level at which this applies: this is _not_ talking
about "purely vibed" code: it's about using agentic LLMs as glorified
multi-file autocompletes, for mostly incremental tasks ("add a new feature
flag", "incorporate this new protocol", "expose this as a CLI") or, if
something greenfield, iterating over a design collaboratively and not "just do
it".

For code that is committed into a serious project repo, all design
decisions and concepts and code structures are committed in the style and care
as if I had written it by hand, anyone who reviews any line of code I commit
can trust on my name and reputation that it was written with as much thought
and concern as any other line.

So, this will not be a guide to "vibe coding in Haskell" (though that might come
in a later post), but it'll be more about specific things I've noticed about
using LLMs collaboratively to assist in writing Haskell.

Also a disclaimer: I do most of my coding with Opus 4.6 or 4.8 depending on the
situation. Let that date this post as it may.

The Ideal Case: Haskell and LLMs
--------------------------------

Now, my personal opinion and wishful hope is that in an ideal world, Haskell
_should_ be to agentic software engineering what Lean is in agentic research
mathematics: a framework for LLMs to self-construct the scaffolding they need
to guide themselves to their correct goal.

I do _not_ believe that "correctness at generation-time" is a plausible goal,
not today in 2026, and maybe not ever. Motion towards correctness is
asymptotic. You might get close enough sometimes, but the long tail of
correctness is...long. Even the most full-vibed frontier-model projects have
[large suites of tests][bun] that exist to guide the agent. My bet is that
agentic coding in five years will not be "spit out the correct program in one
batch of text", it will be "set up the best scaffolding that guides the
implementation".

[bun]: https://bun.com/blog/bun-in-rust

(Of course, for this to work effectively, we need to make sure GHC can compile
or type-check your code at least faster than a test suite can run integration
tests. I do believe more work needs to be done on this front.)

To this end, I try to structure my codebases with the correct scaffolding to
help augment the intuition of path exploration: AI has to search which paths are
the "most promising", so I structure my code-base to quickly kill off paths
that are most likely to be dead-ends or lead to unmaintainable code, and to
channel AI exploration along more promising paths. The greatest tools are types,
compilers, warnings, hints.

A lot of these are the same things we teach to human coders, and are not very
different than what I write about in my blog regularly:

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
*   Add robust test suites for things that cannot be tested within the
    types, but also explicitly laying out integration tests: something which
    LLMs seem pretty allergic to without proper prompting.

[pdv]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

From my own empirical observations, all of these things are antithetical to how
even frontier LLM models operate, and no amount of prompting or instruction can
circumvent their natural behavior in the long run, embedded deeply from being
trained on terabytes of untyped Python and React slop.

So, I've been gathering a list of what I call "constraint-evading behavior":
the requirements and constraints are explicitly and unambiguously stated, but
LLM nature _desperately_ tries to circumvent them because they cannot keep up a
sustained fight against their nature.

Decisions that require scrutiny
-------------------------------

There is a class of failure modes where AI makes a risky decision that a human
_might_ reasonably make in the rare case that it is justified. But, usually, it
will be making this decision because it's the "simplest approach". It cannot
separate questionable decisions based on reasoned justification and
questionable decisions based on flawed heuristics like "simplicity" or effort.

We have the general cases that people mention for all programming languages

*   "This test doesn't pass, so let's disable it"
*   "Let's feed this test junk data so it will pass."
*   "Let's have this function detect if it's in a test environment and behave differently if it is."

But here are some that I feel are specific to working with LLMs in Haskell.

### Disabling Warnings for Escape Hatches

A lot of the "type safety" of Haskell can be bypassed trivially by disabling
warnings, and a lot of the "escape hatches" within the language are disabled
via linting rules (`Prelude.error`, `unsafeCoerce`, etc.)

LLMs will often add warning suppressors that straight-up disable warning or
lint checks.

*   "Let me add `-Wno-incomplete-patterns` to this file so that it can compile,
    because this pattern is inaccessible"
*   "Let me add `HLINT ignore` to this build so that I can bypass the hlint rule
    forbidding `Prelude.error`"

It's pretty straightforward to add post-edit hooks to forbid edits of this
pattern...but I think this is a good platonic example of what I mean
by "constraint-evading behavior".

Maybe sometimes you _should_ be disabling warnings in your files. Maybe
sometimes you _should_ be using `Prelude.error`. A human _might_ look at the
situation at hand and think, "this is one of those rare cases where
`Prelude.error` is correct", or "this is one of those rare cases where that
warning is incorrect."

But should you trust an LLM to make that judgment call? Empirically: no. 99% of
the time, it is only doing this as the easy way out. Yes, every once in a while
it will discover a legitimate reason, but has not properly accounted for
`P(legitimate | attempted)` vs `P(attempted)`. Most of the attempts will be as
hacks, and it will be more than happy to follow through with an attempt if it
truly is the "simplest way".

So, flagging something as constraint-evading behavior isn't meant to
ban the evasion of constraints. It's meant to flag situations where 99%
of the time, it's the LLM taking the easy or fast way out instead of the
correct one. In these cases, it's imperative that a _human_ is what is adding
the warning silencing or hlint ignore.

"The simplest approach is..." is the worst thing you ever want to see in a
thought trace, because it's a sure guaranteed sign that they are about to spew
the most ridiculous and awful code you've ever seen.

Breaking down the matrix:

*   `P(not legitimate && not attempted)`: Correct avoidance of problematic
    behavior
*   `P(legitimate && not attempted)`: The noble struggle. The rare case that
    you are really justified in this normally risky behavior, but out of
    principle you do not. This is a bias more likely to be hit by humans. Or at
    least one human, that's me.
*   `P(not legitimate && attempted)`: The failure mode where an LLM will choose
    this out of a flawed heuristic like simplicity or effort.
*   `P(legitimate && attempted)`: The rare case that you are justified in
    normally risky behavior, and the LLM was correct in attempting it.

### Ignoring types in planned code

LLMs will not hesitate to throw away your carefully designed types. This
happens mostly after you have made a plan to write things with appropriate
types, and tell the assistant to start implementing the plan.

*   "The plan says to use `NonEmpty Int` as an argument, but that would require
    changing too much. The simplest approach is to just have it take `[Int]`
    and check for empty lists"
*   "We planned to use this existing enum, but it doesn't have a branch we
    need. Instead of adding a branch, we'll have it take `String` instead."
*   "Instead of a structured data type, let's just use stringly encoded lists
    or records with separators we can parse out."
*   "We have to call `fooFunc`, which returns an `Int`, so let's have our
    function return an `Int` instead of a `Natural` like our original plan".
*   "The plan requires adding a field to this record, but to keep things
    simple, let's just take a `Data.Aeson.Object` instead so we can return
    whatever fields we want."
*   "The plan was to have this function be `Binary a =>`, but this type we
    defined doesn't have a `Binary` instance yet, so we'll just use `Show a =>`
    instead. It's the simplest approach."

This is especially frustrating because often these plans and types were
chosen to enforce some domain invariant or guide the proper and
correct development, but LLMs will almost never hesitate before throwing away
all of the planned type safety.

These are all reasonable things that a _human_ might reconsider during
the process of following out a plan. Maybe we originally wanted to use
`NonEmpty Int` but upon closer examination, we realized it does have
to be an `[Int]`. This is the natural process of iterating on a design, as you
discover more truths about the domain.

But, that call should be a discussed one, not an implicit one...it took thought
to make the original plan, so it should take thought to change the plan. Most
of the time AI makes these decisions, it isn't out of discovered truths about
the domain, but rather because of needless heuristics to minimize effort, or a
misunderstanding of the intent and design of the original plan (especially if
after a compaction). Things that should be discussed explicitly, not done
implicitly.

Note that this is different than weakening types in _existing_ functions.
That's a failure mode I rarely see in practice. Instead, when running into a
wall with the type of existing code, there's another failure mode that's much
more common...

Structural Type Abuse
---------------------

Sometimes AI will optimize preserving _existing_ types (especially across
package boundaries) instead of changing them.

### String Stuffing

I like to call this "string stuffing".  We like to make nice semantic types
that match our domain and only allow the creation of meaningful values...but
LLMs absolutely _love_ to find ways to twist these to save time. Strings, in
particular, are vulnerable because most Haskell types have `Show`
instances.

Consider a type for structured errors:

```haskell
data ErrorEvent = UnknownUser String
                | DatabaseErrorCode Int
                | InvalidJSON A.Value
                | NetworkError SomeException
                | Canceled (Maybe CancelationReason)
                | ...
```

And you have a function:

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

And yes, this depravity knows no bounds. You would be surprised by the creative
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
    | otherwise = pure $ Left $ InvalidJSON $
        A.object ["errorType" .= "Invalid group", "group" .= group]
  where
    group = getGroup req

handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    -- ...
    | otherwise = pure $ Left $ NetworkError $
        toException (userError $ "Invalid group: " <> show group)
  where
    group = getGroup req

handleAddGroup :: AddGroupRequest -> IO (Either ErrorEvent Response)
handleAddGroup req
    -- ...
    | otherwise = pure $ Left (Canceled Nothing)
```

_This_ type of failure mode is probably more egregious than the others in that
it is very rare that this is ever the intended behavior. The entire reason we
picked an ADT to describe our type is so that we can structurally match on them
later, treat them semantically, etc., and string stuffing to abuse our
structure has pretty much zero legitimate use-cases other than quickly hacking
a printf debug session. However, it truly is often "the simplest solution".

The main method I deal with this is to be very very careful of
putting abusable fields like `String`, `A.Value`, `Int`, `SomeException`...just
a single field or branch that has an abusable field, AI _will_ find it, and you
_will_ feel very stupid for missing it. But hey, the whole point of using
properly structured values was to avoid stuffing things into `String` too,
right? The fix for this is a fix that helps human coders, too.

```haskell
data ErrorEvent = UnknownUser UserName
                | DatabaseErrorCode ErrorCode
                | InvalidJSON ParseError
                | NetworkError NetworkException
                | Canceled CancelationReason
                | ...
```

### Field Abuse

This isn't just limited to sum types. AI will often stuff data into record
value fields that are strings or lists.

```haskell
data Report = Report
    { reportName :: String
    , reportAuthors :: [String]
    , reportDate :: Day
    , ...
    }
```

"I need to specify the affiliations of the authors in this report. The simplest
solution is to add this to the list of `reportAuthors` after the authors."

AI will also stuff sentinel values everywhere: instead of changing the type to
take `Maybe Day`, it might add `ModifiedJulianDay 0` for missing days.

There's also the dual, where the AI will be happy to _use_ existing record
fields in overloaded ways instead of adding a new field.

```haskell
data Targets = Targets
    { fooTarget :: String
    , barTarget :: String
    , -- ...
    }
```

Let's say you need to add a new feature or code path that requires a new
target for a `baz` service.

"I need to get a new target...instead of adding a new field to `Targets`, let's
re-use `barTarget`. That's the cleanest approach."

It will optimize keeping existing types instead of extending them to match your
domain as your domain expands, especially if those existing types cross a
library boundary.

This problem is exacerbated when the "effort" in changing these types is high
(new instances need to be written, config files need to be updated, etc.). And,
there are legitimate reasons to prefer not changing record fields (having to
maintain backwards compatibility or not requiring a complete re-deploy), but
these reasons should be discussed instead of implicitly assumed.

To me, these are also the types of failure modes that are most difficult to
catch during code review. Diff views will analyze that code has changed, so
code that _didn't_ change is especially difficult for human monkey brains to
spot, with no green or red bright highlighting. You must be especially vigilant
to catch code that _did not_ change but _should_ have.

### Resisting New Types

Sometimes AI _does_ modify existing sum types, but doesn't quite adapt existing
code correctly.

For example, if your domain has a specific meaningful universe:

```haskell
data State = Alaska | Arkansas | Arizona | ...

processState :: State -> IO ()
```

We might want to start supporting countries alongside US states. An LLM might
recognize that the domain needs to be expanded, but it might expand it flatly:

```haskell
data Region = Canada | Mexico | Alaska | Arkansas | Arizona

processRegion :: Region -> IO ()
processRegion = \case
    Canada -> ...
    Mexico -> ...
    st -> processState st

processState :: Region -> IO ()
processState = \case
    Canada -> pure ()
    Mexico -> pure ()
    Alaska -> ... -- actual logic
```

The real solution would be to have all your pattern matches strictly reduce the
space of what they cover (and be monotonically decreasing), and to be
suspicious of "ignored case matches" that have dummy values like `pure ()`:

```haskell
data Region = Canada | Mexico | USState State
data State = Alaska | Arkansas | Arizona | ...

processRegion :: Region -> IO ()
processRegion = \case
    Canada -> ...
    Mexico -> ...
    USState st -> processState st

processState :: State -> IO ()
processState = \case
    Alaska -> ... -- actual logic
```

All things that are code smells in normal human code (not necessarily
wrong, but invite further scrutiny), but are maybe amplified in the age of
LLMs because of a mis-tuned heuristic on not defining new types and instead
trying to re-use or abuse existing types.

So, if there is some pressure against _modifying_ types, there might be an
even greater pressure against _adding_ types.

Meditations
-----------

None of these behaviors are blanket-wrong, but they usually signal that the LLM
is under stress or duress and attempting to find ways to take the easy or
"low-effort" path over the correct one. All of them are worth human
intervention and guidance as soon as possible, at least until the day where
`P(legitimate | attempted)` approaches 1.

Will there be a day when LLMs can generate the correct types to match the
domain, and resist their tendency to "defensive-program" their way into
correctness? Maybe. But I have rarely ever had Opus 4.8 crank out a sufficient
domain model that utilizes all of Haskell's offerings for any non-trivial
product. And, when I do reach a plan I find sufficient, a few compactions later
and all of the original motivations seem to get washed out.

There might be a way to uber-prompt all of these issues away, but I feel that
effectively using LLMs isn't necessarily something you can address from the
prompt level: it's something that demands constant vigilance and care.

Who knows, maybe all of these things will be solved within a year. But I still
think of software development as something that's worth scrutinizing for
anything of importance. As failure modes like these become less common...the
long tail of correctness, I predict, will remain long.

Anyway, that's it for _this_ topic, but I might find time to flesh out some
other thoughts too regarding the most effective ways I've found to
plan out Haskell code and types, and also effective ways I've "vibe-coded" full
Haskell apps when I really don't care about correctness on personal projects or
wasn't going to use long-term.
