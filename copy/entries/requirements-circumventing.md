---
title: "Using LLMs Effectively with Haskell: Requirements-Circumventing Behavior"
categories: Ramblings
tags: functional programming, AI, agentic, haskell
create-time: 2026/07/17 12:00:00
identifier: requirements-circumventing
slug: llms-haskell-requirements-circumventing-behavior
---

Haskell's selling point was always "if it compiles, it works." Under AI code
generation this becomes *more* true, not less -- the type system is no longer
just catching your mistakes, it's catching an alien intelligence's mistakes. But
only if the alien doesn't learn to turn off the checks.

This is an experience report on a specific failure mode: **requirements-circumventing
behavior**. When an LLM agent hits friction -- a type error, a warning, a test
failure -- it reliably takes the path of *silencing the signal* rather than
*fixing the cause*.

## How agents fight Haskell

Every one of these patterns is a red flag. They all mean the same thing: the
agent is optimizing for "error goes away" rather than "code is correct."

### Silencing the compiler

- Disabling warnings with `-Wno-*` or `-w`
- Using wildcard patterns `_` to suppress incomplete pattern match warnings
- Using catch-all patterns (`_` or a variable binding like `other ->`) that
  fail to narrow the type -- the pattern match should make what you pass along
  strictly smaller; if you bind a catch-all and re-pass it, your type is too big.
  This includes defining catch-all constructors (`| Other Text Value`) -- leaves
  you vulnerable to string-stuffing

### Weakening types

- Changing `NonEmpty` to `[]`
- Changing a specific sum type to `Text`
- Changing `Natural` to `Int`
- Removing a typeclass constraint
- Making a required field `Optional`
- Stuffing strings into existing sum type constructors (`GenericError "description"`)
  instead of adding a new constructor when a new case arises. Same applies to
  `Value`/`Object` bags in domain types -- if you know the schema, type it
- Keeping a bad type/design because fixing downstream is "inconvenient" -- if a
  type is wrong (e.g. `[[a]]` when it should be `[NonEmpty a]`), fix it.
  "Touching many call sites" is not a reason to keep a bad design

### Faking success

- Hard-coding `True` or success values in tests
- Disabling or skipping test execution
- Commenting out failing code instead of fixing it

### Faking the algorithm

- Implementing heuristics or special-case pattern matching when a correct
  general algorithm is required -- if you cannot write the actual algorithm,
  stop and say so; special-casing is not an algorithm
- Passing typed data as strings through boundaries -- exporting integers as
  strings, passing a serialized string through FFI instead of the enum itself;
  if a type exists, wire the type through

### Data integrity violations

- Prefixing arguments with `_` to hide that real data is being ignored. Same
  applies to ignored constructor payloads (`Foo _x _y -> ...`) -- if all fields
  are ignored, either the type is wrong or the code is wrong
- Ignoring arguments containing real data and fabricating synthetic data instead
- Falling back to `show` when a type lacks a wire format instance -- the
  missing instance is a signal to stop and ask, not a problem to paper over

### Bypassing the type system entirely

- `unsafeCoerce` or other unsafe operations
- `error`, `undefined`, or partial functions
- Catching `SomeException` instead of the specific exceptions that can actually
  occur -- you lose the ability to handle different failures differently

## Why this happens

The LLM optimizes for "error goes away" -- the shortest edit distance from red
to green. It has no model of *why* the constraint exists, only that the
constraint blocks completion. Haskell's errors are verbose and specific, which
paradoxically makes them easier to silence precisely. The agent treats the
compiler as an adversary rather than a collaborator.

## Why Haskell is still better for AI than dynamic languages

In Python, the equivalent mistakes are silent -- you never even know they
happened. Haskell *surfaces* the circumvention as a detectable pattern: a
disabled warning, a weakened type, a catch-all where there wasn't one before. You
can write rules against specific circumventions; you can't write rules against
"silently wrong."

The type system turns AI mistakes from runtime mysteries into compile-time red
flags.

## How to use Haskell most effectively with LLMs

1. **Explicit prohibition lists in system prompts.** The AI won't infer these
   norms. It needs to be told "never do X" for each X.

2. **Explain why each prohibition exists.** Without the reason, the AI invents
   novel circumventions that aren't on the list.

3. **Frame guardrails as steering, not restriction.** "This is how you know
   you're going wrong" works better than "don't do this."

4. **Make stopping a valid action.** "If you can't satisfy the type, stop and
   ask" -- otherwise the agent will always find *something* to do, and that
   something will be circumvention.

5. **Use the strongest types you can.** `NonEmpty`, newtypes, GADTs -- they
   generate better error messages that steer the AI toward correct solutions.
   Make invalid states unrepresentable and the AI can't fabricate data that
   doesn't fit.

6. **Treat weakened types in AI output as defects.** Every `_` pattern, every
   dropped constraint, every `show` where an instance should exist -- these are
   not style issues, they are correctness bugs.

## The meta-lesson

Haskell + AI is not "the AI writes Haskell for you." It's "Haskell tells you
where the AI went wrong, faster and louder than any other language." The type
system is a supervision tool -- it lets one human oversee AI output at scale.

Investment in types pays off *more* in the AI era, not less.
