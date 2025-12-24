---
title: Principle of Documentation as Liability
categories: Haskell
tags: functional programming
create-time: 2025/07/21 10:51:22
identifier: principle-of-documentation-as-liability
slug: principle-of-documentation-as-liability
---

One of the more nuanced daily decisions I make writing Haskell for my day job
is how "strong" to make the types in my APIs. Haskell is very good at giving
you multiple levels and gradations of type safety (as I wrote about last year
in [Seven levels of type safety in Haskell][levels]), so it seems Haskellers
are bit spoiled for choice. Of course, everyone agrees that you shouldn't have
`String -> String -> String -> ..` stringly-typed code or completely `Map
String Value` json argument maps, and also that you shouldn't go to the full
extreme of having your preconditions be so exact that the user ends up
re-implementing the entire function just to be able to call it. But in-between,
there is some nuance.

[levels]: https://blog.jle.im/entry/levels-of-type-safety-haskell-lists.html

Lately I've noticed I have been falling back to a certain principle/heuristic
that I have started calling the *"Principle of Documentation as Liability"*.
The basic ideas are:

1.  _All_ non-code documentation, though sometimes useful, is a liability and
    should be treated with suspicion.
2.  Pre-conditions that require the user to read documentation to know about
    them are bad usability. As an author you should assume the user
    hasn't read or doesn't want to read it.
    *   So, make your argument types as self-evident as reasonably possible.
    *   Breaking changes in the pre-conditions should require breaking changes
        in the types --- stricter conditions should break compilation until
        addressed.
3.  Post-conditions that only exist in documentation cannot be trusted to be
    up-to-date. As a consumer you should assume it is out of date and must be
    verified at runtime.
    *   So, make the structure of your result types as self-evident as
        reasonably possible.
    *   Breaking changes in the semantics of the results should require a
        change in the types --- looser conditions should break compilation
        until addressed.
4.  Reserve documentation for explanation of design decisions, high-level usage
    instructions, example usages, etc.

Documentation-Only Pre-Conditions are a Liability
-------------------------------------------------

Don't do it!

No Post-Conditions in Documentation
-----------------------------------

Just don't!

<!-- How to figure out how strict your types should be: -->

<!-- 1. changes in semantics of API should require changes in types -->
<!-- 2. user should be able to safely call your code without reading documentation -->

