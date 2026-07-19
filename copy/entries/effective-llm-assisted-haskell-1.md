---
title: "Effective LLM-Assisted Haskell: Understanding Requirements-Circumventing Behavior"
categories: Haskell
tags: functional programming, agentic, haskell
create-time: 2026/07/18 14:05:05
identifier: effective-llm-assisted-haskell-1.md
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

But, after some recent community rumblings and fragmentation regarding
high-stakes existential tensions between agentic LLM coding and the future
direction of Haskell, I realized I could probably lend a constructive hand by
describing how I, personally, a Haskeller of 15+ years, (7+ professional)
personally make the most out of them.

Note: none of the views espoused in this post are endorsed by my employer. They
are my own and will most likely change drastically as I learn to use these
tools better over time, and, as presented, are uniquely formed around my
background and professional skill set and most likely not _directly_ applicable
to any arbitrary human. However, I do feel there are people who may appreciate
my personal experience report.

This post will focus on what I call "requirements-circumventing behavior" in
LLMs as it relates to effective Haskell.

Ground Rules
------------

First let me clarify at what level I integrate LLM's at.

For any projects of importance:

- Everything I commit I treat with as much standards line-by-line as any code I
  write.
* All design decisions and concepts and code structures are written in the
  style and care as if I had written it by hand.
* No line of code is "less scrutinized" or "abstracted away" because it was
  LLM-generated, nor is it less important to understand on the line-by-line
  level.
* Anyone who reviews any line of code I commit can trust on my name and
  reputation that it was written with as much thought and concern as any other
  line.
* Absolutely no prose (comments, documentation, PR bodies) authored by LLMs.
  The responsibility is on the human.

In a sense, I treat agentic LLMs as a glorified multi-file autocomplete. I am
not trusting it on any concepts it introduces, because 90% of the time I know
the concept already and probably know it better, or know of a better way to
approach or structure the problem. That's just how it is, empirically today, in
2026, for me personally.

My "fully vibed" code I strictly keep out of source control for repositories of
importance; they are kept in different repositories and appropriately version
controlled and recorded and noted with the circumstances in which they were
written. I have found the most mileage in these as simple test scripts ("write
an executable to test my library out on sample inputs", "grab the spectrogram
trace from the read-only API of my signal analyzer", "aggregate these results
into a quantitative notebook for me to reference") that are otherwise tedious
to write or involve dealing with throwaway APIs with low ROI in learning. In
these I also presume LLM comments are "load-bearing" for their effective
vibe-maintenance but what do I know.

Most of the thoughts in this post will be about the former category (projects
of importance), although sometimes my vibed scripts eventually transition into
code bases I treat with the same level of respect, where these strategies will
also apply.

Haskell and LLMs
----------------

Now my personal opinion and wish: in an ideal world, Haskell _should_ in
agentic software engineering as what LEAN is in agentic research mathematics: a
framework for LLMs to self-construct the scaffolding they need to guide
themselves to their correct goal.

Contrary to others, I do _not_ believe that "correctness at generation-time" is
a plausible goal, not today in 2026, and probably not ever. The motion towards
correctness is asymptotic. You might get close enough for projects, but the
long tail of correctness is...long. And demands respect. That's why even the
most full-vibed frontier-model projects have [large suites of tests][bun] that
exist to guide the agent. My bet is that agentic coding in five years will not
be "spit out the correct program in one batch of text", it will be "set up the
best scaffolding that guides the implementation".

[bun]: https://bun.com/blog/bun-in-rust

