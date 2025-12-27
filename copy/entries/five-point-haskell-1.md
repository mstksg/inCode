---
title: "\"Five Point Haskell\" Part 1: Total Depravity"
categories: Haskell
tags: functional programming, type safety
create-time: 2025/12/26 15:01:46
identifier: five-point-haskell-1
slug: five-point-haskell-part-1-total-depravity
series: five-point-haskell
---

Times have changed. The entire discipline of software development and
engineering is under question, and over the past years has come under multiple
movements re-thinking what it even means to write good, robust, correct code.
Multiple language and framework wars have been fought, the zeitgeist constantly
shifts. It is long overdue to clarify exactly the mindset on which we approach
and define "good" coding principles. This series sets to establish a five-point
unified framework of the "Typed Functional Programming" (and Haskell-derived)
programming philosophy aimed to create code that is maintainable, correct,
long-lasting, extensible, and beautiful to write and work with, in terms of
dispelling "heresies" (thought leader sound-bytes that have become all too
popular on Twitter) and clarifying the tried and true refutations and
guiding rules.

Let's jump right into point 1: Total Depravity.

The "Hero Programmer"
---------------------

Think about the stereotype of a "brilliant programmer" that an inexperienced
programmer has in their mind --- someone who can hold every detail of a complex
system in their head, every intricate connection and relationship between each
component. There's the classic [Monkey User Comic][focus] that valorizes this
ideal.

[focus]: https://www.monkeyuser.com/2018/focus/

![Monkey User --- Focus](/img/entries/five-point-haskell/79-focus.png "Monkey User --- Focus"){style="width:33%;height:auto;"}

The 10x developer is one who can carry the state and inter-connectedness
of an entire system in their brain, and the bigger the state they can carry,
the more 10x they are.

I've been programming long enough to know both that people like this do exist,
but also that it's...not exactly what makes a programmer great, or enables you
to write good code, even if we only consider solo projects (and of course, 100
times so if you are working on a team).

This idea is extremely prevalent in programming subculture --- before the
Monkey User comic in 2018, there's also the [Jason Heeris comic][heeris] in
2013!

[heeris]: http://heeris.id.au/2013/this-is-why-you-shouldnt-interrupt-a-programmer/

![Jason Heeris --- This Is Why You Shouldn't Interrupt a Programmer](/img/entries/five-point-haskell/programmerinterrupted.png "Jason Heeris --- This Is Why You Shouldn't Interrupt a Programmer"){style="width:33%;height:auto;"}

So, if you are a new programmer trying to soak in cultural values and shared
knowledge and ideals, it's natural to think: this is how things should be,
being a programmer is about mastering this art, this is a unique skill of the
distinguished developer, if I can master my mind palace I can become as cool as
Mr. Robot.

There has always been backlash to this of course. One comment of note is by
this user suspiciously named "\_skel" with the top comment on Hacker News:

> It's not reasonable to expect your coworkers to treat you like a monk who
> needs to spend long hours in uninterrupted deep thought and reflection.
>
> Avoiding the cognitive penalty of interruptions is a skill that every
> programmer should develop. It's a big win in terms of productivity, and it
> only becomes more important when you become more senior and need to
> collaborate more frequently with other teams or provide support to other
> developers. Collaboration is where true productivity comes from anyway,
> rather than raw coding skill (above a certain baseline of competence, of
> course.)
>
> (..)
>
> [\_skell, 2018](https://news.ycombinator.com/item?id=18042757)

The key isn't that we should be good at balancing these systems in our head:
the key is that our productivity _not_ be dependent on this fragile state!

We reject the heresy of the hero programmer! We reject that this is how things
must be, or that mastering this skill is the road to successful programs.

