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

![Monkey User --- Focus](/img/entries/five-point-haskell/79-focus.png "Monkey User --- Focus"){style="width:50%;height:auto;"}

The 10x developer is one who can carry the state and inter-connectedness
of an entire system in their brain, and the bigger the state they can carry,
the more 10x they are.

I've been programming long enough to know both that people like this do exist,
but also that it's...not exactly what makes a programmer great, or enables you
to write good code, even if we only consider solo projects (and of course, 100
times so if you are working on a team).

<!-- This idea is extremely prevalent in programming subculture --- before the -->
<!-- Monkey User comic in 2018, there's also the [Jason Heeris comic][heeris] in -->
<!-- 2013! -->

<!-- [heeris]: http://heeris.id.au/2013/this-is-why-you-shouldnt-interrupt-a-programmer/ -->

<!-- ![Jason Heeris --- This Is Why You Shouldn't Interrupt a Programmer](/img/entries/five-point-haskell/programmerinterrupted.png "Jason Heeris --- This Is Why You Shouldn't Interrupt a Programmer"){style="width:50%;height:auto;"} -->

If you are a new programmer trying to soak in cultural values and shared
knowledge and ideals, it's natural to think: this is how things should be,
being a programmer is about mastering this art, this is a unique skill of the
distinguished developer, if I can master my mind palace I can become as cool as
Mr. Robot.

<!-- There has always been backlash to this of course. One comment of note is by -->
<!-- this user suspiciously named "\_skel" with the top comment on Hacker News: -->

<!-- > It's not reasonable to expect your coworkers to treat you like a monk who -->
<!-- > needs to spend long hours in uninterrupted deep thought and reflection. -->
<!-- > -->
<!-- > Avoiding the cognitive penalty of interruptions is a skill that every -->
<!-- > programmer should develop. It's a big win in terms of productivity, and it -->
<!-- > only becomes more important when you become more senior and need to -->
<!-- > collaborate more frequently with other teams or provide support to other -->
<!-- > developers. Collaboration is where true productivity comes from anyway, -->
<!-- > rather than raw coding skill (above a certain baseline of competence, of -->
<!-- > course.) -->
<!-- > -->
<!-- > (..) -->
<!-- > -->
<!-- > [\_skell, 2018](https://news.ycombinator.com/item?id=18042757) -->

<!-- The key isn't that we should be good at balancing these systems in our head: -->
<!-- the key is that our productivity _not_ be dependent on this fragile state! -->

We see this mindset happening at all levels. C++ programmers often deride rust
programmers: memory mismanagement is a skill issue, we should be teaching
people to program without memory bugs. C programmers deride C++ programmers in
the same way, and assembly programmers deride C programmers all the same.  Why
use tools for "safety" or "abstractions" if you can just get good enough that
you don't need them? They are simply a crutch for actually learning how to
program, right? Being a good programmer is about getting to the point where you
just...don't write bad code!

This is the myth of the hero programmer: as long as we keep everything in our
head, and load all of the context into our mental window, we can be
unstoppable. Did you have a bug? Well, you just need to upgrade your mental
awareness and your context window. You just need to be better and better at
keeping more in your mind. You just have to be _perfect_.

The problem, however, is that a big part of what you are struggling to keep in
your mind are _inconsequential minutia_ unrelated to the actual real,
high-level problems you are supposed to be solving. The more of your mental
context window is wasted on minutia like memory management and type safety, the
less you have to actually think about the _real_ high-level problems of the
code you are trying to write.  The more you can outsource, the more you can
care about what really matters.

We reject the heresy of the hero programmer! We acknowledge that the inevitable
conclusion of the "get good" mental process is achieving a perfection that is
not realistic or possible.

The declare the doctrine of **Total Depravity**

> Total Depravity: No programmer can keep the entire state of the program in
> their head, account for the nuances of every abstraction, every assumption,
> every life-cycle, every pre- and post-condition, every implicit requirement
> and interdependency. This only works if if we are perfect programmers, but
> expecting this unachievable state can only ever backfire. Instead, the more
> minutia we can outsource to tooling, the more we can focus on the problems
> that we actually care about.
