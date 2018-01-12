---
title: "Interpreters a la Carte (Advent of Code 2017 Duet)"
categories: Haskell
series: Beginner/Intermediate Haskell Projects
tags: functional programming, haskell, types
create-time: 2018/01/11 16:28:19
date: none
identifier: interpreters
slug: interpreters-a-la-carte
---

This post is just a fun one exploring a wide range of techniques that I applied
to solve the Day 18 puzzles of this year's great [Advent of Code][aoc].  The
puzzles involved interpreting an assembly language on an abstract machine.  The
neat twist is that Part 1 gave you a description of one abstract machine, and
Part 2 gave you a *different* abstract machine to interpret the same language
in.  This twist (one language, but different interpreters/abstract machines) is
basically one of the textbook applications of the *interpreter pattern* in
Haskell and functional programming, so it was fun to implement my solution in
that pattern -- the assembly language source was "compiled" to an abstract data
type once, and the difference between Part 1 and Part 2 was just a different
choice of interpreter.

[aoc]: http://adventofcode.com/2017

Even more interesting is that the two machines are only "half different" --
there's one aspect of the virtual machines that are the same between the two
parts, and aspect that is different.  This means that we can apply the "data
types a la carte" technique in order to mix and match isolated components of
virtual machine interpreters, and re-use code whenever possible in assembling
our interpreters for our different machines!

The Puzzle
----------

The puzzle is [Advent of Code 2017 Day 18][day18]:

[day18]: http://adventofcode.com/2017/day/18

> You discover a tablet containing some strange assembly code labeled simply
> "Duet". Rather than bother the sound card with it, you decide to run the code
> yourself. Unfortunately, you don't see any documentation, so you're left to
> figure out what the instructions mean on your own.
>
> It seems like the assembly is meant to operate on a set of *registers* that are
> each named with a single letter and that can each hold a single integer. You
> suppose each register should start with a value of `0`.
>
> There aren't that many instructions, so it shouldn't be hard to figure
> out what they do. Here's what you determine:
>
> -   `snd X` *plays a sound* with a frequency equal to the value of `X`.
> -   `set X Y` *sets* register `X` to the value of `Y`.
> -   `add X Y` *increases* register `X` by the value of `Y`.
> -   `mul X Y` sets register `X` to the result of *multiplying* the value
>     contained in register `X` by the value of `Y`.
> -   `mod X Y` sets register `X` to the *remainder* of dividing the value
>     contained in register `X` by the value of `Y` (that is, it sets `X` to the
>     result of `X` modulo `Y`).
> -   `rcv X` *recovers* the frequency of the last sound played, but only when
>     the value of `X` is not zero. (If it is zero, the command does nothing.)
> -   `jgz X Y` *jumps* with an offset of the value of `Y`, but only if the value
>     of `X` is *greater than zero*. (An offset of `2` skips the next
>     instruction, an offset of `-1` jumps to the previous instruction, and so
>     on.)
>
> Many of the instructions can take either a register (a single letter) or a
> number. The value of a register is the integer it contains; the value of a
> number is that number.
>
> After each *jump* instruction, the program continues with the instruction to
> which the *jump* jumped. After any other instruction, the program continues
> with the next instruction. Continuing (or jumping) off either end of the
> program terminates it.

