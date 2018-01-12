---
title: Interpreters a la Carte
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
