Advent of Code 2025: Haskell Solution Reflections for all 12 Days
=================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on December 24, 2025.
> [Read online!](https://blog.jle.im/entry/advent-of-code-2025.html)

Merry Christmas all! This is my annual [Advent of
Code](http://adventofcode.com/) post! Advent of Code is a series of (this year)
12 daily Christmas-themed programming puzzles that are meant to be fun
diversions from your daily life, help you find a bit of whimsy in your world,
give you a chance to explore new ideas and program together with your friends. I
always enjoy discussing creative ways to solve these puzzles every day, and it's
become a bit of an annual highlight for me and a lot of others. My favorite part
about these puzzles is that they are open ended enough that there are usually
many different interesting ways to solve them --- it's not like a stressful
interview question where you have to recite the obscure incantation to pass the
test. In the past I've leveraged [group
theory](https://blog.jle.im/entry/alchemical-groups.html), [galilean
transformations and linear
algebra](https://blog.jle.im/entry/shifting-the-stars.html), and [more group
theory](https://blog.jle.im/entry/shuffling-things-up.html).

Haskell is especially fun for these because if you set up your abstractions in
just the right way, the puzzles seem to solve themselves. It's a good
opportunity every year to get exposed to different parts of the Haskell
ecosystem! Last year, I moved almost all of my Haskell code to [an Advent of
Code Megarepo](https://github.com/mstksg/advent-of-code), and I also write up my
favorite ways to solve each one in the [megarepo
wiki](https://github.com/mstksg/advent-of-code/wiki).

All of this year's 12 puzzles [are
here](https://github.com/mstksg/advent-of-code/wiki/Reflections-2025), but I've
also included links to each individual one in this post. I'm also considering
expanding some of these into full on blog posts, so be on the look out, or let
me know if there are any that you might want fully expanded! And if you haven't,
why not try these out yourself? Be sure to drop by the libera-chat
`##advent-of-code` channel to discuss any fun ways you solve them, or any
questions! Thanks again to Eric for a great new fresh take on the event this
year!

-   [Day 1 - Secret
    Entrance](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day01.md)
    --- The classic Day 1 `scanl`
-   [Day 2 - Gift
    Shop](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day02.md)
    --- The super efficient `IntSet`
-   [Day 3 -
    Lobby](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day03.md)
    --- `StateT` + `[]` = backtracking search monad
-   [Day 4 - Printing
    Department](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day04.md)
    --- 2D cellular automata
-   [Day 5 -
    Cafeteria](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day05.md)
    --- The power of the `data-interval` library
-   [Day 6 - Trash
    Compactor](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day06.md)
    --- `Data.List` manipulations
-   [Day 7 -
    Laboratories](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day07.md)
    --- Tying the knot
-   [Day 8 -
    Playground](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day08.md)
    --- Iterative Clustering
-   [Day 9 - Movie
    Theater](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day09.md)
    --- `IntervalSet` and `IntervalMap`
-   [Day 10 -
    Factory](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day10.md)
    --- Guassian Elimation, Type-safe Mutable Vectors
-   [Day 11 -
    Reactor](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day11.md)
    --- More Knot Tying and DP!
-   [Day 12 - Christmas Tree
    Farm](https://github.com/mstksg/advent-of-code/blob/main/reflections/2025/day12.md)
    --- Counting

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

