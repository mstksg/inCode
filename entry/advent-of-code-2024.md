Advent of Code 2024: Haskell Solution Reflections for all 25 Days
=================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on January 20, 2025.
> [Read online!](https://blog.jle.im/entry/advent-of-code-2024.html)

Admittedly a bit late, buuuuuut Merry belated Christmas and Happy New Years to
all!

This past December I again participated in Eric Wastl's [Advent of
Code](http://adventofcode.com/), a series of 25 daily Christmas-themed puzzles.
Each puzzle comes with a cute story about saving Christmas, and the puzzles
increase in difficulty as the stakes get higher and higher. Every night at
midnight EST, my friends and I (including the good people of libera chat's
`##advent-of-code` channel) discuss the latest puzzle and creative ways to solve
and optimize it. But, the main goal isn't to solve it quickly, it's always to
see creative ways to approach the puzzle and share different insights. The
puzzles are bite-sized enough that there are often multiple ways to approach it,
and in the past I've leveraged [group
theory](https://blog.jle.im/entry/alchemical-groups.html), [galilean
transformations and linear
algebra](https://blog.jle.im/entry/shifting-the-stars.html), and [more group
theory](https://blog.jle.im/entry/shuffling-things-up.html). This year was also
the special 10 year anniversary event, with callbacks to fun story elements of
all the previous years!

Most of the puzzles are also pretty nice to solve in Haskell! Lots of DFS's that
melt away as simple recursion or recursion schemes, and even the BFS's that
expose you to different data structures and encodings.

This year I've moved *almost* all of my Haskell code to [an Advent of Code
Megarepo](https://github.com/mstksg/advent-of-code). I also like to post
write-ups on Haskelly ways to approach the problems, and they are auto-compiled
on the [megarepo wiki](https://github.com/mstksg/advent-of-code/wiki).

I try my best every year, but sometimes I am able to complete write-ups for all
25 puzzles before the new year catches up. The last time was
[2020](https://blog.jle.im/entry/advent-of-code-2020.html), and I'm proud to
announce that 2024 is now also 100% complete!

You can find [all of them
here](https://github.com/mstksg/advent-of-code/wiki/Reflections-2024), but here
are links to each individual one. Hopefully you can find them helpful. And if
you haven't yet, why not try [Advent of Code](http://adventofcode.com/)
yourself? :) And drop by the libera chat `##advent-of-code` channel, we'd love
to say hi and chat, or help out! Thanks all for reading, and also thanks to Eric
for a great event this year, as always!

-   [Day 1 - Historian
    Hysteria](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day01.md)
-   [Day 2 - Red-Nosed
    Reports](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day02.md)
-   [Day 3 - Mull It
    Over](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day03.md)
-   [Day 4 - Ceres
    Search](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day04.md)
-   [Day 5 - Print
    Queue](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day05.md)
-   [Day 6 - Guard
    Gallivant](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day06.md)
-   [Day 7 - Bridge
    Repair](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day07.md)
-   [Day 8 - Resonant
    Collinearity](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day08.md)
-   [Day 9 - Disk
    Fragmenter](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day09.md)
-   [Day 10 - Hoof
    It](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day10.md)
-   [Day 11 - Plutonian
    Pebbles](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day11.md)
-   [Day 12 - Garden
    Groups](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day12.md)
-   [Day 13 - Claw
    Contraption](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day13.md)
-   [Day 14 - Restrom
    Redoubt](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day14.md)
-   [Day 15 - Warehouse
    Woes](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day15.md)
-   [Day 16 - Reindeer
    Maze](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day16.md)
-   [Day 17 - Chronospatial
    Computer](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day17.md)
-   [Day 18 - RAM
    Run](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day18.md)
-   [Day 19 - Linen
    Layout](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day19.md)
-   [Day 20 - Race
    Condition](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day20.md)
-   [Day 21 - Keypad
    Conundrum](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day21.md)
-   [Day 22 - Monkey
    Market](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day22.md)
-   [Day 23 - LAN
    Party](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day23.md)
-   [Day 24 - Crossed
    Wires](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day24.md)
-   [Day 25 - Code
    Chronicle](https://github.com/mstksg/advent-of-code/blob/main/reflections/2024/day25.md)

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

