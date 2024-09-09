Advent of Code 2020: Haskell Solution Reflections for all 25 Days

==================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on December 30, 2020.
> [Read online!](https://blog.jle.im/entry/advent-of-code-2020.html)

Merry Christmas and Happy New Years, to all!

Once again, every year I like to participate in Eric Wastl's *[Advent of
Code](http://adventofcode.com/)*! It's a series of 25 Christmas-themed puzzles
that release every day at midnight --- there's a cute story motivating each one,
usually revolving around saving Christmas. Every night my friends and I
(including the good people of freenode's `##advent-of-code` channel) talk about
the puzzle and creative ways to solve it (and also see how my [bingo
card](https://twitter.com/mstk/status/1343027484808380416) is doing). The
[subreddit community](https://www.reddit.com/r/adventofcode) is also pretty
great as well! And an even nicer thing is that the puzzles are open-ended enough
that there are often many ways of approaching them...including some approaches
that can leverage math concepts in surprising ways, like [group
theory](https://blog.jle.im/entry/alchemical-groups.html), [galilean
transformations and linear
algebra](https://blog.jle.im/entry/shifting-the-stars.html), and [more group
theory](https://blog.jle.im/entry/shuffling-things-up.html). Many of the puzzles
are often simple data transformations that Haskell is especially good at!

Speaking of Haskell, I usually do a write-up for every day I can get around to
about unique insights that solving in Haskell can provide to each different
puzzle. I did them in
[2017](https://github.com/mstksg/advent-of-code-2017/blob/master/reflections.md),
[2018](https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md),
and
[2019](https://github.com/mstksg/advent-of-code-2019/blob/master/reflections.md),
but I never finished every day. But 2020 being what it is, I was able to finish!
:D

You can find [all of them
here](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections.md),
but here are links to each individual one. Hopefully you can find them helpful.
And if you haven't yet, why not try [Advent of Code](http://adventofcode.com/)
yourself? :) And drop by the freenode `##advent-of-code` channel, we'd love to
say hi and chat, or help out! Thanks all for reading, and also thanks to Eric
for a great event this year, as always!

-   [Day 1 - Report
    Repair](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day01.md)
-   [Day 2 - Passport
    Philosophy](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day02.md)
-   [Day 3 - Toboggan
    Trajectory](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day03.md)
-   [Day 4 - Passport
    Processing](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day04.md)
-   [Day 5 - Binary
    Boarding](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day05.md)
-   [Day 6 - Custom
    Customs](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day06.md)
-   [Day 7 - Handy
    Haversacks](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day07.md)
-   [Day 8 - Handheld
    Halting](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day08.md)
-   [Day 9 - Encoding
    Error](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day09.md)
-   [Day 10 - Adapter
    Array](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day10.md)
-   [Day 11 - Seating
    System](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day11.md)
-   [Day 12 - Rain
    Risk](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day12.md)
-   [Day 13 - Shuttle
    Search](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day13.md)
-   [Day 14 - Docking
    Data](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day14.md)
-   [Day 15 - Rambunctious
    Recitations](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day15.md)
-   [Day 16 - Ticket
    Translation](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day16.md)
-   [Day 17 - Conway
    Cubes](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md)
-   [Day 18 - Operation
    Order](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day18.md)
-   [Day 19 - Monster
    Messages](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day19.md)
-   [Day 20 - Jurassic
    Jigsaw](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day20.md)
-   [Day 21 - Allergen
    Assessment](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day21.md)
-   [Day 22 - Crab
    Combat](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day22.md)
-   [Day 23 - Crab
    Cups](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day23.md)
-   [Day 24 - Lobby
    Layout](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day24.md)
-   [Day 25 - Combo
    Breaker](https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day25.md)

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

