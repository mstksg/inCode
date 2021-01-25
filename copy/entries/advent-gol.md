---
title: "Degenerate Hyper-Dimensional Game of Life: Pushing Advent of Code to its Limits"
categories: Haskell
tags: advent of code, math, haskell, cellular automata
create-time: 2021/01/07 21:32:49
series: Advent of Code
identifier: advent-gol
slug: degenerate-hyper-dimensional-game-of-life
script: https://cdn.jsdelivr.net/npm/d3@6.3.1, /purescript/gol.js, https://cdn.jsdelivr.net/npm/d3-simple-slider@1.7.0
---

tldr: By exploiting multiple mathematical properties of a "degenerate"
hyper-dimensional game of life, each discovered one-by-one over the course of a
month, we were able to go from "10 dimensions may just barely be possible" to
"10 dimensions in 100ms, 50 dimensions tackled."

This is a story about breaking the degenerate hyper-dimensional game of life!
Let's travel back in time to the night before December 17, 2020: The release of
["Conway Cubes"][puzzle], day 17 of the "Advent of Code" of fun
little coding puzzles building up to Christmas.  One part I've always found
especially fun is that, because the problems are so self-contained and tidy,
they are often *open-ended* in the interesting ways you can solve them or
expand them.

[puzzle]: https://adventofcode.com/2020/day/17

On the surface, it seems to essentially be a straightforward expansion of
[Conway's Game Of Life][life].  GoL is a simulation played out on an infinite 2d
grid, where certain cells are "on" and "off", and at each step of the
simulation, the on/off cells spread and propagate in fascinating ways.

[life]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

The twist of the Advent of Code puzzle is it asks what would happen if we
played out the rules of GoL in 3d, and then 4d!  The "starting conditions" are
a 8x8 2D grid that every puzzle solver receives their own one of, and to solve
the puzzle you must simulate six steps of GoL for your personal input and count
the number of live cells at the end of it.  For example, my xy starting
conditions were

```
#####..#
#..###.#
###.....
.#.#.#..
##.#..#.
######..
.##..###
###.####
```

After 6 steps for 4-dimensional game of life, I had 2620 total points.
I submitted my answer with a direct implementation (scoring the 66th spot on
the leader board)...and that was that.

Simple enough, right?  Afterwards, when discussing with some friends, we
started talking about the trade-offs of different implementations and realized
that dimensionality was no joke...as you upped the number of dimensions, the
number of points you have to consider grow as $O((2t+8)^d)$, and the number of
neighbors of each point to check grows as $O(3^d)$... I took my solution for
d=4 and could *barely* churn out d=6 (in three minutes), since even at d=6 you
have to consider 728 neighbors for (potentially) each of the 64,000,000 points.
I had a dream that t=6, d=10 just *might* be possible...but clearly not without
some major breakthrough.  After all, at d=10, you'd need to check 59,048 neighbors for
potentially each of 10,240,000,000,000 points.

And...a breakthrough soon came. Someone brought up that if we look at the 3d
version, we see there's actually a *mirror symmetry*!  That is, because
everything starts off on the xy plane, with z=0 and w=0, the resulting
progression must be symmetrical on both sides.

![d=3 animation by [u/ZuBsPaCe][]](/img/entries/advent-gol/life3d.gif "d=3 animation u/ZuBsPaCe")

[u/ZuBsPaCe]: https://www.reddit.com/r/adventofcode/comments/kfa3nr/2020_day_17_godot_cubes_i_think_i_went_a_bit_too/

This means that we only have to simulate *half* of the points (for each extra
dimension) to get the answer, *halving* the number of points for d=3, saving a
factor of 4 for d=4, saving a factor of 8 for d=5, etc. ($O(2^{d-2})$).  After
implementing this optimization, I was able to run d=7 in under *six minutes*
(and d=8 in 75 minutes).

What's more is that this made us realize that there were potentially more
breakthroughs we could get by exploiting the fact that we are only given a 2d
slice...we didn't know what those could be yet, but suddenly, d=10 seemed
attainable...maybe?

And such a "maybe" (as posed in [this reddit thread I started][reddit]) turned
into a month-long quest of breakthrough after breakthrough, exploting different
aspects of this 2d degeneracy!  It was a long, harrowing journey full of sudden
twists and turns and bursts of excitement when new innovations came.  And in
the end, the hopeful question "What if d=10 was possible?" turned into "d=10 in
100ms, d=40 in eight minutes."

[reddit]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/

So, let's take a deep dive --- deeper than you probably ever expected to dive
into any particular degenerate starting conditions of a hyper-dimensional game
of life :D

The Baseline
------------

So let's start at the very beginning: how do you solve everything without
anything too fancy?

I discuss my basic Haskell method in [this write-up][basic]

[basic]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md

```haskell
-- | Given a neighbor set
neighbsSet :: Vector Int -> Set (Vector Int)
neighbsSet = S.fromList . tail . traverse (\x -> [x,x-1,x+1])

neighborMap :: Set (V3 Int) -> Map (V3 Int) Int
neighborMap ps = M.unionsWith (+)
    [ M.fromSet (const 1) (neighbsSet p)
    | p <- S.toList ps
    ]

stepper
    :: Set (V3 Int)
    -> Set (V3 Int)
stepper ps = stayAlive <> comeAlive
  where
    neighborCounts = neighborMap ps
    stayAlive = M.keysSet . M.filter (\n -> n == 2 || n == 3) $
                  neighborCounts `M.restrictKeys` ps
    comeAlive = M.keysSet . M.filter (== 3) $
                  neighborCounts `M.withoutKeys`  ps
```

::::: {#gol1}
hi
:::::


```
sim642  I wanted to ask this before but forgot: did anyone try to take advantage of the symmetry, e.g. in z axis in part 1?
sim642  Should halve the amount of calculations you have to do
sim642  Only some extra work at the end to differentiate z=0 and z>0 positions to know which to count twice
sim642  And in part 2 I feel like you could also exploit the symmetry in w axis simultaneously
```
