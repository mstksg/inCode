---
title: "Degenerate Hyper-Dimensional Game of Life"
categories: Haskell
tags: advent of code, math, haskell, cellular automata
create-time: 2021/01/07 21:32:49
series: Advent of Code
identifier: advent-gol
slug: degenerate-hyper-dimensional-game-of-life
script: https://cdn.jsdelivr.net/npm/d3@6.5.0, /purescript/gol.js, https://cdn.jsdelivr.net/npm/d3-simple-slider@1.10.3
---

tldr: Over the course of a month, we were able to successive new mathematical
properties of a "degenerate" hyper-dimensional game of life" to take a "10
dimensions may just barely be possible on a supercomputer" to "10 dimensions is
easy enough to be run on any modern browser, and 40 dimensions can be reached
with a compiled language".  Includes interactive visualizations and
simulations!

This is a story about breaking the degenerate hyper-dimensional game of life by
exploratory visualizations and math!  Let's travel back in time: t'was the
night before December 17, 2020, The release of ["Conway Cubes"][puzzle], day 17
of the "Advent of Code" (fun little coding puzzles building up to Christmas).
One part about Advent of Code I've always found especially fun is that, because
the problems are so self-contained and tidy, they are often *open-ended* in the
interesting ways you can solve them or expand them.

[puzzle]: https://adventofcode.com/2020/day/17

On the surface, Day 17 seemed to essentially be a straightforward extension of
[Conway's Game Of Life][life] ("GoL").  GoL is a simulation played out on a 2d
grid, where cells are "on" and "off", and at each step of the simulation, the
on/off cells spread and propagate in fascinating ways based on the state of
their neighbors.

[life]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life

The twist of the Advent of Code puzzle is it asks what would happen if we
played out the rules of GoL in 3d, and then 4d!  The "starting conditions" are
a 8x8 2D grid picked out for each participant, and the puzzle solution is the
number of live cells after six steps.  My personal starting conditions were:

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

I submitted my answer with a direct implementation (scoring the 66th spot on
the leader board for that day)...and that was that for the "competitive" part.
But the real fun always starts after!  When discussing with some friends, we
started talking about the trade-offs of different implementations and realized
that the extra dimensionality was no joke...as you upped the number of
dimensions, the number of points you have to consider grow as $O((2t+6)^d)$,
and the number of neighbors of each point to check grows as $O(3^d)$.  So for
4D it's definitely possible to solve naively...but anything higher is going to
strain.  My naive solution on 6D took three minutes, and 7D in a reasonable
amount of time (612,220,032 points with 2,186 neighbors each) seemed
*impossible* on commercial consumer hardware because of the sheer number of
points in 7D space.  But I thought...what if a breakthrough in optimization was
possible?  I set my goal as 10D (3,570,467,226,624 points with 59,048 neighbors
each), not knowing if it was possible.

And soon...a breakthrough did come!  Someone brought up that if we look at the
3d version, we see there's actually a *mirror symmetry*!  That is, because
everything starts off on the xy plane, with z=0, the resulting progression must
be symmetrical on both sides (positive and negative z).

![d=3 animation by [u/ZuBsPaCe][]](/img/entries/advent-gol/life3d.gif "d=3 animation u/ZuBsPaCe")

[u/ZuBsPaCe]: https://www.reddit.com/r/adventofcode/comments/kfa3nr/2020_day_17_godot_cubes_i_think_i_went_a_bit_too/

In the end that means we only have to simulate one of the
"halves"/"quadrants" of the higher-dimensional space, since all
"quadrants" are identical!  This saves down the number of points by a factor of
two for each extra dimension ($O(2^{d-2})$).  My 7D implementation completed in
6 minutes!  8D still hung forever, though.

Well, it didn't get us to d=10...but this discovery completely changed how we
saw this puzzle.  With one breakthrough down, we began to believe that there
would be more just around the corner, made possible by our problem's special
degeneracy (that is, that we start on a 2d slice).

Such a dream (as posed in [this reddit thread I started][reddit]) turned
into a month-long quest of breakthrough after breakthrough, exploiting different
aspects of this degeneracy!  It was a long, harrowing journey full of sudden
twists and turns and bursts of excitement when new innovations came.  And in
the end, the hopeful question "What if d=10 was possible?" turned into "d=10 in
100ms, d=40 in eight minutes."  I even got d=10 fast enough to run on easily
any modern browser --- this post includes those simulations!  Furthermore, the
whole journey became an adventure in the power of visualization combined with
abstract thinking.

[reddit]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/

So, let's take a deep dive --- deeper than you probably ever expected to dive
into any particular degenerate starting conditions of a hyper-dimensional game
of life :D

The Baseline
------------

First of all, let's meet our friend for the rest of this journey.  In the
drawer below, you can draw (with your mouse) the 8x8 grid you want to simulate
for the rest of this post.  As you draw, the rest of the visualizations will
update to use this as their initial conditions.

::::: {#golDrawer}
Please enable Javascript
:::::

And for fun, here's a 2D vanilla game of life implementation (for six time
steps) to test out your creation.  I recommend trying out some of the
[interesting well-known patterns][patterns]!

[patterns]: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life#Examples_of_patterns

::::: {#gol2D}
Please enable Javascript
:::::

Now that that's there, let's start at the beginning: what's the naive, baseline
solution?

A reasonable initial thought would be:

1.  Keep a 2D (or 3D, or 4D, etc.) array of booleans.
2.  At each step:
    a.  Make a fresh copy of the entire space ($O(n^d)$).
    b.  Loop over each item in your array ($O(n^d)$).  Count all of the
        neighbors ($O(3^d)$) that are `true` ("alive"), and write to the new
        array based on the rules table of GoL (2 or 3 neighbors for a live cell
        stays alive, 3 neighbors for a dead cell turns alive).
4.  You have a new array!  Loop again six times.

Sounds reasonable enough!  And this does work for the 2D case pretty well (like
in the [Day 11 puzzle][day11]).  However, there are some clear issues when
moving into higher dimensions.  The size of your array grows
exponentially on your dimension, and so does the number of neighbors you'd have
to check.  And the [curse of dimensionality][cod] assures us that more and more
of that array would become wasted as the proportion of "on" points shrinks to
zero for higher dimensions.

[day11]: https://adventofcode.com/2020/day/11
[cod]: https://en.wikipedia.org/wiki/Curse_of_dimensionality

Oh, but what's that?  The percentage of "on" points shrinks to zero for higher
dimensions?  That actually sounds like something we can use to our advantage!
The...*blessing of dimensionality*, I daresay?  Because we know the vast
majority of our points will be "off", there's another method.

1.  Keep a *set* of points that are "on".
2.  At each step:
    a.  Initialize a dynamic map (key-value store) of points to integers (this
        will record the number of live neighbors of each point).
    b.  For each step, iterate over each of your "on" points, expand all of
        their neighbors $n_i$ ($(O(3^d))$), and increment the value associated
        with $n_i$ in your dynamic map.

        For example, if the point `[2,3]` is in your set of live points, you
        would add increment the map's values at keys `[1,2]`, `[2,2]`, `[3,2]`,
        etc.: all 8 neighbors of `[2,3]`.
    c.  Collect your new set of on points: keep all of the keys in your dynamic
        map corresponding to live points if their integers are 2 or 3, and keep
        all of the keys in your dynamic map corresponding to dead points if
        their integers are 3.
4.  You have a new set!  Loop again six times!

I discuss this algorithm much more deeply with actual code in [my solutions
write-up in my Advent of Code reflections journal][basic].

[basic]: https://github.com/mstksg/advent-of-code-2020/blob/master/reflections-out/day17.md

This method nets us a huge advantage because we now only have to loop over the
number of items that we know are alive!  Any points far away from our set of
alive points can be properly ignored.  This narrows down our huge iteration
space, and the benefits compound with every dimension due to the blessing of
dimensionality![^bittweak]

[^bittweak]: There is a small tweak (brought to our attention by [Peter Tseng][bitshift])
that people often add to this to avoid the costly check of the original set in
step 2c: when you iterate over each point, normally you'd increment the eight
neighbors' map values by 1.  Instead, you can increment the eight neighbors'
map values by *2*, and then increment the point itself by 1.  Then in the final
integer under each key, `n / 2` or `n >> 1` gives you the number of neighbors and `n % 2`
(modulo) gives you whether or not that cell was alive.

[bitshift]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/ghmllf8

The nice thing about this method is that it's easy enough to generalize to any
dimension: instead of, say, keeping `[x,y]` in your set, just keep `[x,y,z]`,
or any length array of coordinates.  One minor trick you need to think through
is generating all $3^d-1$ neighbors, but but that's going to come down to a
d-ary [cartesian product][cross] of `[-1,0,1]` to itself.[^crosstrick]

[^crosstrick]: A cute trick (that I forgot who I heard it from first) with this
is that if you cartesian-product `[0,-1,1]` to itself d times, the first item
will be `[0,0,0..]`!  This integrates extremely well with the other bit shifty
tweak: to generate all of the contributions for d=3, cartesian product
`[0,-1,1]` to itself three times, increment the key of the first resulting item
by 1, and increment the key of the rest of them by 2.

[cross]: https://observablehq.com/@d3/d3-cross

We can visualize this in 3D, but it might be nice to render this as a
collection of "slices" in 3D space.  Each square represents a slice at a
different Z level: the middle one is z=0, the ones to the left and right are
z=-1 and z=1, etc.

::::: {#gol3D}
Please enable Javascript
:::::

WIP

::::: {#gol4D}
Please enable Javascript
:::::

::::: {#golFlat}
Please enable Javascript
:::::

::::: {#golSyms3DForward}
Please enable Javascript
:::::

::::: {#golSyms3DReverse}
Please enable Javascript
:::::

::::: {#golSyms4DForward}
Please enable Javascript
:::::

::::: {#golSyms4DReverse}
Please enable Javascript
:::::

::::: {#golTreeForward}
Please enable Javascript
:::::

::::: {#golTreeReverse}
Please enable Javascript
:::::

::::: {#golSyms5D}
Please enable Javascript
:::::

```
sim642  I wanted to ask this before but forgot: did anyone try to take advantage of the symmetry, e.g. in z axis in part 1?
sim642  Should halve the amount of calculations you have to do
sim642  Only some extra work at the end to differentiate z=0 and z>0 positions to know which to count twice
sim642  And in part 2 I feel like you could also exploit the symmetry in w axis simultaneously
```
