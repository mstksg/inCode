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
night before Thursday, December 17, 2020, The release of ["Conway
Cubes"][puzzle], day 17 of the "Advent of Code" (fun little coding puzzles
building up to Christmas). One part about Advent of Code I've always found
especially fun is that, because the problems are so self-contained and tidy,
they are often *open-ended* in the interesting ways you can solve them or
expand them.

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

There will be python code samples here and there, but just for context, my
actual solvers I developed along the way were written in Haskell, and all of
the solving logic embedded in this post was written in Purescript and compiled
to Javascript.

Starting Off
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
    a.  Initialize a dynamic map (key-value store) of points to integers.  This
        map associates each point to the number of live neighbors it has.
    b.  For each step, iterate over each of your "on" points, expand all of
        their neighbors $n_i$ ($(O(3^d))$), and increment the value associated
        with $n_i$ in your dynamic map.

        For example, if the point `<2,3>` is in your set of live points, you
        would add increment the map's values at keys `<1,2>`, `<2,2>`, `<3,2>`,
        etc.: all 8 neighbors of `<2,3>`.
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
dimensionality!

The nice thing about this method is that it's easy enough to generalize to any
dimension: instead of, say, keeping `[x,y]` in your set for 2D, just keep
`[x,y,z]` for 3D, or any length array of coordinates.  One minor trick you need
to think through is generating all $3^d-1$ neighbors, but but that's going to
come down to a d-ary [cartesian product][cross] of `[-1,0,1]` to
itself.

Here's a version of the set-based implementation, using a nice trick I learned
from [phaazon][] to get the right neighbors by doing a cartesian product
against `[0,-1,1]`, which leaves the first item as the `<0,0>` "original point"
we want to exclude:

[cross]: https://observablehq.com/@d3/d3-cross
[phaazon]: https://twitter.com/phaazon_

```python
from itertools import islice, product
from collections import Counter

def mk_neighbs(point):
    """Return neighboring points, each equally weighted

    (1,2)
    => [(1, 1), (1, 3), (0, 2), (0, 1), (0, 3), (2, 2), (2, 1), (2, 3)]
    """
    gen = product(*[[x, x-1, x+1] for x in point])
    # skip the first item, the original point
    next(gen)
    return gen

def step(pts):
    """Takes a set of points (tuples) and steps them in the simulation
    """
    neighbs = Counter()
    for point in pts:
        neighbs += Counter(mk_neighbs(point))

    def validate(point, ncount):
        if point in pts:
            return ncount == 2 or ncount == 3
        else:
            return ncount == 3

    return [p for p, n in neighbs.items() if validate(p, n)]
```

<!-- And...there's actually a neat optimization we can use (brought to our -->
<!-- attention by [Peter Tseng][bitshift]) to avoid the check of the original set in -->
<!-- step 2c above: when you iterate over each point, increment the eight neighbors' -->
<!-- map values by *2*, and then increment the point itself by 1.  Then in the final -->
<!-- integer under each key, `n / 2` or `n >> 1` gives you the number of neighbors and `n % 2` -->
<!-- (modulo) gives you whether or not that cell was alive. -->

<!-- [bitshift]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/ghmllf8 -->

<!-- ```python -->
<!-- from itertools import islice, product, chain,repeat -->
<!-- from collections import Counter -->

<!-- def mk_neighbs(point): -->
<!--     """Return neighboring points, with special optimization trick weights -->

<!--     (1,2) -->
<!--     => [((1, 2), 1), ((1, 1), 2), ((1, 3), 2), ((0, 2), 2), ((0, 1), 2), -->
<!--         ((0, 3), 2), ((2, 2), 2), ((2, 1), 2), ((2, 3), 2) -->
<!--        ] -->
<!--     """ -->
<!--     neighb_pts = product(*[[x, x-1, x+1] for x in point]) -->
<!--     # associate the original point with +1, and the neighbors with +2 -->
<!--     return zip(neighb_pts, chain([1], repeat(2))) -->

<!-- def step(pts): -->
<!--     """Takes a set of points (tuples) and steps them in the simulation -->
<!--     """ -->
<!--     neighbs = Counter() -->
<!--     for point in pts: -->
<!--         neighbs += Counter(dict(mk_neighbs(point))) -->

<!--     def validate(val): -->
<!--         # the true neighbor count, since we inserted +2 for neighbors -->
<!--         ncount = val // 2 -->
<!--         # was originally alive if odd, since we inserted +1 for self -->
<!--         if val % 2 == 1: -->
<!--             return ncount == 2 or ncount == 3 -->
<!--         else: -->
<!--             return ncount == 3 -->

<!--     return [p for p, q in neighbs.items() if validate(q)] -->
<!-- ``` -->

Three Dimensions
----------------

Let's see how this looks for the 3D case!  To make things easier to see, we can
render things in "slices" in 3D space: each grid represents a slice at a
different z level (ie, the z=0 square represents all squares $<x,y,0>$).  Press
"Play" to have the simulation cycle through 6 time steps!

::::: {#gol3D}
Please enable Javascript
:::::

In "reality", each of those 13 slices above are stacked on top of each other in
3D space. You'll see that most initial conditions will spread out from the
center z=0 point, which means they are actually spreading "up and down" the z
axis.

If you mouse over (or tap) any individual tiny `<x,y>` cell, you'll see the all
of the 26 ($3^d-1$) `<x,y,z>` 3D neighbors of the point you're hovering over highlighted
in blue --- these 26 points form a 3D cube around your mouse once everything is
stacked correctly.  You can use this cube to help see how the simulation
progresses. If your mouse is hovering over a live cell, and there are 2 or 3
live cells highlighted in your cube, it'll stay alive in the next time step. If
your mouse is hovering over a dead cell and there are exactly 3 live cells
highlighted in your cube, it will come alive in the next step.

### Axis Reflection Symmetry

Try playing around with different initial conditions to see how they evolve!
See any patterns?

Well, the yellow highlight might have given given things away, but...note that
the entire thing has reflection symmetry across z=0!  z=1 is always the same as
z=-1, z=2 is always the same as z=-2, etc.  Fundamentally, this is because our
starting solution has z-plane symmetry: the initial 2D slice is symmetric with
reflections across z, because z=0.  This is the first "degeneracy" that this
blog post's title is referring to.  The negative and positive directions are
interchangeable!  This is reflected in the yellow highlight on hover: when you
mouse-over a z square, its corresponding reflected twin is highlighted, and
will always be identical.

This means that we actually only need to simulate *positive* z's...and for our
final answer we just "un-reflect" to get the total number.

This is exactly what freenode IRC user sim642 noticed late into the night of
December 16th:

> I wanted to ask this before but forgot: did anyone try to take advantage of
> the symmetry, e.g. in z axis in part 1? Should halve the amount of
> calculations you have to do.
>
> Only some extra work at the end to differentiate z=0 and z>0 positions to
> know which to count twice And in part 2 I feel like you could also exploit
> the symmetry in w axis simultaneously

Wow, let's do this!  Apparently the picture is slightly more complicated than
simply halving the points.  We also need to change how to distribute neighbors.
That's because, once we commit to only keeping the positive z's, some cells
need to be double-counted as neighbors. In particular, any `z=0` cell would
previously had a neighbor at both `z=-1` and `z=1`...but now if we only keep
the positive z's, it would have `z=1` as a neighbor *twice*.

The following interactive demo lets you explore what this looks like:

::::: {#golSyms3DForward}
Please enable Javascript
:::::

Each square represents an entire "slice" of z.  When you mouse-over or tap a z-cell,
its z-neighbors are highlighted with how many times that neighbor has to be
counted, and the green bar tells you from what direction that neighborship
arose from.  For example, mousing over z=3, z=2 and z=4 get highlighted with
the values "1" because they are neighbors of 3, on the left and right side
(respectively).  Note that one neat property for all squares (except for z=6,
which goes off the scale) is that the "total" higher-dimensional neighbors is
always 2 ($3^(d-2)-1$), it's just that where those neighbors fall is
re-arranged slightly.

The tricky square is now z=0: if you mouse-over it, you'll see that it has a
single neighbor z=1 that is counted twice, as a neighbor from both the left and
right side.

We can compute the above diagram by expanding z=0 to its neighbors (z=-1, and
z=1), applying the absolute value function, and seeing how points double-up.
This gives us the "forward neighbors", and we can directly use it for the
original "keep the full array" GoL implementation.

However, for the "keep active points and expand their neighbors" GoL
implementation...we have to find the opposite.  Remember that to build our
"neighbors map" (the map of points to how many active neighbors they have), we
have each cell "pro-actively" add its contributions to all of its neighbors.
`<1,2,3>` is a neighbor to `<1,3,4>` once, so when we expand `<1,2,3>` we
would increment the value in the map at `<1,3,4>` by 1 because `<1,2,3>` is a
neighbor of `<1,3,4>` once.

Now, how do we count `<1,3,1>` expanding into `<1,3,0>`?  Well,
normally, `<1,3,1>` is a neighbor of `<1,3,0>` once.  However, if we only keep
the normalized z values, `<1,3,1>` is a neighbor of `<1,3,0>`...twice!  To
compute the total neighbor count of `<1,3,0>`, we have to count the
contribution from `<1,3,1>` twice (once for `<1,3,1>` and once for `<1,3,-1>`,
which was normalized away).

That means we have to follow the rules in the previous demo *backwards*,
like:

::::: {#golSyms3DReverse}
Please enable Javascript
:::::

These are the "reverse neighbors": how much times a given point counts as a
neighbor for its surrounding points.  Here, mousing over z=1 shows that it
counts as a neighbor for z=0 twice, from both the left and the right.  It also
counts as a neighbor for z=2 once (from the left side).

We can account for this by hard-coding the rules into our step algorithm: if
our z goes from `1` to `0`, increment its value twice in the neighbor map.
Otherwise, simply increment by 1 as normal.

This rule is relatively easy to implement, and as a result we now halved our
total number of points we need to keep and check for 3D!  It's also simple
enough to generalize (just do the `1 -> 0` check for every "higher dimension"
and double its contribution for each `1 -> 0` transition is seen)...and that
means we reduce the number of 4D points we need to track by a factor of four,
the number of 5D points by a factor of eight, the number of 6D points by a
factor of 16... now our total points to check only grows as $O(n^d / 2^{d-2})$
instead of $O(n^d)$!

This discovery late in the Tuesday night of the 16th was what inspired us to
believe and dream that more breakthroughs might be possible to bring things
down even further.

And those breakthroughs soon came...

Four Dimensions
---------------

Let's look at how the 4 dimensions works!  We can visualize this by taking
"z-w" slices at different x-y planes as well.  The labels in these boxes are
the `<z,w>` of each slice.  The very center is `<z,w> = <0,0>` the row in the
middle from the top is `w=0`, and the column in the very middle from the left
is `z=0`.  It's basically taking the 3D visualization above and expanding it in
an extra dimension.  Press "Play" to run your initial conditions!

::::: {#gol4D}
Please enable Javascript
:::::

We get something interesting as well: most initial conditions will spread out
from the center `<z,w> = <0,0>` point radially, spreading outwards into
positive and negative z and w.  Mouse-over or tap any individual tiny `<x.y>`
cell and you'll see each of its 80 ($3^d-1$) `<x,y,z,w>` 4D neighbors
highlighted in blue, forming a little 3x3x3 "tesseract" (4D cube, or
hypercube).  Like in the 3D case, you can use this little hypercube to track
how the simulation progresses: if your mouse if hovering over a live cell with
2 or 3 live cells in its hypercube, it'll stay alive in the next step, if it's
hovering over a dead cell with 3 live cells in its hypercube, it'll come alive
in the next step.

### Diagonal Reflection Symmetry

Play around and explore how simulations evolve!  You will notice that the axis
reflection symmetry is still preserved, but four ways (the slice at `<z,w> = <3,4>` is
always going to be identical to the slice at `<-3,4>`, `<3,-4>`, and
`<-3,-4>`).  These are reflected in the "deep yellow" highlights above when you
mouse over a zw square. (Ignore the lighter yellow highlights for now!)

And now, for the next big breakthrough.  I think this situation shows the power
of visualization well, because this exact visualization was what reddit user
*u/cetttbycett* was looking at when [they made this post][zwsym] late Thursday
the 17th/early Friday the 18th...and everything changed *forever*.

[zwsym]: https://www.reddit.com/r/adventofcode/comments/kfjhwh/year_2020_day_17_part_2_using_symmetry_in_4d_space/

> I noticed that the expansion of active cubes for part 2 is symmetric with
respect to two hyperplanes in 4d space: These hyperplanes can be described by w
= 0 and w-z = 0.
>
> Using these symmetries could make the code nearly eight times as fast.I was
> wondering if anyone tried that.

What *u/cetttbycettt* saw is what you can see now in the demo above: it's
all of the *light yellow* highlighted squares when you mouse-over.  In addition
to the z=0 and w=0 lines (the two lines down the middle, up-down and
left-right), we also have another line of symmetry: z=w and w=z, the diagonal
lines!

That's right, a zw slice at `<z,w>=<3,4>` is *identical* to the one at `<4,3>`, and
so also `<-3,4>`, `<3,-4>`, `<-3,-4>`, `<-4,3>`, `<4,-3>`, and `<-4,-3>`!  Each
slice is potentially repeated *eight* times!  The exceptions are the points on
the lines of symmetry themselves, which are each repeated four times, and also
`<z,w>=<0,0>`, which is in its own class.

So, our first breakthrough meant that we only have to simulate *positive*
coordinates (a single quadrant)...our next breakthrough means that we only have
to simulate coordinates on a single "wedge" half-quadrant...and then duplicate
those eight times at the end.

Arbitrarily, let's say we only simulate the north-by-northeast wedge, because
it's easy to normalize/compact all points onto that wedge: they're the points
`<z,w>` where both components are positive and in non-decreasing order.  So,
`<4,-3>` gets "normalized" to `<3,4>`.

Okay, so we found a new symmetry...but we ran into the same issue as before.
How do we propagate neighbors?  To help us, see what's going on, let's look at
the map of neighbors between different `<z,w>` squares, for the single zw wedge
we are simulating.

::::: {#golSyms4DForward}
Please enable Javascript
:::::

These are the "forward neighbors"; we can compute them by expanding a point to
its neighbors, and then normalizing our points and seeing how they double (or
quadruple) up.

```python
def normalize(point):
    """Normalize a point by sorting the absolute values

    (2, -1)
    => (1, 2)
    """
    return tuple(sorted([abs(x) for x in point]))

def forward_neighbs(point):
    """Generate the forward neighbors of a point

    (0, 1)
    => {(0, 1): 2, (1, 2): 2, (1, 1): 2, (0, 0): 1, (0, 2): 1}
    """
    return Counter([normalize(neighb) for neighb in mk_neighbs(point)])
```

For example, mouse over `<z,w>=<3,3>` and see it has eight
total higher-dimensional neighbors (like all points should, though this
visualization leaves out points at w>6).  It's *supposed* to have a neighbor at
`<4,3>`, but that gets reflected back onto `<3,4>` during our normalization
process, so you see that the point `<3,3>` has a neighbor at `<3,4>`
"double-counted".  The green squares (in the north and west positions) at
`<3,4>` when you hover over `<3,3>` show that `<3,4>` is a neighbor of `<3,3>`
both to its north and to its west.

Also, we have something really odd show up for the first time.  Mouse over a
point like `<z,w>=<2,3>` and see that it has a neighbor in...itself?  What's
going on here?  Well, it is *supposed* to have a neighbor at `<3,2>` but that
gets normalized/reflected back onto `<2,3>` --- it reflects onto itself!  The
green square in the Southeast means that `<2,3>`'s southeast neighbor
is...itself!

The "forward neighbors" are useful for understanding what's going on, but to
actually run our simulation we again need to find the "reverse neighbors": from a
given point A, how many times is that point a neighbor of another point B?

We can compute this in brute-force using a cache: iterate over each point,
expand all its neighbors $a_i$, normalize that neighbor, and then set $a_i$ in
the cache to the multiplicity after normalization.

```python
def reverse_neighbs_table(t_max):
    """Tabulate the reverse neighbors of all zw slices reachable before t_max
    """
    weights = {}

    for i in range(t_max):
        for j in range(i, t_max):
            for neighb, ncount in forward_neighbs((i, j)).items():
                if neighb in weights:
                    weights[neighb][(i, j)] = ncount
                else:
                    weights[neighb] = {(i, j): ncount}

    return weights
```

This seems pretty expensive and wasteful, so we'd like to maybe find a formula
to be able to do this using mathematical operations.  So, let's explore!

::::: {#golSyms4DReverse}
Please enable Javascript
:::::

After exploring this interactively, we can maybe think of some rules we can
apply.

1.  If we have a point `<z,z>` directly on the z=w diagonal, just use its five
    normal left/up neighbors with weight 1 each.
2.  If we have a point `<z,z+1>` on the "inner-er" diagonal, use its five
    normal left/up neighbors with weight 1, but its south and west neighbors
    have weight 2, and the point reflects onto *itself* with weight 1.
3.  If we're on `z=1` and we move into `z=0`, double that count (phew, the same
    rule as in the 3D case earlier)
4.  If we're on w=1 and we move into w=0, double that count (same as before)
5.  And...I guess `<0,1>` reflects onto itself *twice*?  I guess that
    technically falls under a combination of rule 2 and rule 4, but we don't
    directly observe the motion into w=0 before it gets reflected so it has to
    be special-cased.

Okay, those rules are *sliiightly* more complicated than our 3D rules ("if we
go from z=1 to z=0, double-count it")...but they're at least mechanical enough
to code in, even if not beautiful.  You can probably foresee that it might be
tough to generalize, but...we'll tackle that when we get there :)

For now, we have a super-fast implementation of 4D GoL with our special
degeneracy!  The runtime gets reduced by a factor of 8!

Now, onward to 5D!

Breaking Through
----------------

By stepping into looking at 5D, we've stepped into a brand new territory ---
we're now past what the original question was asking about, and into simply
exploring a personal curiosity for fun.  No longer are we "super-optimizing"
the puzzle --- we're now warping the original challenge to levels it was never
designed to handle.

It's difficult to visualize how things look in 5 dimensions, so this is where
it gets a little tricky to make any progress, mentally.  The first thing we
need to figure out is how exactly we can generalize the "z=w" symmetry from 4D
to be able to take advantage of it in 5D...and hopefully in a way that can
generalize to arbitrary dimensions.  Along the way we'd also like to get rid of
our hacky 4D neighbor multiplicity rules and get something a little cleaner.

I struggled with for a while without making too much headway...but on the
morning of Friday, December 18th, arguably one of the biggest revelations of
the entire journey was dropped by Michal Marsalek on u/cetttbycettt's reddit
thread.  It was a big deal, because not only did it allow us to generalize our
symmetries to higher dimensions, but it also *proved* a specific degeneracy
that allowed 10D simulation to be definitely 100% *solvable*.

### Permutation Symmetry

Here was Michal's [historic post][permpost]:

> Yes, all the higher dimensions are interchangeable, there's nothing that
> distinquishes them. That is, if there's an active cell at position (x,y,
> a,b,c,d,e,f,g) then, there's also one at (x,y, c,d,g,e,f,a) and at all other
> permutations, of coordinates a-g). That is the number of cells that one need
> to track can be reduced by factor of $(d-2)! \times 2^{d-2}$ (at least if
> time goes to infinity).
>
> ...we can use symmetries coming from permutations, to only track cells
> where $|x_0| < 13,\, |x_1| < 13,\, 0 \leq x_2 \leq x_3 \leq\,\ldots\, \leq x_{d-1} \leq t$.
> There's $20^2 \times \sum_{k=0}^{t} { {d-3+k} \choose {k} }$ such cells.

[permpost]: https://www.reddit.com/r/adventofcode/comments/kfjhwh/year_2020_day_17_part_2_using_symmetry_in_4d_space/gg9vr6m/

*(equations slightly modified)*

And boy was this exciting to read.  First of all, it gave a way to generalize
the z=w symmetry: it's just permutation symmetry for all higher-dimensional
coordinates!   But the big kicker here: See that last formula?  Let's look at
it more closely, using $\hat{d}$ to represent $d-2$, the number of higher
dimensions:

$$
20^2 \times \sum_{k=0}^{t} { {\hat{d}-1+k}\choose{k} }
$$

That sum has only the amount of terms fixed with the maximum timestamp! That
means we only ever have 6 terms to expand, no matter how high the dimensions
are --- at 10D and even 100D!  Furthermore, we can simplify the above using
properties of the binomial distribution to get

$$
20^2 \times { {\hat{d}+6}\choose{6} }
$$

This binomial coefficient is actually polynomial on $\hat{d}$ --- it's
$\frac{1}{6!} \prod_{k=1}^6 (\hat{d}+k)$ --- a sixth degree polynomial (leading
term $\frac{1}{6!} \hat{d}^6$), in fact.  This means that we have turned the
number of points we potentially need to track from exponential
($O(13^{\hat{d}})$) to slightly smaller exponential ($O(6^{\hat{d}})$) to now
*polynomial* $O(\hat{d}^6)$!

So, not only did we figure out a way to generalize/compute our symmetries, we
also now know that this method lets us keep our point set *polynomial* on the
dimension, instead of exponential.

To put a concrete number for context, for that dream of d=10, here are only
${ {8+6} \choose 6 }$, or 3003 potential unique `<z,w,...>`  points, once you
factor out symmetries!  The number went down from $13^8$ (815,730,721)
potential unique `<z,w,...>` points to $6^8$ (1,679,616) potential unique
points with positive/negative symmetry to just 3003 with permutation
symmetry.[^xy3003]  Furthermore, because of the blessing of dimensionality
mentioned earlier, we can expect more and more of those to be empty as we
increase our dimensions.

[^xy3003]: For dramatic effect, I've omitted the fact that while there are only
3003 possible higher-dimensional points, there are $20^2 \times 3003$ actual
unique points possible factoring in the 20x20 x-y grid.  Still, it's a pretty
big improvement over the original situation ($20^2 \times 815730721$).

And in a flash, 10D didn't feel like a dream anymore.  It felt like an
inevitability.  And now, it was a race to see who could get there first.

### The Race to 10D

Unfortunately, the exact record of who reached and posted 10D first is a bit
lost to history due to reddit's editing records.  A few people maintained and
updated their posts to prevent clutter, but the record and time stamp of when
they first hit 10D is lost.  If any of them happens to read this and can more
accurately verify their times, I'd be happy to update!

For me, I'm sure I was not the first one, but in my chat logs I chimed into
freenode's `##adventofcode-spoilers` channel in excitement in the wee morning
hours (PST) Saturday December 19th:

```
2020-12-19 02:32:42 | jle`    d=10 in 9m58s
2020-12-19 02:33:05 | jle`    hooray my goal :)
2020-12-19 02:33:08 | jle`    time to sleep now
2020-12-19 02:33:12 | xerox_  goodnight
2020-12-19 02:33:35 | jle`    xerox_: thanks :)
```

Pure joy! :D

[Peter Tseng][peterpost] made a post on Thursday night with times, but I can't
remember if it incorporated all the symmetries or originally included d=10.
[Michal Marsalek][michalpost] was apparently able to implement the idea that
they originally proposed by the following Wednesday (December 23rd) in Nim to blow
everyone's time out of the water: 3.0 seconds!

[peterpost]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/ggaaqsy/
[michalpost]: https://www.reddit.com/r/adventofcode/comments/kfb6zx/day_17_getting_to_t6_at_for_higher_spoilerss/ggsx9e9/

At that point, it was pretty unbelievable to me that what started out as a
dream goal that we couldn't have completed on a supercomputer had, through
successive revelations and insights building on each other one by one, could
now be done in 3 seconds.

But hey, I promised 100ms in the introduction, and a fast d=40, right?

With our goal completed, it was now time to dig in a little deeper and see how
far this baby could go.

### Diving Deeper: Terminology

Before we go any further, let's take a break to clarify and introduce some
terminology we'll be using for the rest of this post.

*   I've been using the word **slice** to talk about a 2D grid representing a
    single higher-dimensional `<z,w...>` coordinate --- they're the 13 grids in
    the 3D simulation and the 169 grids in the 4D simulation.
*   I've also been using **cell** to refer to an exact specific `<x,y,z,w,..>`
    spot --- they are the tiny squares inside each grid in the simulations
    above.
*   I'll start using the word **[coset][]** to refer the set of all of the
    duplicates of an `<x,y>` across all permutations and negations of
    `<z,w,q,..>`, since they all behave the same (they are either all on or all
    off together).  So `<x,y,1,2>`, `<x,y,2,1>`, `<x,y,-1,2>`, `<x,y,1,-2>`,
    `<x,y,-1,-2>`, `<x,y,-2,1>`, `<x,y,2,-1>`, and `<x,y,-2,-1>` are all a part
    of the same coset, represented by the normalized form `<x,y,1,2>`.  Now,
    during our simulation, we only need to simulate one member from each coset,
    because every member is identically present or not present.  For the sake
    of implementation, we simulate the arbitrary *normalized* (positive and
    sorted) member only.  Because of this, we'll sometimes refer to the normalized
    item and the coset it represents as the same thing.
*   I'll also start using **slice coset** to talk about the set of all
    `<z,w,...>` slices) across its permutations and negations.  The slices at
    z-w coordinates of  `<1,2>`, `<2,1>`, `<-1,2>`, `<1,-2>`, `<-1,-2>`,
    `<-2,1>`, `<2,-1>`, and `<-2,-1>` are all a part of the same coset,
    represented by the normalized form `<1,2>`.  All of the slices at each of
    those zw coordinates will always be identical, so we can talk the state of
    a single slice at `<1,2>` as representing the state of its entire coset.

    Slice cosets are what are being highlighted on mouseovers for the 3D and 4D
    simulations. They are also what the big squares represent for the forward
    and backward neighbor demos of each: each slice stands in for their entire
    slice coset, and we show the amount of times each normalized slice coset
    element is a neighbor of the other.

[coset]: https://www.youtube.com/watch?v=Dp8sYTlLQRY

Tackling the Neighbor Problem
-----------------------------

My initial d=10 time clocked in at just under 10 minutes initially, but as
early as next Wednesday we knew that a sub-5 second time was possible.  So
where was the gap?

Well, I didn't really know what to do about the neighbor multiplicity problem.
I was still brute-forcing by way of forward neighbors + normalizing (as in the
sample 4D python code snippet earlier).  The naive brute-force method
requires computing *all* $3^{ {\hat{d}} } - 1$ higher-dimensional
neighbors...so even though the number of points I'd have to track grows
polynomially, I still had that pesky exponential factor in building my neighbor
map.  And at high dimensions, that exponential factor dominates over
everything.

So put on your hard hats and working boots ... we're going to dive deep into
the world of hyper-dimensional symmetries!

### Five Dimensions

First, let's start visualizing how things look like in 5 dimensions, now that
we know what our slice coset/representative structure looks like.  Partially to
help us gain an intuition for some of what's going on, and also partially to
show that intuition at the individual component level can only get so far.

It's a bit difficult to duplicate the same forward/reverse neighbor demos for
4D as we had for 4D, so here's a different representation.  Here is a demo of
all of the `<z,w,q>` slice cosets (the wedge of normalized points we track for
our implementation) and both their forward and reverse neighbor weights of each
other (computable using the method we used for 4D).  The `q` axis is
represented as stacked zw sections from left to right.

::::: {#golSyms5D}
Please enable Javascript
:::::

As you mouse-over a slice coset representative (a single square), all of its
neighbors will be highlighted, including reflections.  The red dot on the left
is the "forward" neighbor (how many times that other slice is a neighbor of the
hovered slice) and the blue dot on the left is the "reverse" neighbor (how many
times the hovered slice is a neighbor of the other slice).  For example, if you
hover over `<z,w,q>=<1,3,4>`, you can see that `<0,3,4>` is its neighbor twice,
and `<1,3,4>` is `<0,3,4>`'s neighbor four times.  These four times come from
the non-normalized reflections of `<1,3,4>` at `<1,3,4>`, `<1,4,3>`,
`<-1,3,4>`, and `<-1,4,3>`.  Some squares are also neighbors to themselves
(like `<1,4,5>`, which reflects off of the top edge at `<1,5,4>`) and some are
not (like `<1,3,5>`).  [Mind bottling][bottle]!

[bottle]: https://www.youtube.com/watch?v=rSfebOXSBOE

At least one pattern we can see clearly is that if your points are 4 or lower,
the sum of all the red dots (the forward neighbors) is $3^3-1$ = 26, just like
how the sum of forward neighbors for interior points in 3D is $3^2-1=8$, and
for 2D is $3^2-1 = 2$.

Another very important pattern is that "is a neighbor" seems to be reversible:
the set of all *forward* neighbors of a point is the same as all *reverse*
neighbors of a point --- the only difference is the multiplicities!  But,
wherever you see a red dot, you will also always see a blue dot.  No single-dot
squares.

Anyway, you can explore this a little bit and try to come up with a set of
ad-hoc rules like we did for 4D...but I think we've reached the limits of how
far that method can go.  We can generate these values simply enough using the
expand-normalize-tabulate method we did for 4D, but there should be a way to
compute these weights *directly*, in a clean fashion that doesn't require
branching special cases and patterns.  It's clear that we are limited until we
can find this method.

### Go with the Flow

What do all our valid normalized `<z,w,...>` coordinates look like? Well, they
are always non-decreasing, and always are less than the current timestep.
Keeping t=6 as our goal still, this means that valid coordinates in 10D are
strings of eight numbers, like `0,1,1,1,3,5,5,6`, or `0,0,3,4,4,4,6,6`, or
`1,1,2,3,3,4,5,5`.[^repeated]

[^repeated]: It's also interesting to note that above 9D (where there are 7
higher-dimensional coordinates), there is always at least one duplicated
number.  Although I don't really know a way to explicitly exploit that fact
even now, it does mean that there's a qualitative difference between 9D and
below and 10D and above: anything above 9D is...especially degenerate.

But we run into problems working with this format.  For example, if we're
computing a neighbor of `0,1,1,1,3,5,5,6`,  we can imagine that the very first
`1` moves to be a `2`, resulting in `0,2,1,1,3,5,5,6`. However, we're now in
un-normalized territory...we have to re-sort it to turn it into
`0,1,1,2,3,5,5,6`.  It's just not something we can directly manipulate with
simple rules and still stay in the valid state space without complicated
restrictions or rules.

If you stare at many different sample points, you might start to build an
internal model in your head...these points are really all just consecutive runs
of 1s, 2s, 3s, etc., at different lengths.  What if we encoded each
higher-dimensional coordinate as "number of each position seen?"  For example,
we can encode `0,1,1,1,3,5,5,6` as `1-3-0-1-0-2-1`: the first slot represents
how many 0s we have the second how many 1s, the next how many 2s, the next how
many 3s, etc. We can encode `0,0,3,4,4,4,6,6` as `2-0-0-1-3-0-2` and
`1,1,2,3,3,4,5,5` as `0-2-1-2-1-2-0`.  The *sum* of the components gives you
the total number of higher dimensions (ie, 10D vectors sum to 8)

And now, a "valid transition" becomes easy to enforce: it's an amount "flowing"
from one of those bins to another.  For example, turning a `1` into a `2` in
`1-3-0-1-0-2-1` turns it into `1-2-1-1-0-2-1`.  We took one of the three 1s and
turned them into a single 2.  In this method, we don't have to do any
normalization because this "flowing" operation automatically preserves the
sum-to-a-fixed-number invariant!

That's it, really!  We can walk bin-to-bin, assembling a new vector from the
old ones, by looking at the different possible bin-to-bin flows step-by-step!

Now, the tricky math is the with multiplicities.  Interestingly enough, in this
case the *reverse* direction is actually easier to conceptualize than the
forward direction.  Good for us, because it's the reverse direction we actually
need.

Let's say that we start at `0-2-1-3` (`1,1,2,3,3,3`) and we want it to "flow"
to, say, `0-0-5-0` (`2,2,2,2,2`): dump all our bins into 2.  How many ways
could this flow happen?  Well, we end up with 5 points in the slot, which could
have been picked $5!$ ways.  We came to it via three sources `2+1+3` (two from
the left, one from here, three from the right), so for our final multiplicity
we have to quotient by the $2!$ ways the $1 \rightarrow 2$ flow could have
happened, the $1!$ way the $2 \rightarrow 2$ flow could have happened, and the
$3!$ ways the $3 \rightarrow 2$ flow could have happened (aka, the [multinomial
coefficient][] $5 \choose {2,1,3} $).

[multinomial coefficient]: https://en.wikipedia.org/wiki/Multinomial_theorem

One final note: we have to treat transitions from 0 to 1 slightly
differently, because some of them could have been transitions from 0 to -1. For
example, if we had `2-0-0-0` into `0-2-0-0`, you could have had two 0s both
turn into 1s, or you could have had one 0 turn into a 1 and one turn into a -1
(which get reflected as 0 to 1 once you normalize), or you could have had both
0s turn into -1s.  All in the end this factors to a multiplication of $2^n$
(the sum of the nth row in the pascal triangle), $n$ being the number of 0-to-1
transitions, at the end.

Because of the special care taken for 0 to 1 transitions, it's more convenient
to fill in bin-by-bins "backwards", from the 6 slot to the 5 slot to the 4
slot, etc., because your options at the 0 component are already pre-determined
for you by the choices you have already made.  It keeps the tree a more
manageable shape.

Alright, enough words, let's look at this in action!  Here is a *tree*
describing all the ways you can flow from bin to bin!  As an example, let's
look the 6D case of ways each point is a neighbor of `0,2,2,3` (`1-0-2-1`),
which you can pick from the drop-down.

::::: {#golTreeReverse}
Please enable Javascript
:::::

As you can see, each "branch" in three (flowing from left to right) is a
different way to fill in the bin.  At each node, the upper vector is the
"source" vector, and the lower vector is the "target" vector we build
step-by-step.  Bin-by-bin, we begin to move components from our source
vector into our target vector.  The branches in the tree reflects different
ways we can commit a bin in our target vector.  For example, at the very first
split, we can either pick our final vector to be `?-?-?-?-0` (leaving that 3
bin alone) or `?-?-?-?-1` (swiping a component from that 3 bin in the source
vector).  The number to the right of the node represents how we modify our
weights according to the choices we make according to the logic above.  And all
other nodes on the far right are the end products: the actual neighbors, along
with their multiplicities.

If you mouse-over or tap a node, it'll highlight the trace from the beginning
to the node you are highlighting, so you can see all of the choices made, as
well as all the modifications made to our running multiplicity counter at each
step.  It'll also show the contributions from the left, center, and right of
the current bin being picked (the $2+1+3$ in the example above), and also the
"regular" representation.  For example, `<[2,2],2,4>` means that that node has
already commited to having `<?,?,2,4>` in the target vector, but still has two
2s in the source vector to pull in and distribute.

One final thing we need to keep track of is to not count a point transitioning
to itself if it results from no actual internal changes.  This can be done by
checking if each of our bin choices involved exactly no inter-bin flows.

Phew!  That's a bit of a mathematical doozy, huh?  But trust me when I say it's
easier to understand if you try out a few different points from the drop-down
menu and trace out the different possible paths, and how the multiplicities are
affected.  After a few examples in different dimensions, it might start to make
sense.  Try looking at the lower dimensions too to see if they match up with
what we figured out before.

You can also flip the switch to compute reverse and forward neighbors.
Luckily, as we noted before in the 5D case, "is a neighbor" is a reversible
relationship: If a point is a forward neighbor, it is also a reverse neighbor.
This means that the branching structure for forward and reverse neighbor trees
are all the same.  The only difference is how the multiplicities are
calculated.  In this case, the forward direction is just the original
calculation reversed.  The diagram shows how the multiplicities are
accumulated; feel free to try to work out how this works as an exercise!

That's it, for real!  We have tackled the reverse neighbor weights problem with
some branching bin flows and combinatorics![^honesty]

[^honesy]: Okay, I'll be honest --- I didn't actually know how to do the
combinatorics all up-front.  What I did first was built the trees, and try to
find patterns in the trees that I could explain with simple rules.  I noticed
the relationships between the factorials of numbers of things moved at each
node, and I tweaked with the code doing some random maths until I got the right
answers.  But hey, if it works, it works, right? :)

Stacks On Stacks: Visualizting Arbitrary Dimensions
---------------------------------------------------

::::: {#golFlat}
Please enable Javascript
:::::
