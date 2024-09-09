Shifting the Stars: Advent of Code with Galilean Optimization

==============================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on December 10, 2018.
> [Read online!](https://blog.jle.im/entry/shifting-the-stars.html)

(TL;DR: scroll down to the very bottom for a summary and the closed form
solution)

Another short Advent of Code post! [Advent of Code
2018](https://adventofcode.com/2018) is in full swing; we're 40% of the way
through. Every once in a while, if I find a fun way to solve a problem, I'll
make a short post about it. You can check out my other ones [here on the series
page](https://blog.jle.im/entries/series/+advent-of-code.html), and you can also
find my [daily
reflections](https://github.com/mstksg/advent-of-code-2018/blob/master/reflections.md)
here, as well. And, again, if you're following along in Haskell, why not hop on
[glguy's](https://twitter.com/glguy) semi-official [Haskell
Leaderboard](https://adventofcode.com/2018/leaderboard/private) (join code
`43100-84040706`)! There are also Haskellers on freenode ##adventofcode, and
also #adventofcode on the Functional Programming slack. You might also find my
[advent of code api](https://hackage.haskell.org/package/advent-of-code-api)
haskell bindings helpful too!

Today, we're going to be using linear algebra, calculus, and galilian
transformations to solve the *Day 10* challenge. (That's right, this isn't just
a Haskell blog, I do have
[math](https://blog.jle.im/entries/category/@math.html) posts on occasion too :)
)

## Part 1

> It's no use; your navigation system simply isn't capable of providing walking
> directions in the arctic circle, and certainly not in 1018.
>
> The Elves suggest an alternative. In times like these, North Pole rescue
> operations will arrange points of light in the sky to guide missing Elves back
> to base. Unfortunately, the message is easy to miss: the points move slowly
> enough that it takes hours to align them, but have so much momentum that they
> only stay aligned for a second. If you blink at the wrong time, it might be
> hours before another message appears.
>
> You can see these points of light floating in the distance, and record their
> position in the sky and their velocity, the relative change in position per
> second (your puzzle input). The coordinates are all given from your
> perspective; given enough time, those positions and velocities will move the
> points into a cohesive message!
>
> Rather than wait, you decide to fast-forward the process and calculate what
> the points will eventually spell.
>
> For example, suppose you note the following points:
>
>     position=< 9,  1> velocity=< 0,  2>
>     position=< 7,  0> velocity=<-1,  0>
>     position=< 3, -2> velocity=<-1,  1>
>     position=< 6, 10> velocity=<-2, -1>
>     position=< 2, -4> velocity=< 2,  2>
>     position=<-6, 10> velocity=< 2, -2>
>     position=< 1,  8> velocity=< 1, -1>
>     position=< 1,  7> velocity=< 1,  0>
>     position=<-3, 11> velocity=< 1, -2>
>     position=< 7,  6> velocity=<-1, -1>
>     position=<-2,  3> velocity=< 1,  0>
>     position=<-4,  3> velocity=< 2,  0>
>     position=<10, -3> velocity=<-1,  1>
>     position=< 5, 11> velocity=< 1, -2>
>     position=< 4,  7> velocity=< 0, -1>
>     position=< 8, -2> velocity=< 0,  1>
>     position=<15,  0> velocity=<-2,  0>
>     position=< 1,  6> velocity=< 1,  0>
>     position=< 8,  9> velocity=< 0, -1>
>     position=< 3,  3> velocity=<-1,  1>
>     position=< 0,  5> velocity=< 0, -1>
>     position=<-2,  2> velocity=< 2,  0>
>     position=< 5, -2> velocity=< 1,  2>
>     position=< 1,  4> velocity=< 2,  1>
>     position=<-2,  7> velocity=< 2, -2>
>     position=< 3,  6> velocity=<-1, -1>
>     position=< 5,  0> velocity=< 1,  0>
>     position=<-6,  0> velocity=< 2,  0>
>     position=< 5,  9> velocity=< 1, -2>
>     position=<14,  7> velocity=<-2,  0>
>     position=<-3,  6> velocity=< 2, -1>
>
> Each line represents one point. Positions are given as `<X, Y>` pairs: X
> represents how far left (negative) or right (positive) the point appears,
> while Y represents how far up (negative) or down (positive) the point appears.
>
> At `0` seconds, each point has the position given. Each second, each point's
> velocity is added to its position. So, a point with velocity `<1, -2>` is
> moving to the right, but is moving upward twice as quickly. If this point's
> initial position were `<3, 9>`, after `3` seconds, its position would become
> `<6, 3>`.
>
> Over time, the points listed above would move like this:
>
>     Initially:
>     ........#.............
>     ................#.....
>     .........#.#..#.......
>     ......................
>     #..........#.#.......#
>     ...............#......
>     ....#.................
>     ..#.#....#............
>     .......#..............
>     ......#...............
>     ...#...#.#...#........
>     ....#..#..#.........#.
>     .......#..............
>     ...........#..#.......
>     #...........#.........
>     ...#.......#..........
>
>     After 1 second:
>     ......................
>     ......................
>     ..........#....#......
>     ........#.....#.......
>     ..#.........#......#..
>     ......................
>     ......#...............
>     ....##.........#......
>     ......#.#.............
>     .....##.##..#.........
>     ........#.#...........
>     ........#...#.....#...
>     ..#...........#.......
>     ....#.....#.#.........
>     ......................
>     ......................
>
>     After 2 seconds:
>     ......................
>     ......................
>     ......................
>     ..............#.......
>     ....#..#...####..#....
>     ......................
>     ........#....#........
>     ......#.#.............
>     .......#...#..........
>     .......#..#..#.#......
>     ....#....#.#..........
>     .....#...#...##.#.....
>     ........#.............
>     ......................
>     ......................
>     ......................
>
>     After 3 seconds:
>     ......................
>     ......................
>     ......................
>     ......................
>     ......#...#..###......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#####...#.......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#...#...#.......
>     ......#...#..###......
>     ......................
>     ......................
>     ......................
>     ......................
>
>     After 4 seconds:
>     ......................
>     ......................
>     ......................
>     ............#.........
>     ........##...#.#......
>     ......#.....#..#......
>     .....#..##.##.#.......
>     .......##.#....#......
>     ...........#....#.....
>     ..............#.......
>     ....#......#...#......
>     .....#.....##.........
>     ...............#......
>     ...............#......
>     ......................
>     ......................
>
> After 3 seconds, the message appeared briefly: `HI`. Of course, your message
> will be much longer and will take many more seconds to appear.
>
> *What message will eventually appear in the sky?*

The problem tells us to talk about a system of N particles, each moving at
constant velocity. From this, we can see that the position of particle $i$ at
time $t$ is $\mathbf{r}_i  + \mathbf{v}_i t$, where $\mathbf{r}_i$ is the
*initial position* vector, and $\mathbf{v}_i$ is the *velocity* vector.

More generally, we can express this in terms of matrices. If we talk talk about
$R$ as the $N \times 2$ matrix of initial positions, and $V$ as the $N
\times 2$ matrix of initial velocities:

$$
R =
\begin{bmatrix}
x_1 & y_1 \\
x_2 & y_2 \\
\vdots & \vdots \\
x_N & y_N
\end{bmatrix}
$$

$$
V =
\begin{bmatrix}
v_{x1} & v_{y1} \\
v_{x2} & v_{y2} \\
\vdots & \vdots \\
v_{xN} & v_{yN}
\end{bmatrix}
$$

Then we can say that the state of the total system at time $t$ is given by $R +
V t$

Now, how can we find the time when all of the letters are aligned?

For this, we can make a *somewhat justified guess*: looking at the input data,
we see that things start out as "scattered", and end up in a clean clustered
arrangement. We know that the ending arrangement must be clustered fairly close
together because we only have a few hundred points in the input data set,
whereas the start times are all in the thousands or higher. And, once things get
clustered, they will also get un-clustered right away, because of the randomness
of the directions of motion.

This gives us a clue: if we can find the $t$ that will give us the $R + V t$
with the *least variance*, we are good to go! All of a sudden, this is now an
[optimization](https://en.wikipedia.org/wiki/Mathematical_optimization) problem.
Find the $t$ that minimizes the variance of x plus the variance of y. We can
find this by finding the formula for the sum of variances, taking the first
derivative, and setting it to zero.

The typical formula for finding the sum of variances of a matrix $M$ is to take
the [trace](https://en.wikipedia.org/wiki/Trace_(linear_algebra)) of the
[covariance matrix](https://en.wikipedia.org/wiki/Covariance_matrix),
$\mathrm{Tr} \left[ \left(M - \mu_M
\right)^T \left(M - \mu_M \right) \right]$. However, in this form, it's not too
fun to work with. That's because we have to re-compute the mean of of the
positions at every point, and things will get messy before they get clean.

Conceptually, however, we have a powerful tool: the [Center of Mass
frame](https://en.wikipedia.org/wiki/Center-of-momentum_frame). Essentially,
because our system has no external forces (and no net acceleration), we can
*perform a [Galilean
transform](https://en.wikipedia.org/wiki/Galilean_transformation)* into a frame
of reference where the center of mass is *fixed at the origin*, and *never
changes*. If we can do this, then we only need to compute $\mathrm{Tr}
\left(M^T M \right)$ (since we guarantee that the mean of $M$ is 0), which is
relatively easy peasy.

Because our system has points of all equal "mass", we can shift $R$ into
$\hat{R}$ ($R$ shifted into the center of mass frame) and $V$ into $\hat{V}$ by
just subtracting by the *initial* mean:

$$
\begin{aligned}
\hat{R} & = R - \mu_R \\
\hat{V} & = V - \mu_V
\end{aligned}
$$

This means that our formula for variance at time T is now simple to manipulate.
Because we now know that center of mass *is always zero*, we can compute the sum
of variance as:

$$
\lvert \Sigma(t) \rvert = \mathrm{Tr} \left[ \left( \hat{R} + \hat{V} t \right)^T \left( \hat{R} + \hat{V} t \right) \right]
$$

We can do some simplification, remembering that the trace distributes over
addition, and that $\mathrm{Tr} \left( X^T Y \right) = \mathrm{Tr} \left
(Y^T X \right)$:

$$
\lvert \Sigma(t) \rvert = \mathrm{Tr} (\hat{R}^T \hat{R})
                   + 2 \mathrm{Tr} (\hat{R}^T \hat{V}) t
                   + \mathrm{Tr} (\hat{V}^T \hat{V}) t^2
$$

Now, we want to minimize the sum of variances. So to do that, we can take the
first derivative with respect to $t$, and set it to be zero:

$$
\begin{aligned}
\frac{d}{d t} \lvert \Sigma(t) \rvert & = 2 \mathrm{Tr} (\hat{R}^T \hat{V})
                                  + 2 \mathrm{Tr} (\hat{V}^T \hat{V}) t \\
0 & = 2 \mathrm{Tr} (\hat{R}^T \hat{V}) + 2 \mathrm{Tr} (\hat{V}^T \hat{V}) t_f \\
t_f & = - \frac{\mathrm{Tr} (\hat{R}^T \hat{V})}{\mathrm{Tr}(\hat{V}^T \hat{V})}
\end{aligned}
$$

And just like that, we have a formula for $t_f$!

We can simplify this a little more by remembering that the trace of a matrix
multiplication is the sum of the dot products of the columns of the first matrix
by the rows of the second. That means we can write:

$$
t_f = - \frac{\sum_i \hat{\mathbf{r}}_i \cdot \hat{\mathbf{v}}_i}{\sum_i \hat{\mathbf{v}}_i \cdot \hat{\mathbf{v}}_i}
$$

Once we find this, we can plug into our original form, to find that our final
points are, in our un-shifted coordinates, $R + V t_f$. This is because Galilean
transformations leave time unchanged, unlike other frame transformations, like
the Lorentz transform. However, we have a simpler option: we could just leave
our answer in shifted coordinates as well, since we only care about the shape of
the result, and not the absolute position.

We can write this as a Haskell function, assuming we take in a list of
`V2 Double` for velocities and `V2 Double` for positions, from the
*[linear](https://en.wikipedia.org/wiki/Center-of-momentum_frame)* library:

``` haskell
-- | Shift so that centroid is at zero
centralize :: [V2 Double] -> [V2 Double]
centralize ps = map (subtract mean) ps
  where
    mean = sum ps L.^/ fromIntegral (length ps)
                -- ^ component-wise division

-- | Sum of dot products
sumOfDots :: [V2 Double] -> [V2 Double] -> Double
sumOfDots xs ys = sum (zipWith L.dot xs ys)

findWord
    :: [V2 Double]              -- ^ velocities
    -> [V2 Double]              -- ^ initial positions
    -> ([V2 Double], Double)    -- ^ points in word, and final time t
findWord (centralize->vs) (centralize->xs) = (final, t)
  where
    t     = negate (sumOfDots xs vs / sumOfDots vs vs)
    final = zipWith (\v x -> x + t L.*^ v) vs xs
```

We don't even need to round the answer --- we can directly make a scatter plot
of these points and read off what they look like :)

## Part 2

> Good thing you didn't have to wait, because that would have taken a long
> time - much longer than the `3` seconds in the example above.
>
> Impressed by your sub-hour communication capabilities, the Elves are curious:
> *exactly how many seconds would they have needed to wait for that message to
> appear?*

This one is just $t$, which we solved for in the last part! This time, we do
need to remember to `round` it before we submit.

## Message in the Stars

Optimization by finding the first derivative is a common tool in math that is
definitely under-utilized! In practice, unless we have a really clean system, we
won't be able to analytically "solve for zero" in most situations. However, this
system shows all of the signs of being well-behaved: the thing we are minimizing
is quadratic on our variable, so the first derivative will be linear on our
variable, making "solving for zero" very simple.

To do this in a clean way we:

1.  Represented our system as a matrix formula, giving us key linear algebra
    insights we could exploit.
2.  Saw that our problem is feasible, because our thing we are minimizing is
    quadratic in our variable, meaning the derivative is linear in our variable.
3.  Made this feasible by using a Galilean transform to shift things into the
    center-of-mass frame, so that the mean is *fixed* over the entire time span,
    and *set to zero*. This made the final solution simple enough to work out on
    a small sheet of notebook paper.

And isn't it cute that we use the *Galilean* transform, named after someone who
is famous for having studied the motion of astronomical bodies? Maybe that was a
subtle hint from the author of the challenges ;)

Anyway, I thought this was a fun twist on the typical Advent of Code challenges.
It's always fun when something that you might think can only be solved by
simulation turns out to have a closed-form solution...but even more fun when the
closed-form solution turns out to just be simple linear algebra:

$$
t_f = - \frac{\sum_i \hat{\mathbf{r}}_i \cdot \hat{\mathbf{v}}_i}{\sum_i \hat{\mathbf{v}}_i \cdot \hat{\mathbf{v}}_i}
$$

"It's just dot products all the way down."

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

