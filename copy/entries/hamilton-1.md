---
title: Hamiltonian Dynamics in Haskell
categories: Haskell
tags: functional programming, haskell, physics, numerical methods, dependent types
create-time: 2016/12/08 15:05:10
date: never
identifier: hamilton-1
slug: hamiltonian-dynamics-in-haskell
---

As promised in my [*hamilton* introduction post][intro], I'm going to go over
implementing of the *[hamilton][]* library using *[ad][]* and dependent types.

[intro]: https://blog.jle.im/entry/introducing-the-hamilton-library.html
[hamilton]: http://hackage.haskell.org/package/hamilton
[ad]: http://hackage.haskell.org/package/ad

This post will be a bit heavy in some mathematics and Haskell concepts.  The
expected audience is intermediate Haskell programmers, and no previous
knowledge of dependent types is expected.

The mathematics and physics are "extra" flavor text and could potentially be
skipped, but you'll get the most out of this article if you have basic
familiarity with:

1.  Basic concepts of multivariable calculus (like partial and total
    derivatives).
2.  Concepts of linear algebra (like dot products, matrix multiplication, and
    matrix inverses)

No physics knowledge is assumed, but knowing a little bit of first semester
physics would help you gain a bit more of an appreciation for the end result!

Hamiltonian Mechanics
---------------------

As mentioned in the previous post, Hamiltonian mechanics is a re-imagining of
dynamics and mechanics (think "the world post-$F = m a$") that not only opened
up new doors to solving problems in classical, but also ended up being the
right angle of viewing the world to unlock statistical mechanics and
thermodynamics, and later even quantum mechanics.

Hamiltonian mechanics lets you parameterize your system's "position" in
arbitrary ways (like the angle of rotation, for pendulum problems) and then
posits that the full state of the system exists in something called *phase
space*, and that the system's dynamics is its motion through phase space that
is dictated by the geometry of the *Hamiltonian* of that phase space.

The system's *Hamiltonian* is a $\mathbb{R}^{2n} \rightarrow \mathbb{R}$
function on the phase space (where $n$ is the number of coordinates
parameterizing your system) to $\mathbb{R}$.  And, for a time-independent
system, the picture is quite simple: the system moves along the *contour lines*
of the *Hamiltonian* -- the lines of equal "height".

![Example of contour lines of a $\mathbb{R}^2 \rightarrow \mathbb{R}$ function -- the elevation of land.  From the [Ordinace Survey][] website.](/img/entries/hamilton/contour-lines.jpg "Contour lines")

[Ordinace Survey]: https://www.ordnancesurvey.co.uk/blog/2015/11/map-reading-skills-making-sense-of-contour-lines/

In the example above, if we imagine that phase space is the 2D location, then
the *Hamiltonian* is the mountain.  And for a system dropped anywhere on the
mountain, its motion would be along the contour lines.  For example, if a
system started somewhere along the 10 contour line, it would begin to oscillate
the entire phase space along the 10 contour line.[^time-dependent]

[^time-dependent]: The picture with a time-dependent Hamiltonian is different,
but only slightly.  In the time-dependent case, the system still *tries* to
move along contour lines at every point in time, but the mountain is constantly
changing underneath it and the contour lines keep on shifting underneath it.
Sounds like life!

*Every* [smooth][] $\mathbb{R}^{2n} \rightarrow \mathbb{R}$ function on phase
space can be used as a Hamiltonian to describe the physics of some system.  So,
given any "mountain range" on phase space, any "elevation map" or real-valued
function on phase space, you can treat it as a description of the dynamics of
some physical system.

[smooth]: https://en.wikipedia.org/wiki/Smooth_jazz

The *trick*, then, to using Hamiltonian dynamics to model your system, is:

1.  Finding the phase space to describe your system.  This can be done based on
    any continuous parameterization of your system ("generalized coordinates"),
    like angles of pendulums and so on.

2.  Finding the Hamiltonian on that phase space to describe your system.

And then Hamilton's dynamics will give you the rest!  All you do is "follow the
contour lines" on that Hamiltonian!


### Phase Space

I've sort of hand-waved away describing what phase space is.  The only thing
I've really said in detail is that if your system's state has $n$ parameters,
then the corresponding phase space is $2n$-dimensional (and that Hamiltonian
mechanics is somehow about systems moving around in phase space).

*Phase space* is a $2n$-dimensional space consisting of:

1.  All of the current values of the $n$ parameters ("generalized coordinates")
2.  All of the current "generalized momenta" of those $n$ parameters

So if you were parameterizing your pendulum system by, say, the angle of the
pendulum, the phase space would be the current angle of the pendulum along with
the current "generalized momentum" associated with the angle of the pendulum.

What exactly *is* generalized momentum?  Well, we'll go over calculating it
eventually.  But what does it represent...*physically*?

I could give you some spiel here about the underlying Lie algebra of the Lie
group associated with the generalized coordinates, but I don't think that it
would be very intuitively appealing in a physical sense.  It'd also be out of
the scope of the math prerequisites that I promised I'd stick to going into
this post!

But, what I can say is that the generalized momenta associated with ("conjugate
to") certain sets of familiar coordinates yield things that we typically call
"momenta":

1.  The momentum conjugate to normal Cartesian coordinates is just our normal
    run-of-the-mill *linear momentum* (in the $\mathbf{p} = m \mathbf{v}$) from
    first semester physics.

2.  The momentum conjugate to the angle $\theta$ in polar coordinates is
    *angular momentum* ($\mathbf{L} = m \mathbf{r} \times \mathbf{v}$, or $L = m r \mathbf{\omega}$)
    from first semester physics.

So maybe this can help you feel comfortable with calling it "generalized
momenta", in the sense that it's our normal momentum (for linear and polar
coordinates) generalized to arbitrary coordinates.

### Hamiltonian Dynamics

I've explained Hamiltonian dynamics for time-independent Hamiltonians as
"follow the contour lines", but I didn't really say how quickly to move along
the contour lines and in what direction (clockwise, or counter-clockwise?
left, or right?).  The actual equations of motion are:

$$
\dot{q} = \frac{\partial}{\partial p_q} \mathcal{H}(\mathbf{q},\mathbf{p})
$$

$$
\dot{p_q} = - \frac{\partial}{\partial q} \mathcal{H}(\mathbf{q},\mathbf{p})
$$

Which holds for every generalized coordinate $q$, where $p_q$ is the momentum
conjugate to that coordinate.  $\mathcal{H}$ is the Hamiltonian function,
$\dot{q}$ is the rate of change of $q$, and $\dot{p_q}$ is the rate of change
of $p_q$.

Essentially, these give you "updating functions" for $q$ and $p_q$ -- given
$\mathcal{H}(\mathbf{q},\mathbf{p})$, you have a way to "update" the particle's
position in phase space.  Just take the partial derivatives of $\mathcal{H}$ at
every step in time!

