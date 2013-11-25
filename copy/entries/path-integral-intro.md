The Quantum Mechanical Feynman Path Integral Problem
====================================================

Categories
:   Physics
:   Computation
Tags
:   numerical methods
Series
:   Path Integral Monte Carlo
CreateTime
:   2013/11/19 22:05:34
PostDate
:   Never
Identifier
:   path-integral-intro

Happy to get a chance to write about my first love, Physics and Computational
Physics.  In this series we will be going over many subjects in both physics
and computational techniques, including the Lagrangian formulation of
classical mechanics, basic principles of quantum mechanics, the Feynman Path
Integral formatulion of quantum mechanics, the Metropolis-Hastings Monte Carlo
method, dealing with entropy and randomness in a pure language, and general
principles in numerical computation!  Fun stuff, right?

The end product will be a tool for deriving the wave state solutions of
arbitrary quantum systems, which is somewhat of a big deal in any field that
runs into quantum effects (which is basically every modern field).  But the
real goal will be to hopefully impart some insight that can be applied to
broarder and more abstract applications.  I am confident that these techniques
can be applied to many problems in computation to great results.

I'm going to assume little to no knowledge in Physics and a somewhat
intermediate working knowledge of programming.  We're going to be working in
both my favorite imperative language and my favorite functional language.

In this first post I'm just going to go over the basics of the physics before
we dive into the simulation.  Here we go!

Mechanics
---------

### Newtonian Mechanics

Mechanics has always been a field in physics that has held a special place in
my heart.  It is most likely the field most people are first exposed to in a
physics course.  To me, there really is no more fundamental and pure form of
physics.  I mean...it's the study of how things move under forces.  How can
you get any deeper to the heart of physics than that?

When most people think of mechanics, they think of $F = m a$, inertia, and
that every reaction has an equal and opposite re-action.  These are Newton's
"Laws of Motion" and they provide what can be referred to as a "state-updating
function": Given a state of the world at time $t_0$, Newton's laws can be used
to "generate" the state of the world at time $t_0 + \Delta t$.

This sounds pretty useful, but it wasn't long before physicists began wishing
they had other tools with which to study the mechanics of certain systems.
Newton's equations worked very well for the cases that made it famous, but
were surprisingly unuseful, impractical, or clumsy in many others.

So it was almost exactly one hundred years after Newton's laws that two people
named [Lagrange][] and [Euler][] (who is the "e" in $e$) followed a wild hunch
that ended up paying off and being the solution everyone had been waiting for.

[Lagrange]: http://en.wikipedia.org/wiki/Joseph-Louis_Lagrange
[Euler]: http://en.wikipedia.org/wiki/Leonhard_Euler

### Lagrangian Mechanics

The main idea was this: I tell you an object's location at time $t_0$, and its
location later at time $t_1$, and the potential energy field.  What path did
that object take to get from point A to point B?  (A potential energy field is
sort of an energy landscape that objects will tend to "fall downwards"
on...like rolling down hills...and can be used to describe the forces in many
systems)

A pretty open question, right?  You don't really have that much information to
go off of.  You just know point A and point B.  It could have taken any path,
for all we know!

The solution to this problem is actually rather unexpected.  Consider every
single path/curve from point A to point B.  Every single one.  Now, assign each path
a number known as the **Action**, which is a rather unremarkable propery of
the path.  The action of a path is the sum of an unremarkable function of
every point in the path.  In our universe, this function is $T - U$ --- for a
given path, add up $T$ ($\frac{1}{2} m v^2$) and $U$ (the potential field's
value at that point) for every point in the path.  That's the action.

Anyways, think about every possible path.  Calculate the action for each one.
The path that the object takes is *the path with the lowest action*

It's almost as if the object "does this math" in its head: "I'm going to go
from here to there...let me calculate which path I can take has the lowest
action.  Okay, got it!"

