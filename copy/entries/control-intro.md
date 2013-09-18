Hamster Hotel: An Introduction to Control Theory
================================================

Categories
:   Engineering
Tags
:   control theory
CreateTime
:   2013/09/17 15:35:42
PostDate
:   2013/09/19 16:23:11
Identifier
:   control-intro

Over this summer my work has been in the realm of control theory.  I've found
that it's not exactly straightforward to explain what control theory actually
*is* to people who haven't heard about it before.

It's a little odd that the idea of "controlling something" -- say, the
temperature of a room or the speed of a fan -- is an extremely deep/rich
mathematical and engineering discipline.

Hopefully this series of posts will walk you through this wonderful world that
is control theory, and explore just how much work goes into those simple
things we all take for granted.

Welcome to the *Hamster Hotel*.

The Hamster Hotel
-----------------

You run a hamster hotel (a hotel for hamsters) and you have a problem.
Elevators. Your hamster guests need to get from one storey to another.  They
walk into the elevator, the elevator takes them to the right floor, and they
walk off.

Due to an unfortunate mix-up on your contractor's part, your only option is
elevators operated via water jets (like the ones at water fountains).  You can
control how hard the jets push with a valve you can open or shut partially.
The water jets push little plates in chutes up, and the hamsters hitch along
for a ride.

Let's say you can control how much the valve is open by twisting a knob (kind of
like a garden hose).

How do you know how much you have to open the valve up to go *exactly* to the
second story?

That's Easy! (?)
----------------

Let's try out some simple solutions.  What would you do first?

### Measure it out

The most straightforward solution would be to get a ruler and do some good ol'
empirical science-ing.  You'll open the valve until I get to each floor. Then,
You'll write down the amount that I've twisted the knob (10%, 20%?). Then,
whenever you want to go to a floor, you'll look it up on your table, twist the
knob to the corresponding value, and hamsters rejoice!

Time to do this.  Ground floor?  0% twisted open.  Second floor?  3%.  Third
floor?  12%.  Fourth floor?  27%.  To get to the roof, you need to only set it
to 48% open.

Now that I have completely science'd the situation, it's time to start using
my elevator.  You load up a hamster for the inaugural ride.

#### The Plight of the Hamster

First problem.  With a hamster actually inside the elevator, the whole thing
is heavier, and a 3% open valve just doesn't cut it anymore to get it up even
one storey.  You find out that have to crank it up to 5%.  Second floor now
takes a 20% opened valve.

This is a disaster!  Not only is your valued guest disappointed, this fiasco
has also rendered the entire first table useless.  You might try to change my
table to account for one hamster.  But what if your guest takes along also the
Mrs. as well?  Should we keep two tables -- one for a one-hamster car, and one
for a two-hamster car?

But we musn't stereotype --- hamsters all have different weights.  And what if
the guests had luggage?  Maybe we can measure the luggage, and create a new
table for every possible total weight combination?

What if one day the water pressure of the water jet drops, so less water comes
out when the knob is twisted?  Do you have to remeasure everything all over
again?

Clearly, there must be a better way.

### The Analyzer

You just graduated with your bachelor's degree in Physics.  You know all about
the laws governing force, gravity, water, pressure, potential energy,
friction, turbulence, hamster physiology ... all of that stuff.  You memorized
all of the equations, because that's what Physics is all about,
right?[^itisnt]

[^itisnt]: It isn't

So, you whip out your precious equations (which you hold to be worth at least
the $200,000 you spent to learn them).  Sure enough, you have enough equations
in your toolbelt to describe and model the physical system almost perfectly.
There are some constants you need to figure out --- the friction on the
platforms, for example, or the width of your pipes --- and after that, given
the weight of the load, the water pressure per degree of knob twist (which may
change depending on the day), the desired height, the ambient room
temperature, the heat of the elevator shaft, the temperature inside the pipes,
the current air pressure, the humidity, the state of the lubrication in the
shaft ... you can predict exactly how much you need to twist that water knob.

You write a little program on your TI-89 to calculate the right amount of
twist for all of those parameters, and you are good to go!

#### Does Not Compute

You see now where the flaw in the plan is?

It's very rude to ask a hamster for her weight!

Also, aside from that, there are just to many parameters you have to
constantly monitor, measure, and maintain things that are out of your control.
If you don't get it all exactly right, your hamsters (who are extraordinarily
squeamish guests) won't go anywhere near the elevator, or become stuck inside.
Certainly not the path to gaining a five star reputation!

Even if you somehow managed to find all of the proper parameters to a "good
enough" level every time ... in general, it's unrealistic to expect to be able
to derive an analytic solution to all of your problems.

Also, there are some things this system cannot account for ---

What if the parameters change in mid-trip?  What if the water pressure
suddenly dropped?  What if the lubrication was different along the length of
the shaft?  What if a hamster jumped into the shaft last-minute?

Certainly if any of these things happened, our poor hamster guests would
undoubtedly fall straight to their deaths.

The problem with this system is that it's simply not **dynamic**.

Sure, you could make a model that accounts for all of the changes possible,
but is that really practical?

For a genius the likes of which the world has never seen, it may be
*possible*.

But ... there has to be a better way.







