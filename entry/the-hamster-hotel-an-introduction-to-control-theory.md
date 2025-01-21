The Hamster Hotel: An Introduction to Control Theory (Part 1)

==============================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on September 26, 2013.
> [Read online!](https://blog.jle.im/entry/the-hamster-hotel-an-introduction-to-control-theory.html)

Over this summer my work has been in the field of control theory. I must admit
that I at first found it a little odd that the idea of "controlling something"
--- say, the temperature of a room or the speed of a fan --- could be an
extremely deep/rich mathematical and engineering discipline.

This series should hopefully be an enlightening walk through this wonderful
world that is control theory, and explore (without any complex mathematics) just
how much genius goes into those simple things we all take for granted.

Welcome to **the Hamster Hotel**.

## The Hamster Hotel

You run a hamster hotel (a hotel for hamsters) and you have a problem.
Elevators. Your hamster guests need to get from one story to another. They walk
into the elevator, the elevator takes them to the right floor, and they walk
off.

Due to an unfortunate mix-up on your contractor's part, your only option is
elevators operated via water jets (like the ones at water fountains). You can
control how hard the jets push with a valve that you can open or shut partially.
The water jets push little plates in chutes up, and the hamsters hitch along for
a ride.

Let's say you can control how much the valve is open by twisting a knob (kind of
like a garden hose). The more open the valve is, the higher the jet goes and the
higher the elevator car.

How do you find out how much you have to open the valve up to get the car to go
*exactly* to the second story?

## That's Easy! (?)

Let's try out some simple solutions. What would you do first?

### Measure it out

The most straightforward solution would be to get a ruler and do some good ol'
empirical science-ing. You'll open the valve until you get to each floor. Then,
you'll write down the amount that you've twisted the knob (10%, 20%?). Then,
whenever you want to go to a floor, you'll look it up on your table, twist the
knob to the corresponding value, and hamsters rejoice!

Time to do this. Ground floor? 0% twisted open. Second floor? 3%. Third floor?
12%. Fourth floor? 27%. To get to the roof, you need to only set it to 48% open.

Now that you have completely science'd the situation, it's time to start using
your elevator. You load up a hamster for the inaugural ride.

#### The Plight of the Hamster

First problem. With a hamster actually inside the elevator, the whole thing is
heavier, and a 3% open valve just doesn't cut it anymore to get it up even one
story. You find out that have to crank it up to 5%. Third floor now takes a 20%
opened valve.

This is a disaster! Not only is your valued guest disappointed, but this fiasco
has also rendered the entire first percentages table useless. You might try to
change the table to account for one hamster. But what if your guest takes along
also the missus as well? Should we keep two tables -- one for a one-hamster car,
and one for a two-hamster car?

But we mustn't stereotype --- hamsters all have different weights. And what if
the guests had luggage? Maybe we can measure the luggage, and create a new table
for every possible total weight combination?

What if one day the water pressure of the water jet drops, so less water comes
out when the knob is twisted? Do you have to remeasure everything all over
again?

Clearly, there must be a better way.

### The Analyzer

You just graduated with your bachelor's degree in Physics. You know all about
the laws governing force, gravity, water, pressure, potential energy, friction,
turbulence, hamster physiology ... all of that stuff. You memorized all of the
equations, because that's what Physics is all about, right?[^1]

So, you whip out your precious equations (which you hold to be worth at least
the \$200,000 you spent to learn them). Sure enough, you have enough equations
in your tool belt to describe and model the physical system almost perfectly.
There are some constants you need to figure out --- the friction in the shaft,
for example, or the width of your pipes --- and after that, given the weight of
the load, the water pressure per degree of knob twist (which may change
depending on the day), the desired height, the ambient room temperature, the
heat of the elevator shaft, the temperature inside the pipes, the current air
pressure, the humidity, the state of the lubrication in the shaft ... you can
predict exactly how much you need to twist that water knob.

You write a little program on your TI-89 to calculate the right amount of twist
for all of those parameters, and you are good to go!

#### Does Not Compute

You see where the flaw in the plan is?

Of course: it's very rude to ask a hamster for her weight!

Also, aside from that, there are just too many parameters you have to constantly
monitor, measure, and maintain. If you don't get it all exactly right, your
hamsters are likely to get stuck inside somehow. Certainly not the path to
gaining a five star reputation!

Even if you somehow managed to find all of the proper parameters to a "good
enough" level every time ... in general, it's unrealistic to expect to be able
to derive an analytic solution to all of your problems.

All these things aside, there are even graver issues that plague this system.

What if the parameters change in mid-trip? What if the water pressure suddenly
dropped? What if the lubrication was different along the length of the shaft?
What if a hamster jumps onto the car last-minute?

Certainly if any of these things happened, our poor hamster guests would
undoubtedly fall straight to their doom.

The problem with this system is that it's simply not **dynamic**.

Sure, you could make a model that accounts for all of the changes possible, but
is that really practical?

For a genius the likes of which the world has never seen, it may be *possible*.

But ... there *has* to be a better way.

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

[^1]: It's not

