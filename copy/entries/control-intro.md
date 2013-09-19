The Hamster Hotel: An Introduction to Control Theory
====================================================

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
is control theory, and explore (without any complex mathematics) just how much
genius goes into those simple things we all take for granted.

Welcome to *the Hamster Hotel*.

The Hamster Hotel
-----------------

You run a hamster hotel (a hotel for hamsters) and you have a problem.
Elevators. Your hamster guests need to get from one story to another.  They
walk into the elevator, the elevator takes them to the right floor, and they
walk off.

Due to an unfortunate mix-up on your contractor's part, your only option is
elevators operated via water jets (like the ones at water fountains).  You can
control how hard the jets push with a valve that you can open or shut
partially. The water jets push little plates in chutes up, and the hamsters
hitch along for a ride.

Let's say you can control how much the valve is open by twisting a knob (kind
of like a garden hose).  The more open the valve is, the higher the jet goes
and the higher the elevator car.

How do you find out how much you have to open the valve up to get the car to
go *exactly* to the second story?

That's Easy! (?)
----------------

Let's try out some simple solutions.  What would you do first?

### Measure it out

The most straightforward solution would be to get a ruler and do some good ol'
empirical science-ing.  You'll open the valve until you get to each floor.
Then, you'll write down the amount that you've twisted the knob (10%, 20%?).
Then, whenever you want to go to a floor, you'll look it up on your table,
twist the knob to the corresponding value, and hamsters rejoice!

Time to do this.  Ground floor?  0% twisted open.  Second floor?  3%.  Third
floor?  12%.  Fourth floor?  27%.  To get to the roof, you need to only set it
to 48% open.

Now that you have completely science'd the situation, it's time to start using
your elevator.  You load up a hamster for the inaugural ride.

#### The Plight of the Hamster

First problem.  With a hamster actually inside the elevator, the whole thing
is heavier, and a 3% open valve just doesn't cut it anymore to get it up even
one story.  You find out that have to crank it up to 5%.  Third floor now
takes a 20% opened valve.

This is a disaster!  Not only is your valued guest disappointed, but this
fiasco has also rendered the entire first percentages table useless.  You
might try to change the table to account for one hamster.  But what if your
guest takes along also the missus as well?  Should we keep two tables -- one
for a one-hamster car, and one for a two-hamster car?

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
right?[^itsnot]

[^itsnot]: It's not

So, you whip out your precious equations (which you hold to be worth at least
the $200,000 you spent to learn them).  Sure enough, you have enough equations
in your tool belt to describe and model the physical system almost perfectly.
There are some constants you need to figure out --- the friction in the shaft,
for example, or the width of your pipes --- and after that, given the weight
of the load, the water pressure per degree of knob twist (which may change
depending on the day), the desired height, the ambient room temperature, the
heat of the elevator shaft, the temperature inside the pipes, the current air
pressure, the humidity, the state of the lubrication in the shaft ... you can
predict exactly how much you need to twist that water knob.

You write a little program on your TI-89 to calculate the right amount of
twist for all of those parameters, and you are good to go!

#### Does Not Compute

You see where the flaw in the plan is?

Of course: it's very rude to ask a hamster for her weight!

Also, aside from that, there are just too many parameters you have to
constantly monitor, measure, and maintain. If you don't get it all exactly
right, your hamsters (who are extraordinarily squeamish guests) won't go
anywhere near the elevator, or become stuck inside. Certainly not the path to
gaining a five star reputation!

Even if you somehow managed to find all of the proper parameters to a "good
enough" level every time ... in general, it's unrealistic to expect to be able
to derive an analytic solution to all of your problems.

All these things aside, there are even graver issues that plague this system.

What if the parameters change in mid-trip?  What if the water pressure
suddenly dropped?  What if the lubrication was different along the length of
the shaft?  What if a hamster jumps onto the car last-minute?

Certainly if any of these things happened, our poor hamster guests would
undoubtedly fall straight to their doom.

The problem with this system is that it's simply not **dynamic**.

Sure, you could make a model that accounts for all of the changes possible,
but is that really practical?

For a genius the likes of which the world has never seen, it may be
*possible*.

But ... there *has* to be a better way.

I Detect a Clue
---------------

Despite your elevator problems, your hamster guests appear perfectly content,
and your hotel grows to large acclaim in the hamster world.  Still, you can't
help but be embarrassed every day when you explain that your elevator is still
not adequately controllable.

You gain enough revenue that you decide to try things again with a human
factor.  You hire an elevator boy.

Your scheme is simple: have a little bell attached at the point where every
elevator reaches the perfect height.  Your elevator boy will turn the knob up,
up, up until he hears the bell, and then stop it right after.

The same thing works for going down -- tell him to turn the knob down, down
until he hears a bell.

And suddenly, things seem to click.

### The Key

This system accounts for all of the problems we ran into before.  We have a
human here who can account for everything.  He makes all of the adjustments on
the fly.  He doesn't need to know any exact percentages ... he doesn't need to
worry about water pressure or friction or hamster weights, or any of that
stuff.  All he needs to know is "should I be increasing the flow, decreasing
it, or leaving it alone?"  So maybe the heavier hamsters get to their
destinations slower --- so what?  You've discovered something amazing.

The key difference here is the *detection*.  You are no longer thinking of a
static system that will never change --- you are constantly adjusting on the
fly.  You are doing things, detecting the reactions, and responding to those
detections.  In control theory, this difference is what we call
**feedback**.

***Feedback* is the process of letting what you *observe* from your changes
affect what you *change next, on the fly***, which then affects what you
*observe*, etc. etc.

And *this* is the key.

In control theory, this would be known as moving from an **open loop** (where we
don't let what we observe affect what we change on-the-fly) to a
**closed loop** (where we do).

Also note one other fundamental shift we just made: Based on what we observe,
we *change*.  We no longer are finding out what we should *set* --- we are
instead figuring out how we should *change*.  We don't care about 10%, 20%,
30%, etc. anymore --- we only care about twisting the knob *left* or *right*.

### Simple Improvements

Still, this system isn't perfect.  Sometimes, if you forget to feed your
elevator boy, he will accidentally miss the bell and overshoot slightly.  No
big deal.  You attach some [very simple electronics][snapcircuits] to your
elevator shaft so that a *red* light comes on if the elevator is too low, a
*blue* light comes on if it's too high, and a *green* light if it's just
right.

[snapcircuits]: http://www.snapcircuits.net/

Finally, with all of the electronics installed, your elevator boy knows three
rules: increase the flow if the light is red, decrease the flow if it's blue,
and leave it constant if it's green.

One day you realize that you don't even need an elevator boy anymore; you can
do everything electronically.  To save money, you fire your elevator boy and
set up a motor to twist the knob.  Instead of bothering with the lights, your
circuit will directly trigger the motor to loosen the valve (spin right) if
the car is too low, tighten it (spin left) if it's too high, and stop the
motor when it is just right.

Congratulations, you now have your very first automated closed feedback
loop, known as the [bang--bang controller][bangbang][^bangbangnote].

With this in hand, you are sure to have no obstacles to firmly establishing
your hamster hotel empire.

[bangbang]: http://en.wikipedia.org/wiki/Bang%E2%80%93bang_control

[^bangbangnote]: Technically, it is a modified version of the bang--bang
    controller with an option for "don't do anything".  A true bang--bang
    controller would not have the "green light" option.

### Problems Again

Of course, our bang--bang controller is (as you might expect from the
crudeness of the name) not exactly the be-all and end-all solution that
control theory exists to provide.

Let's look at its shortcomings even in our simple scenario.

In reality, the light will almost never be green for long.  If a platform is
properly aligned as a hamster steps on it, it will be nudged off balance.  The
light will immediately turn red, the motor will immediately adjust the jet and
the car at full speed.  This is felt as a "jitter" (Which, as I have on good
word, is a particularly unpleasant sensation for a hamster.)

Could you possibly make the "adjustment speed" slower?  That is, could you
slow down the speed that your motor runs at, so that the adjustment is slow
enough as to not be felt as a rapid jitter?

Well, you can!  But if your motor is slow, it will actually take much too long
to ever move anywhere.  Perhaps the proper non-jittering motor speed is the
same speed that would cause the car to take an hour to move up one story.  Not
acceptable!

You either jitter, or you take too long to move anywhere.  Whatever motor
speed you choose will always have one problem or the other.

Furthermore, here we assume that our motor can instantly react to the
changes in the red/blue/green lights.  However, real-world motors can't simply
change their direction immediately.  Have you ever tried getting a car going
60 mph forwards to move 60 mph backwards instantly?

Imagine applying this, then, to the elevator.  It'll move up, up, up, then
notice that it's at the right level.  But before it can stop, it's already too
high.  It starts turning the motor the other way, to go down, down at the same
speed ... it reaches the right level, but by the time it can stop, it's too
low.

This idea of *overshoot* will cause your elevator car to forever go up and
down, bouncing up and down without ever settling on the green zone even once.
This is because your motor only has one speed, and however much you overshoot
going up, it'll be exactly as much as you overshoot going down, and as much as
you overshoot going up the next time.

For now ... you might just have to rehire your elevator boy.

The Hamster Hole Grows Deeper
-----------------------------

Are these the last of our problems standing in the way of a fully automated
control system?  Well, if you've noticed one trend in this post, it might be
that as soon as we conquer one problem ... many others inevitably pop up.  Any
simple solution to these that you can think of now will have its own share of
issues and problems.

However, believe it or not, one day we will finally reach an end
to this rat race and arrive at what is today known as the canonical "best"
compromise for our system --- the system that deals with all of the problems
mentioned here, and nearly all of the problems that come up with the naive
solutions of the ones we face now.  The best we got.  Not perfect --- and not
the best for many specific systems --- but the best overall, general scheme for
problems of this particular type.

I'm saying this to prevent you from being weary.  We may have come a long way,
and there may be much more to tackle, but fear not --- there is a satisfying
end to this hamster hole.

But first!  How will we solve these fundamental problems --- jittering and
oscillation --- of the bang--bang controller?



