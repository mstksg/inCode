The Hamster Hotel: An Introduction to Control Theory (Part 2)

==============================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on October 1, 2013.
> [Read online!](https://blog.jle.im/entry/the-hamster-hotel-an-introduction-to-control-theory-2.html)

As we left it off in [part
1](/entry/the-hamster-hotel-an-introduction-to-control-theory), our elevator is
still in trouble. You have an elevator for your humble hotel that consists of a
plate being pushed up a shaft with a fountain of water where we can control the
fountain strength, but you don't really have any way to automate getting it to
go to just the right height.

You've tried writing down the proper fountain strengths for every floor, but
changes in weight of the hamsters and lots of other factors make this
unreliable. You've tried mathematically analyzing and accounting for all of
these other factors, but not only is it impractical, but it can't possibly
account for dynamic changes in the system.

Let's take a look at a possible solution that might have some promise.

## I Detect a Clue

You decide to put aside your elevator problems for now and focus on other
aspects of your business. You do very well, actually, and your hotel grows to
large acclaim in the hamster world. Still, you can't help but be embarrassed
every day when you explain that your elevator is still not adequately
controllable.

You gain enough revenue that you decide to try things again with a human factor.
You hire an elevator boy.

Your scheme is simple: have a little bell attached at the point where every
elevator reaches the perfect height. Your elevator boy will turn the knob up,
up, up until he hears the bell, and then stop it right after.

The same thing works for going down -- tell him to turn the knob down, down
until he hears a bell.

And suddenly, things seem to click.

### The Key

This system accounts for all of the problems we ran into before. We have a human
here who can account for everything. He makes all of the adjustments on the fly.
He doesn't need to know any exact percentages ... he doesn't need to worry about
water pressure or friction or hamster weights, or any of that stuff. All he
needs to know is "should I be increasing the flow, decreasing it, or leaving it
alone?" So maybe the heavier hamsters get to their destinations slower --- so
what? You've discovered something amazing.

The key difference here is the *detection*. You are no longer thinking of a
static system that will never change --- you are constantly adjusting on the
fly. You are doing things, detecting the reactions, and responding to those
detections. In control theory, this difference is what we call **feedback**.

***Feedback* is the process of letting what you *observe* from your changes
affect what you *change next, on the fly***, which then affects what you
*observe*, etc. etc.

And *this* is the key.

In control theory, this would be known as moving from an **open loop** (where we
don't let what we observe affect what we change on-the-fly) to a **closed loop**
(where we do).

Also note one other fundamental shift we just made: Based on what we observe, we
*change*. We no longer are finding out what we should *set* --- we are instead
figuring out how we should *change*. We don't care about 10%, 20%, 30%, etc.
anymore --- we only care about twisting the knob *left* or *right*.

### Simple Improvements

Still, this system isn't perfect. Sometimes, if you forget to feed your elevator
boy, he will accidentally miss the bell and overshoot slightly. No big deal. You
attach some [very simple electronics](http://www.snapcircuits.net/) to your
elevator shaft so that a *red* light comes on if the elevator is too low, a
*blue* light comes on if it's too high, and a *green* light if it's just right.

Finally, with all of the electronics installed, your elevator boy knows three
rules: increase the flow if the light is red, decrease the flow if it's blue,
and leave it constant if it's green.

Of course, you were hoping (from the beginning) to be able to do have this be
done without human intervention.

One day, you figure out that you can do the entire thing electronically. To save
money, you fire your elevator boy and set up a motor to twist the knob. In
addition to the lights, your circuit will also trigger the motor to loosen the
valve (spin right) if the car is too low, tighten it (spin left) if it's too
high, and stop the motor when it is just right.

Congratulations, you now have your very first automated closed feedback loop,
known as the [bang--bang
controller](http://en.wikipedia.org/wiki/Bang%E2%80%93bang_control)[^1].

With this in hand, you are sure to have no obstacles to firmly establishing your
hamster hotel empire.

### Problems Again

Of course, our bang--bang controller is (as you might expect from the crudeness
of the name) not exactly the be-all and end-all solution that control theory
exists to provide.

Let's look at its shortcomings even in our simple scenario.

In reality, the light will almost never be green for long. If a platform is
properly aligned as a hamster steps on it, it will be nudged off balance. The
light will immediately turn red, the motor will immediately adjust the jet and
the car at full speed. This is felt as a "jitter" (Which, as I have on good
word, is a particularly unpleasant sensation for a hamster.)

Could you possibly make the "adjustment speed" slower? That is, could you slow
down the speed that your motor runs at, so that the adjustment is slow enough as
to not be felt as a rapid jitter?

Well, you can! But if your motor is slow, it will actually take much too long to
ever move anywhere. Perhaps the proper non-jittering motor speed is the same
speed that would cause the car to take an hour to move up one story. Not
acceptable!

You either jitter, or you take too long to move anywhere. Whatever motor speed
you choose will always have one problem or the other.

Furthermore, here we assume that our motor can instantly react to the changes in
the red/blue/green lights. However, real-world motors can't simply change their
direction immediately. Have you ever tried getting a car going 60 mph forwards
to move 60 mph backwards instantly?

Imagine applying this, then, to the elevator. It'll move up, up, up, then notice
that it's at the right level. But before it can stop, it's already too high. It
starts turning the motor the other way, to go down, down at the same speed ...
it reaches the right level, but by the time it can stop, it's too low.

This idea of *overshoot* will cause your elevator car to forever go up and down,
bouncing up and down without ever settling on the green zone even once. This is
because your motor only has one speed, and however much you overshoot going up,
it'll be exactly as much as you overshoot going down, and as much as you
overshoot going up the next time.

For now ... you might just have to rehire your elevator boy.

## The Hamster Hole Grows Deeper

Are these the last of our problems standing in the way of a fully automated
control system? Well, if you've noticed one trend in this post, it might be that
as soon as we conquer one problem ... many others inevitably pop up. Any simple
solution to these that you can think of now will have its own share of issues
and problems.

However, believe it or not, one day we will finally reach an end to this rat
race and arrive at what is today known as the canonical "best" compromise for
our system --- the system that deals with all of the problems mentioned here,
and nearly all of the problems that come up with the naive solutions of the ones
we face now. The best we got. Not perfect --- and not the best for many specific
systems --- but the best overall, general scheme for problems of this particular
type.

I'm saying this to prevent you from being weary. We may have come a long way,
and there may be much more to tackle, but fear not --- there is a satisfying end
to this hamster hole.

But first! How will we solve these fundamental problems --- jittering and
oscillation --- of the bang--bang controller?

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

[^1]: Technically, it is a modified version of the bang--bang controller with an
    option for "don't do anything" --- a "bang--bang--shh". A true bang--bang
    controller would not have the "green light" option.

