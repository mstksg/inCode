---
title: "\"Interpreters a la Carte\" in Advent of Code 2017 Duet"
categories: Haskell
series: Beginner/Intermediate Haskell Projects
tags: functional programming, haskell, types, lens
create-time: 2018/01/11 16:28:18
date: none
identifier: duet
slug: interpreters-a-la-carte-duet
---

This post is just a fun one exploring a wide range of techniques that I applied
to solve the Day 18 puzzles of this past year's great [Advent of Code][aoc].
The puzzles involved interpreting an assembly language on an abstract machine.
The neat twist is that Part A gave you a description of *one abstract machine*,
and Part B gave you a *different* abstract machine to interpret the *same
language* in.  This twist (one language, but different interpreters/abstract
machines) is basically one of the textbook applications of the *interpreter
pattern* in Haskell and functional programming, so it was fun to implement my
solution in that pattern --- the assembly language source was "compiled" to an
abstract monad once, and the difference between Part A and Part B was just a
different choice of interpreter.

[aoc]: http://adventofcode.com/2017

Even more interesting is that the two machines are only "half different" --
there's one aspect of the virtual machines that are the same between the two
parts, and aspect that is different.  This means that we can apply the "data
types a la carte" technique in order to mix and match isolated components of
virtual machine interpreters, and re-use code whenever possible in assembling
our interpreters for our different machines!

This blog post will not necessarily be a focused tutorial on this trick, but
rather an explanation on my solution centered around this pattern, hopefully
providing insight on how I approach and solve non-trivial Haskell problems.
Along the way we'll also use mtl typeclasses and classy lenses.

The source code is [available online][Duet.hs] and is executable as a stack
script.

!!![Duet.hs]:duet/Duet.hs

The Puzzle
----------

The puzzle is [Advent of Code 2017 Day 18][day18], and Part A is:

[day18]: http://adventofcode.com/2017/day/18

> You discover a tablet containing some strange assembly code labeled simply
> "Duet". Rather than bother the sound card with it, you decide to run the code
> yourself. Unfortunately, you don't see any documentation, so you're left to
> figure out what the instructions mean on your own.
>
> It seems like the assembly is meant to operate on a set of *registers* that are
> each named with a single letter and that can each hold a single integer. You
> suppose each register should start with a value of `0`.
>
> There aren't that many instructions, so it shouldn't be hard to figure
> out what they do. Here's what you determine:
>
> -   `snd X` *plays a sound* with a frequency equal to the value of `X`.
> -   `set X Y` *sets* register `X` to the value of `Y`.
> -   `add X Y` *increases* register `X` by the value of `Y`.
> -   `mul X Y` sets register `X` to the result of *multiplying* the value
>     contained in register `X` by the value of `Y`.
> -   `mod X Y` sets register `X` to the *remainder* of dividing the value
>     contained in register `X` by the value of `Y` (that is, it sets `X` to the
>     result of `X` modulo `Y`).
> -   `rcv X` *recovers* the frequency of the last sound played, but only when
>     the value of `X` is not zero. (If it is zero, the command does nothing.)
> -   `jgz X Y` *jumps* with an offset of the value of `Y`, but only if the value
>     of `X` is *greater than zero*. (An offset of `2` skips the next
>     instruction, an offset of `-1` jumps to the previous instruction, and so
>     on.)
>
> Many of the instructions can take either a register (a single letter) or a
> number. The value of a register is the integer it contains; the value of a
> number is that number.
>
> After each *jump* instruction, the program continues with the instruction to
> which the *jump* jumped. After any other instruction, the program continues
> with the next instruction. Continuing (or jumping) off either end of the
> program terminates it.
>
> *What is the value of the recovered frequency* (the value of the most
> recently played sound) the *first* time a `rcv` instruction is executed
> with a non-zero value?

Part B, however, says:

> As you congratulate yourself for a job well done, you notice that the
> documentation has been on the back of the tablet this entire time. While you
> actually got most of the instructions correct, there are a few key
> differences. This assembly code isn't about sound at all - it's meant to be
> run *twice at the same time*.
>
> Each running copy of the program has its own set of registers and
> follows the code independently - in fact, the programs don\'t even
> necessarily run at the same speed. To coordinate, they use the *send*
> (`snd`) and *receive* (`rcv`) instructions:
>
> -   `snd X` *sends* the value of `X` to the other program. These values
>     wait in a queue until that program is ready to receive them. Each
>     program has its own message queue, so a program can never receive a
>     message it sent.
> -   `rcv X` *receives* the next value and stores it in register `X`. If
>     no values are in the queue, the program *waits for a value to be
>     sent to it*. Programs do not continue to the next instruction until
>     they have received a value. Values are received in the order they
>     are sent.
>
> Each program also has its own *program ID* (one `0` and the other `1`);
> the register `p` should begin with this value.
>
> Once both of your programs have terminated (regardless of what caused
> them to do so), *how many times did program `1` send a value*?

(In each of these, "the program" is a program (written in the Duet assembly
language), which is different for each user and given to us by the site.  If
you sign up and view the page, you will see a link to your own unique program
to run.)

What's going on here is that both parts execute the same program in two
different virtual machines --- one has "sound" and "recover", and the other has
"send" and "receive".  We are supposed to run the same program in *both* of
these machines.

However, note that these two machines aren't *completely* different --- they
both have the ability to manipulate memory and read/shift program data.  So
really, we want to be able to create a "modular" spec and implementation of
these machines, so that we may re-use this memory manipulation aspect when
constructing our machine, without duplicating any code.

Parsing Duet
------------

First, let's get the parsing of the actual input program out of the way.  We'll
be parsing a program into a list of "ops" that we will read as our program.

Our program will be interpreted as a list of `Op` values, a data type
representing opcodes.  There are four categories: "snd", "rcv", "jgz", and the
binary mathematical operations:

```haskell
!!!duet/Duet.hs "type Addr" "data Op"
```

It's important to remember that "snd", "jgz", and the binary operations can all
take either numbers or other registers.

Now, parsing a single `Op` is just a matter of pattern matching on `words`:

```haskell
!!!duet/Duet.hs "parseOp ::"
```

We're going to store our program in a `PointedList` from the *[pointedlist][]*
package, which is a non-empty list with a "focus" at a given index, which we
use to represent the program counter/program head/current instruction.  Parsing
our program is then just parsing each line in the program string, and
collecting them into a `PointedList`.  We're ready to go!

[pointedlist]: http://hackage.haskell.org/package/pointedlist

```haskell
!!!duet/Duet.hs "parseProgram ::"
```

Our Virtual Machine
-------------------

### MonadPrompt

We're going to be using the great *[MonadPrompt][]* library[^mprompt] to build
our representation of our interpreted language.  Another common choice is to
use *[free][]*, and a lot of other tutorials go down this route.  However,
*free* is a bit more power than you really need for the interpreter pattern,
and I always felt like the implementation of interpreter pattern programs in
*free* was a bit awkward.

[MonadPrompt]: http://hackage.haskell.org/package/MonadPrompt
[free]: http://hackage.haskell.org/package/free

[^mprompt]: Not to be confused with the [prompt][] library, which is more or
less unrelated!  The library is actually my own that I wrote a few years back
before I knew about MonadPrompt, and this unfortunate naming collision is one
of my greatest Haskell regrets.

[prompt]: http://hackage.haskell.org/package/prompt

*MonadPrompt* lets us construct a language (and a monad) using GADTs to
represent command primitives.  For example, to implement something like `State
Int` (which we'll call `IntState`), you might use this GADT:

```haskell
data StateCommand :: Type -> Type where
    Put :: Int -> StateCommand ()
    Get :: StateCommand Int
```

For those unfamiliar with GADT syntax, this is declaring a data type
`StateCommand a` with two constructors --- `Put`, which takes an `Int` and
creates a `StateCommand ()`, and `Get`, which takes no parameters and creates a
`StateCommand Int`.

Our GADT here says that the two "primitive" commands of `IntState` are
"putting" (which requires an `Int` and produces a `()` result) and "getting"
(which requires no inputs, and produces an `Int` result).

You can then write `IntState` as:

```haskell
type IntState = Prompt StateCommand
```

which automatically has the appropriate Functor, Applicative, and Monad
instances.

Our primitives can be constructed using `prompt`:

```haskell
prompt :: StateCommand a -> IntState a

prompt (Put 10) :: IntState ()
prompt Get      :: IntState Int
```

Now, we *interpret* an `IntState` in a monadic context using `runPromptM`:

```haskell
runPromptM
    :: Monad m                              -- m is the monad to interpret in
    => (forall x. StateCommand x -> m x)    -- a way to interpret each primitive in 'm'
    -> IntState a                           -- IntState to interpret
    -> m a                                  -- resulting action in 'm'
```

If you're unfamiliar with *-XRankNTypes*, `forall x. StateCommand x -> m x` is
the type of a handler that can handle a `StateCommand` of *any* type, and
return a value of `m x` (an action returning the *same type* as the
`StateCommand`).  So, you can't give it something like `StateCommand Int -> m
Bool`, or `StateCommand x -> m ()`...it has to be able to handle a
`StateCommand a` of *any* type `a` and return an action in the interpreting
context producing a result of the same type.  If given a `StateCommand Int`, it
has to return an `m Int`, and if given a `StateCommand ()`, it has to return an
`m ()`, etc. etc.

Now, if we wanted to use `IO` and `IORefs` as the mechanism for interpreting
our `IntState`:

```haskell
interpretIO
    :: IORef Int
    -> StateCommand a
    -> IO a
interpretIO r = \case           -- using -XLambdaCase
    Put x -> writeIORef r x
    Get   -> readIORef r

runAsIO :: IntState a -> Int -> IO (a, Int)
runAsIO m s0 = do
    ref <- newIORef s0
    runPromptM (interpretIO ref) m
```

`interpretIO` is our interpreter, in `IO`.  `runPromptM` will interpret each
primitive (`Put` and `Get`) using `interpretIO` and generate the result for
us.

Note that the GADT property of `StateCommand` ensures us that the *result* of
our `IO` action matches with the result that the GADT constructor implies, due
to the magic of dependent pattern matching.  For the `Put x :: StateCommand ()`
branch, the result has to be `IO ()`; for the `Get :: StateCommand Int` branch,
the result has to be `IO Int`.

We can also be boring and interpret it using `State Int`:

```haskell
interpretState :: StateCommand a -> State Int a
interpretState = \case
    Put x -> put x
    Get   -> get

runAsState :: IntState a -> State Int a
runAsState = runPormptM interpretState
```

Basically, an `IntState a` is an abstract representation of a program (as a
Monad), and `interpretIO` and `interpretState` are different ways of
*interpreting* that program, in different monadic contexts.  To "run" or
interpret our program in a context, we provide a function `forall x.
StateCommand x -> m x`, which interprets each individual primitive command.

### Duet Commands

Now let's specify the "primitives" of our program.  It'll be useful to separate
out the "memory-based" primitive commands from the "communication-based"
primitive commands.  This is so that we can write interpreters that operate on
each one individually, and re-use our memory-based primitives and interpreters
for both parts of the puzzle.

For memory, we can access and modify register values, as well as jump around in
the program tape and read the `Op` at the current program head:

```haskell
!!!duet/Duet.hs "data Mem ::"
```

For communication, we must be able to "snd" and "rcv".

```haskell
!!!duet/Duet.hs "data Com ::"
```

Part A requires `CRcv` to take, as an argument, a number, since whether or not
`CRcv` is a no-op depends on the value of a certain register for Part A's
virtual machine.

Now, we can leverage the `:|:` type from *[type-combinators][]*:

[type-combinators]: http://hackage.haskell.org/package/type-combinators

```haskell
data (f :|: g) a = L (f a)
                 | R (g a)
```

`:|:` is a "functor disjunction" --- a value of type `(f :|: g) a` is either `f
a` or `g a`.  `:|:` is in *base* twice, as `:+:` in *GHC.Generics* and as `Sum`
in *Data.Functor.Sum*.  However, the version in *type-combinators* has some
nice utility combinators we will be using and is more fully-featured.

We can use `:|:` to create the type `Mem :|: Com`.  If `Mem` and `Com`
represent "primitives" in our Duet language, then `Mem :|: Com` represents
*primitives from `Mem` and `Com` together*.  It's a type that contains all of
the primitives of `Mem` and the primitives of `Com` -- that is, it contains:

```haskell
L (MGet 'c') :: (Mem :|: Com) Int
L MPk        :: (Mem :|: Com) Op
R (CSnd 5)   :: (Mem :|: Com) ()
```

etc.

Our final data type then --- a monad that encompasses *all* possible Duet
primitive commands --- is:

```haskell
!!!duet/Duet.hs "type Duet ="
```

We can write some convenient utility primitives to make things easier for us in
the long run:

```haskell
!!!duet/Duet.hs "dGet ::" "dSet ::" "dJmp ::" "dPk ::" "dSnd ::" "dRcv ::"
```

### Constructing Duet Programs

Armed with our `Duet` monad, we can now write a real-life `Duet` action to
represent *one step* of our duet programs:

```haskell
!!!duet/Duet.hs "stepProg ::"
```

This is basically a straightforward interpretation of the "rules" of our
language, and what to do when encountering each op code.

The only non-trivial thing is the `ORcv` branch, where we include the contents
of the register in question, so that our interpreter will know whether or not
to treat it as a no-op.

The Interpreters
----------------

Now for the fun part!

### Interpreting Memory Primitives

To interpret our `Mem` primitives, we need to be in some sort of stateful monad
that contains the program state.  First, let's make a type describing our
relevant program state, along with classy lenses for operating on it
polymorphically:

```haskell
!!!duet/Duet.hs "data ProgState ="
```

We store the current program and program head with the `PointedList`, and also
represent the register contents with a `Map Char Int`.

#### Brief Aside on Lenses with State

Using *[lens][]* with lenses (especially classy ones) is one of the only things
that makes programming against `State` with non-trivial state bearable for me!
`makeClassy` gives us a typeclass `HasProgState`, which is for things that
"have" a `ProgState`, as well as lenses into the `psTape` and `psRegs` field
for that type.  We can use these lenses with *lens* library machinery:


[lens]: http://hackage.haskell.org/package/lens

```haskell
-- | "get" based on a lens
use   :: MonadState s m => Lens' s a -> m a

-- | "set" through on a lens
(.=)  :: MonadState s m => Lens' s a -> a -> m ()

-- | "lift" a State action through a lens
zoom  :: Lens' s t -> State t a -> State s a
```

So, for example, we have:

```haskell
-- | "get" the registers
use psRegs  :: (HasProgState s, MonadState s m) => m (M.Map Char Int)

-- | "set" the PointedList
(psTape .=) :: (HasProgState s, MonadState s m) => P.PointedList Op -> m ()
```

The nice thing about lenses is that they compose, so, for example, we have:

```haskell
at :: k -> Lens' (Map k    v  ) (Maybe v)

at 'h'  :: Lens' (Map Char Int) (Maybe Int)
```

We can use `at 'c'` to give us a lens from our registers (`Map Char Int`) into
the specific register `'c'` as a `Maybe Int` --- it's `Nothing` if the item is
not in the `Map`, and `Just` if it is (with the value).

However, we want to treat all registers as `0` by default, not as `Nothing`, so
we can use `non 0`:

```haskell
non 0 :: Lens' (Maybe Int) Int
```

`non 0` is a `Lens` (actually an `Iso`, but who's counting?) into a `Maybe Int`
to treat `Nothing` as if it was `0`, and to treat `Just x` as if it was `x`.

We can chain `at r` with `non 0` to get a lens into a `Map Char Int`, which we
can use to edit a specific item, treating non-present-items as 0.

```haskell
         at 'h' . non 0 :: Lens' (Map Char Int) Int

psRegs . at 'h' . non 0 :: HasProgState s => Lens' s Int
```

#### Interpreting Mem

With these tools to make life easier, we can write an interpreter for our `Mem`
commands:

```haskell
!!!duet/Duet.hs "interpMem"
```

Nothing too surprising here --- we just interpret every primitive in our monadic
context.

We use `MonadFail` to explicitly state that we rely on a failed pattern match
for control flow.[^MonadFail]  `P.moveN :: Int -> P.PointedList a -> Maybe (P.PointedList
a)` will "shift" a `PointedList` by a given amount, but will return `Nothing`
if it goes out of bounds.  Our program is meant to terminate if we ever go out
of bounds, so we can implement this by using a do block pattern match with
`MonadFail`. For instances like `MaybeT`/`Maybe`, this means
`empty`/`Nothing`/short-circuit.  So when we `P.move`, we do-block pattern
match on `Just t'`.

[^MonadFail]: Using `MonadFail` in situations were we would normally use
`Alternative`/`MonadPlus`, to take advantage of pattern match syntax in do
block and have it work with `Alternative` combinators like `many`, is
[coming][mf]!  For good hygiene, remember to turn on the
*-XMonadFailDesugaring* extension so that pattern match failures explicitly use
`fail` from `MonadFail`, thus requiring the typeclass constraint.

[mf]: https://wiki.haskell.org/MonadFail_Proposal

We also use `P.focus :: Lens' (P.PointedList a) a`, a lens that the
*pointedlist* library provides to the current "focus" of the `PointedList`.

Note that most of this usage of lens with state is not exactly necessary (we
can manually use `modify`, `gets`, etc. instead of lenses and operators), but
it does make things a bit more convenient to write.

#### GADT Property

Again, the GADT-ness of `Mem` (and `Com`) works to enforce that the "results"
that each primitive expects is the result that we give.

For example, `MGet 'c' :: Mem Int` requires us to return `m Int`.  This is what
`use` gives us.  `MSet 'c' 3 :: Mem ()` requires us to return `m ()`, which is what `(.=)`
returns.

We have `MPk :: Mem Op`, which requires us to return `m Op`.  That's exactly
what `use (psTape . P.focus) :: (MonadState s m, HasProgState s) => m Op`
gives.

The fact that we can use GADTs to specify the "result type" of each of our
primitives is a key part about how `Prompt` from *MonadPrompt* works, and how
it implements the interpreter pattern.

This is enforced in Haskell's type system (through the "dependent pattern
match"), so GHC will complain to us if we ever return something of the wrong
type while handling a given constructor/primitive.

### Interpreting Com for Part A

Now, Part A requires an environment where:

1.  `CSnd` "emits" items into the void, keeping track only of the *last*
    emitted item
2.  `CRcv` "catches" the last thing seen by `CSnd`, keeping track of only the
    *first* caught item

We can keep track of this using `MonadWriter (First Int)` to interpret `CRcv`
(if there are two *rcv*'s, we only care about the first *rcv*'d thing), and
`MonadAccum (Last Int)` to interpret `CSnd`.  A `MonadAccum` is just like
`MonadWriter` (where you can "tell" things and accumulate things), but you also
have the ability to read the accumulated log at any time.  We use `Last Int`
because, if there are two *snd*'s, we only care about the last *snd*'d thing.

```haskell
!!!duet/Duet.hs "interpComA"
```

Note `add :: MonadAccum w m => w -> m ()` and `look :: MonadAccum w w`, the
functions to "tell" to a `MonadAccum` and the function to "get"/"ask" from a
`MonadAccum`.

#### MonadAccum

Small relevant note --- `MonadAccum` does not yet exist in *mtl*, though it
probably will in the next version.  It's the classy version of `AccumT`, which
is already in *[transformers-0.5.5.0][]*.

[transformers-0.5.5.0]: https://hackage.haskell.org/package/transformers-0.5.5.0

For now, [I've added `MonadAccum`][MonadAccum] and [appropriate
instances][MonadAccumInstances] in the sample source code, but when the new
version of *mtl* comes out, I'll be sure to update this post to take this into
account!

!!![MonadAccum]:duet/Duet.hs "class (Monad m, Monoid w) => MonadAccum w m"
!!![MonadAccumInstances]:duet/Duet.hs "instance (Monoid w, Monad m) => MonadAccum w (A.AccumT w m)"

### Interpreting Com for Part B

Part B requires an environment where:

1.  `CSnd` "emits" items into into some accumulating log of items, and we need
    to keep track of all of them.
2.  `CRcv` "consumes" items from some external environment, and fails when
    there are no more items to consume.

We can interpret `CSnd`'s effects using `MonadWriter [Int]`, to collect all
emitted `Int`s.  We can interpret `CRcv`'s effects using `MonadState s`, where
`s` contains an `[Int]` acting as a source of `Int`s to consume.

We're going to use a `Thread` type to keep track of all thread state.  We do
this so we can merge the contexts of `interpMem` and `interpComB`, and really
treat them (using type inference) as both working in the same interpretation
context.

```haskell
!!!duet/Duet.hs "data Thread =" "instance HasProgState Thread"
```

(We write an instance for `HasProgState Thread`, so we can use `interpMem` in a
`MonadState Thread m`, since `psRegs :: Lens' Thread (M.Map Char Int)`, for
example, will refer to the `psRegs` inside the `ProgState` in the `Thread`)

And now, to interpret:

```haskell
!!!duet/Duet.hs "interpComB"
```

Note again the usage of do block pattern matches and `MonadFail`.

### Combining Interpreters

To combine interpreters, we're going to be using, from *type-combinators*:

```haskell
(>|<) :: (f a -> r)
      -> (g a -> r)
      -> ((f :|: g) a -> r)
```

Basically, `>|<` lets us write a "handler" for a `:|:` by providing a handler
for each side.  For example, with more concrete types:

```haskell
(>|<) :: (Mem a -> r)
      -> (Com a -> r)
      -> ((Mem :|: Com) a -> r)
```

We can use this to build an interpreter for `Duet`, which goes into
`runPromptM`, by using `>|<` to generate our compound interpreters.

```haskell
runPromptM
    :: Monad m
    => (forall x. (Mem x :|: Com x) -> m x)
    -> Duet a
    -> m a
```

This is how we can create interpreters on `Duet` by "combining", in a modular
way, interpreters for `Mem` and `Com`.  This is the essence of the "data types
a la carte" technique.

Getting the Results
-------------------

We now just have to pick concrete monads now for us to interpret into.

### Part A

Our interpreter for Part A is `interpMem >|< interpComA` --- we interpret the
`Mem` primitives the usual way, and interpret the `Com` primitives the Part A
way.

Let's check what capabilities our interpreter must have:

```haskell
ghci> :t interpMem >|< interpComA
interpMem >|< interpComA
    :: ( MonadWriter (First Int) m
       , MonadAccum (Last Int) m
       , MonadFail m
       , MonadState s m
       , HasProgState s
       )
    => (Mem :|: Com) a
    -> m a
```

So it looks like we need to be `MonadWriter (First Int)`, `MonadAccum (Last
Int)`, `MonadFail m`, and `MonadState s m`, where `HasProgState s`.

Now, we can write such a Monad from scratch, or we can use the *transformers*
library to generate a transformer with all of those instances for us.  For the
sake of brevity and reducing duplicated code, let's go that route.  We can use:

```haskell
MaybeT (StateT ProgState (WriterT (First Int) (A.Accum (Last Int))))
```

And so we can write our final "step" function in that context:

```haskell
!!!duet/Duet.hs "stepA ::"
```

`stepA` will make a single step of the tape, according to the interpreters
`interpMem` and `interpComA`.

Our final answer is then just the result of *repeating* this over and over
again until there's a failure.  We take advantage of the fact that `MaybeT`'s
`Alternative` instance uses `empty` for `fail`, so we can use `many ::
MaybeT m a -> MaybeT m [a]`, which repeats a `MaybeT` action several times
until a failure is encountered.  In our case, "failure" is when the tape goes
out of bounds.

Note, however, that if we only want the value of the `First Int` in the
`WriterT`, this will actually only repeat `stepA` until the first valid `CRcv`
uses `tell`, thanks to laziness.  If we only ask for the `First Int`, it'll
stop running the rest of the computation!

Here is the entirety of running Part A --- as you can see, it consists mostly
of unwrapping *transformers* newtype wrappers.

```haskell
!!!duet/Duet.hs "partA ::"
```

A `Nothing` result means that the `Writer` log never received any outputs
before `many` ends looping, which means that the tape goes out of bounds before
a successful *rcv*.

### Part B

Our interpreter's type for Part B is a little simpler:

```haskell
ghci> :t interpMem >|< interpComB
interpMem >|< interpComB
    :: ( MonadWriter [Int] m
       , MonadFail m
       , MonadState Thread m
       )
    => (Mem :|: Com) a
    -> m a
```

We can really just use:

```haskell
WriterT [Int] (MaybeT (State Thread))
```

Writing our concrete `stepB` is a little more involved, since we have to juggle
the state of each thread separately.  We can do this using:

```haskell
zoom _1 :: MaybeT (State s) a -> MaybeT (State (s, t)) a
zoom _2 :: MaybeT (State t) a -> MaybeT (State (s, t)) a
```

To "lift" our actions on one thread to be actions on a "tuple" of threads.  We
have, in the end:

```haskell
!!!duet/Duet.hs "stepB ::"
```

Our final `stepB` really doesn't need a `WriterT [Int]` --- we just use that
internally to collect *snd* outputs.  So we use `execWriter` after
"interpreting" our actions (along with `many`, to repeat our thread steps until
they block) to just get the resulting logs.

We then reset the input buffers appropriately (by putting in the collected
outputs of the previous threads).

If both threads are blocking (they both have to external outputs to pass on),
then we're done (using `guard`).

We return the number of items that "Program 1" (the second thread) outputs,
because that's what we need for our answer.

This is one "single pass" of both of our threads.  As you can anticipate, we'll
use `many` again to run these multiple times until both threads block.

```haskell
!!!duet/Duet.hs "partB ::"
```


`many :: MaybeT s Int -> MaybeT s [Int]`, so `runMaybeT` gives us a `Maybe
[Int]`, where each item in the resulting list is the number of items emitted by
Program 1 at every iteration of `stepB`.  Note that `many` produces an action
that *cannot fail*, so its result *must be `Just`*.  To get our final answer,
we only need to sum.

### Examples

In the [sample source code][Duet.hs], I've included [my own puzzle
input][testProg] provided to me from the advent of code website.  We can now
get actual answers given some sample puzzle input:

!!![testProg]:duet/Duet.hs "testProg ::"

```haskell
!!!duet/Duet.hs "main ::"
```

And, as a stack script, we can run this and see my own puzzle input's answers:

```bash
$ ./Duet.hs
Just 7071
8001
```

Wrap Up
-------

That's it!  Hope you enjoyed some of the techniques used in this post,
including --

1.  Leveraging the interpreter pattern to create a monad that can be
    interpreted in multiple contexts with multiple different interpreters
2.  Using functor disjunctions like `:|:` (or `:+:`, `Sum`, etc.) to combine
    interpretable primitives
3.  Writing modular interpreters for each set of primitives, then using
    deconstructors like `>|<` to easily combine and swap out interpreters.
4.  Lenses with State and classy lenses
5.  Programming against polymorphic monadic contexts like `MonadState`,
    `MonadWriter`, `MonadAccum`, etc.

Pushing the Boundaries
----------------------

That's the main part of the post!  However, just for fun, we can take things a
little further and expand on this technique.  The *type-combinators* library
opens up a lot of doors to combining modular interpreters in more complex ways!

### Functor conjunctions

We see that `:|:` (functor disjunction) can be used to merge sets of
primitives.  We can also use `:&:` (functor conjunction), also known as `:*:`
from *Generics.GHC* and `Product` from *Data.Functor.Product*!

```haskell
newtype (f :&: g) a = f a :&: g a
```

Used with GADTs representing primitives, `:&:` lets us "tag" our primitives
with extra things.

For example, we were pretty sneaky earlier by using `zoom` to manually lift our
`Mem` interpreter to work on specific threads.  Instead, we can actually use
`Const Int` to attach a thread ID to `Mem`s:

```haskell
-- | type-combinators exports its own version of 'Const'
newtype C r a = C { getC :: r }

-- | Peek into Thread 0
C 0 :&: MPk         :: (C Int :&: Mem) Op

-- | Get contents of register 'c' of Thread 1
C 1 :&: MGet 'c'    :: (C Int :&: Mem) Int
```

The advantage we gain by using these tags is that we now have an approach that
can be generalized to multiple threads, as well.

If we had a version of `interpMem` that takes a thread:

```haskell
interpMemThread
    :: (MonadState s m, MonadFail m, HasProgState s)
    => C Int a
    -> Mem a
    -> m a
```

we can use the analogy of `>|<`, `uncurryFan`:

```haskell
uncurryFan
    :: (forall x. f x -> g x -> r)
    -> (f :&: g) a
    -> r

uncurryFan interpMemThread
    :: (MonadState s m, MonadFail m)
    => (C Int :&: Mem) a
    -> m a
```

We can build interpreters of combinations of `:|:` and `:&:` by using
combinations of `>|<` and `uncurryFan`.

```haskell
interpMem >|< interpComB
    :: ( MonadWriter [Int] m
       , MonadFail m
       , MonadState Thread m
       )
    => (Mem :|: Com) a
    -> m a

uncurryFan interpMemThread >|< interpComB
    :: ( MonadWriter [Int] m
       , MonadFail m
       , MonadState Thread m
       )
    => ((C Int :&: Mem) :|: Com) a
    -> m a
```

### Manipulating Disjunctions and Conjunctions

So, we have a `Mem :|: Conj`.  How could we "tag" our `Mem` after-the-fact, to
add `C Int`?  We can manipulate the structure of conjunctions and
disjunctions using the `Bifunctor1` from *Type.Class.Higher*, in
*type-combinators*.

`bimap1` can be used to modify either half of a `:|:` or `:&:`:

```haskell
bimap1
    :: (forall x. f x -> h x)
    -> (forall x. g x -> j x)
    -> (f :|: g) a
    -> (h :|: j) a

bimap1
    :: (forall x. f x -> h x)
    -> (forall x. g x -> j x)
    -> (f :&: g) a
    -> (h :&: j) a
```

So we can "tag" the `Mem` in `Mem :|: Cmd` using:

```haskell
bimap1 (C 0 :&:) id
    :: (      Mem       :|: Com) a
    -> ((C Int :&: Mem) :|: Com) a
```

Which we can use to re-tag a `Prompt (Mem :|: Com)`, with the help of
`runPromptM`:

```haskell
\f -> runPromptM (prompt . f)
    :: (forall x. f x -> g x)
    -> Prompt f a
    -> Prompt g a

runPromptM (prompt . bimap1 (C 0 :&:) id)
    :: Prompt (      Mem       :|: Com) a
    -> Prompt ((C Int :&: Mem) :|: Com) a
```

### Many sets of primitives

Instead of `f >|< g >|< h`, you can use `FSum '[f, g, h]` to combine
multiple sets of primitives in a clean way.

If there are no duplicates in your type-level list, you can even use `finj`
to create your `FSum`s automatically:

```haskell
finj :: f ∈ fs => f a -> FSum fs a

finj :: Mem a -> FSum '[Mem, Com, Foo] a
finj :: Com a -> FSum '[Mem, Com, Foo] a
finj :: Foo a -> FSum '[Mem, Com, Foo] a

prompt (finj (MGet 'c')) :: Prompt (FSum '[Mem, Com, Foo]) Int
```

There isn't really a built-in way to handle these, but you can whip up a
utility function with `Prod` (from *type-combinators*) and `ifoldMapFSum`.

```haskell
newtype Handle a r f = Handle { runHandle :: f a -> r }

handleFSum :: Prod (Handle a r) fs -> FSum fs a -> r
handleFSum hs = ifoldMapFSum $ \i -> runHandle (index i hs)
```

`Prod` lets you bunch up a bunch of handlers together, so you can build
handlers like:

```haskell
handleMem :: Mem a -> m a
handleCom :: Com a -> m a
handleFoo :: Foo a -> m a

handleFSum (Handle handleMem :< Handle handleCom :< Handle handleFoo :< Ø)
    :: FSum '[Mem, Com, Foo] a -> m a

runPromptM (handleFSum ( Handle handleMem
                      :< Handle handleCom
                      :< Handle handleFoo
                      :< Ø)
           )
    :: Prompt (FSum '[Mem, Com, Foo]) a -> m a
```


### Endless Possibilities

Hopefully this post inspires you a bit about this fun design pattern!  And,
if anything, I hope after reading this, you learn to recognize situations where
the interpreter pattern might be useful in your everyday programming.
