---
title: "Interpreters a la Carte (Advent of Code 2017 Duet)"
categories: Haskell
series: Beginner/Intermediate Haskell Projects
tags: functional programming, haskell, types, lens
create-time: 2018/01/11 16:28:18
date: none
identifier: interpreters
slug: interpreters-a-la-carte
---

This post is just a fun one exploring a wide range of techniques that I applied
to solve the Day 18 puzzles of this year's great [Advent of Code][aoc].  The
puzzles involved interpreting an assembly language on an abstract machine.  The
neat twist is that Part 1 gave you a description of one abstract machine, and
Part 3 gave you a *different* abstract machine to interpret the same language
in.  This twist (one language, but different interpreters/abstract machines) is
basically one of the textbook applications of the *interpreter pattern* in
Haskell and functional programming, so it was fun to implement my solution in
that pattern -- the assembly language source was "compiled" to an abstract data
type once, and the difference between Part 1 and Part 2 was just a different
choice of interpreter.

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

The Puzzle
----------

The puzzle is [Advent of Code 2017 Day 18][day18], and Part 1 is:

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

Part 2, however, says:

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

Note that in each of these, "the program" is a program (written in the Duet
assembly language), which is different for each user and given to us by the
site.

What's going on here is that both parts execute the same program in two
different virtual machines -- one has "sound" and "recover", and the other has
"send" and "receive".  We are supposed to run the same program in *both* of
these machines.

However, note that these two machines aren't *completely* different -- they
both have the ability to manipulate memory and read/shift program data.  So
really , we want to be able to create a "modular" spec and implementation of
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
!!!interpreters/Duet.hs "type Addr" "data Op"
```

It's important to remember that "snd", "jgz", and the binary operations can all
take either numbers or other registers.

Now, parsing a single `Op` is just a matter of pattern matching on `words`:

```haskell
!!!interpreters/Duet.hs "parseOp ::"
```

We're going to store our program in a `PointedList` from the *[pointedlist][]*
package, which is a non-empty list with a "focus" at a given index, which we
use to represent the program counter/program head/current instruction.  Parsing
our program is then just parsing each line in the program string, and
collecting them into a `PointedList`.  We're ready to go!

[pointedlist]: http://hackage.haskell.org/package/pointedlist

```haskell
!!!interpreters/Duet.hs "parseProgram ::"
```

Our Virtual Machine
-------------------

### MonadPrompt

We're going to be using the great *[MonadPrompt][]* library to build our
representation of our interpreted language.  Another common choice is to use
*[free][]*, and a lot of other tutorials go down this route.  However, *free*
is a bit more power than you really need for the interpreter pattern, and I
always felt like the implementation of interpreter pattern programs in *free*
was a bit awkward.

[MonadPrompt]: http://hackage.haskell.org/package/MonadPrompt
[free]: http://hackage.haskell.org/package/free

*MonadPrompt* lets us construct a language (and a monad) using GADTs to
represent command primitives.  For example, to implement something like `State
Int`, you might use this GADT:

```haskell
data StateCommand :: Type -> Type where
    Put :: Int -> StateCommand ()
    Get :: StateCommand Int
```

Which says that the two "primitive" commands of `State Int` are "putting" (which
requires an `Int` and produces a `()` result) and "getting" (which requires no
inputs, and produces an `Int` result).

You can then write `State Int` as:

```haskell
type IntState = Prompt StateCommand
```

And our primitives can be constructed using:

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

So, if we wanted to use `IO` and `IORefs` as the mechanism for interpreting our
`IntState`:

```haskell
interpretIO
    :: IORef Int
    -> StateCommand a
    -> IO a
interpretIO r = \case           -- using -XLambdaCase
    Put x -> writeIORef r x
    Get   -> readIORef r

runAsIO :: IntState a -> Int -> IO a
runAsIO m s0 = do
    r <- newIORef s0
    runPromptM (interpretIO r) m
```

`interpretIO` is our interpreter, in `IO`.  `runPromptM` will interpret each
primitive (`Put` and `Get`) using `interpretIO`, and generate the result for
us.

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
each one individually.

For memory, we can access and modify register values, as well as jump around in
the program tape and read the `Op` at the current program head:

```haskell
!!!interpreters/Duet.hs "data Mem ::"
```

For communication, we must be able to "snd" and "rcv".

```haskell
!!!interpreters/Duet.hs "data Com ::"
```

Part 1 requires `CRcv` to take, as an argument, a number, since whether or not
`CRcv` is a no-op depends on the value of a certain register for Part 1's
virtual machine.

Now, we can leverage the `:|:` type from *[type-combinators][]*:

[type-combinators]: http://hackage.haskell.org/package/type-combinators

```haskell
data (f :|: g) a = L (f a)
                 | R (g a)
```

`:|:` is a "functor disjunction" -- a value of type `(f :|: g) a` is either `f
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

Our final data type then -- a monad that encompasses *all* possible Duet
primitive commands, is:

```haskell
!!!interpreters/Duet.hs "type Duet ="
```

We can write some convenient utility primitives to make things easier for us in
the long run:

```haskell
!!!interpreters/Duet.hs "dGet ::" "dSet ::" "dJmp ::" "dPk ::" "dSnd ::" "dRcv ::"
```

### Constructing Duet Programs

Armed with our `Duet` monad, we can now write a real-life `Duet` action to
represent *one step* of our duet programs:

```haskell
!!!interpreters/Duet.hs "stepProg ::"
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
!!!interpreters/Duet.hs "data ProgState ="
```

Using *[lens][]* with lenses (especially classy ones) is one of the only things
that makes programming against `State` with non-trivial state bearable for me!

[lens]: http://hackage.haskell.org/package/lens

`makeClassy` gives us 
