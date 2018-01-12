---
title: "Interpreters a la Carte (Advent of Code 2017 Duet)"
categories: Haskell
series: Beginner/Intermediate Haskell Projects
tags: functional programming, haskell, types, lens
create-time: 2018/01/11 16:28:19
date: none
identifier: interpreters
slug: interpreters-a-la-carte
---

This post is just a fun one exploring a wide range of techniques that I applied
to solve the Day 18 puzzles of this year's great [Advent of Code][aoc].  The
puzzles involved interpreting an assembly language on an abstract machine.  The
neat twist is that Part 1 gave you a description of one abstract machine, and
Part 2 gave you a *different* abstract machine to interpret the same language
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
type Addr = Either Char Int

data Op = OSnd Addr
        | ORcv Char
        | OJgz Addr Addr
        | OBin (Int -> Int -> Int) Char Addr
```

It's important to remember that "snd", "jgz", and the binary operations can all
take either numbers or other registers.

Now, parsing a single `Op` is just a matter of pattern matching on `words`:

```haskell
parseOp :: String -> Op
parseOp inp = case words inp of
    "snd":c    :_   -> OSnd (addr c)
    "set":(x:_):y:_ -> OBin (const id) x (addr y)
    "add":(x:_):y:_ -> OBin (+)        x (addr y)
    "mul":(x:_):y:_ -> OBin (*)        x (addr y)
    "mod":(x:_):y:_ -> OBin mod        x (addr y)
    "rcv":(x:_):_   -> ORcv x
    "jgz":x    :y:_ -> OJgz (addr x) (addr y)
    _               -> error "Bad parse"
  where
    addr :: String -> Addr
    addr [c] | isAlpha c = Left c
    addr str = Right (read str)
```

We're going to store our program in a `PointedList` from the *[pointedlist][]*
package, which is a non-empty list with a "focus" at a given index, which we
use to represent the program counter/program head/current instruction.  Parsing
our program is then just parsing each line in the program string, and
collecting them into a `PointedList`.  We're ready to go!

[pointedlist]: http://hackage.haskell.org/package/pointedlist

```haskell
parse :: String -> P.PointedList Op
parse = fromJust . P.fromList . map parseOp . lines
```

Our Virtual Machine
-------------------

Hi
