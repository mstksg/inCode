The Compromiseless Reconciliation of I/O and Purity
===================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
CreateTime
:   2013/10/04 18:31:44
PostDate
:   never
Identifier
:   io-purity


One of the crazy ideals of functional programming is the idea that your
program is simply a list of definitions of mathematical functions.  And like
real math functions, FP functions are **pure**.  That means that (1) they
cannot affect any state, and (2) that they must return the same thing every
time they are called.

When you first learn functional programming, this manifests as "your variables
are immutable and you can't do loops; use recursion instead."  And if you do
that, everything is "fine".

However, there is an apparent glaring problem with this adherence to purity:
**I/O**.  Input and output are inherently stateful.

The Problem
-----------

For the obvious (input) example, consider `getchar()` in C.  It returns the
character that a user enters.  Obviously, if `getchar()` returned the same
thing every time, you'd have an extraordinarily useless function.  Input
*inherently violates* purity, it seems.  (Also, consider a function generating
random numbers)

The idea of output violates purity as well.  Consider calling `printf()` in C.
You're going to change the state of the terminal.  A benign example, of
course; but what about a function `add_database_row()` that adds a row to your
database?  A call of `get_database_row()` will now return something different
than it would have returned before.  `get_database_row()` now returns two
different things when run at two different times --- impure!  Blasphemy!

Of course, it should be obvious that not being able to perform IO means that
your program is essentially useless in most real world applications.  But
purity is pretty cool, and it gives us guarantees that let us reason with our
code in ways that are impossible with impure code, and with unprecedented
safety.  It allows the compiler to do crazy optimization tricks.  It allows
for extremely powerful mathematical analysis of our programs.

So how can we reconcile the idea of a pure language with ~~anything useful~~
I/O?

A Functional "Program"
----------------------

### Declarations

Let's look at an almost-typical Haskell program.

~~~haskell
--| factorial n: n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--| fib n: the nth Fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

--| fibs: the list of all Fibonacci numbers
fibs :: [Int]
fibs = map fib [1..]

--| first_n_fibs n: a list of the first n Fibonacci numbers
first_n_fibs :: Int -> Int
first_n_fibs n = take n $ fibs
~~~

One of the first things you should notice is that this looks strikingly
similar to a list of math equations...and almost not like a program.

In particular, look at the "constant" `fibs`.  In Haskell, `[1..]` represents
a list from 1 to infinity.  `fibs` is basically an infinite list of Fibonacci
numbers.

In a normal language, this would explode.  However, imagine writing this
declaration down on a piece of paper in math notation.

Now, do it.

Anything explode?  Did the world freeze and hang?

Probably not.  That's because it's just a *declaration* of what things are,
not an actual instruction to compute/execute.

Now, notice one important thing about this: there is no inherent ordering in
any of these statements.  When you write declarations of mathematical objects
on paper, the order in which you declare them should have no bearing on what
they represent.  These are functions.  Immortal, unchanging, ethereal,
separate from time and space.  It is simply nonsensical to talk about order in
this context.

Also note that these declarations don't always declare integers/numbers.
`fibs` actually declares a data structure --- a list that contains integers.
Of course this is no big problem...mathematical functions can map integers to
matrices, or matrices to functions, anything you can think of.  We aren't
limited to simply defining primitive things.  We can also define structures
that contain things.

Of course, this "program" doesn't actually *do* anything.  Let's look at some
more programs and see if we can address this.

### Actions

There are a lot of data structures/data types that may be expressed in
Haskell.  One in particular is called `IO`.  `IO a` represents a computation
that returns something of type `a`.  There are a couple of pre-packaged
computations included in the standard library.  Let's write another
almost-typical Haskell program with some.


~~~haskell
--| getStringFromStdin: returns a computation that represents the act of
--|     getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--| printFibN: returns a computation that represents the act of printing the
--|     nth Fibonacci number to stdout and returns () (Nothing).
printFibN :: Int -> IO ()
printFibN n = print (fib n)
~~~

Let's look at these.

These are simply functions/declarations, just like the ones above.  Although
instead of returning an integer or a list data structure, it returns a special
data structure that represents a computation.  `[a]` represents a list of
`a`'s.  `IO a` represents an abstract computation that returns an `a`.

These declarations and functions are also simply "math" functions.  Instead of
returning a set or a matrix or a vector, it returns another type of object.

Note that this has nothing to do with execution.  `printFibN` does *not*
execute a print statement.  No more than writing `printFibN` on a piece of
paper will cause it to magically evaluate.  It does not execute anything ---
it is simply an abstract data structure representing a computation.

Note again that there is no inherent ordering involved.  Whether you define
one or the other first, it does not change what the two names really
*represent*.  Just like if you defined two matrices on a piece of paper in a
different order, it does not change the matrices they represent.

Also note that all of these declarations are completely pure.
`getStringFromStdin` will return the exact same *representation of a
computation* every single time.  `printFibN n` will return the exact same
*computation* for every `n` every single time.

So these objects represent computations, but do not actually execute anything
(how can math functions execute anything, anyway?  Nonsense!), how do we
actually *do* something?

### The Main Point

So what happens when a Haskell program is interpreted/compiled?

1.  The interpreter/compiler internalizes and understands all of the
    declarations, without actually executing them or evaluating them.

2.  The runtime environment picks one item/declaration, established by
    convention, and then chooses to execute it.

Your program is a list of declarations of numbers, strings, and data
structures.  Some of those data structures that your program defines may be
`IO` objects (data structures that represent computations).

The runtime environment picks a pre-agreed-upon defined data structure, and
chooses to execute it.

In Haskell, we have generally agreed that the function we "choose" to pass
onto the run time environment is called `main`.  So let's finish up our
Haskell program:

~~~haskell
--| printFibN: returns a computation that represents the act of printing the
--|     nth Fibonacci number to stdout and returns () (Nothing).
printFibN :: Int -> IO ()
printFibN n = print (fib n)

--| main: The function that we agree that the runtime environment will
--|     execute.
main :: IO ()
main = printFibN 10
~~~

And here we are.  A full, executable Haskell program.

As we can see, every function or declaration that makes up our program is
completely pure and side-effectless. In fact, the assembly of `main` itself is
side-effectless and pure.  We assemble the `IO ()` that `main` returns in a
pure way. `printFibN 10` will return the exact same computation every single
time we run it.

`printFibN 10` is **pure**.  Every time we *evaluate* `printFibN 10`, we get
the exact same computation.

Therefore, `main` is pure, as well.  Every time we evaluate `main`, we get the
exact same computational data structure.

Now consider:

~~~{.haskell}
--| getStringFromStdin: returns a computation that represents the act of
--|     getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--| main: The function that we agree that the runtime environment will
--|     execute.  The `>>=` operator says "take the result of the first
--|     computation and use it as an argument to the second".  The second
--|     thing should be a function that takes one argument for this to make
--|     sense...which is what `print` is.  Phew!
main :: IO ()
main = getStringFromStdin >>= print
~~~

`main` gets something from the standard input, and then prints it.

Oh wait.  This means that if I type something different into









<!-- Program?  What's a program? -->
<!-- --------------------------- -->

<!-- Let's jump right to the core of the issue here and get down and dirty, -->
<!-- philosophically. -->

<!-- When you think about writing a program, what do you think of? -->

<!-- You might think of code in a compiled low-level language like C.  You might -->
<!-- think of code in a scripting language, like Python or Ruby.  You might have -->
<!-- even jumped straight to thinking about assembly or machine code. -->

<!-- What do all of these ideas of programming have in common?  In all of these, -->
<!-- you are *writing instructions to manipulate memory*.  This is especially -->
<!-- obvious in assembly...it's somewhat obvious in C (where all memory is -->
<!-- essentially a giant array of bytes, and the program revolves around -->
<!-- manipulating chunks of those bytes at a time).  It's a little less obvious in -->
<!-- a scripting language, but scripting languages can be thought about as a list -->
<!-- of lines for a runtime environment to execute, in order to modify/manipulate -->
<!-- memory. -->

<!-- ### What is it really? -->

<!-- There are some slight issues with this definition of the "idea" of -->
<!-- programming, because in a way it betrays a lot of what we mentally do when we -->
<!-- program.  It betrays the abstractions that we spend so much effort to -->
<!-- carefully craft.  It betrays key principles in modular code or object oriented -->
<!-- programming. -->

<!-- When you work with a data structure, such as a hash table, for example, you -->
<!-- don't actually think about the low-level shuffling of bits for the complex -->
<!-- storage and lookup operations.  When you deal with an object-oriented library, -->
<!-- for example, you make method calls without any care about what memory -->
<!-- shuffling is actually being done. -->

<!-- ### Another thought -->

<!-- I propose that you think about programs not as instructions to manipulate -->
<!-- memory, but as a **language** by which you construct **ideas and concepts** -->
<!-- and structures, and the way those ideas and constructs interact.  A program is -->
<!-- not the manipulation of memory, but rather the manipulation of *ideas*. -->

<!-- So what is compilation?  Compilation is the process of turning ideas into an -->
<!-- "executable" data structure.  As most of us have learned it, compilation is -->
<!-- turning the text file (ideas) into machine code.  But really, is machine code -->
<!-- the "inherent" target of a program?  What about turning the ideas in a text -->
<!-- file into, say...executable javascript code?  What about turning the ideas in -->
<!-- a text file into a screenplay that actors can act out on camera and complete a -->
<!-- computation? -->

<!-- Think hard about "separating" the **ideas/concepts** of coding with a possible -->
<!-- **execution environment** of those ideas.  This is like saying how "ideas" of -->
<!-- human thought can be put into multiple languages.  The idea is an entity in -->
<!-- and of itself, and is not "tied" to a spoken sound representation, or a -->
<!-- particular execution environment. -->

<!-- Another Model -->
<!-- ------------- -->

<!-- In functional languages, everything is a function that returns an answer given -->
<!-- inputs. -->

<!-- So what if our "pure" functions return...instructions? -->

<!-- What about instead of thinking about a program as a list of instructions for a -->
<!-- computer...we thought about a program as a function that **returns** a list of -->
<!-- instructions? -->

<!-- Let's put this thought aside for a bit and imagine a DSL we can embed in Ruby. -->

<!-- ### A simple implementation -->

<!-- Let's say that our DSL in ruby will output some list of instructions.  It'll -->
<!-- be in the form of a list of symbols. -->

<!-- Let's create a sample program in this DSL: -->

<!-- ~~~ruby -->

<!-- def get_and_say_n_times(n) -->
<!--   instruction_list = [] -->
<!--   instruction_list << [:getline] -->
<!--   instruction_list << [:store_result_in, "to_say"] -->
<!--   n.times do -->
<!--     instruction_list << [:print_line, "to_say"] -->
<!--   end -->
<!-- end -->

<!-- get_and_say_n_times(4) -->
<!-- # [ [:getline], -->
<!-- #   [:store_result_in, "to_say"], -->
<!-- #   [:print_line, "to_say"], -->
<!-- #   [:print_line, "to_say"], -->
<!-- #   [:print_line, "to_say"], -->
<!-- #   [:print_line, "to_say"] ] -->

<!-- ~~~ -->

<!-- Let's break down how this is going to work: -->

<!-- 1.  The interpreter will move down the list, executing each instruction -->
<!--     one-by-one. -->
<!-- 2.  Every list item will have access to the result of the item before. -->
<!-- 3.  The first item in each instruction is the instruction, and rest are the -->
<!--     parameters. -->

<!-- So this `get_and_say_n_times` will take a number and return a program that -->
<!-- takes a string in stdin and echoes it *n* times. -->






<!-- Let's make a big leap of concepts.  How about we create some sort of dsl for -->
<!-- Ruby where we can make ruby -->





-----

<!-- What is Purity? -->
<!-- --------------- -->

<!-- Let's think --> 






<!-- What does this look like, practically?  The function *sin(x)*, for example, -->
<!-- does not imply any change in the world.  Sure, if you sit down and try to -->
<!-- calculate the sine of some number, you might change the state of your paper -->
<!-- and pencil. But the actual mathematical ideal of the sine function...it does -->
<!-- not involve any change in the world, -->
<!-- [lest we run into some really big problems][smbc]. The entire idea kind of -->
<!-- breaks down if you try to imagine it. Multiplication is an abstract, -->
<!-- non-physical concept.  Not a physical machine you run.  And why should two -->
<!-- times two change every time you calculate it? -->

<!-- [smbc]: http://www.smbc-comics.com/?id=2595 -->

