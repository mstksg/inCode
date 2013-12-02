Functional Programming Is Awesome: Parser Combinators
=====================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
CreateTime
:   2013/11/30 22:56:45
PostDate
:   Never
Identifier
:   combinators-parsers

If you've spent your entire life programming imperatively, you could not even
begin to imagine the vast worlds of thinking that are inaccessible because of
restrictions in your thinking.  Not "better", or "harder" ways of thinking ---
just as easy, but...different.

Don't believe me?  Here is a sample of an amazingly awesome and amazing and
incomprehensibly mind-opeining design pattern that shines in functional
programming: applicative combinators.  In specific, we are going to write a
small parser library!  Yay!

Now, if you are a sane person, you should know that *nobody* enjoys writing
parsers...imperatively.  The whole deal is a mess; you have to keep track of
multiple layers of state, work out back tracking and edge cases, and to top it
off, parsers tend to be very uncomposable, and parsing two different things
usually requires either source code surgery in the best case or a complete
rewrite in the worst.

But what if I told you that combinator-based parsers are so easy to work with
and composable and intuitive that they are...well, fun?

Let's take a look, shall we?  We will first write our library, and then see
how we build complex parsers out of smaller, simpler ones.  And maybe learn a
little bit of Haskell along the way.


The Library
-----------

So; our goal here is to write a library for parsing that is powerful to be
completely general and adaptable for any purpose.  And also be extremely
intuitive to use.  Easy, right?

Let's start!

### Our Parser Type

Okay, so ideally, how would we want to call our parser?  How would we want to
be able to use it to parse something?

How about as a function call?

~~~haskell
-- `parse`: takes a String and returns a parsed object of type a, and the
--          remainder of the string
parse :: String -> (a, String)
~~~

Simple enough.  A simple function that you call with a string, like `parse
"hello"`, and it returns whatever object you are parsing the string to, with
the remaining unparsed part of the string.  So if we, say, wanted to write a
simple parser that parses an integer, we would want to call it as: (`λ: `
indicates the interactive [repl][] prompt, ghci)

[repl]: http://en.wikipedia.org/wiki/Read-eval-print_loop

~~~haskell
λ: :type parse              -- ":type" asks ghci for a type
parse :: String -> (Int, String)
                            -- parse takes a String and returns an Int
λ: parse "12"
(12, "")                    -- returns the integer 12, with nothing leftover
λ: parse "-82 zero"
(82, " zero")               -- returns the integer -82, with some leftovers
λ: parse "hello"
??????
~~~

Hm.  There's a problem.  Sometimes, the parse will fail.  How can we indicate
that a parse is failable?

In most languages, we might return a `nil` or `null` value.  But this is not
cool in Haskell because we like having type safety --- having things be `nil`
or `null` willy-nilly means that your compiler can't help you, and you open up
the door to runtime errors.

In Haskell, we use `Maybe a`.  If you've ever used an object with Templates or
Generics, a `Maybe a` is an object that can be in two states: `Just a`, where
it contains an `a`, and `Nothing`, where it contains nothing.  Our function
will return a `Just a` if our parse succeeds and a `Nothing` if it fails.

~~~haskell
λ: :t parse
parse :: String -> Maybe (Int, String)
                            -- parse takes a string returns an object that
                            -- might contain a parsed integer and the rest
                            -- of the string
λ: parse "12"
Just (12, "")               -- succesfully returns the integer 12, inside a
                            -- Maybe object
λ: parse "-82 zero"
Just (-82, " zero")         -- succesfully returns the integer -82
λ: parse "hello"
Nothing                     -- fails
~~~

Okay, cool.  Let's get started.

### Our base parsers

So remember, the whole point is to be able to somehow build complex parsers
out of very simple ones.  So let's start with the simplest parsers of all: a
parser that always fails.

~~~haskell
fail :: String -> Maybe (a, String)
                            -- fail is a function that takes a string and
                            -- returns a Maybe

fail _ = Nothing            -- in Haskell, _ is a wildcard.  fail will ignore
                            -- its input and return Nothing
~~~

~~~haskell
λ: fail "12"
Nothing
λ: fail "anything"
Nothing
~~~

Easy enough.  How about some parsers that always succeeds with a pre-defined
answer, and does not consume any of the string?

~~~haskell
succeedWithTrue :: String -> Maybe (Bool, String)
succeedWithTrue str = Just (True, str)

succeedWithZero :: String -> Maybe (Int, String)
succeedWithZero str = Just (0, str)
~~~

~~~haskell
λ: succeedWithTrue "12"
Just (True, "12")
λ: succeedWithZero "anything"
Just (0, "anything")
~~~

We have two functions here.  If you notice, they pretty much do the same
thing, and have very similar structure.  This is a key flag that we can
probably abstract out some part of it.  Let's write a new function that covers
both of them:

~~~haskell
succeed :: a -> (String -> Maybe (a, String))
succeed value = succeedWithValue
    where
        succeedWithValue str = Just (val, str)

succeedWithTrue :: String -> Maybe (Bool, String)
suceedWithTrue = succeed True

succeedWithZero :: String -> Maybe (Int, String)
succeedWithZero = succeed 0
~~~

So `succeed` is a function that takes any value, and returns a
"succeedWithValue" function/parser that always succeeds with the value you
specify. Let's try it out.

~~~haskell
λ: (succeed 5) "hello"
Just (5, "hello")
λ: (succeed 8.26) "hello"
Just (8.26, "hello")
λ: :t (succeed True)
succeed True :: String -> Maybe (Bool, String)
~~~

Note that the type of `succeed True` is exactly the same type as our
`succeedWithTrue` function.  Which is what we would expect.  Our `succeed`
function takes a value and returns a parser/function that parses anything into
that given value.  So if we pass in `True`, it will return a parser/function
that parses anything into `True`.

Okay, so our two base parses are nice, but it doesn't sound like they are
useful at all for any real parsing work.  Let's write our first
somewhat-useful parser: `anyChar`

~~~haskell
anyChar :: String -> Maybe (Char, String)
anyChar (x:xs) = Just (x, xs)   -- calling anyChar on a non-empty string will
                                -- return a Just with the first character in
                                -- the string and the rest of the string

anyChar _    = Nothing          -- calling anyChar on anything else will fail
~~~

The syntax here might be new to some of you, but `anyChar (x:xs)` "matches" on
any non-empty list, and assigns `x` to the first item and `xs` to the rest.
And similar to languages like `C`, `String` is just a fancy alias for a list
of characters.  So the first item `x` is the first character, and the rest
`xs` is the rest of the string.  We then return `Just (x, xs)` to return a
`Just`with the first character 'parsed' and the rest of the string.

If you try using `anyChar` to parse an empty string, you fail.

Let's see it in work.

~~~haskell
λ: anyChar "h"
Just ('h', "")
λ: anyChar "hello"
Just ('h', "ello")
~~~

So `anyChar` parses a string into a...character.  That's...kind of boring,
admittedly.  I'm sure you could have written something that parses a
one-letter string into a character in any language without a problem.







