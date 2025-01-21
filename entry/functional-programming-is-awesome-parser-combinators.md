Functional Programming is Awesome: Parser Combinators

======================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/functional-programming-is-awesome-parser-combinators.html)

If you've spent your entire life programming imperatively, you could not even
begin to imagine the vast worlds of thinking that are inaccessible because of
restrictions in your thinking. Not "better", or "harder" ways of thinking ---
possibly just as easy, but...different.

Don't believe me? Here is an example of an amazingly awesome and amazing and
incomprehensibly mind-opeining design pattern that shines in functional
programming: applicative combinators. In specific, we are going to write a small
parser library! Yay!

Now, if you are a sane person, you should know that *nobody* enjoys writing
parsers...imperatively. The whole deal is a mess; you have to keep track of
multiple layers of state, work out back tracking and edge cases, and to top it
off, parsers tend to be very uncomposable, and parsing two different things
usually requires either source code surgery in the best case or a complete
rewrite in the worst.

But what if I told you that combinator-based parsers are so easy to work with
and composable and intuitive that they are...well, fun? I'll tease you:

``` haskell
-- a parser for a special variable name, which can optionally start with an
-- `@` symbol, and then an alphabet character, followed by many alphanumeric
-- characters
variableName :: Parser String
variableName = (optional '@') <:> alpha <:> (many alphaNum)

-- usage:
-- λ: parse variableName "9one"
-- Nothing
-- λ: parse variableName "var01"
-- Just ("var01", "")
-- λ: parse variableName "@var2"
-- Just ("@var2", "")
-- λ: parse variableName "@9one"
-- Nothing
```

Let's take a look, shall we? We will first write our library, and then see how
we build complex parsers out of smaller, simpler ones. And maybe learn a little
bit of Haskell along the way --- in particular, the magic of Functors and
Applicative Functors (and maybe some Monads if we're lucky).

(Much of this post is taken from a University of Göteborg [lecture
series](http://www.cse.chalmers.se/edu/course/TDA452/FPLectures/Vid/) available
online; it has been slightly rephrased to be more applicative-centric than
monad-centric)

## Our Parser Type

So; our goal here is to write a library for parsing that is powerful to be
completely general and adaptable for any purpose. And also be very intuitive to
use. Easy, right? Let's start with the type of our Parser!

### Our Parser type

So our Parser type is going to be an object of type `Parser a` that contains one
thing: a parsing function. This parsing function will take in a string and
return the parsed value of type `a`, together with the "leftover" string that is
unparsed.

In Haskell, we represent this object as:

``` haskell
data Parser a = P (String -> (a, String))
```

which reads "The type `Parser a` is an object that you specify by saying
'`P f`', where `f` is a function that takes in a string and returns an `a` with
another string.

::: note
**Aside**

Hi! These asides are for people unfamiliar with Haskell syntax. Feel free to
skip them if you already are comfortable.

If you've ever used an object-oriented language like Java or C++, `Parser a` is
a template, or generic, and would be written in those languages as the class
`Parser<a>`, with only one instance variable: a function taking a string and
returning something of type `a` with a leftover string.
:::

And we write a function `parse` that will take any `Parser a` object and any
string, and return the parsed string:

``` haskell
parse :: Parser a -> String -> (a, String)              -- 1
parse (P f) str = f str                                 -- 2
```

::: note
**Aside**

A bunch of Haskell syntax features here!

1.  This first line says "the function `parse` is a function that takes a
    `Parser a` and a `String` and return something of the value `(a, String)`"

2.  Now to define the function. It has two arguments; the first one is of the
    form `P f`, and the second is just a string `str`. Remember, `P f` is how
    you specify a `Parser a`; the `f` is the function inside the object. We then
    call `f` with `str`, and that's what we want.
:::

Let's say we had a parser `integerParser` pre-made, that parses a string into an
integer. Here is how we would use it in the interactive
[repl](http://en.wikipedia.org/wiki/Read-eval-print_loop) prompt:

``` haskell
λ: :type integerParser
integerParser :: Parser Int
λ: parse integerParser "12"
(12, "")                    -- returns the integer 12, with nothing leftover
λ: parse integerParser "-82 zero"
(82, " zero")               -- returns the integer -82, with some leftovers
λ: parse integerParser "hello"
??????                      -- ??????
```

::: note
**Aside**

In this article, code that begins with `λ:` means stuff entered at the
interactive prompt, ghci. You enter an expression, and it is evaluated, its
result printed.

`:type` is a ghci command that returns the type of the thing in question.
:::

Hm. There's a problem. Sometimes, the parse will fail. How can we indicate that
a parse is failable?

In most languages, we might return a `nil` or `null` value. But this is not cool
in Haskell because we like having type safety --- having things be `nil` or
`null` willy-nilly means that your compiler can't help you, and you open up the
door to runtime errors.

In Haskell, we have an object of type `Maybe a`, which can either be `Just a`
(the object contains a value `a`), or `Nothing` (the object contains nothing).
Let's change our function so that it will return `Just a` if our parse succeeds
and `Nothing` if it fails.

::: note
**Aside**

Again keeping with the analogies to Object-Oriented Programming, you can think
of `Maybe a` as a superclass, `Maybe<a>`, with two subclasses: `Just a` and
`Nothing`. `Just a` contains one instance variable of type `a`, and `Nothing`
contains...nothing.
:::

``` haskell
data Parser a = P (String -> Maybe (a, String))

parse :: Parser a -> String -> Maybe (a, String)
parse (P f) str = f str
```

``` haskell
λ: parse integerParser "12"
Just (12, "")               -- successfully returns the integer 12, inside a
                            -- Maybe object
λ: parse integerParser "-82 zero"
Just (-82, " zero")         -- successfully returns the integer -82
λ: parse integerParser "hello"
Nothing                     -- fails
```

Okay, cool. Let's get started.

### Our base parsers

So remember, the whole point is to be able to somehow build complex parsers out
of very simple ones. So let's start with the simplest parsers of all: a parser
that always fails.

``` haskell
failure :: Parser a
failure = P returnNothing                   -- 1
    where
        returnNothing _ = Nothing           -- 2
```

``` haskell
λ: parse failure "12"
Nothing
λ: parse failure "anything"
Nothing
```

::: note
**Aside**

1.  Remember, we specify/construct parsers as `P f`, where `f` is the parsing
    function.
2.  `_` is a wildcard in Haskell; `returnNothing` is a function that takes
    *anything* and returns `Nothing`.
:::

Easy enough. How about some parsers that always succeeds with a pre-defined
answer, and does not consume any of the string?

``` haskell
successfulTrue :: Parser Bool
successfulTrue = P returnTrue
    where
        returnTrue str = Just (True, str)

successfulZero :: Parser Int
successfulZero = P returnZero
    where
        returnZero str = Just (0, str)
```

``` haskell
λ: parse successfulTrue "12"
Just (True, "12")
λ: parse successfulZero "anything"
Just (0, "anything")
```

We have two parsers here, with two different parse functions. If you notice,
they pretty much do the same thing, and have very similar structure. This is a
key flag that we can probably abstract out some part of it. Let's write a new
parse function that covers both of them:

``` haskell
successful :: a -> Parser a
successful val = P returnVal
    where
        returnVal str = Just (val, str)

successfulTrue :: Parser Bool
successfulTrue = successful True

successfulZero :: Parser Int
successfulZero = successful 0
```

So `successful val` is a function that takes any value and gives us a `Parser a`
with the function `returnVal` --- `returnVal str` yields a `Just` with the `val`
we gave to `successful`, with the string untouched. Let's try it out.

``` haskell
λ: parse (successful 5) "hello"
Just (5, "hello")
λ: parse (successful 8.26) "hello"
Just (8.26, "hello")
λ: :type (successful True)
successful True :: Parser Bool
```

Note that the type of `successful True` is exactly the same type as our
`successfulTrue` function. Which is what we would expect. Our `successful val`
function takes a value and returns a parser that parses anything into that given
value. So if we pass in `True`, it will return a parser/function that parses
anything into `True`.

Okay, so our two base parses are nice, but it doesn't sound like they are useful
at all for any real parsing work. Let's write our first
somewhat-useful-kinda-maybe parser: `anyChar`

``` haskell
anyChar :: Parser Char
anyChar = P getFirst
    where
        getFirst (x:xs) = Just (x, xs)          -- 1
        getFirst []     = Nothing               -- 2
```

::: note
**Aside**

1.  If you call `getFirst` on a non-empty list, it will set the `x` to the first
    element and `xs` to the rest of the list. Kind of like in C, a string is
    just an alias for a list of characters. So `getFirst "hello"` will return
    `Just ('h', "ello")`.
2.  If you call `getFirst` on an empty list, it fails.
:::

Let's see it at work.

``` haskell
λ: parse anyChar "h"
Just ('h', "")
λ: parse anyChar "hello"
Just ('h', "ello")
λ: parse anyChar ""
Nothing
```

So `anyChar` parses a string into a...character. That's...kind of boring,
admittedly. I'm sure you could have written something that parses a one-letter
string into a character in any language without a problem...one that would parse
`"h"`, the string, into `'h'`, the character.

Hm. Let's try something else. Let's try a parser that always fails *unless* the
taken character satisfies a certain condition:

``` haskell
z :: Parser Char
z = P isA_z
    where
        isA_z (x:xs) =  if x == 'z'
                            then Just (x, xs)
                            else Nothing
        isA_z []     =  Nothing

lowercase :: Parser Char
lowercase = P isLowercase
    where
        isLowercase (x:xs)  =   if isLower x
                                    then Just (x, xs)
                                    else Nothing
        isLowercase []      =   Nothing
```

Remember that Haskell's built-in `Data.Char` module provides the function
`isLower :: Char -> Bool`, which is true if the given character is a lowercase
letter and false otherwise.

``` haskell
λ: parse z "not z"
Nothing
λ: parse lowercase "A"
Nothing
λ: parse lowercase "a"
Just ('a', "")
```

But wait! Like in the case for `successful`, we notice a definite pattern...and
we can abstract this out.

``` haskell
satisfies :: (Char -> Bool) -> Parser Char
satisfies p = P passesPredicate
    where
        passesPredicate (x:xs) =  if p z
                                      then Just (x, xs)
                                      else Nothing
        passesPredicate []     =  Nothing


z :: Parser Char
z = satisfies (== 'z')

lowercase :: Parser Char
lowercase = satisfies isLower
```

::: note
**Aside**

Similar in structure to `(*2)`, `(== 'z')` is a function that returns true if
the input is equal to `'z'` and false otherwise. That is, `(== 'z') 'a'` is
false, while `(== 'z') 'z'` is true.
:::

Remember, just like in the case with `successful`: `satisfies p` returns a
`Parser Char` that fails unless the first character satisfies the given
predicate `p`; `p` has to be a function from `Char` to `Bool` --- a predicate on
`Char`s. So then `satisfies isLower` returns a new parser with the predicate
`isLower` as the "checker".

Let's try this out with some more built-in predicates from `Data.Char`:

``` haskell
λ: parse (satisfies isAlpha) "hello"
Just ('h', "ello")
λ: parse (satisfies isDigit) "123abc"
Just ('1', "23abc")
λ: parse (== 'a') "abc"
Just ('a', "bc")
```

In fact, some of these parsers are so useful, let's name some:

``` haskell
digit :: Parser Char
digit = satisfies isDigit

alphaNum :: Parser Char
alphaNum = satisfies isAlphaNum

alpha :: Parser Char
alphaNum = satisfies isAlpha

char :: Char -> Parser Char
char c = satisfies (== c)
```

The last one is particularly neat --- it says "accept only the given character"
--- `char 'a'` generates a parser that succeeds only on the character `a`.

Note that we even can rewrite `anyChar` in terms of `satisfies`:

``` haskell
anyChar :: Parser Char
anyChar = satisfies (const True)
```

where `const True :: a -> Bool` is a function that ignores all its input and
always returns true no matter what. `anyChar` is a parser that will take any
character, no matter what.

### Now we're cooking

Okay, so we are making baby steps. Here are our "base" parsers. They can't
really do much on their own; you can either always fail, always succeed, and
parse any one-character string into a character. But the last one...the ability
to *selectively* parse a string into a character...that might be interesting.
It's not totally obvious yet how we can make it useful, but...I think we have
enough base parsers to start building our *combinators* --- functions that
combine and modify parsers.

## Functors

You have probably worked with functors before in your programming life with or
without knowing it. Functors represent objects or data structures that "contain"
data that can be "mapped over" --- that is, functors are objects where you can
apply functions to the values inside. The most famous functor in any language is
the list, `[]`; another neat one is `Maybe`.

``` haskell
λ: (*2) <$> [1,2,3]
[2,4,6]
λ: (*2) <$> Just 3
Just 6
```

::: note
**Aside**

First of all, to clarify, `(*2)` is a function that doubles whatever is passed
to it --- `(*2) 3` is 6.

Next, in haskell, we can use the `<$>` operator to say "apply this...'inside' of
the structure". This is analogous to the more-often-used `$` operator:

``` haskell
λ: (*2) $ 3
6
```

Which says "apply this (`(*2)`)...to this (`3`)".

Compare that to:

``` haskell
λ: (*2) <$> [3]
[6]
λ: (*2) <$> Just 3
6
```

As you can see, `<$>` is the "container" version of `$`. It is like `$`, but
"inside".

For more information, refer to adit's [amazing
article](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
on the topic.
:::

You can probably see where I'm going with this.

### Turning our Parser into a Functor

#### What does that even mean?

Hopefully you saw this coming. Functors seem to be very cool. But does it even
make sense that our Parser object is a functor? The `Parser Int` object doesn't
"contain" any `Int`s...does it?

Well...it kind of "does"...in two ways, sorta kinda maybe.

1.  You could say that a `Parser Int` "contains" the to-be-parsed integer.
2.  You could say that a `Parser Int` "contains" the **idea** of an
    integer...abstractly. Like, whoa, man.

So...does it make sense to apply a function "inside" a `Parser a`? The answer is
yes!

1.  If a `Parser Int` contains a to-be-parsed integer, then applying a function
    inside a `Parser Int` would be like applying the function to the
    to-be-parsed integer!
2.  If a `Parser Int` contains the *idea* of an integer, then applying a
    function inside a `Parser Int` would be applying the function to the idea of
    the integer.

Huh. This is kinda cool, isn't it? So let's say that when we apply a function
"inside" our Parser...it means that we apply a function to the finished result
of a successful parse.

Basically, we want something like this:

``` haskell
λ: let successfulSix = (*2) <$> (successful 3)
λ: :type successfulSix
successfulSix :: Parser Int
λ: parse successfulSix "something"
Just 6
```

Okay, cool. So instead of `successful 3` always succeeding with a 3, it succeeds
with `(*2) 3`, which is 6. Still kinda boring but w/e.

Let's try something else...we're going to take advantage of the `Data.Char`
method `digitToInt :: Char -> Int`, which takes a `Char` and returns the digit
that it represents (for example, `digitToInt '5'` is 5, the integer):

``` haskell
λ: let digitInt = digitToInt <$> digit      -- 1
λ: :type digitInt
digitInt :: Parser Int
λ: parse digitInt "2"
Just (2, "")
λ: parse digitInt "a"
Nothing
```

1.  Remember, we defined the parser `digit` earlier, as a parser that only
    succeeds if the first character is a digit.

Did you just see that? That sounds genuinely useful! If we manage to implement
`<$>` properly, we just have a way to selectively parse any digit character into
a digit, and fail otherwise.

Let's try chaining `<$>`'s:

``` haskell
λ: let digitIntDoubled = (*2) <$> (digitToInt <$> digit)
λ: :type digitIntDoubled
digitIntDoubled :: Parser Int
λ: parse digitIntDoubled "2"
Just (4, "")
λ: parse digitIntDoubled "a"
Nothing
```

What just happened here?

1.  `digit` is a parser that succeeds only on digit characters.
2.  `digitToInt <$> digit` is the result of applying `digitToInt` on the value
    of a successful parse. Basically, `digitToInt <$> digit` is a *new parser*
    that succeeds on digit characters, returning an integer.
3.  `(*2) <$> (digitToInt <$> digit)` is the result of applying `(*2)` (the
    doubling function) to `digitToInt <$> digit`, the return-digit-as-int
    parser. The `(*2) <$>` doubles the result of the return-digit-as-int parser.
    Essentially, the entire expression is a *new parser* that succeeds on digit
    character, returning the doubled integer as the parse result.

\*Phew\*. That was a conceptual doozy. But this seems like a good path. Do you
see what we did? We turned old, boring parsers into new, interesting parsers by
"transforming" them. This is the kind of thing we were looking for in the first
place, right?

#### Implementing the Functor instance

That's all fine and dandy. Now we have to actually implement `<$>`.

Luckily in Haskell, there are mechanisms for uniting functor interfaces. That
is, if you tell it that `Parser a` is a functor, then you open up all sorts of
powerful functor tools that were written for things like lists and maybe's ---
now we can use them all on `Parser a`, too.

To do this, we "instance" the Functor *typeclass*. A typeclass in Haskell is
like an interface in Object-Oriented Programming. Basically, we say "`Parser a`
is a functor...treat it like one!" But in order to make it a valid functor, we
must tell it how to `fmap`. `fmap` is basically `<$>` --- it takes a function, a
functor object, and returns a new functor object with that function applied
"inside". Once we define `fmap`, then we get `<$>` for free. Or rather, Haskell
knows that our parser is a functor, and knows how to use `<$>` on functors.

Here's how we do it. Hold on tight!

``` haskell
instance Functor Parser where                                       -- 1
    fmap f parser = P mapped_parser_function                        -- 2
        where

            unmapped_result :: Maybe (a, String)
            unmapped_result = parse parser str                      -- 3

            mapped_parser_function :: String -> Maybe (b, String)
            mapped_parser_function str =
                case unmapped_result of                             -- 4
                    Just (result, rest) -> Just (f result, rest)    -- 5
                    Nothing             -> Nothing                  -- 6
```

::: note
**Aside**

1.  This line is syntax for "we are declaring that `Parser` is a functor!", so
    that Haskell can treat `Parser` like a functor (and have `<$>` work).
2.  In order to declare something as a functor, we must define the function
    `fmap :: (a -> b) -> Parser a -> Parser b`, which takes a function from `a`
    to `b`, a `Parser a`, and returns the mapped-over parser/parser with the
    function applied "inside".
3.  Grab the result of the parse with the unmapped parser, first.
4.  Some more Haskell syntax. Basically, the result of `mapped_parser_function`
    is dependent on what `unmapped_result` is.
5.  If `unmapped_result` is a `Just`, then return the success, but apply `f` to
    the result first.
6.  If `unmapped_result` is `Nothing`...well, you can't apply anything to the
    result if the result is a failure (a `Nothing`). Pass on the failure.
:::

And now we can use our `Parser a` as a functor; all of our use cases above will
now work.

### Fun with functors

Let's take a brief moment to see what kind of things we can do with only
functors, and nothing else. It's probably not going to be too much more useful
than what you can write imperatively, but it is interesting what you can do by
only 'modifying' simpler parsers...which is what the whole point of combinators
are.

With that in mind, let's try out some neat use cases.

What if you want to parse a digit and look it up in a list of keywords, and
return that keyword?

``` haskell
keywords :: [String]
keywords = ["debug","normal","warning","error"]

digit :: Parser Char
digit = satisfies isDigit

digitInt :: Parser Int
digitInt = digitToInt <$> digit

keyword :: Parser String
keyword = (!! keywords) <$> digitInt
```

::: note
**Aside**

`(!! keywords)` is a function that looks up the given index in the given list.
In our case, `(!! keywords) 2` would return what in other languages is
`keywords[2]`, which is the string "warning".
:::

``` haskell
λ: parse keyword "1"
Just ("normal", "")
λ: parse keyword "hello"
Nothing
```

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

