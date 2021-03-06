---
title: Abstract Validating Forms with Free Applicative/Alternative
categories: Haskell
tags: functional programming, haskell, types, free, applicative, alternative, ghcjs
create-time: 2018/01/19 23:30:51
date: none
identifier: free-applicative-forms
slug: forms-with-free-applicative-alternative
---

One tool I've been finding myself using a lot recently is the *Free
Applicative* (and *Free Alternative*), from the *[free][]* package.

[free]: https://hackage.haskell.org/package/free

Free Monads are great, and they're often used to implement the "interpreter
pattern" (although I personally prefer *[operational][]*, as I wrote about in a
[previous blog post][duet], for that design pattern).  However, Free
Applicatives are really a completely different type of thing, and the use cases
for each are pretty disjoint.

[operational]: https://hackage.haskell.org/package/operational
[duet]: https://blog.jle.im/entry/interpreters-a-la-carte-duet.html

If I had to make a general statement, I'll say that free monads are especially
good at representing the idea of abstract *sequential* generators (sequences
that are chained dependently one after the other), and that
free applicatives are especially good at representing the idea of abstract
*parallel* generators (things operating in parallel without any interconnected
data dependencies).

For this post, I'll be talking about using the Free Applicative `Ap` (and the
Free Alternative, `Alt`) with an abstract representation of a form element in
order to generate an abstract representation of a validating form, and
leveraging this representation to realize these forms in terminal IO,
JSON/YAML, PDF documents, and even on the browser using *ghcjs* and *[miso][]*.

[miso]: https://hackage.haskell.org/package/miso

Overview
--------

The general approach to utilizing the Free Applicative for this type of
application is to start with some Functor `F` (`F a` represents the act of
generating a value of type `a`).  Once you throw `F` into `Ap` to get `Ap F`,
you now are able to *combine `F`s in parallel* with `<$>`, `<*>`, `liftA2`,
`sequence`, `traverse`, etc., even though `F` normally could not support such
combinations.  Then, finally, you have the ability to provide a concrete
generator function `forall a. Applicative f => F a -> f a` (given `F a`, return
an actual generator of `a`s in some `Applicative`), and the magic of the Free
Applicative will go in and actually run all of your combined `F` actions "in
parallel".  The trick is that, with the same value of `Ap F a`, you can *run
multiple different concrete generators* on it, so you can realize `Ap F` in
multiple different contexts and situations, adapting it for whatever you need.

So, in our case, we're going to be making a Functor representing a form element:

```haskell
data FormElem a
```

Where a `FormElem a` represents a *single form element* producing an `a`.  A
`FormElem Int`, for instance, will represent a single form element producing an
`Int`.

Then, we can create, using [Ap][]:

[Ap]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html

```haskell
type Form = Ap FormElem
```

And now we have a type where `Form a` is *whole form with multiple elements*
that all work together to produce a value of type `a`!

For example, if we had `intElem :: FormElem Int`, then `liftAp intElem :: Form
Int`, a single-item form that makes an `Int`:

```haskell
intElem :: FormElem Int

intForm :: Form Int
intForm = liftAp intElem
```

We can now do all of our Applicativey stuff with it, to generate, for instance,
a form with two items that produces their sum:

```haskell
-- | A form with two elements, whose overal result is the sum of the two
-- element's inputs
addingForm :: Form Int
addingForm = (+) <$> intForm <*> intForm
```

Or, we can even generate a form with many 'Int' elements, and produce a
list of all of their items:

```haskell
-- | A form with five elements, whose result is a list of all of their inputs
bunchaInts :: Form [Int]
bunchaInts = replicateM 5 intForm
```

Of course, in real life, forms usually have *Alternative* instances, which
allows you to use `<|>` to "chose" a result between potentially invalid
entries, and also create "optional" entries for free using `optional`.  To do
that, we actually use the *Free Alternative*, *[Alt][]*, instead:

[Alt]: https://hackage.haskell.org/package/free/docs/Control-Alternative-Free-Final.html

```haskell
type Form = Alt FormElem

intForm :: Form Int
intForm = liftAlt intElem
```

And now we get the ability to represent forms with multiple options with `<|>`:

```haskell
boolForm :: Form Bool

-- | A form with two elements (one producing an 'Int' and one producing a
-- 'Bool'), where the result is 'Either Int Bool' -- a "the first or the
-- second".
eitherInt :: Form (Either Int Bool)
eitherInt = (Left <$> intForm) <|> (Right <$> boolForm)
```

From multiple forms, with `choice`:

```haskell
stringForm :: Form String

-- | A form with five elements -- an 'Int' element, a 'Bool' element, and three
-- 'String' elements.  The result is the first of the four options to succeed.
oneOfMany :: Form String
oneOfMany = choice [ show <$> intForm
                   , show <$> boolForm
                   , stringForm
                   , (++) <$> stringForm <*> stringForm
                   ]
```

And create "optional" entries:

```haskell
-- | A form with a single 'Int' element that returns a 'Maybe Int', because
-- it's optional.
optionalInt :: Form (Maybe Int)
optionalInt = optional intForm
```

We get all of these capabilities *for free*!  All we did was *define a single
form element*.  Our form element type doesn't have have any concept of
combining with other elements or of being able to choose between different
elements or of having optional results.  Then, `Alt` gives us the ability to
combine them with `<*>`/`<$>`, create optional form items with `optional`, and
create multiple form options with `<|>`!

Our Types
---------

### Form Element

Our form elements will all have monomorphic base element paired with a
"parser", a description, and an identifier.

To start off, we'll make a GADT representing a concrete element, as well as
what is required to actually represent it for the user to interact with:

```haskell
!!!free-applicative-forms/Form.hs "data Elem ::"
```

We tag the `Elem` with a type representing the element's *native* output.  So
an `Elem String` is an element that natively/naively outputs a `String` (like a
text input), an `Elem Bool` is an element that natively/naively outputs a
`Bool` (like check box), etc.

Each native `Elem` contains the information necessary to render it -- so we
have:

*   `EText`, natively outputting a `String`.
*   `ENumber`, natively outputting a number (`Scientific`).
*   `ESelect`, a drop-down menu, a list of strings to show as items, natively
    outputting `Maybe Int` ("maybe" a selected index)
*   `ECheck`, a check box containing labels for its on and off positions,
    natively outputting a `Bool`.

And then we our representation of a generator for a single form element output:

```haskell
!!!free-applicative-forms/Form.hs "data FormElem ::"
```

A `FormElem a` will contain:

*   The `Elem b` representing the actual native form element, with information
    required to render it.
*   A function `b -> Either String a`, which lets you *parse* the element's
    native output into an `a` (what the `FormElem` outputs), with a potential
    `String` error message
*   A display name and identifier, which we will use to describe the element
    and as a part of the JSON schema for our JSON backend.

### Form

We now have a functor, `FormElem`, that is a single form element and all of its
decorations.  Now, for the magic --- to make a full `Form` type, it's just:


```haskell
!!!free-applicative-forms/Form.hs "type Form ="
```

Ta dah!

And just like that, we now have functions like:

```haskell
-- | Map over the results of a Form
fmap   :: (a -> b) -> Form a -> Form b

-- | Combine two forms together, and merge their results with a combining
-- function
liftA2 :: (a -> b -> c) -> Form a -> Form b -> Form c

-- | Combine all of the forms in a list to create a single form, whose result
-- is the collection of all of their results
sequence :: [Form a] -> Form [a]

-- | Duplicate a form multiple times, creating a new form whose reuslt is a
-- collection of all of their results
replicateM :: Int -> Form a -> Form [a]

-- | Combine two forms together to create a final form whose result picks from
-- the two input forms
(<|>) :: Form a -> Form a -> Form a

-- | Combine several forms together to create a final form whose result picks
from one of the several options
choice :: [Form a] -> Form a

-- | Turn a form into an optional form
optional :: Form a -> Form (Maybe a)
```

All that for free.  Neat!

Note -- it is very important to use `Alt` from
*Control.Alternative.Free.Final*, and **not** the one from
*Control.Alternative.Free*.  The *Control.Alternative.Free* `Alt` is broken for
our use case, because it normalizes against [an extra Alternative
law][leftdist] that forms do not obey.

[leftdist]: https://stackoverflow.com/q/45647253/292731

Making Real Forms
-----------------

### Primitive Forms

Let's create a set of primitive `Form`s, which we will use to build up our more
complex ones.

First, a `Form String` with a single text box:

```haskell
!!!free-applicative-forms/Form.hs "stringInput"
```

Note that its `String -> Either String String` function is just `Right`, since
it is always a "successful" parse.

And maybe one that is based on a text box, but instead of just outputting the
output `String`, it parses it into a Haskell value using `Read`:

```haskell
!!!free-applicative-forms/Form.hs "readInput"
```

Now two "numerical" inputs -- one expecting `Integral` values, and another
expecting floating-point values.

```haskell
!!!free-applicative-forms/Form.hs "intInput" "floatInput"
```

A simple drop-down menu element that lets the user pick from a list of items:

```haskell
!!!free-applicative-forms/Form.hs "selectInput"
```

And a simple check box, which outputs one of two items:

```haskell
!!!free-applicative-forms/Form.hs "checkInput" "boolInput"
```

### Sample Form

To explore this type, let's make a sample form which we will be re-using for
the rest of this post!

We will be making a registration form, a form producing an account, which
contains:

1.  A name
2.  An optional age
3.  A favorite color (from one of the pre-defined color, or a custom one)
4.  An account type (normal account, or premium account?)

```haskell
!!!free-applicative-forms/Form.hs "data AccountType" "data Color" "data Account ="
```

And we'll make the form using Applicative style:

```haskell
!!!free-applicative-forms/Form.hs "accountForm ::"
```

If you're feeling fancy, you can also make it using "Applicative Do" style,
which makes it a little more flexible if you want to re-arrange the items in
the form (put the "age" field before the "name" field, etc.)

```haskell
!!!free-applicative-forms/Form.hs "accountFormAdo ::"
```


