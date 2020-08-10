---
title: Enhancing Functor Structures Step-By-Step (Part 1)
categories: Haskell
tags: functional programming, free, haskell, interpreters, invariant functors, functor combinators
series: Enhancing Functor Structures
create-time: 2020/07/28 22:12:16
date: never
identifier: functor-structures-1
slug: enhancing-functor-structures-step-by-step-1
---

A style of Haskell programming that I've been pretty excited about with over
the past year or so is something that I can maybe call a "functor structure"
designed pattern.  This is the interest that culminated in my [Functor
Combinatorpedia][fpedia] post last year and the [functor-combinators][]
library.  In the blog post I called this style the "functor combinator" style
because it involved building these functor structures out of small, simple
pieces.  This is just one way to build functor structures --- you could always
just make them directly from scratch, but they can get rather messy and it's
often times easier to use pre-made structures that have all the helper
functions already defined for you.  But I never really talked about *how* you
would build them from scratch, and also never really explored the more exotic
types of lowercase-f functors in Hask --- contravariant functors and invariant
functors.

[fpedia]: https://blog.jle.im/entry/functor-combinatorpedia.html
[functor-combinators]: https://hackage.haskell.org/package/functor-combinators

In this post we're going to be exploring these different types of functor
structures step-by-step, by starting with a simple useful structure and
enhancing it piece by piece.  This process reflects a lot of the way I
personally work through these things --- I normally don't get the whole
powerful structure all the way; instead I incrementally add things as I see how
things fit together.

<!-- We'll be using both a combinator-style approach and also -->
<!-- exploring building some things from scratch --- which may help make them feel -->
<!-- less magical, and also shed some light on the relative drawbacks and advantages. -->

We're going build the tools to describe a *json schema*, in the form of an
algebraic data type -- sums and products.  We'll start off just building things
we can use to *describe* the schema (by printing out documentation), and by the
end of the journey we'll also be able to parse and generate values with our
schema.

This series is designed for an intermediate Haskeller with familiarity in
things like monadic parser combinators.

The Schema
----------

Let's start with the simplest level of describing our schema: a plain ol'
AST describing the possibilities our schema can take.

```haskell
!!!functor-structures/doc.hs "data Schema" "data Field" "data Choice" "data Primitive"
```

Our schema will either represent a record of many different fields, a sum of
many different options, or a primitive value.  If it's a sum type, it'll be
described by a list of `Choice`, which describes each branch.  If it's a record
type, it'll be described by a list of `Field`, which describes each field.  If
it's a primitive type, it'll a `Primitive`, which is either a string, number,
or boolean.

Our end goal is to be able to write a schema for a type like

```haskell
!!!functor-structures/doc.hs "data Customer"
```

and be able to represent documenting, parsing, and printing it all within
`Schema`.  For our basic `Schema` above, this looks like:

```haskell
!!!functor-structures/doc.hs "customerSchema"
```

And a value like

```haskell
PCustomer { cpName = "Sam", cpAge = 40 }
```


might be represented by a json value like

```json
{ "tag": "Customer",
  "contents":
    { "Name": "Sam"
    , "Age": 40.0
    }
}
```

Documentation
-------------

Using our schema type, let's make a documentation generator.  It'll take a
`Schema` and nicely formatted documentation describing what the schema itself.

To make our lives easier, we'll be using the *[prettyprinter][]* library, which
will handle indentation, horizontal and vertical concatenation, and other
printing concerns for us.

[prettyprinter]: https://hackage.haskell.org/package/prettyprinter

Let's build things up by defining documentation generators for our individual
types, so they'll be easier to assemble.

```haskell
!!!functor-structures/doc.hs "schemaDoc"4 "fieldDoc ::"1 "choiceDoc ::"1 "primDoc ::"1
```

So `schemaDoc` will take the name of our type and a schema, and generate a
`PP.Doc x`, the type of a text document in the *prettyprinter* library.[^xvar]
And `fieldDoc`, `choiceDoc`, and `primDoc` just generate the documentation for
each individual field or constructor.

[^xvar]: I'm using `x` as the name of the type variable (instead of something
more traditional like `a`) to indicate that it isn't meant to be referenced or
used anywhere in any consistent way.  Just remember it doesn't mean anything
special syntactically!

```haskell
!!!functor-structures/doc.hs "fieldDoc ::" "choiceDoc ::" "primDoc ::"
```

Nothing too fancy here --- since `Field` and `Choice` just have a name and a
sub-schema, we can have them call `schemaDoc`.  `primDoc` requires making our
leaf documentation, so we can just print what type they have.

We tie it all together with `schemaDoc`:

```haskell
!!!functor-structures/doc.hs "schemaDoc"
```

Here we use `PP.vsep`, which takes a list of docs and concatenates them
vertically, `PP.<+>` which concatenates two docs horizontally, and `PP.indent`
which indents things before going down a level.  We appropriately call
`fieldDoc`, `choiceDoc`, and `primDoc` when we actually need to print one of
them.

Hopefully that wasn't too bad!  There were a lot of moving parts because we
have a recursive data type, but in the end hopefully each specific branch was
self-contained enough to understand on their own.  In the end the important
thing to take away isn't the mechanics of document generation, but rather how
the data flows.  Make sure you at least understand how the functions call each
other, and how --- this pattern is going to be very consistent across all the
schema processors we write!

We can test out our function on `customerSchema`, taking
advantage of the fact that `PP.Doc`'s `Show` instance will render the document:

```
ghci> schemaDoc "Customer" customerSchema
(Customer)
Choice of:
  {Person}
    *   Name: string
    *   Age: number
  {Business}
    *   Employees: number
```

It works!

Parsing with Covariance
-----------------------

Now, let's talk about using our `Schema` type to define a json parser.  We're going
to rewrite `Schema` to take a type parameter to represent the type we want to
parse into.  A `Schema a` will be a schema that can be used to generate
documentation *and* describe a parser of `a`s.  In the end, we want
`customerSchema :: Schema Customer`, and a function like

```haskell
schemaParser :: Schema a -> Parse ErrType a
```

to generate a json parser of `a`s.  We'll be using the json parser type `Parse
err a` from *[aeson-better-errors][]* (not because of the better errors, but just
because it's closer to an actual incremental/stateful parser than other
alternatives out there), which can be run with `parse :: Parse err a ->
ByteString -> Either (ParseError err) a`.  So our final interface will look
like:

[aeson-better-errors]: https://hackage.haskell.org/package/aeson-better-errors

```haskell
!!!functor-structures/parse.hs "parseSchema ::"
```

To do this, we now have to include information on "how to parse an `a`" in our
schema.  Remember that our actual json format is pretty fixed/structured, so
all our `Schema` has to express are variations within that structure. The only
variation that our `Schema` can express are how sums/products are structured,
and also how to parse primitive types.  And so, the main thing we need to
modify is just `Primitive`:

```haskell
!!!functor-structures/parse.hs "data Primitive"
```

A `Primitive a` will be a way to parse a json primitive --- it can be `PString`,
`PNumber`, or `PBool`.  To create a "String Parser", you need to use `PString`
with a function on "what to do with the string you get".  To create a "Bool
parser", you need `PBool` with a function on what to do with the bool you get.

We can write some helper primitives:

```haskell
!!!functor-structures/parse.hs "pString ::" "pInt ::" "pBool ::"
```

`pString :: Primitive String` is the most basic way to parse a primitive json
string: just return the `String` itself.  `pInt` needs to reject any
non-integer numbers, so `toBoundedInteger :: Scientific -> Maybe Int` works
well.

### Deducing Ap

However, this small change (and adding the type parameter) leaves in a
predicament.  What should `Schema` look like?

At first glance, we might think we could just write

```haskell
data Schema a =
      RecordType  [Field a]
    | SumType     [Choice a]
    | SchemaLeaf  (Primitive a)
  deriving Functor
!!!functor-structures/parse.hs "data Field"1 "data Choice"1
```

But there's a problem here: `RecordType` is a combination of `Field`s,
but...each `Field` is of a different type!  For example, in our `Customer`
example, the `Person` branch has two fields: `Name` and `Age`.  Our name schema
would look like `nameField :: Field String`, and our age schema would look like
`ageField :: Field Int`...and so you can't really put that into a list like
`[Field a]` since they each have different types.  And further more, we want a
final `Customer` (in our `Schema Customer`), a type which is different from
both `String` and `Int`.

What we need is a way to express heterogeneous collection/sequence of `Field
a`, coupled with a way of "combining" all of them to create an aggregate value
of a final type.  A type that says "use a bunch of `Field` of `x`s of different
types to generate a final `a`".

There are a couple of ways to arrive at this mystery type.  One way is to
recognize "combine a bunch of `f x`s of different types to create an `f b`" is
essentially the MO of the *Applicative* abstraction, and so essentially we want
to give `Field` some sort of `Applicative` structure.  And so we can reach for
"the type that gives something an `Applicative` structure", the [free
applicative][].

[free applicative]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html

Another way is to think about it as an enhancement along a functor combinator
described in the [functor combinatorpedia][fpedia].  Here we know we want to
enhance `Field` in a specific way, so we can scan the list of functor
combinators until there is one that we need.  And scrolling down, we see:

> [**Ap / Ap1 **](https://blog.jle.im/entry/functor-combinatorpedia.html#ap-ap1)
>
> **Origin**: *[Control.Applicative.Free][]* / *[Data.Functor.Apply.Free][]*
>
> **Enhancement**: The ability to provide multiple `f`s that the interpreter
> *must* consume *all* of. (...)
>
> While `ListF` may be considered "multiple options *offered*", `Ap` can be
> considered "multiple actions all *required*".  The interpreter must
> consume/interpret *all* of the multiple `f`s in order to interpret an `Ap`.
>
> Note that ordering is not enforced: while the consumer must handle each `f`
> eventually, they are free to handle it in whatever order they desire.  In
> fact, they could even all be handled in parallel.  See `Free` for a version
> where ordering is enforced.
>
> ...
>
> Because this has an `Applicative` instance, you can use `(<*>) :: Ap f (a
> -> b) -> Ap f a -> Ap f b` to sequence multiple `Ap f`s together, and `pure
> :: a -> Ap f a` to produce a "no-op" `Ap` without any `f`s.

[Control.Applicative.Free]: https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html
[Data.Functor.Apply.Free]: https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Apply-Free.html

That sounds like it matches to me!  In order to parse a `RecordType`, we need
to parse *every* `Field`.  It doesn't make any sense to skip one field or the
other: they all need to be processed and parsed.  This sounds like just the
thing we need.

The description here also gives a clue for what we might want to use for
`SumType` (`ListF` sounds like a good companion for the behavior we want sum
type parsers to have)

Another way to come to this conclusion is to think about it in terms of
tensoring functors: "how do we want to tensor together *two* `Field`s"?

Well, earlier we said that we want to combine fields of different types, while
also providing a way of combining the two types together to create a final
aggregate type.  We can express this in Haskell with something like:

```haskell
data TwoFields = forall x y. TwoFields (Field x) (Field y) (x -> y -> a)
```

In existential syntax, this says that `TwoFields a` consists of `Field x`
and `Field y` of a "hidden" `x` and `y`, as well as a function to combine the
`x` and `y` to make an `a`.

We can look up what type of tensor this is in the functor combinatorpedia.
Scrolling down, we see:

> [**Day**](https://blog.jle.im/entry/functor-combinatorpedia.html#day)
>
> **Origin**: *[Data.Functor.Day][]*
>
> **Mixing Strategy**: "Both, together forever": provide values from *both*
> functors, and the user *must* also *use* both.
>
> ...
>
> Unlike for `:*:`, you always have to interpret *both* functor values in
> order to interpret a `Day`.  It's a "full mixing".
>
> The mechanism for this is interesting in and of itself.  Looking at the
> definition of the data type:
>
> ~~~haskell
> data Day f g a = forall x y. Day (f x) (g y) (x -> y -> a)
> ~~~
>
> We see that because `x` and `y` are "hidden" from the external world, we
> can't directly use them without applying the "joining" function `x -> y ->
> a`.  Due to how existential types work, we can't get anything out of it
> that "contains" `x` or `y`.  Because of this, *using* the joining function
> requires *both* `f x` and `g y`.  If we only use `f x`, we can only get, at
> best,`f (y -> a)`; if we only use `g y`, we can only get, at
> best, `g (x -> a)`.  In order to fully eliminate *both* existential
> variables, we need to get the `x` and `y` from *both* `f x` and `g y`, as
> if the two values held separate halves of the key.

[Data.Functor.Day]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Day.html

It seems as if our `TwoFields` is exactly `Day Field Field`, so we're on the
right track.

Reading further on in the `Day` section, we see:

> **List type**
>
> ... `Ap f a` is a bunch of `f x`s `Day`d with each other.

All three different ways you might have arrived at the conclusion of using
`Ap`!

### Building Ap

With this, we can write our final `Schema` type.

```haskell
!!!functor-structures/parse.hs "data Schema" "data Field" "data Choice" "data Primitive"
```

Note that I switched from `[Choice a]` to `ListF Choice a` --- the two are the
same, but the latter has the Functor instance we want (`fmap :: (a -> b) ->
ListF Choice a -> ListF Choice b`), and is an instance of useful functor
combinator typeclasses.  Furthermore, it illustrates the symmetry between sum
types, since `Ap` and `ListF` are contrasting types: `Ap` represents a product
between many required fields, and `ListF` represents a sum between many
possible choices.  It's more clear how product types and list types are
"opposites" in a nice clean way.

We can now make our `Customer` schema:

```haskell
!!!functor-structures/parse.hs "customerSchema ::"
```

The main new thing is using `inject :: Choice a -> ListF Choice a`
and `inject :: Field a -> Ap Field a` to lift our base types into their
appropriate combinators.  Then after that, we just use `Ap`'s `Applicative`
instance and `ListF`'s `Plus` instance to combine them together.  Overall it
should look very similar to the schema we wrote for the documentation section.

### Interpreting Ap

Now, the typical way to "run" an applied functor combinator is with
interpreting functions, like:

```haskell
interpret :: Applicative g => (forall x. f x -> g x) => Ap f a    -> g a
interpret :: Plus g        => (forall x. f x -> g x) => ListF f a -> g a
```

You can interpret an `Ap f a` into any `Applicative g`, and you can interpret a
`ListF f a` into any `Plus g` (`Plus` is basically `Alternative` without an
`Applicative` requirement).  Basically, you write a function to interpret any
`f` you find, and `interpret` will accumulate them all together for you.

In our case, if we decided to use `interpret`, we could write:

```haskell
interpret :: (forall x. Field  x -> Parse ErrType x) -> Ap    Field  a -> Parse ErrType x
interpret :: (forall x. Choice x -> Parse ErrType x) -> ListF Choice a -> Parse ErrType x
```

Basically, if we have a way to parse each `Field`, then we have a way to parse
an `Ap Field a`.  If we have a way to parse each `Choice`, then we have a way
to parse a `ListF Choice a`.

Let's write those individual parsers for each smaller type:

```haskell
!!!functor-structures/parse.hs "fieldParser ::"
```

Here we use *aeson-better-errors*'s `key :: Text -> Parser a -> Parser a`,
which takes a key and a parser, and runs that parser on whatever is under that
key.  For `fieldParser`, we run the schema parser for our sub-schema under that
key.

```haskell
!!!functor-structures/parse.hs "choiceParser ::"
```

Our sum type encoding is a bit more involved, because json doesn't have any
native sum type construct.  So we're going to parse whatever is in the key
`"tag"`, and if that tag matches our current choice's constructor, we parse the
schema parser for our sub-schema under that key.  Otherwise, this choice isn't
what is currently in our json value.

```haskell
!!!functor-structures/parse.hs "primParser ::"
```

Our primitive parser is just going to use *aeson-better-error*'s primitive
value parsers with the function in them --- nothing too fancy, just some
plumbing.

Finally, to wrap bring it all together, we use the `interpret` functions we
talked about:

```haskell
!!!functor-structures/parse.hs "schemaParser ::"
```

And that's it!

Ah well, not exactly so fast.  Even though they could support it,
*aeson-better-errors* doesn't provide `Plus` a for `Parse`.
We can write them as orphans here just because this is a fun learning
experience (but we usually do like to avoid defining instances for types or
typeclasses that aren't ours).

```haskell
!!!functor-structures/parse.hs "instance Monad f =>"
```

And...that should work!

```haskell
ghci> :set -XOverloadedStrings
ghci> parseSchema customerSchema  "{ \"tag\": \"Person\", \"contents\": { \"Name\": \"Same\", \"Age\": 40 } }"
Right (CPerson {cpName = "Same", cpAge = 30})
ghci> parseSchema customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": 3 } }"
Right (CBusiness {cbEmployees = 3})
```

We were able to generate a fully functional parser from our schema, by only
providing parsers for the smaller, more specific types we had (`Field` and
`Choice`), and having them all fit together in a way directed by their
`Apply` and `Alt` typeclass instances.

### Direct Structural Inspection

However, sometimes the typeclass instances aren't really the best way to handle
things.  It gives us a nice principled shortcut --- for example, to interpret
out of an `Ap`, GHC needs a way to know "how to sequence `Parse`s", and so
`interpret` uses the `Applicative` instance for that.  But we know there are
usually different ways to sequence or combine actions --- famously in IO, we
have the option to sequence IO actions in series or in parallel.  So,
offloading our logic to a typeclass can be a convenient route, but it's not
necessarily the most pragmatic way.

In our case, the `Plus` instance actually combines failed fallback behavior in
an undesirable way: our errors become not too useful, because `<!>` always
picks the right side's errors, and we eventually run into `A.throwCustomError
"No options were validated"`.

```haskell
ghci> parseSchema customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": \"Mustard\" } }"
Left (BadSchema [] (CustomError "No options were validated"))
ghci> parseSchema customerSchema  "{ \"tag\": \"Grape\", \"contents\": { \"Color\": \"purple\" } }"
Left (BadSchema [] (CustomError "No options were validated"))
```

Since `Plus`'s `zero` definition always falls back to the same error, this is
not very useful!

So `interpret` for `ListF`, while convenient, isn't necessarily the best way to
tear down a `ListF`.  Luckily, most functor combinators are just ADTs that we
can pattern match and break down and access the structures manually.  In the
case of `ListF`, the structure is pretty simple:

```haskell
data ListF f a = ListF { runListF :: [f a] }
```

Our `ListF Choice a` is just `[Choice a]`.  This is something we can work
with!  Let's write a better `ListF Choice a` processor by working with the list
itself.

```haskell
!!!functor-structures/parse.hs "schemaParser2 ::"
```

We can use the structure of `ListF` to generate a `Map` associating any tags
with the schemas they are meant to encode.  We then parse the tag, look up what
schema it represents (if any) and then use that schema under the contents key.

```haskell
λ: parseSchema2 customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": \"Mustard\" } }"
Left (BadSchema [ObjectKey "contents",ObjectKey "Employees"] (WrongType TyNumber (String "Mustard")))
λ: parseSchema2 customerSchema  "{ \"tag\": \"Grape\", \"contents\": { \"Color\": \"purple\" } }"
Left (BadSchema [] (CustomError "tag Grape not recognized: Expected one of Business, Person"))
```

Much better messages!

<!-- Note that, due to the mathematics behind `Plus` and other related abstractions, -->
<!-- any function that consumes `ListF f` or `Ap f`, etc., can be written in terms -->
<!-- of `interpret` over an appropriate `Plus` or `Applicative` instance and -->
<!-- extraction function.  The thing that using `Ap`/`ListF` guarantees is that -->
<!-- however way we construct our `Schema` type, the end result will be "normalized" -->
<!-- towards a canonical structure.  So if we `inject x <!> (inject y <!> inject -->
<!-- z)`, it has to be the same overall schema as `(inject x <!> inject y) <!> -->
<!-- inject z`. -->

### Backporting documentation

Remember that the whole point of this exercise was to *add* functionality to
our schema.  That means we also have to upgrade our documentation function as
well.

Hopefully it is clear from the structure of our data type that we haven't
*lost* any information.  Updating our documentation generator should
be just a matter of changing how to we get the items from our `ListF` and `Ap`.

Yes, we could manually pattern match and take advantage of the structure, or
use an interpretation function directly, etc., but if we just want to get a
list of monomorphic items from a functor combinator, there's a convenient
function in *functor-combinators* called `icollect`:[^icollect]

```haskell
icollect :: (forall x. f x -> b) -> ListF f a -> [b]
icollect :: (forall x. f x -> b) -> Ap    f a -> [b]
```

[^icollect]: `icollect` function is nothing magical --- it's essentially
    `interpret` wrapped with `Const`.

    ~~~haskell
    -- essentially
    icollect f = getConst . interpret (\x -> Const [f x])
    ~~~

Give it a function to "get" a `b` out of every `f`, it collects the `b` from
every `f` inside the structure and puts it in a list for us.  Note that this
type is very similar to the `map` we used earlier:

```haskell
-- what we used before
map      :: (          Field   -> b) -> [Field]    -> [b]
-- what we can use now
icollect :: (forall x. Field x -> b) -> Ap Field a -> [b]
```

So it looks like `icollect` should work as a drop-in replacement for `map` ...

```haskell
!!!functor-structures/parse.hs "schemaDoc"
```

Neat, we just had to replace `map (\fld -> ..) fs` with `icollect (\fld -> ...)
fs`, and `map choiceDoc cs` with `icollect choiceDoc cs`.  We were able to
re-use the exact same logic --- we lose no power and upgrading was a
straightforward mechanical transformation.

Contravariant Consumption
-------------------------

Now, let's consider instead the situation where we would want to *serialize*
an `a` with a schema.  We'll make a type `Schema a` that represents something
that can encode an `a` as a json value; we'll write a function:

```haskell
schemaToValue :: Schema a -> a -> Aeson.Value
```

To keep things simple, let's forget all the parsing stuff for now; we'll add it
back in later.  Let's just create a type that can *only* serialize by enhancing
our documentation schema.

Again, for the same reasons as before, we can get away with the only
fundamental change being at the leaves/primitives.  Our structure is very
limited, and our schema type only expresses only variations within that limit,
and the only variations (aside from sum/record structure) are how each leaf can
be serialized.

```haskell
!!!functor-structures/serialize.hs "data Primitive"
```

A `Primitive a` will be a way to *serialize* a json primitive --- it can be
`PString`, `PNumber`, or `PBool`.  To create a "String Serializer", you need to
use `PString` with a function on "how to turn it into a `String`".  To
create a "Bool parser", you need `PBool` with a function on what how to turn
the value into a `String`.

Again, it can be useful to add some helper primitives:

```haskell
!!!functor-structures/serialize.hs "pString ::" "pInt ::" "pBool ::"
```

`pString :: Primitive String` is the most basic way to serialize a primitive
json string: just return the `String` itself.  `pInt` needs to serialize the
`Int` into a `Scientific` (the numeric type of the aeson library).


### Covariance vs Contravariance

Before we go further, let's take a moment to pause and discuss the difference
between covariant and contravariant functors, and the usefulness of those
concepts.  "Covariant" functors (or capital-F `Functor`s in Haskell) are
functors `f` where you can consider `f a` as a "producer" of `a` --- for
example, `Schema a` from our parsing section is a thing you can use to
parse/produce an `a` out of a bytestring.  These are things where it makes
sense to `fmap :: (a -> b) -> f a -> f b`: if you have a producer of `a`s, you
can always "post-filter" the result with an `a -> b` to get a producer of `b`s.

"Contravariant" functors (`Contravariant` in Haskell) are functors `f` where
you can consider `f a` as a "consumer" of `a`.  For example, `Primtive a` (and
the `Schema a` we want to make) from our serializing section is something that
consums `a`s and produces json values.  These are things where it makes sense
to `contramap`:

```haskell
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a
```

which says: if you have a consumer of `b`s, you can always "pre-filter" the
input with an `a -> b` to get a consumer of `a`s.

### Finding Div

Now, back on to building our `Schema` type.  Again, we might want to write
something like

```haskell
data Schema a =
      RecordType  [Field a]
    | SumType     [Choice a]
    | SchemaLeaf  (Primitive a)
  deriving Functor
!!!functor-structures/schema.hs "data Field"1 "data Choice"1
```

However, this has the same problems as before.  `RecordType` is a combination
of `Field`s, and each `Field` is (again) a different type!  We also have a
unique situation in this case where each `Choice` has to consume a specific
type (the sub-type of our `Sum`) if we want each one to not be a partial
consumer.  For example, for the `CBusiness` branch, we'd want it to have a
`Choice Int` (the `cbEmployees` field), not `Choice Customer` --- `Customer` is
too general of a type, since we want that specific branch to consume only
`Int`s.

So again we have the challenge of "mixing" together the types of our individual
components somehow.

*   For `RecordType`, we need something that can combine multiple `Field x`s
    into a `Schema a` by distributing the `a` input and sharing it across all
    the `Field x`s.
*   For `SumType`, we need something that can combine multiple `Choice x`s into
    a `Schema a` by *redirecting* the `a` input into the appropriate `Choice`
    that is meant to handle it.

These ones are a little trickier because contravariant abstractions like these
are a little less commonly used than the covariant ones we talked about
earlier.




