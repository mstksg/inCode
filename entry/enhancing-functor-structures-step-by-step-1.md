Enhancing Functor Structures Step-By-Step (Part 1)

===================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on August 18, 2020.
> [Read online!](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-1.html)

A style of Haskell programming that I've been pretty excited about with over the
past two years or so is something that I can maybe call a "functor structure"
design pattern. In this post we're going to be exploring the idea of enhancing
normal data types with different types of functor structures step-by-step, by
starting with a simple useful structure and enhancing it piece by piece in order
to reap incremental benefits. This process reflects a lot of the way I
personally work through these things --- I normally don't get the whole powerful
structure all the way; instead I incrementally add things as I see how things
fit together.

We're going build the tools to describe a *data type schema*, which can
represent algebraic data types --- sums and products. We'll start off just
building things we can use to *describe* the schema (by printing out
documentation), and by the end of the journey we'll also be able to use our
schema to generate parsers and serializers through json.

This interest in functor structures culminated in my [Functor
Combinatorpedia](https://blog.jle.im/entry/functor-combinatorpedia.html) post
last year and the
*[functor-combinators](https://hackage.haskell.org/package/functor-combinators)*
library. But personally I had never really explored the less commonly used
lowercase-f functor abstractions in Hask --- contravariant functors and
invariant functors until recently.

This series is designed for an intermediate Haskeller with familiarity in things
like product/sum types, using `Applicative`/`Alternative`, and monadic parser
combinators, and is written in sync with *functor-combinators-0.3.6.0*.

## The Schema

Let's start with the simplest level of describing our schema: a plain ol' AST
describing the possibilities our schema can take.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L9-L31

data Schema =
      RecordType  [Field]
    | SumType     [Choice]
    | SchemaLeaf  Primitive
  deriving Show

data Field = Field
    { fieldName  :: String
    , fieldValue :: Schema
    }
  deriving Show

data Choice = Choice
    { choiceName  :: String
    , choiceValue :: Schema
    }
  deriving Show

data Primitive =
      PString
    | PNumber
    | PBool
  deriving Show
```

Our schema will either represent a record of many different fields, a sum of
many different options, or a primitive value. If it's a sum type, it'll be
described by a list of `Choice`, which describes each branch. If it's a record
type, it'll be described by a list of `Field`, which describes each field. If
it's a primitive type, it'll a `Primitive`, which is either a string, number, or
boolean.

Our end goal is to be able to write a schema for a type like

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L33-L36

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show
```

and be able to represent documenting, parsing, and printing it all within
`Schema`. For our basic `Schema` above, this looks like:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L38-L52

customerSchema :: Schema
customerSchema = SumType
    [ Choice
        { choiceName  = "Person"
        , choiceValue = RecordType
            [ Field { fieldName = "Name", fieldValue = SchemaLeaf PString }
            , Field { fieldName = "Age" , fieldValue = SchemaLeaf PNumber }
            ]
        }
    , Choice
        { choiceName  = "Business"
        , choiceValue = RecordType
            [ Field { fieldName = "Employees", fieldValue = SchemaLeaf PNumber } ]
        }
    ]
```

And a value like

``` haskell
PCustomer { cpName = "Sam", cpAge = 40 }
```

might be represented by a json value using our schema like

``` json
{ "tag": "Customer",
  "contents":
    { "Name": "Sam"
    , "Age": 40.0
    }
}
```

## Documentation

Using our schema type, let's make a documentation generator. It'll take a
`Schema` and nicely formatted documentation describing the schema itself.

To make our lives easier, we'll be using the
*[prettyprinter](https://hackage.haskell.org/package/prettyprinter)* library,
which will handle indentation, horizontal and vertical concatenation, and other
printing concerns for us.

Let's build things up by defining documentation generators for our individual
types, so they'll be easier to assemble.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L54-L79

schemaDoc
    :: String       -- ^ name
    -> Schema       -- ^ schema
    -> PP.Doc x

fieldDoc :: Field -> PP.Doc x

choiceDoc :: Choice -> PP.Doc x

primDoc :: Primitive -> PP.Doc x
```

So `schemaDoc` will take the name of our type and a schema, and generate a
`PP.Doc x`, the type of a text document in the *prettyprinter* library. And
`fieldDoc`, `choiceDoc`, and `primDoc` just generate the documentation for each
individual field or constructor.

(I'm using `x` as the name of the type variable (instead of something more
traditional like `a`) to indicate that it isn't meant to be referenced or used
anywhere in any consistent way. Just remember it doesn't mean anything special
syntactically!)

The `\case` syntax is known as *LambdaCase syntax*, and `\case blah -> blah` is
just sugar for `\x -> case x of blah -> blah`; we use it extensively here to
save us from having to think of a throwaway variable name.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L73-L83

fieldDoc :: Field -> PP.Doc x
fieldDoc (Field name val) = schemaDoc name val

choiceDoc :: Choice -> PP.Doc x
choiceDoc (Choice name val) = schemaDoc name val

primDoc :: Primitive -> PP.Doc x
primDoc = \case
  PString -> "string"
  PNumber -> "number"
  PBool   -> "bool"
```

Nothing too fancy here --- since `Field` and `Choice` just have a name and a
sub-schema, we can have them call `schemaDoc`. `primDoc` requires making our
leaf documentation, so we can just print what type they have.

We tie it all together with `schemaDoc`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/doc.hs#L54-L71

schemaDoc
    :: String       -- ^ name
    -> Schema       -- ^ schema
    -> PP.Doc x
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          map (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          map choiceDoc cs
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
```

Here we use `PP.vsep`, which takes a list of docs and concatenates them
vertically, `PP.<+>` which concatenates two docs horizontally, and `PP.indent`
which indents things before going down a level. We appropriately call
`fieldDoc`, `choiceDoc`, and `primDoc` when we actually need to print one of
them.

Hopefully that wasn't too bad! There were a lot of moving parts because we have
a recursive data type, but in the end hopefully each specific branch was
self-contained enough to understand on their own. In the end the important thing
to take away isn't the mechanics of document generation, but rather how the data
flows. Make sure you at least understand how the functions call each other, and
how --- this pattern is going to be very consistent across all the schema
processors we write!

We can test out our function on `customerSchema`, taking advantage of the fact
that `PP.Doc`'s `Show` instance will render the document:

    ghci> schemaDoc "Customer" customerSchema
    (Customer)
    Choice of:
      {Person}
        *   Name: string
        *   Age: number
      {Business}
        *   Employees: number

It works!

## Parsing with Covariance

Now, let's talk about using our `Schema` type to generate a *json parser*. We
want to be able to use our schema type and use it to parse information from a
json value, of a given json format we are expecting.

To do this, we're going to rewrite `Schema` to take a type parameter to
represent the type we want to parse into. A `Schema a` will be a schema that can
be used to generate documentation *and* describe a parser of `a`s. In the end,
we want `customerSchema :: Schema Customer`, and a function like

``` haskell
schemaParser :: Schema a -> Parse ErrType a
```

to generate a json parser of `a`s. We'll be using the json parser type
`Parse err a` from
*[aeson-better-errors](https://hackage.haskell.org/package/aeson-better-errors)*
(not because of the better errors, but just because it's closer to an actual
incremental/stateful parser than other alternatives out there), which can be run
with `parse :: Parse err a -> ByteString -> Either (ParseError err) a`. So our
final interface will look like:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L154-L155

parseSchema :: Schema a -> ByteString -> Either (A.ParseError ErrType) a
parseSchema sc = A.parse (schemaParser sc)
```

To do this, we now have to include information on "how to parse an `a`" in our
schema. "How to parse a record" and "how to parse a sum" are going to be handled
universally for all schemas...so the only thing that really will vary from type
to type (aside from the structure) is how to parse those primitive leaf types
when we encounter them in the json. And so, the main thing we need to modify is
just `Primitive`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L43-L47

data Primitive a =
      PString (String     -> Maybe a)
    | PNumber (Scientific -> Maybe a)
    | PBool   (Bool       -> Maybe a)
  deriving Functor
```

A `Primitive a` now encodes a way to *parse* an `a` if given the appropriate
json primitive. It can be `PString`, `PNumber`, or `PBool`. To create a "String
Parser", you need to use `PString` with a function on "what to do with the
string you get". To create a "Bool parser", you need `PBool` with a function on
what to do with the bool you get. Note that the `PNumber` parser takes a
`Scientific`, which is the type *aeson* (the underlying json library) uses to
represent valid JSON numbers (it's basically `Either Integer Double`).

We can write some helper primitives:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L49-L56

pString :: Primitive String
pString = PString Just

pInt :: Primitive Int
pInt = PNumber toBoundedInteger

pBool :: Primitive Bool
pBool = PBool Just
```

`pString :: Primitive String` is the most basic way to parse a primitive json
string: just return the `String` itself. `pInt` needs to reject any non-integer
numbers, so `toBoundedInteger :: Scientific -> Maybe Int` works well.

We can now start writing our parsers for each branch of `Schema`. The
`SchemaLeaf` branch should be the simplest. We can use *aeson-better-error*'s
primitive value parsers:

``` haskell
-- | Parse successfuly only if the current value is a String, running the
-- validation function
withString     :: (String     -> Either ErrType a) -> Parse ErrType a

withScientific :: (Scientific -> Either ErrType a) -> Parse ErrType a
withBool       :: (Bool       -> Either ErrType a) -> Parse ErrType a
```

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L131-L138

primParser :: Primitive a -> A.Parse String a
primParser = \case
  PString f -> A.withString $
    maybe (Left "error validating string") Right . f
  PNumber f -> A.withScientific $
    maybe (Left "error validating number") Right . f
  PBool f -> A.withBool $
    maybe (Left "error validating bool") Right . f
```

Nothing too fancy, mostly plumbing.

### Deducing Ap

However, this small change (and adding the type parameter) leaves in a
predicament. What should `Schema` look like?

At first glance, we might think we could just write

``` haskell
data Schema a =
      RecordType  [Field a]
    | SumType     [Choice a]
    | SchemaLeaf  (Primitive a)
  deriving Functor

-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L31-L37

data Field a = Field

data Choice a = Choice
```

But there's a problem here: `RecordType` is a combination of `Field`s,
but...each `Field` is of a different type! For example, in our `Customer`
example, the `Person` branch has two fields: `Name` and `Age`. Our name schema
would look like `nameField :: Field String`, and our age schema would look like
`ageField :: Field Int`...and so you can't really put that into a list like
`[Field a]` since they each have different types. And further more, we want a
final `Customer` (in our `Schema Customer`), a type which is different from both
`String` and `Int`.

What we need is a way to express heterogeneous collection/sequence of `Field a`,
coupled with a way of "combining" all of them to create an aggregate value of a
final type. A type that says "use a bunch of `Field` of `x`s of different types
to generate a final `a`".

There are a couple of ways to arrive at this mystery type. One way is to
recognize "combine a bunch of `f x`s of different types to create an `f b`" is
essentially the M.O. of the *Applicative* abstraction, and so essentially we
want to give `Field` some sort of `Applicative` structure. And so we can reach
for "the type that gives something an `Applicative` structure", the [free
applicative](https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html).
(This is the strategy I talk about in my [Applicative Regular
Expressions](https://blog.jle.im/entry/free-alternative-regexp.html) post: if
you know exactly the interface you want, you can just use that interface's free
structure)

Another way is to think about it as an enhancement along a functor combinator
described in the [functor
combinatorpedia](https://blog.jle.im/entry/functor-combinatorpedia.html). Here
we know we want to enhance `Field` in a specific way, so we can scan the list of
functor combinators until there is one that we need. And scrolling down, we see:

> [**Ap / Ap1 **](https://blog.jle.im/entry/functor-combinatorpedia.html#ap-ap1)
>
> **Origin**:
> *[Control.Applicative.Free](https://hackage.haskell.org/package/free/docs/Control-Applicative-Free.html)*
> /
> *[Data.Functor.Apply.Free](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Apply-Free.html)*
>
> **Enhancement**: The ability to provide multiple `f`s that the interpreter
> *must* consume *all* of. (...)
>
> While `ListF` may be considered "multiple options *offered*", `Ap` can be
> considered "multiple actions all *required*". The interpreter must
> consume/interpret *all* of the multiple `f`s in order to interpret an `Ap`.
>
> Note that ordering is not enforced: while the consumer must handle each `f`
> eventually, they are free to handle it in whatever order they desire. In fact,
> they could even all be handled in parallel. See `Free` for a version where
> ordering is enforced.
>
> ...
>
> Because this has an `Applicative` instance, you can use
> `(<*>) :: Ap f (a -> b) -> Ap f a -> Ap f b` to sequence multiple `Ap f`s
> together, and `pure :: a -> Ap f a` to produce a "no-op" `Ap` without any
> `f`s.

That sounds like it matches to me! In order to parse a `RecordType`, we need to
parse *every* `Field`. It doesn't make any sense to skip one field or the other:
they all need to be processed and parsed. This sounds like just the thing we
need.

The description here also gives a clue for what we might want to use for
`SumType` (`ListF` sounds like a good companion for the behavior we want sum
type parsers to have)

### Building Ap

With this, we can write our final `Schema` type.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L25-L47

data Schema a =
      RecordType  (Ap    Field  a)
    | SumType     (ListF Choice a)
    | SchemaLeaf  (Primitive a)
  deriving Functor

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }
  deriving Functor

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }
  deriving Functor

data Primitive a =
      PString (String     -> Maybe a)
    | PNumber (Scientific -> Maybe a)
    | PBool   (Bool       -> Maybe a)
  deriving Functor
```

Note that I switched from `[Choice a]` to `ListF Choice a` as hinted earlier ---
the two are the same, but the latter has the `Functor` instance we want
(`fmap :: (a -> b) -> ListF Choice a -> ListF Choice b`), and is an instance of
useful functor combinator typeclasses. Furthermore, it illustrates the symmetry
between sum types and record, since `Ap` and `ListF` are contrasting types: `Ap`
can be used to a represent the "product" between many required fields, and
`ListF` can be used to the option between many possible choices. It's more clear
how product types and sum types are "opposites" in a nice clean way.

We can now make our `Customer` schema:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L63-L77

customerSchema :: Schema Customer
customerSchema = SumType $
      inject Choice
        { choiceName  = "Person"
        , choiceValue = RecordType $
            CPerson
              <$> inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString }
              <*> inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
  <!> inject Choice
        { choiceName  = "Business"
        , choiceValue = RecordType $
            CBusiness
              <$> inject Field { fieldName = "Employees", fieldValue = SchemaLeaf pInt }
        }
```

The main new thing is using `inject :: Choice a -> ListF Choice a` and
`inject :: Field a -> Ap Field a` to lift our base types into their appropriate
combinators. Then after that, we just use `Ap`'s `Applicative` instance and
`ListF`'s `Plus` instance to combine them together. Overall it should look very
similar to the schema we wrote for the documentation section.

### Interpreting Ap

Now, the typical way to "run" an applied functor combinator is with interpreting
functions, like:

``` haskell
interpret :: Applicative g => (forall x. f x -> g x) -> Ap    f a -> g a
interpret :: Plus g        => (forall x. f x -> g x) -> ListF f a -> g a
```

You can interpret an `Ap f a` into any `Applicative g`, and you can interpret a
`ListF f a` into any
[`Plus g`](https://hackage.haskell.org/package/semigroupoids-5.3.4/docs/Data-Functor-Plus.html)
(`Plus` is basically `Alternative` without an `Applicative` requirement,
supporting `(<!>) :: f a -> f a -> f a`).

Basically, the strategy for using `interpret` is that you write a function to
interpret any individual `f` you might find in the structure, and `interpret`
will accumulate them all together for you.

In our case, if we decided to use `interpret`, we could write:

``` haskell
interpret :: (forall x. Field  x -> Parse ErrType x) -> Ap    Field  a -> Parse ErrType x
interpret :: (forall x. Choice x -> Parse ErrType x) -> ListF Choice a -> Parse ErrType x
```

Basically, if we have a way to parse each `Field`, then we have a way to parse
an `Ap Field a`. If we have a way to parse each `Choice`, then we have a way to
parse a `ListF Choice a`.

Let's write those individual parsers for each smaller type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L128-L129

fieldParser :: Field a -> A.Parse String a
fieldParser (Field name val) = A.key (T.pack name) (schemaParser val)
```

Here we use *aeson-better-errors*'s `key :: Text -> Parser a -> Parser a`, which
takes a key and a parser, and runs that parser on whatever is under that key.
For `fieldParser`, we run the schema parser for our sub-schema under that key.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L121-L126

choiceParser :: Choice a -> A.Parse String a
choiceParser (Choice name val) = do
  tag <- A.key "tag" A.asString
  unless (tag == name) $
    A.throwCustomError "Tag does not match"
  A.key "contents" $ schemaParser val
```

Our sum type encoding has to be a bit more involved, because json doesn't have
any native sum type construct. The one we're going to use for this post is the
`{"tag": <tag>, "contents": <contents>}"` form. We're going to parse whatever is
in the key `"tag"`, and if that tag matches our current choice's constructor, we
parse the schema parser for our sub-schema under that key. Otherwise, this
choice isn't what is currently in our json value.

Finally, to bring it all together, we use the `interpret` functions we talked
about:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L115-L119

schemaParser :: Schema a -> A.Parse ErrType a
schemaParser = \case
    SumType    cs -> interpret choiceParser cs
    RecordType fs -> interpret fieldParser fs
    SchemaLeaf p  -> primParser p
```

And that's it!

Ah well, not exactly so fast. Even though they could support it,
*aeson-better-errors* doesn't provide `Plus` a for `Parse`. We can write them as
orphans here just because this is a fun learning experience (but we usually do
like to avoid defining instances for types or typeclasses that aren't ours).

`Alt` and `Plus` represent fallback choices: `x <!> y` will try `x` first, then
if `x` fails, try `y`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L108-L111

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance Monad f => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"
```

And...that should work!

``` haskell
ghci> :set -XOverloadedStrings
ghci> parseSchema customerSchema  "{ \"tag\": \"Person\", \"contents\": { \"Name\": \"Same\", \"Age\": 40 } }"
Right (CPerson {cpName = "Same", cpAge = 30})
ghci> parseSchema customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": 3 } }"
Right (CBusiness {cbEmployees = 3})
```

We were able to generate a fully functional parser from our schema, by only
providing parsers for the smaller, more specific types we had (`Field` and
`Choice`), and having them all fit together in a way directed by their
`Applicative` and `Plus` typeclass instances.

### Direct Structural Inspection

However, sometimes the typeclass instances aren't really the best way to handle
things. It gives us a nice principled shortcut --- for example, to interpret out
of an `Ap`, GHC needs a way to know "how to sequence `Parse`s", and so
`interpret` uses the `Applicative` instance for that. But we know there are
usually different ways to sequence or combine actions --- famously in IO, we
have the option to "sequence" IO actions in series or in parallel, with the
default `Applicative` instance being series sequencing. So, offloading our logic
to a typeclass can be a convenient route, but it's not necessarily the behavior
we want.

In our case, the `Plus` instance actually combines failed fallback behavior in
an undesirable way: our errors become not too useful, because `<!>` always picks
the right side's errors, and we eventually run into
`A.throwCustomError "No options were validated"`.

``` haskell
ghci> parseSchema customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": \"Mustard\" } }"
Left (BadSchema [] (CustomError "No options were validated"))
ghci> parseSchema customerSchema  "{ \"tag\": \"Grape\", \"contents\": { \"Color\": \"purple\" } }"
Left (BadSchema [] (CustomError "No options were validated"))
```

Since the definition of `zero` (which was our fault because we wrote it here as
an orphan instance --- oops!) always falls back to the same error, this is not
very useful!

As we see, `interpret` for `ListF`, while convenient, isn't necessarily the best
way to tear down a `ListF`. Luckily, most functor combinators are just ADTs that
we can pattern match and break down and access the structures manually. In the
case of `ListF`, the structure is pretty simple:

``` haskell
data ListF f a = ListF { runListF :: [f a] }
```

Our `ListF Choice a` is just `[Choice a]`. This is something we can work with!
Let's write a better `ListF Choice a` processor by working with the list itself.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L140-L152

schemaParser2 :: Schema a -> A.Parse ErrType a
schemaParser2 = \case
    RecordType fs -> interpret fieldParser fs
    SumType    cs -> do
      let schemaMap = M.fromList 
            [ (nm, vl) | Choice nm vl <- runListF cs ]
      tag <- A.key "tag" A.asString
      case M.lookup tag schemaMap of
        Nothing -> A.throwCustomError $
                "tag " <> tag <> " not recognized: Expected one of "
             <> intercalate ", " (M.keys schemaMap)
        Just sc -> A.key "contents" (schemaParser2 sc)
    SchemaLeaf p  -> primParser p
```

We can use the structure of `ListF` to generate a `Map` associating any tags
with the schemas they are meant to encode. We then parse the tag, look up what
schema it represents (if any) and then use that schema under the contents key.

``` haskell
λ: parseSchema2 customerSchema  "{ \"tag\": \"Business\", \"contents\": { \"Employees\": \"Mustard\" } }"
Left (BadSchema [ObjectKey "contents",ObjectKey "Employees"] (WrongType TyNumber (String "Mustard")))
λ: parseSchema2 customerSchema  "{ \"tag\": \"Grape\", \"contents\": { \"Color\": \"purple\" } }"
Left (BadSchema [] (CustomError "tag Grape not recognized: Expected one of Business, Person"))
```

Much better messages!

### Backporting documentation

Remember that the whole point of this exercise was to *add* functionality to our
schema. That means we also have to upgrade our documentation function as well.

Hopefully it is clear from the structure of our data type that we haven't *lost*
any information. Updating our documentation generator should be just a matter of
changing how to we get the items from our `ListF` and `Ap`.

Following what we just learned, one way to do this would be to use `interpret`
or manually pattern match and take advantage of the structure. However, if we
just want to get a list of monomorphic items from a functor combinator, there's
an abstraction in *functor-combinators* that gives you a "higher-order" version
of `toList` called `htoList`:

``` haskell
htoList :: (forall x. f x -> b) -> ListF f a -> [b]
htoList :: (forall x. f x -> b) -> Ap    f a -> [b]
```

Give it a function to "get" a `b` out of every `f`, it collects the `b` from
every `f` inside the structure and puts it in a list for us. Note that this type
is very similar to the `map` we used earlier:

``` haskell
-- what we used before
map     :: (          Field   -> b) -> [Field]    -> [b]
-- what we can use now
htoList :: (forall x. Field x -> b) -> Ap Field a -> [b]
```

So it looks like `htoList` should work as a drop-in replacement for `map` ...

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L79-L106

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          htoList (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          htoList choiceDoc cs
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
  where
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc (Field name val) = schemaDoc name val
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc (Choice name val) = schemaDoc name val
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ -> "string"
      PNumber _ -> "number"
      PBool   _ -> "bool"
```

Neat, we just had to replace `map (\fld -> ..) fs` with
`htoList (\fld -> ...) fs`, and `map choiceDoc cs` with `htoList choiceDoc cs`.
We were able to re-use the exact same logic --- we lose no power and upgrading
was a straightforward mechanical transformation.

## Contravariant Consumption

Now, let's consider instead the situation where we would want to *serialize* an
`a` with a schema. We'll make a type `Schema a` that represents something that
can encode an `a` as a json value. The overall interface of using that type
would be:

``` haskell
schemaToValue :: Schema a -> a -> Aeson.Value
```

(`Aeson.Value` being the json representation from the *aeson* library)

To keep things simple, let's forget all the parsing stuff for now; we'll add it
back in later. Let's just create a type that can *only* serialize by enhancing
our documentation schema.

Again, for the same reasons as before, we can get away with the only fundamental
change being at the leaves/primitives. Our structure itself is defined by the
ADT, and all of the variations outside of the structure itself comes from how
each leaf is serialized.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L36-L39

data Primitive a =
      PString (a -> String)
    | PNumber (a -> Scientific)
    | PBool   (a -> Bool)
```

A `Primitive a` will be a way to *serialize* a json primitive --- it can be
`PString`, `PNumber`, or `PBool`. To create a "String Serializer", you need to
use `PString` with a function on "how to turn it into a `String`". To create a
"Bool parser", you need `PBool` with a function on what how to turn the value
into a `String`.

Again, it can be useful to add some helper primitives:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L58-L65

pString :: Primitive String
pString = PString id

pInt :: Primitive Int
pInt = PNumber fromIntegral

pBool :: Primitive Bool
pBool = PBool id
```

`pString :: Primitive String` is the most basic way to serialize a primitive
json string: just return the `String` itself. `pInt` needs to serialize the
`Int` into a `Scientific` (the numeric type of the aeson library).

We can start off by writing the serializer for `Primitive` just go get a feel
for how our serializer will work:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L138-L142

primToValue :: Primitive a -> a -> Aeson.Value
primToValue = \case
  PString f -> \x -> Aeson.String (T.pack (f x))
  PNumber f -> \x -> Aeson.Number (f x)
  PBool   f -> \x -> Aeson.Bool   (f x)
```

Again, nothing too fancy --- mostly plumbing along the *aeson* library's
primitive constructors.

### Covariance vs Contravariance

Before we go further, let's take a moment to pause and discuss the difference
between covariant and contravariant functors, and the usefulness of those
concepts. "Covariant" functors (or capital-F `Functor`s in Haskell) are functors
`f` where you can consider `f a` as a "producer" of `a` --- for example,
`Schema a` from our parsing section is a thing you can use to parse/produce an
`a` out of a bytestring. These are things where it makes sense to
`fmap :: (a -> b) -> f a -> f b`: if you have a producer of `a`s, you can always
"post-filter" the result with an `a -> b` to get a producer of `b`s.

"Contravariant" functors (`Contravariant` in Haskell) are functors `f` where you
can consider `f a` as a "consumer" of `a`. For example, `Primtive a` (and the
`Schema a` we want to make) from our serializing section is something that
consums `a`s and produces json values. These are things where it makes sense to
`contramap`:

``` haskell
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a
```

which says: if you have a consumer of `b`s, you can always "pre-filter" the
input with an `a -> b` to get a consumer of `a`s.

### Deducing Dec

Now, back on to building our `Schema` type. Again, we might want to write
something like

``` haskell
data Schema a =
      RecordType  [Field a]
    | SumType     [Choice a]
    | SchemaLeaf  (Primitive a)
  deriving Functor
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L26-L31

data Field a = Field

data Choice a = Choice
```

However, we have a problem here (incidentally, it's the opposite of the problem
we had in the previous case). `Choice a` doesn't quite make sense as the sum
type consumer for `Schema a`, because each `Choice` is only meant to handle the
types in a *specific* branch. For example, in our `Customer` example, for the
`CPerson` branch we need a `Choice (String, Int)` to consume its contents, and
in the `CBusiness` branch we need a `Choice Int` to consume its contents.

What we need is a way to express a hetereogenous collection/sequence of
`Choice a`, coupled with a way of "choosing" exactly one of them to handle one
form that our input `a` can take. A type that says "use exactly one of a bunch
of `Choice`s of different `x`s, and choose one to dispatch depending on what `a`
we get". So how do we find the tool we need?

*If* you are already familiar with contravariant abstractions (but who is?) you
might recognize this as the essence of the
[Decidable](https://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant-Divisible.html#g:6)
typeclass, from the
*[contravariant](https://hackage.haskell.org/package/contravariant)*
library...or more accurately, "Decidable without a Divisible constraint", which
is
[Conclude](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Contravariant-Conclude.html).
A `Conclude f` allows you to combine two `f` values, and one will be picked to
use based on inspection of the input value. Upon recognizing this, we look for
find a way to give `Choice` some `Conclude` interface and search up "the type
that gives us a free `Conclude` structure". Following that search, we arrive at
[`Dec`](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Contravariant-Divisible-Free.html),
and so we use `Dec Choice a` for our sum type consumer.

But let's say you're like the vast majority of Haskell users and have never had
any reason to look at the contravariant abstraction hierarchy. How would you
think of this?

Like before, we could also look through the [functor
combinatorpedia](https://blog.jle.im/entry/functor-combinatorpedia.html) (in
specific, the contravariant section) and find:

> [**Dec /
> Dec1**](https://blog.jle.im/entry/functor-combinatorpedia.html#dec-dec1)
>
> **Enhancement**: The ability to provide multiple `f`s, one of which will be
> chosen to consume the overall input.
>
> If `f x` is a consumer of `x`s, then `Dec f a` is a consumer of `a`s that does
> its job by choosing a single one of those `f`s to handle that consumption,
> based on what `a` is received.
>
> Contrast this with `Div`, where the multiple `f` actions are *all* used to
> consume the input. `Dec` only uses *one single* `f` action to consume the
> input, chosen at consumption time.
>
> For example, let's say you had a type `Socket a` which represents some IO
> channel or socket that is expecting to receive `a`s. A `Dec Socket b` would be
> a collection of sockets that expects a single `b` overall, and will pick
> exactly one of those `Socket`s to handle that `b`.

Sounds like exactly what we need! It also gives us a nice hint of what we might
want to use for `RecordType`.

### Building Dec

With this, we can write our final `Schema` type.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L21-L39

data Schema a =
      RecordType  (Div Field  a)
    | SumType     (Dec Choice a)
    | SchemaLeaf  (Primitive a)

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }

data Primitive a =
      PString (a -> String)
    | PNumber (a -> Scientific)
    | PBool   (a -> Bool)
```

Note that I switched from `[Field a]` to `Div Field a` --- the two are the same
(`Div Field a` is essentially a newtype wrapper over `[Field a]`), but the
latter has useful functor combinator typeclass instance methods like `interpret`
(like `ListF` before)[^1]. And, again, I feel like it illustrates the symmetry
between sum types and record types; `Div` and `Dec` are opposite types, as `Dec`
represents a contravariant choice between different choices, and `Div`
represents a contravariant merger between different consumers. It makes more
clear the duality between product types and sum types.

We can assemble our `Customer` schema, in a way that looks a lot like our parser
schema:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L72-L87

customerSchema :: Schema Customer
customerSchema = SumType $
    decide (\case CPerson x y -> Left (x, y); CBusiness x -> Right x)
      (inject Choice
        { choiceName = "Person"
        , choiceValue = RecordType $ divided
            (inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString })
            (inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    })
        }
      )
      (inject Choice
        { choiceName  = "Business"
        , choiceValue = RecordType $
            inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
      )
```

Here we use a few contravariant combinators to combine and merge contravariant
functors values (like `Div Field a` and `Dec Choice a`):

`decide` works like:

``` haskell
decide
    :: Conclude f
    => (a -> Either b c)    -- ^ break into branches
    -> f b                  -- ^ handle first branch
    -> f c                  -- ^ handle second branch
    -> f a                  -- ^ overall handler

decide
    :: (Customer -> Either (String, Int) Int)   -- ^ break into branches
    -> Dec Choice (String, Int)                 -- ^ handle CPerson branch
    -> Dec Choice Int                           -- ^ handle CBusiness branch
    -> Dec Choice Customer
```

And `divided` works like:

``` haskell
divided
    :: Divisible f
    => f a          -- ^ first handler
    -> f b          -- ^ second handler
    -> f (a, b)     -- ^ merged handler

divided
    :: Div Field String          -- ^ handle the cpName field
    -> Div Field Int             -- ^ handle the cpAge field
    -> Div Field (String, Int)   -- ^ handle both together
```

### Interpreting Dec

To write our schema serializers, we can use `interpret` again:

``` haskell
interpret :: Divisible g => (forall x. Field  x -> g x) -> Div Field  a -> g a
interpret :: Conclude g  => (forall x. Choice x -> g x) -> Dec Choice a -> g a
```

But, what should we choose as our choice of `g`?

``` haskell
choiceToValue :: Choice a -> g a
```

Well, how do we want to "use" a `Choice a`? Remember that `Schema a` encodes a
way to serialize an `a` to an json value. A `Choice a` would encode a way to
serialize an `a` into a json value. We want to turn a `Choice a` into an
`a -> Aeson.Value`:

``` haskell
choiceToValue :: Choice a -> (a -> Aeson.Value)

-- is supposed to match up with
choiceToValue :: Choice a -> g a
```

So, we need to pick some `g` where `g a` is `a -> Aeson.Value`. This is exactly
`Op` from
*[Data.Functor.Contravariant](https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html)*,
in *base*:

``` haskell
data Op r a = Op { getOp :: a -> r }
```

So, if we write

``` haskell
choiceToValue :: Choice a -> Op Aeson.Value a

-- a newtype wrapper away from
choiceToValue :: Choice a -> a -> Aeson.Value
```

then we have

``` haskell
interpret choiceToValue :: Dec Choice a -> Op Aeson.Value a

-- a newtype wrapper away from
interpret choiceToValue :: Dec Choice a -> a -> Aeson.Value
```

Let's write it!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L128-L132

choiceToValue :: Choice a -> Op Aeson.Value a
choiceToValue (Choice name val) = Op $ \x -> Aeson.object
    [ "tag"      Aeson..= T.pack name
    , "contents" Aeson..= schemaToValue val x
    ]
```

Now onto `RecordType`'s `Div Field`. Here, we want to build an object using
`Aeson.object :: [Aeson.Pair] -> Aeson.Value` (one way that the *aeson* library
allows us to build objects). Therefore, our type for `fieldToValue` should be:

``` haskell
fieldToValue :: Field a -> a -> [Aeson.Pair]
```

This looks familiar; it's the same thing as before, but with `Op [Aeson.Pair]`
instead of `Op Aeson.Value`.

``` haskell
fieldToValue :: Field a -> Op [Aeson.Pair] a

interpret fieldToValue :: Div Field a -> Op [Aeson.Pair] a

-- a newtype wrapper away from
interpret fieldToValue :: Div Field a -> a -> [Aeson.Pair]
```

We can go ahead and write it out, actually:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L134-L136

fieldToValue :: Field a -> Op [Aeson.Pair] a
fieldToValue (Field name val) = Op $ \x ->
    [T.pack name Aeson..= schemaToValue val x]
```

(Note that this behavior relies on the fact that the `interpret` instance for
`Div` --- using the `Divise` instance for `Op r` --- will combine the
`[Aeson.Pair]` list monoidally, concatenating the results of calling
`fieldToValue` on every `Field` in the `Div Field a`.)

We should now have enough to write our entire serializer:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L119-L126

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    SumType    cs -> getOp (interpret choiceToValue cs)
    RecordType fs -> Aeson.object . getOp (interpret fieldToValue fs)
    SchemaLeaf p  -> primToValue p
```

Running our `schemaToValue` on a sample `Person` gives the json value we expect:

    ghci> Aeson.encode (schemaToValue customerSchema (CPerson "Sam" 40))
    {"tag":"Person","contents":{"Age":40,"Name":"Sam"}}

#### Some Convenience

Note that this contravariant interpretation pattern (wrapping in `Op` and then
unwrapping it again to run it) is so common that *functor-combinators* has a
helper function to make things a bit neater:

``` haskell
iapply  :: (forall x. f x -> x -> b) -> Dec f a -> a -> b
ifanout :: (forall x. f x -> x -> b) -> Div f a -> a -> [b]
```

With these we could write

``` haskell
choiceToValue :: Choice a -> a -> Aeson.Value
fieldToValue  :: Field a  -> a -> Aeson.Pair
```

And then:

``` haskell
iapply choiceToValue :: Dec Choice a -> a -> Aeson.Value
ifanout fieldToValue :: Div Field  a -> a -> [Aeson.Pair]
```

### Backporting documentation

Because our new structure is pretty much the same as before (data types wrapped
by functor combinators), and `Div`/`Dec` support `htoList` just like
`Ap`/`ListF` did before, the implementation of `schemaDoc` is pretty much
word-for-word identical as it was for our parser schema:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L89-L116

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          htoList (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          htoList choiceDoc cs
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
  where
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc (Field name val) = schemaDoc name val
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc (Choice name val) = schemaDoc name val
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ -> "string"
      PNumber _ -> "number"
      PBool   _ -> "bool"
```

Neat!

## Looking Forward

We first started with a simple structure to represent our schema. We then added
*covariant* capabilities to get us parser generation. Then we added
*contravariant* capabilities to get us serializers.

The next step might be to add *both* enhancements to the same structure! The
benefits for this seem pretty significant: we can write our structure once (less
code, less bugs), and we also write our serializer, parser, and documenting
functions in a way that are automatically kept in-sync, and can never be
incompatible with each other. Solving the documentation rot and mismatched
parser/serializer problem in one stroke!

For this, we'll wait until the next post, where we explore not one, but two ways
to combine our two capabilities into something known as an *invariant* functor!

[Proceed to the next post
here!](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-2.html)

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

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

[^1]: And, if we want it, it has the more useful `Contravariant` instance:
    `contramap :: (a -> b) -> Div Field b -> Div Field a`.

