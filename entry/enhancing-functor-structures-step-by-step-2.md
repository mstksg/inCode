Enhancing Functor Structures Step-By-Step (Part 2)

===================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on August 18, 2020.
> [Read online!](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-2.html)

Welcome to Part 2 of the ["Enhancing Functor Structures"
series](https://blog.jle.im/entries/series/+enhancing-functor-structures.html)!
Here we are taking a base structure describing a data type schema and enhancing
it step-by-step with new functory capabilities: first, covariant capabilities
(to generate parsers), then contravariant capabilities (to generate
serializers)...who knows what might be in store next?

Please do check out [Part
1](https://blog.jle.im/entry/enhancing-functor-structures-step-by-step-1.html)
if you haven't already, since this post pretty much jumps straight into things!

## Parsing and Serializing Invariantly

As we left off our project, we had done three things:

1.  Started with a simple ADT representing the structure we want to be able to
    express
2.  Enhanced that simple ADT with Covariant Functor capabilities, in order to
    interpret it as a parser
3.  Enhanced that original simple ADT with Contravariant Functor, in order to
    interpret it as a serializer.

From this, it seems the next logical step would be to add *both* enhancements to
the same structure!

There are some clear benefits to this --- on the surface, it means we only have
to write code once to get all three things (documentation, parsing, and
serialization). Less code means less bugs!

Even deeper, we can now ensure that our "serialization" and "parsing" functions
are always "in sync". If we defined a separate process/type for serializing and
a separate process/type for parsing, then it's possible we might accidentally
make errors in keeping them in sync...one might use a different tag, or we might
make changes to one but not the other during refactoring. There's a good chance
you have been bitten by situations where documentation becomes out of sync with
actual code.

### Adding Invariance

Like before, the main thing we need to change at the fundamental level is
`Primitive`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L39-L42

data Primitive a =
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)
```

We're just basically combining the additions we made to enable parsing with the
additions we made to enable serialization. Our new `Primitive` type gives us the
capability to do both!

We can say this new `Primitive` is an ["Invariant"
Functor](https://hackage.haskell.org/package/invariant/docs/Data-Functor-Invariant.html):
these are functors that give you "both" capabilities: interpreting covariantly
*and* contravariantly.

Because we must be able to eventually *use* either covariant or contravariant
interpretation on an invariant functor, the corresponding mapping function takes
functions in both ways in order to support both on consumption-time.

``` haskell
class Invariant f where
    invmap :: (a -> b) -> (b -> a) -> f a -> f b
```

### DivAp and DecAlt

By now, we know the drill. We also need to change our `RecordType` and `SumType`
constructors to get the right type of container.

``` haskell
-- Covariant Schema
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/parse.hs#L25-L29

data Schema a =
      RecordType  (Ap    Field  a)
    | SumType     (ListF Choice a)
    | SchemaLeaf  (Primitive a)
  deriving Functor
```

``` haskell
-- Contravariant Schema
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/serialize.hs#L21-L24

data Schema a =
      RecordType  (Div Field  a)
    | SumType     (Dec Choice a)
    | SchemaLeaf  (Primitive a)
```

For the covariant `RecordType`, we used `Ap Field a`. For the contravariant
`RecordType`, we used `Div Field a`. Is there a type that combines *both* `Ap`
and `Div`?

If we browse around, we see that we have
*[DivAp](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Invariant-DivAp.html)*
from the *functor-combinatotrs* library...which appears to be named to in a way
to invoke the idea of having both `Ap` and `Div` capabilities, combined
together.

For the covariant `SumType`, we used `ListF Choice a`. For the contravariant
`SumType`, we used `Dec Choice a`. Is there a type that combines *both* `ListF`
and `Dec`?

If we look nearby `DivAp`, we see the answer:
*[DecAlt](https://hackage.haskell.org/package/functor-combinators/docs/Data-Functor-Invariant-DecAlt.html)*!
It combines both `ListF` and `Dec`.

### Building an Invariant Schema

Let's wire it up:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L24-L42

data Schema a =
      RecordType  (DivAp  Field  a)
    | SumType     (DecAlt Choice a)
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
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)
```

Writing a schema using this type is going to be very similar to writing one for
our other schema types:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L55-L72

customerSchema :: Schema Customer
customerSchema = SumType $
    swerve (\case CPerson x y -> Left (x,y); CBusiness x -> Right x)
           (uncurry CPerson)
           CBusiness
        (inject Choice
          { choiceName  = "Person"
          , choiceValue = RecordType $ gathered
              (inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString })
              (inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    })
          }
        )
        (inject Choice
          { choiceName  = "Business"
          , choiceValue = RecordType $
              inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt }
          }
        )
```

The main difference is, while `decide` expects the `a -> Either b c` splitting
function, `swerve` (the invariant `DecAlt` equivalent) expects also the
functions to "recombine" the `b` and `c` back to `a`.

``` haskell
swerve
    :: (a -> Either b c)    -- ^ break into branches
    -> (b -> a)             -- ^ put the branch back into the original input
    -> (c -> a)             -- ^ put the branch back into the original input
    -> DecAlt f b           -- ^ handle first branch
    -> DecAlt f c           -- ^ handle second branch
    -> DecAlt f a           -- ^ overall handler

swerve
    :: (Customer -> Either (String, Int) Int)   -- ^ break into branches
    -> ((String, Int) -> Customer)              -- ^ put the CPerson branch back into a Customer
    -> (Int -> Customer)                        -- ^ put the CBusiness branch back into a Customer
    -> DecAlt Choice (String, Int)              -- ^ handle CPerson branch
    -> DecAlt Choice Int                        -- ^ handle CBusiness branch
    -> DecAlt Choice Customer

-- compare to what we used last time:
decide
    :: (Customer -> Either (String, Int) Int)   -- ^ break into branches
    -> Dec Choice (String, Int)                 -- ^ handle CPerson branch
    -> Dec Choice Int                           -- ^ handle CBusiness branch
    -> Dec Choice Customer
```

We also note that the invariant version of `divided` is `gathered`.

``` haskell
gathered
    :: DivAp f a          -- ^ first handler
    -> DivAp f b          -- ^ second handler
    -> DivAp f (a, b)     -- ^ merged handler

gathered
    :: DivAp Field String          -- ^ handle the cpName field
    -> DivAp Field Int             -- ^ handle the cpAge field
    -> DivAp Field (String, Int)   -- ^ handle both together

-- compare to what we used last time:
divided
    :: Div Field String          -- ^ handle the cpName field
    -> Div Field Int             -- ^ handle the cpAge field
    -> Div Field (String, Int)   -- ^ handle both together
```

### Using Invariant Schema

Let's look into writing our interpreters. Luckily, we already did most of the
work in the previous post. Writing `schemaDoc`, `schemaParser`, and
`schemaToValue`, we can re-use pretty much all of our code!

The main (unfortunate) difference is that instead of using `interpret` in every
case, we must use `runCoDivAp` to run our `DivAp` in a covariant setting, and
`runContraDivAp` to run our `DivAp` in a contravariant setting (similarly for
`runCoDecAlt` and `runContraDecAlt`).[^1]

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L74-L150

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
      PString _ _ -> "string"
      PNumber _ _ -> "number"
      PBool   _ _ -> "bool"

schemaParser
    :: Schema a
    -> A.Parse String a
schemaParser = \case
    RecordType fs -> runCoDivAp  fieldParser  fs
    SumType    cs -> runCoDecAlt choiceParser cs
    SchemaLeaf p  -> primParser p
  where
    choiceParser :: Choice b -> A.Parse String b
    choiceParser (Choice name val) = do
      tag <- A.key "tag" A.asString
      unless (tag == name) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser val
    fieldParser :: Field b -> A.Parse String b
    fieldParser (Field name val) = A.key (T.pack name) (schemaParser val)
    primParser :: Primitive b -> A.Parse String b
    primParser = \case
      PString _ f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber _ f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool _ f -> A.withBool $
        maybe (Left "error validating bool") Right . f

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    RecordType fs -> Aeson.object
                   . getOp (runContraDivAp  fieldToValue  fs)
    SumType    cs -> getOp (runContraDecAlt choiceToValue cs)
    SchemaLeaf p  -> primToValue p
  where
    choiceToValue :: Choice x -> Op Aeson.Value x
    choiceToValue (Choice name val) = Op $ \x -> Aeson.object
      [ "tag"      Aeson..= T.pack name
      , "contents" Aeson..= schemaToValue val x
      ]
    fieldToValue :: Field x -> Op [Aeson.Pair] x
    fieldToValue (Field name val) = Op $ \x ->
        [T.pack name Aeson..= schemaToValue val x]
    primToValue :: Primitive x -> x -> Aeson.Value
    primToValue = \case
      PString f _ -> Aeson.String . T.pack . f
      PNumber f _ -> Aeson.Number . f
      PBool   f _ -> Aeson.Bool . f
```

And there we have it --- a fully functional bidirectional parser schema type
that we assembled step-by-step, adding each piece incrementally and exploring
the space until we found something useful for us. We have a single schema that
can represent documentation, parsing, and serialization in a way that they are
all kept in sync, after writing things only once!

A cute function we could write to tie things together would be one that does a
round-trip, serializing and then parsing, to make sure things worked properly.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/invariant.hs#L152-L156

testRoundTrip
    :: Schema a
    -> a
    -> Either (A.ParseError String) a
testRoundTrip sch = A.parseValue (schemaParser sch) . schemaToValue sch
```

``` haskell
ghci> testRoundTrip customerSchema (CPerson "Sam" 40)
Right (CPerson {cpName = "Sam", cpAge = 40})
```

Looks solid to me!

## An Alternative Invariant Strategy

The thought process "I want to use both `Div` and `Ap`, let's just look for
`DivAp`" is kind of nice and straightforward. However, there's a major downside
in using `DivAp` and `DecAlt` that make their ergonomics not so great when
building them up.

A major part about what makes `Ap` and `ListF` (and, to an extent, `Div` and
`Dec`) so nice to use is that they are instances of popular Haskell typeclasses
like `Applicative` and `Alternative` (or `Plus`) and using `Applicative` and
`Alternative` interfaces are pretty common in Haskell. Because of this, they are
pretty comfortable for most Haskellers to use.

However, `DivAp` and `DecAlt` aren't really instances of any commonly used
typeclass (aside from `Invariant`).[^2] So you really don't have any nice
interface for them other than just using functions specifically written for
them, like `gather` and `swerve`, which may feel ad-hoc.

Luckily, there's another way to achieve the same goals and also be able to take
advantage of our favorite familiar interfaces. We can "add Contravariance"
directly into `Ap` itself, using
[`Pre`](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Route.html#t:Pre).
This is a trick I first saw used in the
*[unjson](https://hackage.haskell.org/package/unjson)* library.

Recall that `Ap Field a` is a collection that contains a bunch of `Field x`s of
different `x`s, and can be used to covariantly *produce* an `a` by combining all
of the `x`s back together.

Now, a value of type:

``` haskell
Ap (Pre r Field) a
```

will "produce" `a`s covariantly...but will "consume" `r`s contravariantly. You
can think of the `Pre r` as adding an "tunnel" to guide the `r` to each `Field`
in the `Ap`.

Because `Ap` is `Ap` (famous for its `Applicative` instance), we can use normal
Applicative combinators to combine our fake invariant type:

``` haskell
pure :: a -> Ap (Pre r Field) a

(<*>)
    :: Ap (Pre r Field) (a -> b)
    -> Ap (Pre r Field) a
    -> Ap (Pre r Field) b

liftA2
    :: (a -> b -> c)
    -> Ap (Pre r Field) a
    -> Ap (Pre r Field) b
    -> Ap (Pre r Field) c
```

We see that the `Applicative` combinators will recombine our "output" covariant
types appropriately, but will keep the "input" contravariant type constant[^3]

We can construct a value of type `Ap (Pre r Field) a` using `injectPre`, which
asks us to provide that "get an `a` from `r`" function up-front:

``` haskell
injectPre :: (r -> a) -> f a -> Ap (Pre r f) a
```

How do we interpret out of `Ap (Pre r f) a`? Well, there's a useful newtype
wrapper over `Pre` called `PreT` that makes consuming and interpreting it very
clean, by requiring the `r` and `a` to be the same:

``` haskell
newtype PreT t f a = PreT (t (Pre a f) a)

-- | `inject` works just like it did before with `Ap` and `Div`: put that `f`
-- into a `PreT`
inject :: f a -> PreT Ap f a

-- | interpret for PreT treats `PreT Ap f a` as if it were just `Ap f a`, so we
-- interpret into an `Applicative` context, like we did with the parsers when
-- we used `Ap f a`.
interpret
    :: Applicative g
    => (forall x. f x -> g x)
    -> PreT Ap f a
    -> g a

-- | But we can also interpret into a `Divisible` context!  Just like when we
-- used `Div f a` to write our serializer!
preDivisibleT
    :: Divisible g
    => (forall x. f x -> g x)
    -> PreT Ap f a
    -> g a

-- | We can also use htoList like before
htoList
    :: (forall x. f x -> b)
    -> PreT Ap f a
    -> [b]
```

We see that `interpret` for `PreT Ap f a` works just like `interpret` for
`Ap f a`; we don't lose any power, it's the same as always if we wanted to just
use `Ap f a` covariantly to interpret into a parser. Exactly what we did when we
wrote our parser generation.

But, we also gain `preDivisibleT`, which lets us `interpret` into a
contravariant `Divisible` context! Just like as if we had `Div f a`! This is
exactly what we did when we wrote our serializers.

So using `Pre` and `PreT`, we get to *assemble* it using our favorite
`Applicative` combinators...then when we wrap it in `PreT`, we get to
*interpret* it in whatever way we want by choosing different interpreters. It's
the best of both worlds!

We can do the opposite thing with `Dec` as well: we can use
[`Post`](https://hackage.haskell.org/package/functor-combinators/docs/Data-HFunctor-Route.html#t:Post)
to embed covariant capabilities in `Dec`.

Recall that `Div Choice a` is a collection that contains a bunch of `Choice x`s
of different `x`s, and can be used to contravariantly *consume* an `a` (by
sending the `a` to one of the different `Choice x`s).

A value of type:

``` haskell
Dec (Post r Choice) a
```

will "consume" `a`s contravariantly (like a normal `Dec`), but will also produce
`r`s covariantly. You can think of the `Post r` as adding an "tunnel" allowing
the output of each `Choice` to exit out of the `Dec`.

This means we can now use normal `Conclude` contravariant typeclass-based
combinators to combine our fake invariant type:

``` haskell
decide
    :: (a -> Either b c)        -- ^ break into branches
    -> Dec (Post r Choice) b    -- ^ handle first branch
    -> Dec (Post r Choice) c    -- ^ handle second branch
    -> Dec (Post r Choice) a    -- ^ overall handler
```

We see that `decide` will recombine our "input" contravariant types
appropriately, but will keep the "output" covariant type constant[^4].

Again, we can construct a value of type `Dec (Post r Choice) a` using
`injectPost`, which asks us to provide that "embed the `a` in the `r`" function
up-front:

``` haskell
injectPost :: (a -> r) -> f a -> Dec (Post r f) a
```

And again, we have the newtype wrapper `PostT` that gives us convenient
interpreting functions:

``` haskell
newtype PostT t f a = PostT (t (Post a f) a)

-- | `inject` works just like it did before with `Dec` and `ListF`: put that `f`
-- into a `PostT`
inject :: f a -> PostT Dec f a

-- | interpret for PostT treats `PostT Dec f a` as if it were just `Dec f a`, so we
-- interpret into a `Conclude` context, like we did with the serializers when
-- we used `Dec f a`
interpret
    :: Conclude g
    => (forall x. f x -> g x)
    -> PostT Dec f a
    -> g a

-- | But we can also interpret into a `Plus` context!  Just like when we
-- used `ListF f a` to write our parser generation!
postPlusT
    :: Plus g
    => (forall x. f x -> g x)
    -> PostT Choice f a
    -> g a

-- | We can also use htoList like before
htoList
    :: (forall x. f x -> b)
    -> PostT Choice f a
    -> [b]
```

We get the same benefits as for `PreT`: if we want to interpret into a
`Conclude` (like we did for our serializers), we can use `interpret`. If we want
to interpret into a `Plus` (like we did for our parser generation), we can use
`postPlusT`.

With these new tools, we can imagine a different invariant `Schema` type:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/routing.hs#L36-L85

data Schema a =
      RecordType  (PreT  Ap  Field  a)
    | SumType     (PostT Dec Choice a)
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
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)

customerSchema :: Schema Customer
customerSchema = SumType . PostT $
    decide (\case CPerson x y -> Left (x, y); CBusiness x -> Right x)
      (injectPost (uncurry CPerson) Choice
        { choiceName = "Person"
        , choiceValue = RecordType . PreT $ (,)
            <$> injectPre fst Field { fieldName = "Name", fieldValue = SchemaLeaf pString }
            <*> injectPre snd Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
      )
      (injectPost CBusiness         Choice
        { choiceName = "Person"
        , choiceValue = RecordType . inject $
            Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
      )
```

Note that to build up `choiceValue` for `Person`, we can use our normal favorite
`Appliciative` combinators, like `<$>` and `<*>`! And at the top level, we use
`decide` like we did before with our general contravariant combinators.

All of our running functions look pretty much the same as well:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/functor-structures/routing.hs#L87-L167

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
    fieldDoc Field{..} = schemaDoc fieldName fieldValue
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc Choice{..} = schemaDoc choiceName choiceValue
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ _ -> "string"
      PNumber _ _ -> "number"
      PBool   _ _ -> "bool"

schemaParser :: Schema a -> A.Parse ErrType a
schemaParser = \case
    RecordType fs -> interpret fieldParser fs
    SumType    cs -> postPlusT choiceParser cs
    SchemaLeaf p  -> primParser p
  where
    fieldParser :: Field a -> A.Parse String a
    fieldParser Field{..} = A.key (T.pack fieldName) (schemaParser fieldValue)
    choiceParser :: Choice a -> A.Parse String a
    choiceParser Choice{..} = do
      tag <- A.key "tag" A.asString
      unless (tag == choiceName) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser choiceValue
    primParser :: Primitive a -> A.Parse String a
    primParser = \case
      PString _ f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber _ f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool _ f -> A.withBool $
        maybe (Left "error validating bool") Right . f

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    RecordType fs -> Aeson.object . getOp (preDivisibleT fieldToValue fs)
    SumType    cs -> getOp (interpret choiceToValue cs)
    SchemaLeaf p  -> primToValue p
  where
    fieldToValue :: Field a -> Op [Aeson.Pair] a
    fieldToValue Field{..} = Op $ \x ->
        [T.pack fieldName Aeson..= schemaToValue fieldValue x]
    choiceToValue :: Choice a -> Op Aeson.Value a
    choiceToValue Choice{..} = Op $ \x -> Aeson.object
        [ "tag"      Aeson..= T.pack choiceName
        , "contents" Aeson..= schemaToValue choiceValue x
        ]
    primToValue :: Primitive a -> a -> Aeson.Value
    primToValue = \case
      PString f _ -> \x -> Aeson.String (T.pack (f x))
      PNumber f _ -> \x -> Aeson.Number (f x)
      PBool   f _ -> \x -> Aeson.Bool   (f x)
```

Using `DivAp`/`DecAlt` and `PreT Ap`/`PostT Dec` are just two separate styles
for you to consider if we want to go into combining *both* covariant production
*and* contravariant consumption!

## Concluding Thoughts

If you've come this far, thank you for reading!

The thought process described in this series was pretty much my actual thought
process when writing something similar. I needed to provide documentation, a
json parser, and a json serializer for a collection of data formats that I had.
At first I had written three separate systems, and wrote all three separately
for each format. I struggled with keeping all of them in sync, but everything
clicked when I realized I could combine the documentation generator and the
parser generation. I looked at my serializer system with regret on how it had to
be a separate thing. But then I stared really really hard at it, and all of a
sudden the idea of uniting all three of them became something I realized was
worthwhile.

It really was a truly "step-by-step" process...and I think it's pretty rare that
these fully formed united abstractions just pop out of your brain without going
through the process of looking at each individual piece!

In real code this pairing of the covariant and covariant is pretty prevalent. In
another recent situation, I had to deal with "incoming" typed sockets (covariant
outputters) and "outgoing" typed sockets (contravariant consumers)...the
contexts where you get these sort of opposing dual pairs comes up a lot. Being
able write a functor structure that lets you deal with them together can save a
lot of code, reduce the space for errors, and relieve a lot of maintenance
burden. For example, in the case of sockets, we could even write:

``` haskell
-- | allocate an outgoing socket that only accepts values of your schema
makeOutSocket :: Schema a -> IO (OutSocket a)

-- | allocate an incoming socket that awaits values of your schema
makeInSocket :: Schema a -> IO (InSocket a)
```

Here `Schema a` could represent a data protocol; under this system, you have the
assurance that the protocol of sending a data type over a channel is always
going to be the same as the protocol for receiving data, no matter what changes
you make to your type. And you only have to write the code once, not twice!

Try to investigate situations in your life where "structures" could be more
useful as "functor structures"...and then maybe see if there's even more value
you could add by enhancing them with more functor-ness!

Hopefully
*[functor-combinators](https://hackage.haskell.org/package/functor-combinators)*
and the *\[functor combinatorpedia\]\[\]* may be a useful guide along the way!
You don't have to build things "functor combinator style" like in this post (you
could make everything from scratch without using `Ap`/`Dec`, etc.), but I have
found that thinking in this style helps guide your search to solutions that
already exist (like how we found `ListF` by reading about `Ap`), instead of
reinventing the wheel every time. If anything, it can help you reframe the
problem in a way that might make it more easy to grasp.

Until next time, happy Haskelling!

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

[^1]: These are unfortunate consequences of the fact that there is no general
    typeclass that contains both `Applicative` and `Divisible` together, or no
    typeclass that contains both `Plus` and `Conclude` together. If these
    existed, we could just use `interpret` for all four of those functions.

[^2]: There *could* be a typeclass for "combination of `Applicative` and
    `Divisible`" and "combination of `Plus` and `Conclude`":

    ``` haskell
    class DivisibleApplicative f where
      conquerpure :: a -> f a
      divideAp :: (a -> (b, c)) -> (b -> c -> a) -> f b -> f c -> f a
    ```

    And every `Applicative` and `Divisible` instance would be a valid instance
    of this. However, this doesn't really exist in any common Haskell
    libraries...and I'm not sure it exists anywhere at all.

    Having this typeclass would also give us an `interpret` that we can use for
    both `A.Parser ErrType` and `Op Aeson.Value`, so we don't need the awkward
    two-different-interpreter situation we had before.

[^3]: This works out because each of the `Field`s inside could work off of the
    same input type. Remember that `Div f a ~ [f a]`, it's just a list of things
    that consume the same `a`.

[^4]: This works out because each of the `Choice`s inside could be embedded into
    the same output type. Remember that we used `List f a ~ [f a]` for our
    contravariant choice collection before, just a list of things that produce
    the same `a`.

