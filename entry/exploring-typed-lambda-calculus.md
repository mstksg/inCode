dhall-typed: Exploring an implementation of a typed lambda calculus

====================================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/exploring-typed-lambda-calculus.html)

I've been wrapped up on a side-project for a while that has been an interesting
deep dive into the limits of type-level Haskell. I've been stuck many times, but
I've often come back later with new ideas to go around certain problems. Now at
another stuck point, I just wanted to take some time to put my thoughts out into
a blog post for those interested to see what it feels like to be down in these
miry pits, and also for those more skilled than me to be able to maybe help :)

Keep in mind that the struggles contained in this post are:

-   Not representative of most day-to-day practical type-level Haskell, a la
    *servant* and other type-level user-friendly libraries.
-   More likely than not to be very trivial in a language with actual dependent
    types, like Idris.

Furthermore, this post isn't exactly "beginner friendly" :) It contains mention
of a lot of type-level tools, including GADTs, type-level lists, singletons,
etc. I'll try to explain the more out-there things when I can, but I won't go
into *too* much detail on any single thing. That's because this is less of a
tutorial and more of a journal of my current struggles.

## The motivation

Here I summarize the motivation behind this project. Feel free to skip!

One of my favorite Haskell projects in the past year or so has been
[dhall](https://dhall-lang.org/), a typed and functional turing-incomplete
strongly normalizing configuration language. I honestly believe that dhall is
the solution to many of the problems of configuration languages (w.r.t json,
yaml, xml) in modern software development, and I hope to see it gain widespread
adoption some day as a "killer app" of functional turing-incomplete programming.

One day I came across a post on the Haskell subreddit asking if there was a nice
way to allow user scripts within a Haskell application, and I suggested maybe to
allow users to provide scripts as dhall programs that are interpreted by Haskell
applications. This idea interested me; I tried out a proof-of-concept with
monoidal actions, and it wasn't too bad. But to allow for more expressive
scripts, I tried implementing a framework with *monadic* actions. I tried
implementing a general framework using church-encoded free monads, and (aside
from the lack of type inference) it wasn't altogether too awful.

However, writing the Haskell code to *import* these monadic actions into Haskell
wasn't very fun. The only way I knew how was basically to interpret an untyped
Dhall AST (parsed into Haskell) into something you could run within Haskell.

Interpreting an untyped Dhall AST within Haskell is as much fun as you'd think
it would be (that is, not very). It's essentially the same thing as writing a
`FromJSON` instance --- something you want to avoid doing by hand whenever
possible.

This is where I got the idea: instead of interpreting an untyped Dhall AST...why
don't we interpret a *typed* Dhall AST? That is, an AST where the *type* of the
Dhall expression is directly a part of the type of the AST value. In my mind, a
`DTerm a` would be a dhall term that represents something of type `a`. From
there, you can then convert any typed Dhall AST term into a Haskell value
representing a type you can know statically.

Instead of parsing a `Expr` into your ADT in a way that might fail (because of a
bad `Expr`), you can parse an *explicitly typed* `Expr` into your ADT, in a way
that you ensure all of the fields exist, etc., in a *total* way.

And, in the context of dhall scripting, you could actually just explicitly "run"
the typed ADT directly within Haskell to produce something of an expected type
(like a free monad instantiation).

So, that was the dream. How hard could it be?

## Dhall Calculus

The first step in my journey was to understand the formal system that Dhall is
based on. According to the documentation, dhall has roots in [System
Fω](https://en.wikipedia.org/wiki/System_F#System_F.CF.89).

After doing some heavy digging and reading, this is what I gathered:

### The Hierarchy

We have a hierarchy of values and their meta-level "types". At the bottom, we
have terms (`1`, `True`, `"hello"`, etc.). You can write functions,
applications, etc. with terms. To help us reason with terms, all terms are
categorized into *types* (`Bool`, `Natural`, etc.).

However, types are also their own level of values, in that we can have
type-level functions and things like that. In order to allow us to reason with
these type-level constructs, we categories types into *kinds* (`Type`,
`Type -> Type`), and say that the kind of types that can categorize values is
`Type`.

In Dhall, kinds are also their own level of values. We can have kind-level
functions and function application, and all of those complex structures. To help
us reason with kinds, we categorize kinds into *sorts* (`Kind`, `Kind -> Kind`,
etc.), and say that the sort of kinds that can categorize types is `Kind`.

In Dhall, the level of sorts is where things essentially end. While Dhall sorts
are also their own level of values in a sense, there is a limitation: we cannot
make *sort functions*, and we cannot have *sort variables*. They are (perhaps
superficially) forbidden in Dhall. This effectively stops the hierarchy, because
there is no need to consider the meta-level "type" of a sort. By forbidding sort
variables and sort functions and similar abstractions, we basically make "order"
(the "type" of a sort) a meaningless construct. We make `Sort` the "order" of
sorts that can categorize kinds, but this fundamental limitation means that
`Sort` is the only order that exists.

### Within a Level

Alright, so we've established *why* the hierarchy exists, what gives rise to
them, and the mechanism that essentially cuts it off at *sort*.

So, what actually *lives* at each of these levels? What sort of structures do we
allow that exist completely *within* a level (discounting things that span
multiple levels)?

#### Primitives

At each level exists the primitives of that level that are provided by the
language. Think of these as the "built-in terms", "built-in types", "built-in
kinds", etc. Without primitives, we'd just be manipulating lambdas all day
(which is, incidentally, what [morte](http://hackage.haskell.org/package/morte)
is. In a way, Dhall can be thought of as just a fork of morte that adds
primitives).

At the *term* level, we have primitives like:

``` dhall
1
"hello"
-8
List/fold
Natural/even
```

And also more complex primitive "constructors", that combine sub-terms

``` dhall
x + y                       -- operators in Dhall
[x,y,z]                     -- list literals in Dhall
{ foo = x, bar = y }        -- record literals in Dhall
x ∧ y                       -- record merging in Dhall
< Left = x | Right : y >    -- union literals in Dhall; x is a term, y is a type
```

At the *type* level, we have primitives like:

``` dhall
Bool
Text
List
```

And primitive "constructors:"

``` dhall
{ foo : x, bar : y }       -- record types in Dhall; x and y are types of kind Type
x ⩓ y                      -- merging record types in Dhall
{ foo = x, bar = y }       -- type-level record literals in Dhall; x and y are types
< Left : x | Right : y >   -- union types in Dhall; x and y are types of kind Type
< Left = x | Right : y >   -- type-level union literals in Dhall; x is a type, y is a kind
```

At the *kind* level, we only have the kinds of type-level records and unions and
also kind-level records and unions (and the operations on them) as primitives.
At the *sort* level, we only have the sorts of kind-level records and unions,
and also sort-level records and unions (and operations on them).

#### Function abstraction

At all levels (except for *sort*), we have the *function abstraction*, that
looks like:

``` dhall
\(x : blah) -> thing
```

Here are examples at the term, type, and kind levels:

``` dhall
-- Term-level
\(x : Natural) -> x + 2
-- Type-level
\(x : Type) -> List x
-- Kind-level
\(x : Kind -> Kind) -> x Type
```

Disallowing sort-level function abstractions is a mechanism of artificially
"cutting off" the effective hierarchy at sort. In theory, we could go on
forever; but what ever level we disallow lambdas at is the level that our
hierarchy stops at.

An important point here is that these are all lambdas "within" a single level.
The *input* and the *output* are all within the same level: term to term, type
to type, kind to kind, etc.

As a consequence of "having function abstractions", we also gain a couple of
other constructs:

-   **Variables**: At all levels with function abstractions, we can talk about
    *variables* at that level. A variable is *bound* if it's inside the body of
    a lambda that introduces it, and is *free* otherwise.

-   **Function Application**: At all levels with function abstractions, we also
    have *function applications*, which is the application of a function
    abstraction to a value:

    ``` dhall
    -- Term level
    (\(x : Natural) -> x + 2) 4
    -- Type level
    (\(x : Type) -> List x) Bool
    ```

    *Technically*, we can actually have function application on levels without
    function applications, of that level has the appropriate primitives (like
    `List` on the type level, `Natural/show` at the value level). However, in
    Dhall, this doesn't happen, so we're going to address this as essentially
    paired with function abstraction.

#### Type levels

If a level is a "type" of another (type, kind, sort), they also have a *function
type* construct, which is the *type* of lambda abstractions on the level below.

For example, a term-level lambda abstraction of type `\(x : Natural) -> x + 2`
has the type `Natural -> Natural`, which lives on the type level. Again, this is
specifically for functions on the "same level".

This construct might also classify primitives on the level above. For example,
the `Natural/isEven` primitive in Dhall has type `Natural -> Bool`.

#### Type-of-type levels

If a level is the "type" of a level that is a "type" of another type, it must
also have a "constant" to tie the levels below together. That is, there is a
kind `Type`, that is the kind of all types that categorize terms. We also have
the sort `Kind`, that is the sort of all kinds that categorize types.

#### Recap

That's a lot of constructs, and a lot of conditions. So just to recap, here's a
list of all constructors on each of the levels that we have gone over so far:

Term
:   Primitives
:   Function abstraction
:   Term variables
:   Function application

Type
:   Primitives
:   Function abstraction
:   Type variables
:   Function application
:   Function type

Kind
:   Primitives
:   Function abstraction
:   Kind variables
:   Function application
:   Function type
:   Constant (`Type`)

Sort
:   Primitives
:   Function type
:   Constant (`Kind`)

All of these constructs are things that are "within the same level".

### Inter-Level

Finally, we have three inter-level constructs: *type abstraction*, *type
application*, and *type abstraction type*. I like to call them *polymorphism*,
*instantiation*, and *forall*, as they are realized in Haskell.

**Type abstraction** (polymorphism) lets us parameterize a value based on
something of a type above. A common example is the identity function in Haskell,
which has type `forall a. a -> a`. In Haskell, type abstractions are always
implicit; we don't ever explicitly write them out, and instead the Haskell
compiler implicitly creates it for us with no syntactic overhead. In Dhall, type
abstractions are always explicit: the polymorphic identity function is written
as `\(a : Type) -> \(x : a) -> x`.

In Dhall, there are three type abstractions allowed:

1.  Terms parameterized on types (polymorphic terms)
2.  Terms parameterized on kinds (kind-polymorphic terms)
3.  Types parameterized on kinds (kind-polymorphic types)

I believe this is a fundamental limit of disallowing sort variables. If we
allowed sort-variables, then we'd also have terms parameterized on sorts, types
parameterized on sorts, and kinds parameterized on sorts, adding three new type
abstractions.

**Type application** (instantiation) lets us *apply* a type abstraction to some
input of the appropriate level, or to "instantiate a type variable", in
Haskell-speak.

Finally, the **type abstraction type** is the meta-level "type\* of a type
abstraction, just like how a function type is the"type" of a function
abstraction. We have three corresponding to the above:

1.  The type of terms parameterized on types
2.  The type of terms parameterized on kinds
3.  The kind of types parameterized on kinds[^1]

Note that although these are the types of type abstractions, *primitives* can
also have these types. For example, the `None` term primitive in Dhall has type
`forall (a : Type). Optional a`. Its #1 on first the list above: `None` is a
term parameterized on a type, and its type is the type of terms parameterized on
types.

### Final Summary

To summarize, here's all of the constructs in Dhall:

Term
:   Primitives
:   Function abstraction
:   Term variables
:   Function application
:   Type abstraction (parameterized on types)
:   Type application (parameterized on types)
:   Type abstraction (parameterized on kinds)
:   Type application (parameterized on kinds)

Type
:   Primitives
:   Function abstraction
:   Type variables
:   Function application
:   Type abstraction (parameterized on kinds)
:   Type application (parameterized on kinds)
:   Function type
:   Type abstraction type (terms parameterized on types)
:   Type abstraction type (terms parameterized on kinds)

Kind
:   Primitives
:   Function abstraction
:   Kind variables
:   Function application
:   Function type
:   Type abstraction type (types parameterized on kinds)
:   Constant (`Type`)

Sort
:   Primitives
:   Function type
:   Constant (`Kind`)

Note that Dhall also has `let .. in ..` constructs, for mostly quality of life
purposes. They don't really affect the semantics, but we can essentially treat
them like function abstractions or type applications that are immediately
applied. So, `let x = Natural in blah` is `(\(x : Type) -> blah) Natural`.
However, this does restrict us to only have `let .. in ..`s that correspond to
an appropriate function or type abstraction --- so we can't, say, do
`let x = 1 in Kind`, since there is no abstraction that takes a term and returns
a sort.

### Normalization and Typing Rules

Of course, listing these constructs is one story. Getting the type of them (and
the type of their components) is a different one. For example, we know that
applying a function of type `Natural -> Bool` to a term of type `Natural` gives
us a term of type `Bool`. However, we'll gloss over this for now as we continue
on this high-level overview.

### How does Dhall do it?

In the typical Haskell way, the system is implemented as an ADT, where each
construct is (surprise) a constructor.

However, the untyped Dhall AST squishes all of the distinction between all of
the levels. Half of my journey was actually in re-separating each of these
levels!

The untyped Dhall AST has the following constructors:

-   A constructor for each primitive (on all levels)
-   A constructor for each constant
-   A single constructor for all function abstractions *and* type abstractions
    (that is, a single constructor for all levels).
-   A single constructor for all function applications *and* type applications
    (again, one constructor for all levels).
-   A single constructor for all function types *and* type application (of all
    levels).

(And also the `let .. in ..` constructor)

So at this point, the plan seems clear: simply distinguish each of the
"squished" abstractions into the four different levels ("unsquish" them), and
make them a GADT parameterized on on the types with the right inductive rules.

## Let's Do It

Alright, let's do it! Note that the way we described it, each level only ever
refers to levels above it --- so we get no circular dependencies.

### Sorts

First, sorts. Remember, sorts contain primitives, function types, and `Kind`.
For the sake of simplicity, we're going to ignore Dhall's sort-level primitives:
record and union sorts (the sort of kind-level records).

``` haskell
data DSort = Kind
           | DSort :*> DSort
```

That's it!

### Kinds

Second, kinds. Kinds have sorts, so we're going to make them a GADT
parameterized by the sort of the constructor. However, we also need a
constructor for a kind variable. However, kind variables can all have different
sorts. We're going to implement this using `Index`, a GADT that lets us specify
a specific item from a type-level list:

``` haskell
data Index :: [k] -> k -> Type where
    IZ :: Index (a ': as) a
    IS :: Index as b -> Index (a ': as) b
```

This type comes up pretty often in type-level programming. I won't go too deep
into it, but you can think of a value of type `Index as a` as a witness that `a`
exists in the type-level list `as` --- and that the witness tells you what the
index of that item is:

``` haskell
IZ         :: Index '[1,2,3] 1
IS IZ      :: Index '[1,2,3] 2
IS (IS IZ) :: Index '[1,2,3] 3
```

This essentially gives us a way to specify the kind of free variables. An
`Index` will tell us "which free variable". Here we go!

We'll have a type `DKind ts a`, where `ts` is the sort of each free variable,
and `a` is the *sort* of the kind. For example:

``` haskell
DKind '[ 'Kind, 'Kind ':*> 'Kind ] 'Kind
```

will be a kind of sort `Kind`, with free variables of sort `Kind` and
`Kind -> Kind`. A `DKind '[] a` is a kind of sort `a` with no free variables.

Let's make the data type, then! As we mentioned earlier, kinds have:

-   Primitives
-   Function abstraction
-   Kind variables
-   Function application
-   Function type
-   Type abstraction type (types parameterized on kinds)
-   Constant (`Type`)

Again, we're going to ignore the kind-level primitives for simplicity. This
means no record and union kinds (the kind of type-level records and unions) or
kind records or kind unions.

``` haskell
data DKind :: [DSort] -> DSort -> Type where
    -- | Function abstraction
    KLam  :: Sing t -> DKind (t ': ts) a -> DKind ts (t ':*> a)
    -- | Kind variables
    KVar  :: Index ts a -> DKind ts a
    -- | Function application
    KApp  :: DKind ts (a ':*> b) -> DKind ts a -> DKind ts b
    -- | Funcion type
    (:~>) :: DKind ts 'Kind -> DKind ts 'Kind -> DKind ts 'Kind
    -- | Type abstraction type
    KPi   :: Sing t -> DKind (t ': ts) 'Kind -> DKind ts 'Kind
    -- | Constant
    Type  :: DKind ts 'Kind
```

Note the nice property of GADTs that allow us to encode the typing rules
directly into our constructors. For example, it lets us specify that applying a
kind of sort `a :*> b` to a kind of sort `a` will give us a kind of sort `b`. It
lets us say that `KLam` (function abstraction) "removes" a free variable,
turning it into a bound variable, producing something of kind `t :*> a`.

Note that `KLam` and `KPi` require a singleton of sort `t`. This is basically
indicating the "type" of the argument, mirroring `\(x : Kind) -> ...`.

Some examples of values and their translations:

    Type -> Type
        ==> Type :~> Type
              :: DKind ts 'Kind

    \(x : Kind) -> x
        ==> KLam SKind (KVar IZ)
              :: DKind ts ('Kind ':*> 'Kind)

    \(f : Kind -> Kind) -> f (Type -> Type)
        ==> KLam (SKind :%*> SKind) $
                KVar IZ `KApp` (Type :~> Type)
              :: DKind ts (('Kind ':*> 'Kind) ':*> 'Kind)

    forall (k : Kind) -> Type
        ==> KPi SKind Type
              :: DKind ts 'Kind

Note that according to the footnote above, we are considering kind-polymorphic
types as types (not kinds), and so they have pi-kinds. This differs from
dhall-proper, but is more internally consistent.

### Types

Okay, this is where things get interesting. The type level contains the most
constructs of any level:

-   Primitives
-   Function abstraction
-   Type variables
-   Function application
-   Type abstraction (parameterized on kinds)
-   Type application (parameterized on kinds)
-   Function type
-   Type abstraction type (terms parameterized on types)
-   Type abstraction type (terms parameterized on kinds)

We basically add two things: *type abstraction* and *type application*.

At this point, the decisions become a little less straightforward. I'm going to
describe the path I am currently on, after a lot of dead-ends and backtracking.

The final GADT I came up with was:

``` haskell
data DType ts :: [DKind ts 'Type] -> DKind ts 'Type -> Type where
    -- | Type variables
    TVar  :: Index us a -> DType ts us a
    -- | Function application
    TApp  :: DType ts us (a ':*> b) -> DType ts us a -> DType ts us b
    -- | Funcion type
    (:->) :: DType ts us 'Type -> DType ts us 'Type -> DType ts us 'Type
    -- | Primitives
    Bool  :: DType ts us 'Type
    List  :: DType ts us ('Type '~> 'Type)
```

That's the first half, containing the uncontroversial constructors once we pick
a basic overall schema.

The choice here was to make `DType ts us a` parameterized on both kind variables
*and* type variables, and that all of the type variables must have the same
number of kind variables. So a value of type

``` haskell
DType '[ 'Kind, 'Kind ~> 'Kind    ]
      '[ 'Type, 'KPi 'SKind 'Type ]
      ('Type ':~> 'Type)
```

is a type of kind `Type -> Type` with unbound kind variables of sorts `Kind` and
`Kind -> Kind` and unbound type variables of kinds `Type` and
`forall (k : Kind) -> Type`.

So far, so good. It's pretty easy to bring in some simple type primitives, like
`Bool : Type` and `List : Type -> Type`.

Now, note that I excluded function abstraction and type abstraction type. These
need some care. Naively, we'd want to write:

``` haskell
TLam :: Sing u -> DType ts (u ': us) a -> DType t us (u ':~> a)
TPi  :: Sing u -> DType ts (u ': us) a -> DType t us a
```

Which mirrors `KLam`. However, there is a major problem with this, that we'll
also run into when we implement the term-level lambda: consider these two
identical dhall expression ---

``` haskell
\(a : Type) -> Natural

\(a : ((\(x : Kind) -> x) Type)) -> Natural
```

The *kind* of `a` in both cases is `Type`, but it's not as clear in the second
case. But if we normalize `(\(x : Kind) -> x) Type`, we get `Type` (it's just
the identity function applied to `Type`).

-- TODO: talk about equality rules

The issue here arises when we ask about the *type* of both expressions. In the
first case, it's `Type -> Type` (the input is a `Type`, and so is the output).
In the second case, we can either say it's `((\(x: Kind) -> x) Type) -> Type`,
or also `Type -> Type`.

If we pick the latter, then we need to normalize the kind before putting it into
the type of our GADT. If we pick the former, then we need to make sure we can
later *apply* the function to something like `Bool : Type`. So either we
normalize at the function abstraction stage, or normalize at the function
application stage.

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

[^1]: Actually, it should be noted that in Dhall, kind-polymorphic types are
    actually *kinds*, for some reason. This means that the "type" of types
    parameterized on kinds is actually a sort, not a kind. I actually don't
    fully understand why this is the case, as it seems a little internally
    inconsistent. If anyone does have a reason, please let me know! For the rest
    of this post, the types parameterized on kinds will be thought of as kinds,
    and they will have a kind, not a sort.

