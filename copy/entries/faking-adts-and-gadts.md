---
title: Faking ADTs and GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
date: 2025/04/01 10:29:41
identifier: faking-adts-and-gadts
slug: faking-adts-and-gadts
---

Haskell is the world's best programming language[^best], but let's face the
harsh reality that a lot of times in life you'll have to write in other
programming languages. But alas you have been fully [Haskell-brained][kmett]
and lost all ability to program unless it is type-directed, you don't even know
how to start writing a program without imagining its shape as a type first.

[^best]: I bet you thought there was going be some sort of caveat in this
footnote, didn't you?

[kmett]: https://x.com/kmett/status/1844812186608099463

Well, fear not. The foundational theory behind Algebraic Data Types and
Generalized Algebraic Data Types (ADTs and GADTs) are so fundamental that
they'll fit (somewhat) seamlessly into whatever language you're forced to
write. After all, if they can fit [profunctor optics in Microsoft's Java
code][profunctor], the sky's the limit!

[profunctor]: https://www.reddit.com/r/haskell/comments/9m2o5r/digging_reveals_profunctor_optics_in_mineacraft/

This is an "April Fools" joke in the tradition of [my previous one][april] in
some of these ways that we are going to twist these other languages might seem
unconventional or possibly ill-advised... but also the title is definitely a
lie: these languages definitely _should_ have them! :D

[april]: https://blog.jle.im/entry/verified-instances-in-haskell.html


Normal ADTs
-----------

As a reminder, algebraic Data Types (ADTs) are products and sums; that's why
they're algebraic, after all!

### Product Types

Products are just immutable structs, which pretty much every language supports
--- as long as you're able to make sure they are never mutated.

Structs in `c`, for example, look like:

```c
#include <stdint.h>

typedef struct {
    uint32_t timestamp;
    double amount;
} Transaction;
```

But you'll need proper immutable API for it:

```c
Transaction createTransaction(uint32_t timestamp, double amount) {
    return (Transaction){ timestamp, amount};
}

uint32_t getTimestamp(const Transaction* t) {
    return t->timestamp;
}

double getAmount(const Transaction* t) {
    return t->amount;
}

Transaction setTimestamp(const Transaction* t, uint32_t timestamp) {
    return (Transaction){timestamp, t->amount};
}

Transaction setAmount(const Transaction* t, double amount) {
    return (Transaction){t->timestamp, amount};
}
```

This is much simpler in languages where you can associate functions with data,
like OOP and classes.  For example, this is the common "value object" pattern
in java (roughly related to the java bean[^java]):

```java
public class Transaction {
    private final long timestamp;
    private final double amount;

    public Transaction(long timestamp, double amount) {
        this.timestamp = timestamp;
        this.amount = amount;
    }

    public long getTimestamp() { return timestamp; }
    public double getAmount() { return amount; }

    public Transaction setTimestamp(long newTimestamp) {
        return new Transaction(newTimestamp, this.amount);
    }

    public Transaction setAmount(double newAmount) {
        return new Transaction(this.timestamp, newAmount);
    }
}
```

[^java]: I didn't think I'd ever write "java bean" non-ironically on my blog,
but there's a first time for everything.

And there you go.  Nothing too surprising there!

In this case, not only are these ADTs (algebraic data types), they're also
ADTs (**abstract** data types): you are meant to work with them based on a
pre-defined abstract interface based on type algebra, instead of their internal
representations.

### Sum Types

If your language doesn't support sum types, usually the way to go is with the
_visitor pattern_: the underlying implementation is hidden, and the only way to
process a sum type value is by providing handlers for every branch --- a
pattern match as a function, essentially. Your sum values then basically
determine which handler is called.

For example, we can implement it for a network address type that can either be
IPv4 or IPv6. Here we are using C++ just for generics and lambdas with
closures, for simplicity, but we'll discuss how this might look in C later.

```cpp
#include <iostream>
#include <format>
#include <cstdint>

struct IPAddress {
    bool isIPv4;
    union {
        uint32_t ipv4;
        uint8_t ipv6[16];
    };
};

template <typename R>
struct IPAddressVisitor {
    R (*visitIPv4)(uint32_t);
    R (*visitIPv6)(const uint8_t (&)[16]);
};

template <typename R>
R acceptIPAddress(const IPAddress& ip, IPAddressVisitor<R> visitor) {
    return ip.isIPv4 ? visitor.visitIPv4(ip.ipv4)
                     : visitor.visitIPv6(ip.ipv6);
}
```

You can create the values using:

```cpp
IPAddress mkIPv4(uint32_t value) {
    return { true, { value } };
}

IPAddress mkIPv6(const uint8_t (&value)[16]) {
    IPAddress out = { false };
    std::copy(std::begin(value), std::end(value), out.ipv6);
    return out;
}

```

And we can show an address:

```cpp
std::string showIPAddress(const IPAddress& ip) {
    IPAddressVisitor<std::string> visitor = {
        [](uint32_t v) {
            return std::format("{}.{}.{}.{}",
                               (v >> 24) & 0xFF, (v >> 16) & 0xFF,
                               (v >> 8) & 0xFF, v & 0xFF);
        },
        [](const uint8_t (&v)[16]) {
            return std::format("{:02X}{:02X}:{:02X}{:02X}:{:02X}{:02X}:{:02X}{:02X}:"
                               "{:02X}{:02X}:{:02X}{:02X}:{:02X}{:02X}:{:02X}{:02X}",
                               v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7],
                               v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15]);
        }
    };
    return acceptIPAddress(ip, visitor);
}
```

Note that in this way, the compiler enforces that we handle every branch. And,
if we ever add a new branch, everything that ever consumes `IPAddress` with an
`IPAddressVisitor` will have to add a new handler.

In a language _without_ generics or powerful enough polymorphism, it's
difficult to enforce the "pure" visitor pattern because you can't ensure that
all branches return the same type.

One common pattern is to have an "effectful" visitor pattern, where the point
isn't to _return_ something, but to execute something on the payload of the
present branch. This is pretty effective for languages like C, javascript,
python, etc. where types aren't really a rigid thing.

For example, this might be how you treat an "implicit nullable":

```javascript
export const visitMaybe = (visitNothing, visitJust, val) =>
  (val == null) ? visitNothing() : visitJust(val);
```

This is basically `for_` from Haskell: You can do something like conditionally
launch some action if the value is present.

```javascript
visitMaybe(
  () => console.log("Nothing to request"),
  (reqPayload) => makeRequest("google.com", reqPayload),
  maybeRequest
);
```

On a simpler note, if your language as subtyping built in (maybe with classes
and subclasses) or some other form of dynamic dispatch, you can implement it in
terms of that, which is nice in python, java, C++, etc.

```java
interface ExprVisitor<R> {
    R visitLit(int value);
    R visitNegate(Expr unary);
    R visitAdd(Expr left, Expr right);
    R visitMul(Expr left, Expr right);
}

abstract class Expr {
    public abstract <R> R accept(ExprVisitor<R> visitor);
}
```

Alternatively, you're in a language where lambdas are easy, instead of tupling
up the visitor, you could just have `accept` itself take a number of arguments
corresponding to each constructor:

```haskell
// Alternative definition without an explicit Visitor class
abstract class Expr {
    public abstract <R> R accept(
        Function<int,R> visitLit,
        Function<Expr,R> visitNegate,
        BiFunction<Expr,Expr,R> visitAdd,
        BiFunction<Expr,Expr,R> visitMul
    );
}
```

(Note that C++ doesn't allow template virtual methods --- not because it's not
possible within the language semantics and syntax, but rather because the
maintainers are too lazy to add it --- so doing this faithfully requires a bit
more creativity)

Now, if your language has dynamic dispatch or subclass polymorphism, you can
actually do a different encoding, instead of the tagged union.  This will work
in languages that don't allow or fully support naked union types, too.
In this method, each constructor becomes a class, but it's important to _only allow_
access using `accept` to properly enforce the sum type pattern.

```java
class Lit extends Expr {
    private final int value;

    public Lit(int value) {
        this.value = value;
    }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) {
        return visitor.visitLit(value);
    }
}

class Negate extends Expr {
    private final Expr unary;

    public Negate(Expr unary) { this.unary = unary; }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) {
        return visitor.visitNegate(unary);
    }
}

class Add extends Expr {
    private final Expr left;
    private final Expr right;

    public Add(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) {
        return visitor.visitAdd(left, right);
    }
}

class Mul extends Expr {
    private final Expr left;
    private final Expr right;

    public Mul(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) {
        return visitor.visitMul(left, right);
    }
}
```

(But, just wanted to note that if you actually _are_ working in java, you can
actually do something with sealed classes, which allows exhaustiveness checking
for its native switch/case statements.)

Alternatively you could make all of the subclasses anonymous and expose them as
factory methods, if your language allows it:

```java
abstract class Expr {
    public abstract <R> R accept(ExprVisitor<R> visitor);

    public static Expr lit(int value) {
        return new Expr() {
            @Override
            public <R> R accept(ExprVisitor<R> visitor) {
                return visitor.visitLit(value);
            }
        };
    }

    public static Expr negate(Expr unary) {
        return new Expr() {
            @Override
            public <R> R accept(ExprVisitor<R> visitor) {
                return visitor.visitNegate(unary);
            }
        };
    }

    public static Expr add(Expr left, Expr right) {
        return new Expr() {
            @Override
            public <R> R accept(ExprVisitor<R> visitor) {
                return visitor.visitAdd(left, right);
            }
        };
    }

    // ... etc
}
```

You'd then call using:

```java
public class Main {
    public static void main(String[] args) {
        Expr expr = new Mul(new Negate(new Add(new Lit(4), new Lit(5))), new Lit(8));
        // or
        // Expr expr = Eval.mul(Eval.negate(Eval.add(Eval.lit(4), Eval.lit(5))), Eval.lit(8));

        ExprVisitor<Integer> eval = new ExprVisitor<>() {
            @Override public Integer visitLit(int value) {
                return value;
            }
            @Override public Integer visitNegate(Expr unary) {
                return -unary.accept(this);
            }
            @Override public Integer visitAdd(Expr left, Expr right) {
                return left.accept(this) + right.accept(this);
            }
            @Override public Integer visitMul(Expr left, Expr right) {
                return left.accept(this) * right.accept(this);
            }
        };

        System.out.println("Result: " + expr.accept(eval));
    }
}
```

Passing around function references like this is actually pretty close to the
scott encoding of our data type --- and for non-recursive types, it's
essentially the church encoding.

### Recursive Types

Speaking of recursive types...what if your language doesn't allow recursive
data types? What if it doesn't allow recursion at all, or what if recursively
generated values are just annoying to deal with?  Just imagine writing that
`Expr` type in a language with explicit memory management, for example. Or,
what if you wanted a way to express your recursive types in a more elegant and
runtime-safe manner?

One thing you can instead do is have your visitor be in its "catamorphism", or
church encoding.  Instead of having the "visitor" take the recursive
sub-values, instead have it return the result of recursively applying itself.

Let's do this in _dhall_, one of the most famous non-recursive languages.
Dhall _does_ have native sum types, so we won't worry about manually writing a
visitor pattern. But it does _not_ have recursive data types.

Let's define a type like:

```haskell
data Expr = Lit Natural
          | Add Expr Expr
          | Mul Expr Expr
```

But we can't define data types in dhall that refer to themselves.  So instead,
we can define them in their "church encoding": give what you would do with an
`Expr` to consume it, where the consumption function is given as if it were
recursively applied.

```dhall
let ExprF : Type -> Type
      = \(r : Type) ->
        { lit : Natural -> r
        , add    : r -> r -> r
        , mul    : r -> r -> r
        }

let Expr : Type
      = forall (r : Type) -> ExprF r -> r
```

Note that `ExprF r` is essentially `ExprVisitor<R>`, except instead of `add`
being `Expr -> Expr -> r`, it's `r -> r -> r`: the input values aren't the
expression, but rather the results of recursively folding on the expression. In
fact, our original non-recursive `ExprVisitor<R>` (to be more precise, the `R
accept(ExprVisitor<R>)`) is often called the "scott encoding", as opposed to
the recursive "church encoding" fold.

For value creation, you _take_ the visitor and recursively apply:

```dhall
let lit : Natural -> Expr
      = \(x : Natural) ->
        \(r : Type) ->
        \(handlers : ExprF r) ->
            handlers.lit x

let add : Expr -> Expr -> Expr
      = \(left : Expr) ->
        \(right : Expr) ->
        \(r : Type) ->
        \(handlers : ExprF r) ->
            handlers.add (left r handlers) (right r handlers)

let mul : Expr -> Expr -> Expr
      = \(left : Expr) ->
        \(right : Expr) ->
        \(r : Type) ->
        \(handlers : ExprF r) ->
            handlers.mul (left r handlers) (right r handlers)
```

And finally, _using_ the data type involves providing the `handler` to fold up
from the bottom to top.  Note that `add : \(left : Natural) -> \(right :
Natural) -> left + right` already assumes that the handler has been applied to
the sub-expressions, so you get `Natural`s on both sides instead of `Expr`.

```dhall
let eval : Expr -> Natural
      = \(e : Expr) ->
          e Natural
            { lit = \(x : Natural) -> x
            , add = \(left : Natural) -> \(right : Natural) -> left + right
            , mul = \(left : Natural) -> \(right : Natural) -> left * right
            }

let testVal : Expr
      = mul (add (lit 4) (lit 5)) (lit 8)

in  assert : eval testVal === 72
```

This pattern is useful even in languages with good datatype recursion, like
Haskell --- it's actually the [recursion-schemes][] refactoring of a recursive
data type, and it can be useful to have it live alongside your normal recursive
types. I've written [this blog post][prequel memes] talking about how useful
this pattern is to have alongside your normal recursive types.

[recursion-schemes]: https://hackage.haskell.org/package/recursion-schemes
[prequel memes]: https://blog.jle.im/entry/tries-with-recursion-schemes.html

This pattern is pretty portable to other languages too, as long as you can
scrounge together something like Rank-N types:

```java
interface ExprFold<R> {
    R foldLit(int value);
    R foldNegate(R unary);
    R foldAdd(R left, R right);
    R foldMul(R left, R right);
}

interface Expr {
    public abstract <R> R accept(ExprFold<R> fold);

    public static Expr lit(int value) {
        return new Expr() {
            @Override
            public <R> R accept(ExprFold<R> fold) {
                return fold.foldLit(value);
            }
        };
    }

    public static Expr negate(Expr unary) {
        return new Expr() {
            @Override
            public <R> R accept(ExprFold<R> fold) {
                return fold.foldNegate(unary.accept(fold));
            }
        };
    }

    // etc.
}
```

By "Rank-N types" here, I mean that your objects can generate polymorphic
functions: given an `Expr`, you could _generate_ an `<R> R accept(ExprFold <R>
fold)` for any `R`, and not something pre-determined or pre-chosen by your
choice of representation of `Expr`.

Generalized Algebraic Data Types
--------------------------------

You've implemented ADTs in your language of choice, or you are currently in a
language with native ADTs. Life is good, right? Until that sneaky voice starts
whispering in your hear: "we need more type safety." You resist that urge,
maybe even get a lot done without it, but eventually you are compelled to give
in and embrace the warm yet harsh embrace of ultimate type safety.  Now what?

### Singletons and Witnesses

In Haskell, singletons are essentially enums used to associate a value with a
reifiable type. "Reifiable" here means that you can take the runtime value of a
singleton and use it to bring evidence to the type-level. I ran into a
real-world usage of this while writing <https://coronavirus.jle.im/>, a
web-based data visualizer of COVID-19 data ([source here][corona-charts]) in
purescript. I needed a singleton to represent _scales_ for scatter plots and
linking them to the data that can be plotted. And, not only did it need to be
type-safe in purescript (which has ADTs but not GADTs), it had to be type-safe
in the javascript ffi as well.

[corona-charts]: https://github.com/mstksg/corona-charts/tree/master

Here's how it might look in Haskell:

```haskell
-- | Numeric types
data NType :: Type -> Type where
    NInt :: NType Int
    NDouble :: NType Double
    NPercent :: NType Percent

-- | Define a scale
data Scale :: Type -> Type where
    ScaleDate :: Scale Date
    ScaleLinear :: Bool -> NType a -> Scale a   -- ^ whether to include zero in the axis or not
    ScaleLog :: NType a -> Scale a
```

You'd then run it like this:

```haskell
plot :: Scale a -> Scale b -> [(a, b)] -> Canvas
```

So, we have the _type_ of the input tuples being determined by the _values_ you
pass to `plot`:

```haskell
ghci> :t plot ScaleDate (ScaleLinear True (LNumeric NInt))
[(Date, Int)] -> Canvas
```

But let's say we only had ADTs. And then we're passing them down to a
javascript FFI which only has structs and functions. We could drop the
type-safety and instead error on runtime, but...no. Type unsafety is not
acceptable.

The fundamental ability we want to gain is that if we pattern match on
`ScaleDate`, then we _know_ `a` has to be `Date`. If we match on `NInt`, we
know that `a` _has_ to be `Int`.

For the sake of this example, we're going to be implementing a simpler function
in purescript and in javascript: a function that takes a scale type and a list
of points prints the bounds. In Haskell, this looks like:

```haskell
data AxisBounds a = AB
    { minValue :: a
    , minLabel :: String
    , maxValue :: a
    , maxLabel :: String
    }

displayAxis :: Scale a -> [a] -> AxisBounds a
displayAxis = \case
    ScaleDate -> \xs ->
      let xMin = minimum xs
          xMax = maximum xs
       in AB xMin (showDate xMin) xMax (showDate xMax)
    ScaleLinear hasZero nt -> \xs ->
      displayNumericAxis (if hasZero then 0:xs else xs)
    ScaleLog nt ->
      displayNumericAxis nt xs

displayNumericAxis :: NType a -> [a] -> AxisBounds a
displayNumericAxis = \case
    NInt -> \xs ->
      let xMin = minimum xs
          xMax = maximum xs
       in AB xMin (printf "%d" xMin) xMax (printf "%d" xMax)
    NDouble -> \xs ->
      let xMin = minimum xs
          xMax = maximum xs
       in AB xMin (printf "%.4f" xMin) xMax (printf "%.4f" xMax)
    NPercent -> \xs ->
      let xMin = minimum xs
          xMax = maximum xs
       in AB xMin (printf "%.1f%%" (xMin*100)) xMax (printf "%.1f%%" (xMax*100))
```

(Pretend the `Percent` type is just a newtype-wrapped `Float` or something)

There are at least two main approaches to do this. We'll be discussing runtime
equality witnesses and Higher-Kinded Eliminators.

#### Runtime Witnesses and Coyoneda Embedding

The [Yoneda Lemma][] is one of the most powerful tools that Category Theory has
yielded as a branch of math, but its sibling [coyoneda][] is one of the most
useful Haskell abstractions.

[Yoneda Lemma]: https://ncatlab.org/nlab/show/Yoneda+embedding
[coyoneda]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Coyoneda.html

This doesn't give you GADTs, but it's a very lightweight way to "downgrade"
your GADTs into normal GADTs which is appropriate if you don't need the full
power.

The trick is this: if you have `MyGADT a`, and you know you are going to be
using it to _produce_ `a`s, you can do a covariant coyoneda transform.

For example, if you have this type representing potential data sources:

```haskell
data Source :: Type -> Type where
    ByteSource :: Handle -> Source Word
    StringSource :: FilePath -> Source String

readByte :: Handle -> IO Word
readString :: FilePath -> IO String

readSource :: Source a -> IO a
readSource = \case
    ByteSource h -> readByte h
    StringSource fp -> readString fp
```

You could instead turn `Source` into a non-GADT by making it a normal
parameterized ADT and adding a `X -> a` field, which is a type of CPS
transformation:

```haskell
data Source a =
    ByteSource Handle (Word -> a)
  | StringSource FilePath (String -> a)

byteSource :: Handle -> Source Word
byteSource h = ByteSource h id

stringSource :: FilePath -> Source String
stringSource fp = StringSource fp id

readSource :: Source a -> IO a
readSource = \case
    ByteSource h out -> out <$> readByte h
    StringSource fp out -> out <$> readString fp
```

A nice benefit of this method is that `Source` can now have a `Functor`
instance, which the original GADT could not.

And, if `MyGADT a` is going to be _consuming_ `a`s, you can do the [contravariant
coyoneda][] transform:

[contravariant coyoneda]: https://hackage.haskell.org/package/kan-extensions/docs/Data-Functor-Contravariant-Coyoneda.html

```haskell
data Sink a =
    ByteSink Handle (a -> Word)
  | StringSink FilePath (a -> String)
```

This gives it a free [Contravariant][] instance too!

[Contravariant]: https://hackage.haskell.org/package/base/docs/Data-Functor-Contravariant.html

And, if you are going to be both consuming and producing `a`s, you can do the
*invariant coyoneda* transform

```haskell
data Interface a =
    ByteInterface Handle (Word -> a) (a -> Word)
  | StringInterface FilePath (String -> a) (Word -> a)
```

However, in practice, _true equality_ involves being able to lift under
injective type constructors, and carrying _every single_ continuation is
unwieldy.  We can package them up together with a **runtime equality witness**.

This is something we can put "inside" `NInt` such that, when we pattern match
on a `NType a`, the type system can be assured that `a` is an `Int`.

You need some sort of data of type `IsEq a b` with functions:

*   `refl :: IsEq a a`
*   `to :: IsEq a b -> a -> b`
*   `sym :: IsEq a b -> IsEq b a`
*   `trans :: IsEq a b -> IsEq b c -> IsEq a c`
*   `inj :: IsEq (f a) (f b) -> IsEq a b`

If you have `to` and `sym` you also get `from :: IsEq a b -> b -> a`.

From all of this, we can recover our original `IsEq a Word -> Word -> a` and
`IsEq a Word -> a -> Word` functions, saving us from having to put two
functions.

Your language of choice might already have this `IsEq`. But one of the more
interesting ways to me is Leibniz equality (discussed a lot in [this Ryan Scott
post][ryanglscott]), which works in languages with higher-kinded polymorphism.
Leibniz quality in languages with higher-kinded polymorphism means that `a` and
`b` are equal if `forall p. p a -> p b`: any property of `a` is also true of
`b`.

[ryanglscott]: https://ryanglscott.github.io/2021/08/22/leibniz-equality-in-haskell-part-1/

In Haskell, we write this like:

```haskell
newtype Leibniz a b = Leibniz (forall p. p a -> p b)

refl :: Leibniz a a
refl = Leibniz id
```

The only possible way to construct a 'Leibniz' is with both type parameters
being the same: You can only ever _create_ a value of type `Leibniz a a`, never
a value of `Leibniz a b` where `b` is not `a`.

You can prove that this is actually equality by writing functions `Leibniz a
b -> Leibniz b a` and `Leibniz a b -> Leibniz b c -> Leibniz a c` ([this Ryan
Scott post][ryanglscott] goes over it well), but in practice we realize this
equality by safely coercing `a` and `b` back and forth:

```haskell
newtype Identity a = Identity { runIdentity :: a }

to :: Leibniz a b -> a -> b
to (Leibniz f) = runIdentity . f . Identity

newtype Op a b = Op { getOp :: b -> a }

from :: Leibniz a b -> b -> a
from (Leibniz f) = getOp (f (Op id))
```

So, if your language supports higher-kinded Rank-2 types, you have a solution!

There are other solutions in other languages, but they will usually all be
language-dependent.

Let's write everything in purescript. The key difference is we use `map (to
isNumber) :: Array a -> Array Number`, etc., to get our `Array` as something we
know it has the type of.

```purescript
import Text.Printf

newtype Leibniz a b = Leibniz (forall p. p a -> p b)

to :: Leibniz a b -> a -> b
from :: Leibniz a b -> b -> a

data NType a =
    NInt (Leibniz a Int)
  | NNumber (Leibniz a Number)
  | NPercent (Leibniz a Percent)

type AxisBounds a =
    { minValue :: a
    , minLabel :: String
    , maxValue :: a
    , maxLabel :: String
    }

displayNumericAxis :: NType a -> Array a -> AxisBounds a
displayNumericAxis = \case
    NInt isInt -> \xs ->
      let xMin = minimum $ map (to isInt) xs
          xMax = maximum $ map (to isInt) xs
          showInt = show
       in { minValue: xMin
          , minLabel: showInt xMin
          , maxValue: xMax
          , maxLabel: showInt xMax
          }
    NNumber isNumber -> \xs ->
      let xMin = minimum $ map (to isNumber) xs
          xMax = maximum $ map (to isNumber) xs
          showFloat = printf (Proxy :: Proxy "%.4f")   -- it works a little differently
       in { minValue: xMin
          , minLabel: showFloat xMin
          , maxValue: xMax
          , maxLabel: showFloat xMax
          }
    NPercent isPercent -> \xs ->
      let xMin = minimum $ map (to isPercent) xs
          xMax = maximum $ map (to isPercent) xs
          showPercent = printf (Proxy :: Proxy "%.1f%%") <<< (_ * 100.0)
       in { minValue: xMin
          , minLabel: showPercent xMin
          , maxValue: xMax
          , maxLabel: showPercent xMax
          }
```

To work with our `[a]` as if it were `[Int]`, we have to map the coercion
function over it that our `Leibniz a Int` gave us. Admittedly, this naive way
adds a runtime cost of copying the array. But we could be more creative with
finding the minimum and maximum in this way in constant space and no extra
allocations.

And, if we wanted to outsource this to the javascript FFI, remember that
javascript doesn't quite have sum types, so we can create a quick visitor:

```purescript
type NVisitor a r =
    { nvInt :: Leibniz a Int -> r
    , nvNumber :: Leibniz a Number -> r
    , nvPercent :: Leibniz a Percent -> r
    }

type NAccept a = forall r. NVisitor a r -> r

toAccept :: NType a -> NAccept a
toAccept = case _ of
    NInt isInt -> \nv -> nv.nvInt isInt
    NNumber isNumber -> \nv -> nv.nvNumber isNumber
    NPercent isPercent -> \nv -> nv.nvPercent isPercent

foreign import _formatNumeric :: forall a. Fn2 (NAccept a) a String

formatNumeric :: NType a -> a -> String
formatNumeric nt = runFn2 _formatNumeric (toAccept nt)
```

The FFI binding looks like: (taken from [my actual source code][fmtNumber])

[fmtNumber]: https://github.com/mstksg/corona-charts/blob/master/src/D3/Scatter/Type.js

```javascript
import * as d3 from "d3-format";

export const _formatNumeric = (naccept, xs) =>
  naccept(
    { nvInt: (isInt) => d3.format("~s")
    , nvNumber: (isNumber) => d3.format(".3~s")
    , nvPercent: (isPercent) => d3.format("+.3~p")
    }
  );
```

Admittedly in the javascript we are throwing away the "GADT type safety"
because we throw away the equality. But we take what we can --- we at least
retain the visitor pattern for sum-type type safety and exhaustiveness
checking. I haven't done this in typescript yet so there might be a way to
formalize Leibniz equality to do this in typescript and keep the whole chain
type-safe from top to bottom.

#### Higher-Kinded Eliminators

This is essentially the higher-kinded version of the visitor pattern, except in
dependent type theory these visitors are more often called "eliminators" or
destructors, which is definitely a cooler name.

In the normal visitor you'd have:

```haskell
data User = TheAdmin | Member Int

data UserHandler r = UH
    { uhTheAdmin :: r
    , uhMember :: Int -> r
    }
```

But note that if you have the right set of continuations, you have something
that is essentially equal to `User` without having to actually use `User`:

```haskell
type User' = forall r. UserHandler r -> r

fromUser :: User -> User'
fromUser = \case
    TheAdmin -> \UH{..} -> uhTheAdmin
    Member userId -> \UH{..} -> uhMember userId

toUser :: User' -> Foo
toUser f = f $ UH { fhTheAdmin = TheAdmin, fhMember = Member }
```

This means that `User` is actually equivalent to `forall r. UserHandler r ->
r`: they're the same type, so if your language doesn't have sum types, you
could encode it as `forall r. UserHandler r -> r` instead. Visitors, baby.

But, then, what actually does the `r` type variable represent here,
semantically? Well, in a `UserHandler r`, `r` is the "target" that we interpret
into.  But there's a deeper relationship between `r` and `User`: A `UserHandler
r` essentially "embeds" a `User` into an `r`. And, a `UserHandler r -> r` is
the application of that embedding to an actual `User`.

If we pick `r ~ ()`, then `UserHandler ()` embeds `User` into `()`. If we pick
`r ~ String`, then `UserHandler ()` embeds `User` into `String` (like,
"showing" it).  And if we pick `r ~ User`, a `UserHandler User` embeds a `User`
into...itself?

So here, `r` is essentially the projection that we view the user through.  And
by making sure we are `forall r. UserHandler r -> r` for _all_ `r`, we ensure
that we do not lose any information: the embedding is completely 1-to-1. It
lets you "create" the `User` faithfully in a "polymorphic" way.

In fact, to hammer this home, some people like to use the name of the type as
the type variable: `UserHandler user`:

```haskell
-- | The same thing as before but with things renamed to prove a point
data MakeUser user = MakeUser
    { uhTheAdmin :: user
    , uhMember :: Int -> user
    }

type User' = forall user. MakeUser user -> user
```

The `forall user.` lets us faithfully "create" a `User` within the system we
have, without actually having a `User` data type. Essentially we can imagine
the `r` in the `forall r` as "standing in" for `User`, even if that type
doesn't actually exist.

Now, here's the breakthrough: If we can use `forall (r :: Type)` to substitute
for `User :: Type`, how about we use a `forall (p :: Type -> Type)` to
substitute for a `Scale :: Type -> Type`?

```haskell
data Scale :: Type -> Type where
    ScaleDate :: Scale Date
    ScaleLinear :: Bool -> LType a -> Scale a
    ScaleLog :: NType a -> Scale a

data ScaleHandler p a = SH
    { shDate :: p Date
    , shLinear :: Bool -> NType a -> p a
    , shLog :: NType a -> p a
    }

type Scale' a = forall p. ScaleHandler p a -> p a

fromScale :: Scale a -> Scale' a
fromScale = \case
    ScaleDate -> \SH{..} -> shDate
    ScaleLinear hasZero lt -> \SH{..} -> shLinear hasZero lt
    ScaleLog nt -> \SH{..} -> shLog nt

toScale :: Scale' a -> Scale a
toScale f = f $ SH { shDate = ScaleDate, shLinear = ScaleLinear, shLog = ScaleLog }
```

So in our new system, `forall p. ScaleHandler p a -> p a` is identical to
`Scale`: we can use `p a` to substitute in `Scale` in our language even if our
language itself cannot support GADTs.

So let's write `formatNType` in purescript. We no longer have an actual `Scale`
sum type, but its higher-kinded church encoding:

```purescript
type NType a = forall p.
    { int :: p Int
    , number :: p Number
    , percent :: p Percent
    } -> p a

type Scale a = forall p.
    { date :: p Date
    , linear :: Bool -> NType a -> p a
    , log :: NType a -> p a
    } -> p a

ntInt :: NType Int
ntInt nth = nth.int

ntNumber :: NType Number
ntNumber nth = nth.number

ntPercent :: NType Percent
ntPercent nth = nth.percent

formatNType :: NType a -> a -> String
formatNType nt = f
  where
    Op f = nt
      { int: Op show
      , number: Op $ printf (Proxy "%.4f")
      , percent: Op $ printf (Proxy "%.1f%%") <<< (_ * 100.0)
      }
```

Here we are using

```purescript
newtype Op b a = Op (a -> b)
```

as our "target": turning an `NType a` into an `Op String a`. And an `Op String
a` is an `a -> String`, which is what we wanted! The `int` field is `Op String
Int`, the `number` field is `Op String Number`, etc.

In many languages, using this technique effectively requires having a newtype
wrapper on-hand, so it might be unwieldy in non-trivial situations. For
example, if we wanted to write our previous axis function which is `NType a ->
[a] -> String`, we'd have to have a newtype wrapper for `[a] -> String` that
has `a` as its argument:

```purescript
newtype OpList b a = Op ([a] -> b)
```

or you could re-use `Compose`:

```purescript
newtype Compose f g a = Compose (f (g a))
```

and your `p` projection type would be `Compose Op []`.  So, you don't
necessarily have to write a bespoke newtype wrapper, but you do have to
devote some brain cycles to think it through (unless you're in a language
that doesn't need newtype wrappers to have this work, like we'll discuss
later).

By the way, this method generalizes well to multiple arguments: if you have a
type like `MyGADT a b c`, you just need to project into a `forall (p :: k1 ->
k2 -> k3 -> Type)`.

I believe I have read somewhere that the two methods discussed here (runtime
equality witness vs. higher-kinded eliminator) are not actually fully identical
in their power, and there are GADTs where one would work and not the other ...
but I can't remember where I read this and I'm also not big-brained enough to
figure out what those situations are. But if you, reader, have any idea, please
let me know!

### Existential Types

Let's take a quick break to talk about something that's not _technically_
related to GADTs but is often used alongside them.

What if we wanted to store a value with its `NType` and hide the type variable?
In Haskell we'd write this like:

```haskell
data NType :: Type -> Type where
    NInt :: NType Int
    NDouble :: NType Double
    NPercent :: NType Percent

data SomeNType = forall a. SomeNType (NType a) a

formatNType :: NType a -> a -> String
formatNType nt x = ...

formatSomeNType :: SomeNType -> String
formatSomeNType (SomeNType nt x) = formatNType nt x

myFavoriteNumbers :: [SomeNType]
myFavoriteNumbers = [SomeNType NInt 3, SomeNType NDouble pi]
```

But what if our language doesn't have existentials? Remember, this is basically
a value `SomeNType` that _isn't_ a Generic, but _contains_ both a `NType a` and
an `a` of the _same_ variable.

One strategy we have available is to CPS-transform our existentials into their
CPS form (continuation-passing style form).  Basically, we write exactly what we
want to do with our contents _if we pattern matched_ on them. It's essentially
a Rank-N visitor pattern with only a single constructor:

```purescript
type SomeNType = forall r. (forall a. NType a -> a -> r) -> r

someNType :: NType a -> a -> SomeNType
someNType nt x = \f -> f nt x

formatSomeNumeric :: SomeNType -> String
formatSomeNumeric snt = snt
    \nt x -> formatNumeric nt x
```

You can imagine, syntactically, that `snt` acts as its "own" pattern match,
except instead of matching on `SomeNType nt x -> ..`, you "match" on `\nt x ->
..`

This general pattern works for languages with traditional generics like Java
too:

```java
interface SomeNTypeVisitor<R> {
    <A> R visit(NType<A> nt, A val);
}

interface SomeNType {
    public abstract <R> R accept(SomeNTypeVisitor<R> visitor);

    // One option: the factory method
    public static <A> SomeNType someNType(NType<A> nt, A val) {
        return new SomeNType() {
            @Override
            public <R> R accept(SomeNTypeVisitor<R> visitor) {
                return visitor.visit(nt, val);
            }
        };
    }
}

// Second option: the subtype hiding a type variable, which you have to always
// make sure to upcast into `SomeNType` after creating
class SomeNTypeImpl<A> extends SomeNType {
    private NType<A> nt;
    private A val;

    public SomeNTypeImpl(NType<A> nt, A val) {
        this.nt = nt;
        this.val = val;
    }

    @Override
    public <R> R accept(SomeNTypeVisitor<R> visitor) {
        return visitor.visit(nt, val);
    }
}
```

Does...anyone write java like this?  I tried committing this once while at
Google and I got automatically flagged to be put on a PIP.

### Recursive GADTs

The climax of this discussion: what if your language does not support GADTs
_or_ recursive data types?

We're going to be using *dhall* as an example again, but note that the lessons
applied here are potentially useful even when you _do_ have recursive types:
we're going to be talking about a higher-kinded church encoding, which can be a
useful form of your data types that live alongside your normal recursive ones.

Let's imagine `Expr` as a GADT, where `Expr a` represents an `Expr` that
evaluates to an `a`:

```haskell
data Expr :: Type -> Type where
    NatLit :: Natural -> Expr Natural
    BoolLit :: Bool -> Expr Bool
    Add :: Expr Natural -> Expr Natural -> Expr Natural
    LTE :: Expr Natural -> Expr Natural -> Expr Bool
    Ternary :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval = \case
    NatLit n -> n
    BoolLit b -> b
    Add x y -> eval x + eval y
    LTE a b -> eval a <= eval b
    Ternary b x y -> if eval b then eval x else eval y
```

Adding this type variable ensures that our `Expr` is type-safe: it's impossible
to `Add` an `Expr Bool`, and the two branches of a `Ternary` must have the same
result type, etc. And, we can write `eval :: Expr a -> a` and know exactly what
type will be returned.

Now, let's combine the two concepts: First, the church encoding, where our
handlers take the "final result" of our fold `r` instead of the recursive value
`Expr`. Second, the higher-kinded eliminator pattern where we embed `Expr ::
Type -> Type` into `forall (p :: Type -> Type)`.

And finally, we get:[^dhalllazy]

[^dhalllazy]: Be aware that this implementation is not necessarily
appropriately lazy or short-circuiting in `Ternary`: it might evaluate both
sides returning the chosen branch.

```dhall
let ExprF =
      \(p : Type -> Type) ->
        { natLit : Natural -> p Natural
        , boolLit : Bool -> p Bool
        , add : p Natural -> p Natural -> p Natural
        , ternary : forall (a : Type) -> p Bool -> p a -> p a -> p a
        }

let Expr
    : Type -> Type
    = \(a : Type) -> forall (p : Type -> Type) -> ExprF p -> p a

let eval
    : forall (a : Type) -> Expr a -> a
    = \(a : Type) ->
      \(e : Expr a) ->
        e
          (\(q : Type) -> q)
          { natLit = \(x : Natural) -> x
          , boolLit = \(x : Bool) -> x
          , add = \(x : Natural) -> \(y : Natural) -> x + y
          , ternary =
              \(a : Type) ->
              \(b : Bool) ->
              \(x : a) ->
              \(y : a) ->
                if b then x else y
          }
```

Again, now instead of `add` taking `Expr`, it takes `p Natural`: the "`Natural`
result of the fold". `p` not only stands in for what we embed `Expr` into, it
stands in for the result of the recursive fold. That's why in `eval`, the first
arguments of `add` are the `Natural` results of the sub-evaluation.

These values can be created in the same way as before, merging the two
techniques, sending the handlers downstream:

```dhall
let natLit
    : Natural -> Expr Natural
    = \(n : Natural) ->
      \(p : Type -> Type) ->
      \(handlers : ExprF p) ->
        handlers.natLit n

let boolLit
    : Bool -> Expr Bool
    = \(n : Bool) ->
      \(p : Type -> Type) ->
      \(handlers : ExprF p) ->
        handlers.boolLit n

let add
    : Expr Natural -> Expr Natural -> Expr Natural
    = \(x : Expr Natural) ->
      \(y : Expr Natural) ->
      \(p : Type -> Type) ->
      \(handlers : ExprF p) ->
        handlers.add (x p handlers) (y p handlers)

let ternary
    : forall (a : Type) -> Expr Bool -> Expr a -> Expr a -> Expr a
    = \(a : Type) ->
      \(b : Expr Bool) ->
      \(x : Expr a) ->
      \(y : Expr a) ->
      \(p : Type -> Type) ->
      \(handlers : ExprF p) ->
        handlers.ternary (b p handlers) (x p handlers) (y p handlers)

let testVal
    : Expr Natural
    = add (natLit 5) (add (natLit 6) (natLit 7))

in  assert : eval testVal === 18
```

If all of this is difficult to parse, try reviewing both the recursive ADT
section and the higher-kinded eliminator section and making sure you understand
both well before tackling this, which combines them together!

Admittedly in Haskell (and purescript) this is a lot simpler because we don't
have to explicitly pass in type variables:

```haskell
data ExprF p = ExprF
    { natLit :: Natural -> p Natural
    , boolLit :: Bool -> p Bool
    , add :: p Natural -> p Natural -> p Natural
    , ternary :: forall a.  p Bool -> p a -> p a -> p a
    }

type Expr a = forall p. ExprF p a -> p a

eval :: Expr a -> a
eval e = runIdentity $
  e
    { natLit = Identity
    , boolLit = Identity
    , add = \(Identity x) -> \(Identity y) -> Identity (x + y)
    , ternary = \(Identity b) -> \(Identity x) -> \(Identity y) -> if b then x else y
    }

ternary :: Expr Bool -> Expr a -> Expr a -> Expr a
ternary b x y handlers = handlers.ternary (b handlers) (x handlers) (y handlers)
```

But one nice thing about the dhall version that's incidental to dhall is that
it doesn't require any extra newtype wrappers like the Haskell one does. That's
because type inference tends to choke on things like this, but dhall doesn't
really have any type inference: all of the types are passed explicitly. It's
one of the facts about dhall that make it nice for things like this.

Congratulations
---------------

In any case, if you've made it this far, congratulations! You are a master of
ADTs and GADTs. Admittedly every language is different, and some of these
solutions have to be tweaked for the language in question. And, if your program
gets very complicated, there is a good chance that things will become
ergonomically unfeasible.

But I hope, at least, that this inspires your imagination to try to bring your
haskell principles, techniques, standards, practices, and brainrot into the
language of your choice (or language you are forced to work with).

And, if you ever find interesting ways to bring these things into a language
not discussed here (or a new interesting technique or pattern), I would
absolutely love to hear about it!

Until next time, happy "Haskelling"!

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
