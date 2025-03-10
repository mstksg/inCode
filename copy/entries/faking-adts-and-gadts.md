---
title: Faking ADTs and GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
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

Normal ADTs
-----------

Algebraic Data Types (ADT's) are products and sums (that's why they're
algebraic, after all)

### Product Types

Products are just immutable structs, which pretty much every language supports
--- as long as you're able to make sure they are never mutated.

Structs in `c`, for example, look like:

```c
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

[^java]: I didn't think I'd ever write this non-ironically on my blog

And there you go.  Nothing too surprising there!

Remember, not only are these ADT's (algebraic data types), they're also ADT's
(abstract data types): you are meant to work with them based on a pre-defined
abstract interface based on type algebra, instead of their internal
representations.

### Sum Types

Alright, moving on to sum types.  If your language doesn't support sum types,
usually the way to go is with the _visitor pattern_: the underlying
implementation is hidden, and the only way to process a sum type value is by
providing handlers for every branch --- a pattern match as a function,
essentially. Your sum values then basically determine which handler is called.

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
all branches return the same type. Instead, the best you can do is have an
"effectul" visitor pattern, which triggers an action on each branch instead of
returning a value.

In languages without context-binding functions, you might also need to add a
closure-simulating context into your visitor:

```c
struct IPAddressVisitor {
    void (*visitIPv4)(uint32_t, void*);
    void (*visitIPv6)(const uint8_t[16], void*);
    void *context;
};

void acceptIPAddress(const struct IPAddress* ip, struct IPAddressVisitor visitor) {
    if (ip->isIPv4) {
        visitor.visitIPv4(ip->ipv4, visitor.context);
    } else {
        visitor.visitIPv6(ip->ipv6, visitor.context);
    }
}
```

and you wouldn't be able to "return" values from them: only execute actions.

```c
void showIPv4(uint32_t v, void* context) {
    char* out = (char*) context;
    sprintf(out, "%u.%u.%u.%u",
            (v >> 24) & 0xFF, (v >> 16) & 0xFF,
            (v >> 8) & 0xFF, v & 0xFF);
}

void showIPv6(const uint8_t v[16], void* context) {
    char* out = (char*) context;
    sprintf(out, "%02X%02X:%02X%02X:%02X%02X:%02X%02X:"
                 "%02X%02X:%02X%02X:%02X%02X:%02X%02X",
            v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7],
            v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15]);
}

void showIPAddress(const struct IPAddress* ip, char* out) {
    struct IPAddressVisitor visitor = {
        .visitIPv4 = showIPv4,
        .visitIPv6 = showIPv6,
        .context = out
    };
    acceptIPAddress(ip, visitor);
}
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
possible within the language, but rather because the maintainers are too lazy
to add it --- so your options are a little bit more limited there. Basically,
`accept` is not allowed because of this. But we'll discuss a method to get
around this later.)

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
        return visitor.visitNegate(expr);
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

### Recursive Types

Okay well...what if your language doesn't allow recursive data types? Or, what
if recursively generated values are just annoying to deal with?  Just imagine
writing that `Expr` type in a language with explicit memory management, for
example.

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
      = forall (expr : Type) -> ExprF r -> r
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

### Limits of Generics

If you try applying this in java it's pretty much a straightforward
translation. However, if you're in C++, it gets a little wonky because it
doesn't allow virtual template methods, but you can get around it with some
clever use of `auto` to appease the compiler. The problematic function is
`accept`, which is template on `R`. One trick you can do is to move the
template to the top level, and have `Expr<R>` instead of `Expr`. The `R`
represents, now, the type that you eventually fold the `Expr` into.

```cpp
```

However, remember that we really want `R` to be polymorphic, because an `Expr`
_instance_ (or value) has to work for multiple values to be useful: you might
want to evaluate an `Expr` into an `int`, or print it into a `std::string`. So,
you have to use the same `Expr<R>` as both an `Expr<int>` or an
`Expr<std::string>`. And, in C++, the only way I could think of this was to use
`auto` as the type:

```cpp
```

This works only in C++14 and higher.  It works because both `Expr<R>`,
`Expr<int>`, `Expr<std::string>` have the same runtime representation, but
there's no actual `forall r. Expr r` type possible in C++.  I also don't think
you can take the same `Expr<R>` as an argument to a function in a nice way and
have that function use it with both an `ExprVisitor<int>` and an
`ExprVisitor<std::string>` (a rank-2 type)...so your mileage may vary.  Maybe
the C++ committee can have ChatGPT implement virtual template methods for them
and we won't have to worry about it.

<!-- Our visitor is now: -->

<!-- ```haskell -->
<!-- interface ExprFold<R> { -->
<!--     R foldLit(int value); -->
<!--     R foldNegate(R unary); -->
<!--     R foldAdd(R left, R right); -->
<!--     R foldMul(R left, R right); -->
<!-- } -->

<!-- abstract class Expr { -->
<!--     public abstract <R> R withFold(ExprFold<R> fold); -->
<!-- } -->
<!-- ``` -->

<!-- Note the difference: the arguments take `R expr` instead of `Expr expr`. -->

<!-- And hey, if you don't mind recursive types, you could still do the typical -->
<!-- non-recursive `accept` alongside the recursive `withFold`. -->

<!-- Your instances now look like this: -->

<!-- ```java -->
<!-- abstract class Expr { -->
<!--     public abstract <R> R withFold(ExprFold<R> fold); -->

<!--     public static Expr lit(int value) { -->
<!--         return new Expr() { -->
<!--             @Override -->
<!--             public <R> R withFold(ExprFold<R> fold) { -->
<!--                 return fold.foldLit(value); -->
<!--             } -->
<!--         }; -->
<!--     } -->

<!--     public static Expr negate(Expr unary) { -->
<!--         return new Expr() { -->
<!--             @Override -->
<!--             public <R> R withFold(ExprFold<R> fold) { -->
<!--                 return fold.foldNegate(unary.withFold(fold)); -->
<!--             } -->
<!--         }; -->
<!--     } -->

<!--     public static Expr add(Expr left, Expr right) { -->
<!--         return new Expr() { -->
<!--             @Override -->
<!--             public <R> R withFold(ExprFold<R> fold) { -->
<!--                 return fold.foldAdd(left.withFold(fold), right.withFold(fold)); -->
<!--             } -->
<!--         }; -->
<!--     } -->

<!--     // ... etc -->
<!-- } -->
<!-- ``` -->

<!-- Examples: -->

<!-- 1. x,y tuples -->
<!-- 2. one-way message protocol, or state machine? tree? untyped expr? -->
<!-- 3. lists -->
<!-- 4. vector -->
<!-- 5. expr -->
<!-- 6. message protocol -->

<!-- Let's put these here -->
