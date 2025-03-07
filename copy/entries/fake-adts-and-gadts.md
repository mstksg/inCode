---
title: Faking ADTs and GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
identifier: fake-adts-and-gadts
slug: faking-adts-gadts-in-languages-that-shouldnt-have-them
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
in java:

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

For example, here's C++ (without classes or the stdlib) to implement this with
for the famous `Either` using a tagged union:

```cpp
template <typename E, typename A>
struct Either {
    bool isLeft;
    union {
        E left;
        A right;
    };
};

template <typename E, typename A, typename R>
struct EitherVisitor {
    R (*visitLeft)(E);
    R (*visitRight)(A);
};

template <typename E, typename A, typename R>
R acceptEither(const Either<E, A>& either, EitherVisitor<E, A, R> visitor) {
    if (either.isLeft) {
        return visitor.visitLeft(either.left);
    } else {
        return visitor.visitRight(either.right);
    }
}
```

You can create the values using:

```cpp
template <typename E, typename A>
Either<E, A> mkLeft(E value) { return { true, .left = value }; }

template <typename E, typename A>
Either<E, A> mkRight(A value) { return { false, .right = value }; }
```

And we can show an `Either<std::string, int>`, printing the error case if it
exists:

```cpp
std::string showEitherInt(const Either<std::string, int>& either) {
    return acceptEither(either, {
        [](std::string left) { return "There is an error: " + left; },
        [](int right) { return "Something is here: " + std::to_string(right); }
    });
}
```

Note that in this way, the compiler enforces that we handle every branch. And,
if we ever add a new branch, everything that ever consumes `EitherVisitor` will
have to add a new handler.

However, if your language as subtyping built in (maybe with classes and
subclasses), you can implement it in terms of that, which is nice in python,
java, etc.

```java
interface ExprVisitor<R> {
    R visitLit(int value);
    R visitNegate(Expr expr);
    R visitAdd(Expr left, Expr right);
    R visitSub(Expr left, Expr right);
    R visitMul(Expr left, Expr right);
}

abstract class Expr {
    public abstract <R> R accept(ExprVisitor<R> visitor);
}
```

Now instead of manually implementing a tag, we take advantage of our language's
default dynamic dispatch to handle it for us.  Each constructor becomes a
class, but it's important to _only allow_ access using `accept` to properly
enforce the sum type pattern.

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
    private final Expr expr;

    public Negate(Expr expr) { this.expr = expr; }

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

class Sub extends Expr {
    private final Expr left;
    private final Expr right;

    public Sub(Expr left, Expr right) {
        this.left = left;
        this.right = right;
    }

    @Override
    public <R> R accept(ExprVisitor<R> visitor) {
        return visitor.visitSub(left, right);
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
            @Override public Integer visitLit(int value) { return value; }
            @Override public Integer visitNegate(Expr expr) { return -expr.accept(this); }
            @Override public Integer visitAdd(Expr left, Expr right) { return left.accept(this) + right.accept(this); }
            @Override public Integer visitSub(Expr left, Expr right) { return left.accept(this) - right.accept(this); }
            @Override public Integer visitMul(Expr left, Expr right) { return left.accept(this) * right.accept(this); }
        };

        System.out.println("Result: " + expr.accept(eval));
    }
}
```


<!-- Examples: -->

<!-- 1. x,y tuples -->
<!-- 2. one-way message protocol, or state machine? tree? untyped expr? -->
<!-- 3. lists -->
<!-- 4. vector -->
<!-- 5. expr -->
<!-- 6. message protocol -->

<!-- Let's put these here -->
