---
title: Faking ADTs and GADTs in Languages That Shouldn't Have Them
categories: Haskell
tags: functional programming, haskell, purescript, dhall, java
create-time: 2024/11/12 13:59:35
identifier: fake-adts-and-gadts
slug: faking-adts-gadts-in-languages-that-shouldnt-have-them
---

Haskell is the world's best programming language[^best], but let's face the harsh
reality that a lot of times in life you'll have to write in other programming
languages. But alas you have been fully [Haskell-brained][kmett] and lost all
ability to program unless it is type-directed, you don't even know how to start
writing a program without imagining its shape as a type first.

[^best]: I bet you thought there was going be some sort of caveat in this
footnote, didn't you?
[kmett]: https://x.com/kmett/status/1844812186608099463

Well, fear not. The foundational theory behind ADTs and GADTs are so
fundamental that they'll fit (somewhat) seamlessly into whatever language
you're forced to write. After all, if they can fit [profunctor optics in
Microsoft's Java code][profunctor], the sky's the limit!

[profunctor]: https://www.reddit.com/r/haskell/comments/9m2o5r/digging_reveals_profunctor_optics_in_mineacraft/

ADTs and the Visitor Pattern
----------------------------

Let's get normal ADT's out of the way. Most languages do have *structs*. Not
all languages have _immutable_ structs, but we'll take what we can get. With
immutable structs we get product types.

Examples:

1. x,y tuples
2. one-way message protocol, or state machine? tree? untyped expr?
3. lists
4. vector
5. expr
6. message protocol


<!-- Let's put these here -->

### Implementation

Typically if your language does not natively support sum types, you can achieve
the same design patterns and guarantees using the "visitor pattern", which is
essentially the "case match" as a data structure or argument list.

If you don't have subtyping and inheritance, usually this looks like defining
some sort of union type with a tag.

I'm going to use C++ _without classes_ or the stdlib (using only raw function
pointers and structs) to demonstrate how you'd do this with generic types:

```cpp
template <typename T>
struct Maybe {
    bool present;
    T value;
};

template <typename T, typename R>
struct MaybeVisitor {
    R (*visitJust)(T);
    R (*visitNothing)();
};

template <typename T, typename R>
R acceptMaybe(Maybe<T> maybe, MaybeVisitor<T, R> visitor) {
    if (maybe.present) {
        return visitor.visitJust(maybe.value);
    } else {
        return visitor.visitNothing();
    }
}
```

If you have multiple types of payloads (instead of just `T`) then you can use a
union to save space. This is why sum types are often called 'tagged unions':
they are a tag with a union!

You can create the values using:

```cpp
template <typename T>
Maybe<T> mkJust(T value) { return { true, value }; }

template <typename T>
Maybe<T> mkNothing() { return { false }; }
```

And, to implement `showMaybeInt`, we'd create a `MaybeVisitor<int,std::string>`:

```cpp
std::string showMaybeInt(Maybe<int> maybe) {
    return acceptMaybe(maybe, {
        [](int value) { return "Something is here: " + std::to_string(value); },
        []() { return "There's nothing here"; }
    });
}
```

Note that in this way, the compiler enforces that we handle every branch. And,
if we ever add a new branch, everything that ever consumes `MaybeVisitor` will
have to add a new handler.

TODO: use Command to illustrate the union

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

