---
title: You Could Have Invented Matrices!
categories: Math, Rambling
tags: linear algebra
create-time: 2018/03/09 14:25:14
identifier: invent-matrices
slug: you-could-have-invented-matrices
---

You could have invented matrices!

Let's talk about vectors.  A **vector** (denoted as $\mathbf{v}$, a lower-case
bold italicized letter) is an element in a **vector space**, which means that
it can be "scaled", like $c \mathbf{v}$ (the $c$ is called a "scalar" ---
creative name, right?) and added, like $\mathbf{v} + \mathbf{u}$.

In order for vector spaces and their operations to be valid, they just have to
obey some common-sense rules (like associativity, commutativity,
distributivity, etc.) that allow us to make meaningful conclusions.[^laws]

[^laws]: In short, vector spaces form an Abelian group (which is another way of
just saying that addition is commutative, associative, has an identity, and an
inverse), and scalars have to play nice with addition ($c(\mathbf{v} +
\mathbf{u}) = c \mathbf{v} + c \mathbf{u}$, and $(c + d)\mathbf{v} = c
\mathbf{v} + d \mathbf{v}$).  Also, scalars themselves form a field.

Dimensionality
--------------

One neat thing about vector spaces is that *some* of them (if you're lucky)
have a notion of **dimensionality**.  We say that a vector space is
N-dimensional if there exists N "basis" vectors $\mathbf{e}_1, \mathbf{e}_2
\ldots \mathbf{e}_N$ where *any* vector can be described as scaled sums of all
of them, and that N is the lowest number of basis vectors needed.  For example,
if a vector space is 3-dimensional, then it means that *any* vector
$\mathbf{v}$ in that space can be broken down as:

$$
\mathbf{v} = a \mathbf{e}_1 + b \mathbf{e}_2 + c \mathbf{e}_3
$$

Where $a$, $b$, and $c$ are scalars.

Dimensionality is really a statement about being able to decompose any vector
in that vector space into a useful set of bases.  For a 3-dimensional vector
space, you need at least 3 vectors to make a bases that can reproduce *any*
vector in your space.

In physics, we often treat reality as taking place in a three-dimensional
vector space.  The basis vectors are often called $\hat{\mathbf{i}}$,
$\hat{\mathbf{j}}$, and $\hat{\mathbf{k}}$, and so we say that we can describe
our 3D physics vectors as $\mathbf{v} = v_x \hat{\mathbf{i}} + v_y
\hat{\mathbf{j}} + v_x \hat{\mathbf{k}}$

### Encoding

One neat thing that physicists take advantage of all the time is that if we
*agree* on a set of basis vectors and a specific ordering, we can actually
*encode* any vector $\mathbf{v}$ in terms of those basis vectors.

So in physics, we can say "Let's encode vectors in terms of $\hat{\mathbf{i}}$,
$\hat{\mathbf{j}}$, and $\hat{\mathbf{k}}$, in that order."  Then, we can
*write* $\mathbf{v}$ as $\langle v_x, v_y, v_z \rangle$, and understand that we
really mean$\mathbf{v} = v_x \hat{\mathbf{i}} + v_y \hat{\mathbf{j}} + v_x
\hat{\mathbf{k}}$.

Note that $\langle v_x, v_y, v_z \rangle$ is **not** the same thing as the
**vector** $\mathbf{v}$.  It is *an encoding* of that vector, that only makes
sense once we choose to *agree* on a specific set of basis.

For an N-dimensional vector space, it means that, with a minimum of N items, we
can represent any vector in that space.  And, if we agree on those N items, we
can devise an encoding, such that:

$$
\langle v_1, v_2 \dots v_N \rangle
$$

will *represent* the vector:

$$
v_1 \mathbf{e}_1 + v_2 \mathbf{e}_2 + \ldots + v_N \mathbf{e}_N
$$

Note that what this encoding represents is *completely dependent* on what
$\mathbf{e}_1, \mathbf{e}_2 \ldots \mathbf{e}_N$ we pick, and in what order.
The basis vectors we pick are arbitrary, and determine what our encoding looks
like.

To highlight this, note that the same vector $\mathbf{v}$ has many different
potential encodings --- all you have to do is pick a different set of basis
vectors, or even just re-arrange the ones you already have.  However, all of
those encodings correspond go the same vector $\mathbf{v}$.

One interesting consequence of this is that any N-dimensional vector space
whose scalars are in $\mathbb{R}$ is actually isomorphic to $\mathbb{R}^N$ ---
the vector space of N-tuples of real numbers.  This means that we can basically
treat any N-dimensional vector space with $\mathbb{R}$ scalars as if it was
$\mathbb{R}^N$, once we decide on the basis vectors.  Because of this, we often
call *all* N-dimensional vector spaces (whose scalars are in $\mathbb{R}$) as
$\mathbb{R}^N$.  You will often hear physicists saying that the
three-dimensional vector spaces they use are $\mathbb{R}^3$.  However, what
they really mean is that their vector spaces is *isomorphic* to $\mathbb{R}^3$.

Linear Transformations
----------------------

Now, one of the most interesting things in mathematics is the idea of the
**linear transformation**.  Linear transformations are useful to study because:

1.  They are ubiquitious.  They come up everywhere in engineering,
    physics, mathematics, data science, economics, and pretty much any
    mathematical theory.  And there are even more situations which can be
    *approximated* by linear transformations.
2.  They are mathematically very nice to work with and study, in practice.

A linear transformation, $f(\mathbf{x})$, is a function that "respects"
addition and scaling:

$$
\begin{aligned}
f(c\mathbf{x}) & = c f(\mathbf{x}) \\
f(\mathbf{x} + \mathbf{y}) & = f(\mathbf{x}) + f(\mathbf{y})
\end{aligned}
$$

This means that if you scale the input, the output is scaled by the same
amount.  And also, if you transform the sum of two things, it's the same as the
sum of the transformed things (it "distributes").

Note that I snuck in vector notation, because the concept of vectors are
*perfectly suited* for studying linear transformations.  That's because talking
about linear transformations requires talking about scaling and adding,
and...hey, that's just exactly what vectors have!

From now on, we'll talk about linear transformations specifically on
*N-dimensional vector spaces* (vector spaces that have dimensions and bases we
can use).

### Studying linear transformations

From first glance, a linear transformation's description doesn't look too
useful or analyzable.  All you have is $f(\mathbf{v})$.  It could be anything!
Right?  Just a black box function?

But, actually, we can exploit its linearity and the fact that we're in a vector
space with a basis to analyze the heck out of any linear transformation, and
see that all of them actually have to follow some specific pattern.

Let's say that $A(\mathbf{x})$ is a linear transformation from N-dimensional
vector space $V$ to M-dimensional vector space $U$.  That is, $A : V
\rightarrow U$.

Because we know that, once we pick a set of basis vectors $\mathbf{e}_i$, any
vector $\mathbf{v}$ in $V$ can be decomposed as $v_1 \mathbf{e}_1 + v_2
\mathbf{e}_2 + \ldots v_n \mathbf{e}_N$, we really can just look at how a
transformation $A$ acts on this decomposition.  For example, if $V$ is
three-dimensional:

$$
A(\mathbf{v}) = A(v_1 \mathbf{e}_1 + v_2 \mathbf{e}_2 + v_3 \mathbf{e}_3)
$$

Hm.  Doesn't seem very insightful, does it?

### A simple definition

But!  We can exploit the linearity of $A$ (that it distributes and scales) to
rewrite that as:

$$
A(\mathbf{v}) = v_1 A(\mathbf{e}_1) + v_2 A(\mathbf{e}_2) + v_3 A(\mathbf{e}_3)
$$

Okay, take a moment to pause and take that all in.  This is actually a pretty
big deal!  This just means that, to study $A$, **all you need to study** is how
$A$ acts on our *basis vectors*.  If you know how $A$ acts on our basis vectors
of our vector space, that's really "all there is" about $A$!  Not such a black
box anymore!

That is, if I were to ask you, "Hey, what is $A$ like?", *all you'd have to
tell me* is the result of $A(\mathbf{e}_1)$, $A(\mathbf{e}_2$, and
$A(\mathbf{e}_3)$.  Just give me those three *vectors*, and we *uniquely
determine $A$*.

To put in another way, *any linear transformation* from a three-dimensional
vector space is uniquely characterized by *three vectors*: $A(\mathbf{e}_1)$,
$A(\mathbf{e}_2)$, and $A(\mathbf{e}_3)$.

Those three vectors *completely define* $A$.

In general, we see that *any linear transformation* from an N-dimensional
vector space can be *completely defined* by N vectors: the N results of that
transformation on each of N basis vectors we choose.

### Enter the Matrix

Okay, so how do we "give"/define/state those N vectors?

Well, recall that the result of $A(\mathbf{v})$ and $A(\mathbf{e}_1)$, etc. are
*themselves* vectors, in M-dimensional vector space $U$.  Let's say that $U$ is
2-dimensional, for now.

This means that any vector $\mathbf{u}$ in $U$ can be represented as $u_1
\mathbf{q}_1 + u_2 \mathbf{q}_2$, where $\mathbf{q}_1$ and $\mathbf{q}_2$ is an
arbitrary choice of basis vectors.

This means that $A(\mathbf{e}_1)$ etc. can also all be represented in terms of
these basis vectors.  So, laying it all out:

$$
\begin{aligned}
A(\mathbf{e}_1) & = a_{11} \mathbf{q}_1 + a_{21} \mathbf{q}_2 \\
A(\mathbf{e}_2) & = a_{12} \mathbf{q}_1 + a_{22} \mathbf{q}_2 \\
A(\mathbf{e}_3) & = a_{13} \mathbf{q}_1 + a_{23} \mathbf{q}_2
\end{aligned}
$$

Or, to use our bracket notation from before:

$$
\begin{aligned}
A(\mathbf{e}_1) & = \langle a_{11}, a_{21} \rangle \\
A(\mathbf{e}_2) & = \langle a_{12}, a_{22} \rangle \\
A(\mathbf{e}_3) & = \langle a_{13}, a_{23} \rangle
\end{aligned}
$$

So, we now see two facts:

1.  A linear transformation from an N dimensional vector space to an M
    dimensional vector space can be *defined* using N vectors.
2.  Each of those N vectors can, themselves, be defined using M scalars each.

Our final conclusion: *any* linear transformation from an N dimensional vector
space to an M dimensional vector space can be defined using $N M$
scalars.

That's right -- *all* possible linear transformations from a 3-dimensional
vector space to a 2-dimensional are parameterized by only *six* scalars!  These
six scalars uniquely determine and define our linear transformation, given a
set of basis vectors that we agree on.  All linear transformations
$\mathbb{R}^3 \rightarrow \mathbb{R}^2$ can be defined/encoded/expressed with
just six real numbers.

These six numbers are pretty important.  Just like how we often talk about
3-dimensional vectors in terms of the encoding of their three coefficients, we
often talk about linear transformations from 3-d space to 2-d space in terms of
their six defining coefficients.

We group these things up in something called a *matrix*.

If our linear transformation $A$ from a 3-dimensional vector space to a
2-dimensional vector space is defined by:

$$
\begin{aligned}
A(\mathbf{e}_1) & = a_{11} \mathbf{q}_1 + a_{21} \mathbf{q}_2 \\
A(\mathbf{e}_2) & = a_{12} \mathbf{q}_1 + a_{22} \mathbf{q}_2 \\
A(\mathbf{e}_3) & = a_{13} \mathbf{q}_1 + a_{23} \mathbf{q}_2
\end{aligned}
$$

(for arbitrary choice of bases $\mathbf{e}_i$ and $\mathbf{q}_i$)

We "encode" it as the matrix:

$$
\begin{bmatrix}
a_{11} & a_{12} & a_{13} \\
a_{21} & a_{22} & a_{23}
\end{bmatrix}
$$

And that's why we use matrices in linear algebra -- like how $\langle x, y, z
\rangle$ is a convenient way to represent and define a *vector* (once we agree
on a bases), a $M \times N$ matrix is a convenient way to represent and define
a *linear transformation* from an N-dimensional vector space to a M-dimensional
vector space (once we agree on the bases in both spaces).

<!-- And, sometimes we just think of the $\langle x, y, z \rangle$ encoding as the -->
<!-- vector itself, we often also talk about the $M \times N$ matrix as if it were -->
<!-- the linear transformation itself.  The matrix "is" the linear transformation, -->
<!-- informally. -->

Matrix Operations
-----------------

In this light, we can understand the definition of the common matrix
operations.

## Matrix-Vector Multiplication

Matrix-vector multiplication is essentially the *decoding* of the linear
transformation that the matrix represents.

Let's look at the $2 \times 3$ example.  Recall that we had:

$$
A(\mathbf{v}) = v_1 A(\mathbf{e}_1) + v_2 A(\mathbf{e}_2) + v_3 A(\mathbf{e}_3)
$$

And we say that $A$ is completely defined by:

$$
\begin{aligned}
A(\mathbf{e}_1) & = a_{11} \mathbf{q}_1 + a_{21} \mathbf{q}_2 \\
A(\mathbf{e}_2) & = a_{12} \mathbf{q}_1 + a_{22} \mathbf{q}_2 \\
A(\mathbf{e}_3) & = a_{13} \mathbf{q}_1 + a_{23} \mathbf{q}_2
\end{aligned}
$$

This means that:

$$
\begin{aligned}
A(\mathbf{v}) & = v_1 (a_{11} \mathbf{q}_1 + a_{21} \mathbf{q}_2) \\
              & + v_2 (a_{12} \mathbf{q}_1 + a_{22} \mathbf{q}_2) \\
              & + v_3 (a_{13} \mathbf{q}_1 + a_{23} \mathbf{q}_2)
\end{aligned}
$$

Which is itself a vector in $U$, so let's write this as a combination of its
components $\mathbf{q}_1$ and $\mathbf{q}_2$, by distributing and rearranging
terms:

$$
\begin{aligned}
A(\mathbf{v}) & = (v_1 a_{11} + v_2 a_{12} + v_3 a_{13}) \mathbf{q}_1
              & + (v_1 a_{21} + v_2 a_{22} + v_3 a_{23}) \mathbf{q}_2
\end{aligned}
$$

And this is exactly the formula for matrix-vector multiplication!

$$
\begin{bmatrix}
a_{11} & a_{12} & a_{13} \\
a_{21} & a_{22} & a_{23}
\end{bmatrix}

\begin{bmatrix}
v_1 \\
v_2 \\
v_3
\end{bmatrix}

=

\begin{bmatrix}
v_1 a_{11} + v_2 a_{12} + v_3 a_{13} \\
v_2 a_{21} + v_2 a_{22} + v_3 a_{23}
\end{bmatrix}
$$

