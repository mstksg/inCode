Practical Dependent Types in Haskell: Type-Safe Neural Networks
===============================================================

Categories
:   Haskell
:   Ramblings
Tags
:   functional programming
:   depedent types
:   numerical
:   haskell
:   singletons
:   types
CreateTime
:   2016/02/25 11:47:50
PostDate
:   Never
Identifier
:   dependent-haskell-1

Whether you like it or not, programming with dependent types in Haskell moving
slowly but steadily to the mainstream of Haskell programming.  In the current
state of Haskell education, dependent types are often considered topics for
"advanced" Haskell users.  However, I can definitely foresee a day where the
ease of use of modern Haskell libraries relying on dependent types as well as
their ubiquitousness forces programming with dependent types to be an integral
part of regular intermediate (or even beginner) Haskell education, as much as
Traversable or Maps.

So, the point of this post is to show some practical examples of using
dependent types in the real world, and to also walk through the "why" and
high-level philosophy of the way you structure your Haskell programs.  It'll
also hopefully instill an intuition of a dependently typed work flow of
"exploring" how dependent types can help your current programs.

There are other great tutorials I'd recommend online if you want to explore
dependent types in Haskell further, including [this great servant
"tutorial"][servtut].  Also, I should provide a disclaimer --- I'm also
currently exploring all of this as I'm going along too. It's a wild world out
there.  Join me and let's be a part of the frontier!

[servtut]: http://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/

Neural Networks
---------------

[Artificial neural networks][ann] have been somewhat of a hot topic in
computing recently.  At their core they involve matrix multiplication and
manipulation, so they do seem like a good candidate for a dependent types. Most
importantly, implementations of training algorithms (like back-propagation) are
tricky to implement correctly --- despite being simple, there are many
locations where accidental bugs might pop up when multiplying the wrong
matrices, for example.

[ann]: https://en.wikipedia.org/wiki/Artificial_neural_network

However, it's not always easy to gauge before-the-fact what would or would not
be a good candidate for adding dependent types to, and often times, it can be
considered premature to start off with "as powerful types as you can".  So
we'll walk through a simple implementation *without*, and see all of the red
flags that hint that you might want to start considering stronger types.

Vanilla Types
-------------

![Feed-forward ANN architecture](/img/entries/dependent-haskell-1/ffneural.png "Feed-forward ANN architecture")

We're going to be implementing a feed-forward neural network, with
back-propagation training.  A feed-forward neural network consists structurally
of layers of "nodes", each connected to the each of the nodes of the previous
layer and each of the nodes of the next layer.  The most important feature of
the network itself is the "strength" of these connections, called "weights". To
make a prediction, each node takes, as an input, the weighted sum of all of the
outputs of the previous layer, weighted by the connection weights.  It then
outputs a function of this weighted sum, $f(x)$, to be used by all of the nodes
of the next layer.  At the high-level, the user feeds in an input vector to the
top-level nodes, the network processes these layer-by-layer, and the result of
the final nodes is what is taken as the network's output.  The "goal" of
designing/training a network is to somehow pick the right set of weights that
will give the output that you want for the given input.

While it's nice to think about neural networks in terms of their nodes, it
makes more sense computationally to only identify a network by simply the
matrices of weights alone.

~~~haskell
data Network = O !(Matrix Double)
             | !(Matrix Double) :&~ !Network
  deriving (Show, Eq)

infixr 5 :&~
~~~

We're using the `Matrix` type from the awesome [hmatrix][] library for linear
algebra, implemented using blas/lapack under the hood.  We'll say that a $3
\times 4$ matrix is a matrix connecting the outputs of layer of 3 nodes to
inputs for layer of 4 nodes, and that $W_{ij}$ is the specific weight from node
$i$ of the first layer to node $j$ of the second.  The first *row* of $W$ is
the weights directed at the first node in the second layer.

[hmatrix]: http://hackage.haskell.org/package/hmatrix

TODO: graphs using diagrams?

We can write a simple function to "run" our network:

~~~haskell
logistic :: Double -> Double
logistic x = 1 / (1 + e^(-x))

runLayer :: Matrix Double -> Vector Double -> Vector Double
runLayer l v = cmap logistic (l #> v)

runNet :: Network -> Vector Double -> Vector Double
runNet (O l)      v = runLayer l v
runNet (l :&~ n') v = runNet n' (runLayer l v)
~~~

TODO: examples of running

If you're a normal programmer, this might seem perfectly fine.  If you are a
Haskell programmer, you should already be having heart attacks. Let's imagine
all of the bad things that could happen:

*   How do we even know that each subsequent matrix in the network is
    "compatible"?   We want the outputs of one matrix to line up with the
    inputs of the next, but there's no way to know unless we have "smart
    constructors" to check while we add things.  But it's possible to build a
    bad network, and things will just explode at runtime.

*   How do we know the size vector the network expects?  What stops you from
    sending in a bad vector at run-time and having everything explode?

*   How do we verify that we have implemented `runLayer` and `runNet` in a way
    that they won't suddenly fail at runtime?  We write `l #> v`, but how do we
    know that it's even correct?  We can it prove ourselves, but the compiler
    won't help us.









