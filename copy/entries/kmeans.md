---
title: "Haskell Nuggets: k-means"
categories: Haskell
tags: haskell, machine learning, dependent types, functional programming
create-time: 2024/07/16 22:55:56
date: 2024/07/26 12:06:27
identifier: kmeans
slug: haskell-nuggets-kmeans
---

AI is hot, so let's talk about some "classical machine learning" in Haskell
with k-means clustering! Let's throw in some dependent types too.

There are a bazillion ways of implementing such a simple algorithm, but this is
how *I'd* do it, as someone who develops almost exclusively in Haskell (or
functional pure languages) in both personal projects and work. It's not the
"right" way or the "best" way, but it's the way that brings me joy.  Hopefully
it can also break beyond the simple toy projects you'll often see in conceptual
tutorials. You'll see how I integrate dependent types, type-driven development,
mutable data structures, generating random data, and preparation for
parallelism. I have been meaning to shift away from "conceptual"
posts and instead post a bit more about small, practical snippets that
demonstrate some useful Haskell techniques and principles drive how I approach
coding in Haskell overall.

For reference, the intended audience is for people with knowledge of Haskell
syntax and basic idioms (mapping, traversing, folding, applicatives).  The
source code [is online here][source], and is structured as a nix flake script.
If you have [nix][] installed (and flakes enabled), you should be able to run
the script as an executable (`./kmeans.hs`).  You can also load it for editing
with `nix develop` + `ghci`.

!!![source]:kmeans/kmeans.hs
[nix]: https://nixos.org/

The Algorithm
-------------

[K-means][] is a method of assigning a bunch of data points and samples into
*k* clusters. For the purpose of this post, we're going to talk about data
points as points in a vector space and clustering as grouping together clusters
of points that are close to each other (using Euclidean/L2 distance).

[K-means]: https://en.wikipedia.org/wiki/K-means_clustering

The basic iteration goes like this:

1.  Start with *k* cluster centers ("means", or "centroids" sometimes), *k*
    arbitrary points in your space.
2.  Repeat until the stop condition:
    *   Assign/bucket each data point to its closest cluster center/mean.
    *   Move each of the cluster centers to the mean/centroid of the points
        that were assigned to it, or the points in its bucket.

Basically, we repeatedly say, "if this was the true cluster center, what points
would be in it?". Then we adjust our cluster center to the center of those
points at were assigned to it, updating to a better guess.  Then we repeat
again. A simple stopping condition would be if none of the k centers move after
the update step.

The algorithm leaves the assigning of the original points undefined, and it's
also not optimal either, since it might converge on clusters that aren't the
best.  But it's simple enough conceptually that it's taught in every beginner
machine learning course.

The Haskell
-----------

We're going to be dealing with points in a vector space and distances between
them, so a good thing to each for is the *[linear][]* library, which offers
types for 2D vectors, 3D vectors, etc. and how to deal with them as points in a
vector space. *linear* offers an abstraction over multiple vector space points.
A point has type `p a`: `p` is a vector space over field `a`.  The library has
`V2 a` for 2D points, so `V2 Double` is *essentially* $\mathbb{R}^2$, a 2
dimensional point with double-valued components.

[linear]: http://hackage.haskell.org/package/linear

We want a collection of k cluster centers.  We can use *[vector-sized][]* for a
fixed-size collection of items, `Vector k (V2 Double)` for k 2-D double points,
or `Vector k (p a)` for k of any type of points.[^vector]

[^vector]: Be mindful, for `Vector` here we are using things strictly as a
"fixed-sized collection of values", whereas for *linear*, we have types like
`V2` which represent *points in a mathematical vector space*.  It's a bit
unfortunate that the terminology overlaps here a bit.

[vector-sized]: http://hackage.haskell.org/package/vector-sized

So overall, our function will have type:

```haskell
kMeans :: [p a] -> Vector k (p a)
```

It will take a collection of `p a` points, and provide the `k` cluster centers.
Note here that we have "return-type polymorphism", where the `k` (number of
items) is determined by what type the user expects the function to return.  If
they want 3 clusters of 2d points, they will call it expecting `Vector 3 (V2
Double)`.  If they want 10 clusters of 2d points, they would call it expecting
`Vector 10 (V2 Double)`.

We take a *list* of `p a`'s here because all we are going to do is *iterate*
over each one...we don't really care about random access or updates, so it's
really the best we can hope for, asymptotically[^branch].

[^branch]: Yes, yes, linked lists are notoriously bad for the CPU-level cache
and branch prediction, so if we are in a situation where we really cared, using
a contiguous memory data structure (like Storable Vector) might be better.

We have some leeway to how we initialize our initial clusters. One simple
solution is to just assign point 0 to cluster 0, point 1 to cluster, point 2 to
cluster 2, etc., cycling around the clusters.

```haskell
!!!kmeans/kmeans.hs "initialClusters ::"
```

`runST` runs the mutable algorithm where we initialize a vector of point sums
and a vector of point counts. We then iterate over all of the points with their
index (with `ifor_`), and we add that point to the index of the cluster, modulo
`k`. A sized vector `Vector k a` is indexed by a `Finite k` (an integer from 0
to *k-1*). So, `modulo :: Integer -> Finite k` will convert an integer index to
the `Finite k` index type, using modulus to wrap it around if it's too big.

Here we are using some functions from *linear*:

*   `(^+^) :: (Additive p, Num a) => p a -> p a -> p a` which adds together two
    points
*   `(^/) :: (Functor p, Fractional a) => p a -> a -> p a` which divides a
    point by a scalar

At the end of it all, we use `V.generateM` to assemble our final (immutable)
centroids by reading out the sums and totals at each cluster:

```haskell
V.generateM :: (Finite k -> m a) -> m (Vector k a)
```

Note that we the lengths of all our intermediate vectors (`sums`, `counts`, and
the final result) are all implicitly inferred through type inference (by `k`).

We can actually do a similar loop to assign/bin each point and compute the new
centroids:

```haskell
!!!kmeans/kmeans.hs "moveClusters ::"
```

We just have to be careful to not move the centroid if there is no points
assigned to it, otherwise we'd be dividing by 0.

Notice there's also something a little subtle going on with `closestIx`, which
exposes a bit of the awkwardness with working with type-level numbers in
Haskell today.  The type of `V.minIndex` is:

```haskell
V.minIndex :: forall a n. Ord a => Vector (n + 1) a -> Finite (n + 1)
```

This is because we only ever get a minimum if the vector is non-empty.  So the
library takes `n + 1` as the size to ensure that only positive length vectors
are passed.

In our case, we want `V.minIndex blah :: Finite k`. However, remember how
typechecking works: we need to unify the type variables `a` and `n` so that `n
+ 1` is equal to `k`. So, what does *n* have to be so that $n + 1 = k$? Well,
we can see from algebra that `n` needs to be `k - 1`: `(k - 1) + 1` is equal
to `k`. However, GHC is a little dumb-dumb here in that it cannot solve for
`n` itself. We can explicitly pass in `@(k - 1)` to say that `n` has to be `k - 1`.

For this to work we need to pull in a GHC plugin [ghc-typelits-natnormalise][]
which will allow GHC to simplify `(k - 1) + 1` to be `k`, which it can't do by
itself for some reason.  It also requires the constraint that `1 <= k` in order
for `k - 1` to make sense for natural number `k`. We can pull in the plugin with:

```haskell
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
```

[ghc-typelits-natnormalise]: http://hackage.haskell.org/package/ghc-typelits-natnormalise

Honestly if we were to design the library from scratch today, I'd define it as:

```haskell
V.minIndex :: forall a n. (Ord a, 1 <= n) => Vector n a -> Finite n
```

in the first place, and we wouldn't need the typechecker plugin.

Anyway so that's the whole thing:

```haskell
!!!kmeans/kmeans.hs "kMeans ::"
```

Note I also added a stop after 100 steps, just to be safe.

### Type-Level Advantages and Usability

Having `k` in the type is useful for many reasons:

1.  It helps us ensure that `moveClusters` doesn't change the number of
    clusters/centroids.  If it was just `[p a] -> [p a]` we
    cannot guarantee it does not add or drop clusters.
2.  The type system means we don't have to manually pass `int` sizes around.
    For example, in `initialClusters`, we implicitly pass the size around *four
    times* when we do `MV.replicate` (twice), `modulo`, and `generateM`! And, in
    the definition of `kMeans`, we implicitly pass it on to our call to
    `initialClusters`.
3.  We don't have to worry about out-of-bounds indexing because any indices we
    generate (using `modular` or `minIndex`) are guaranteed (by their types) to
    be valid.
4.  It's useful for the caller to guarantee they are getting what they are
    asking for.  If `kMeans :: Int -> [p a] -> [p a]`, then we (as the caller)
    can't be sure that the result list has the number of items that you
    requested.  But because we have `kMeans :: [p a] -> Vector k (p a)`, the
    compiler ensures that the result has *k* items.

However you won't *always* be able to necessarily put in a literal `3` in
`Vector 3 (V2 Double)`.  Maybe your *k* comes from a configuration file or
something else you pull in at runtime.  We need a way to call `kMeans` with
just an `Int`! (also known as "reification")

Normally, this means using `someNatVal` to convert a value-level `Natural` into
a type-level `Nat`.  However, in this case we have to be a bit more careful
because *k* must be at least 1.  As of GHC 9.2, we can use `cmpNat` (before
this, you could use [typelits-witnesses][]) to bring this constraint into
scope.

[typelits-witnesses]: http://hackage.haskell.org/package/typelits-witnesses

```haskell
!!!kmeans/kmeans.hs "kMeans' ::"
```

### Applying the Clusters

Of course, `kMeans` only gets us our centroids, so it would be useful to
actually create the clusters themselves and all their member points.  We can do
something similar to what we did before with `ST` and mutable vectors and
`runST`, but life is too short to always be using mutable state.  Let's instead
build up a map of indices to all the points that are closest to that index.
Then we use `generate :: (Finite k -> a) -> Vector k a` to create a vector by
picking out the maps' value at the index at each spot in the vector.  Again
here we see that the type system helps us by not having to manually pass in a
size, and `generate` giving us indices `i` that match the number of the
centroids we are grouping on.

```haskell
!!!kmeans/kmeans.hs "applyClusters ::"
```

### Parallelization

Typically we parallelize this by assigning each worker thread a chunk of points
it has to deal with, and having each one compute sums and counts and
coordinating it all back in the end.  In this case we want to keep the
intermediate sums and counts:

```haskell
!!!kmeans/kmeans.hs "groupAndSum ::"
```

Running an example
------------------

For funsies let us generate sample points that we know are clustered based on k
random cluster centers, using [mwc-random][] for randomness.

[mwc-random]: http://hackage.haskell.org/package/mwc-random

```haskell
!!!kmeans/kmeans.hs "generateSamples ::"
```

By the way isn't it funny that everything just ends up being `traverse` or some
derivation of it (like `replicateM` or `sequenceA`)? Anyways,

```haskell
!!!kmeans/kmeans.hs "main ::"
```

```
* points
V2 15.117809404050517 2.4824833627968137
V2 14.825686288414198 2.569457175505424
V2 14.806948346588289 2.3222471406644867
V2 15.012490917145703 2.41735577349797
V2 15.007612893836304 2.3823051676970746
V2 14.866016893659538 2.590777185848723
V2 14.83908442030534 2.5756382736578343
V2 14.969996769619264 2.549568226274995
V2 14.983371307935904 2.4823314218207586
V2 14.931617828479244 2.469607213743923
V2 29.426938075603196 9.90899836541481
V2 29.657363050066813 9.844458859292706
V2 29.487332896419872 9.65240948313236
V2 29.717470180982964 9.756325723236502
V2 29.67198068295402 9.688676918673274
V2 29.564673351390947 9.63896189703656
V2 29.56057222121772 9.833541221236656
V2 29.563747509453506 9.75593412158655
V2 29.497322568720026 9.684752183878274
V2 29.598339480038018 9.968546198295204
V2 3.204536005881443 30.039372398954175
V2 3.1684921057193005 30.082909536200095
V2 3.2040077021183793 29.90694542057959
V2 3.151859377604784 29.89198303817146
V2 3.1027920089123935 30.240061564528673
V2 3.2323285236152937 30.037812094337777
V2 3.2722229374242366 30.05215727709455
V2 2.9723263815754652 30.06281544324189
V2 3.1935700833126437 30.068367400732857
V2 3.253701544151972 29.875079507116222
* actual centers
[V2 14.938139892220267 2.4859265040850276,V2 29.55811494146035 9.808348344980386,V2 3.239842205071254 30.070304958459946]
* kmeans centers
[V2 14.936063507003428 2.484177094150801,V2 29.57457400168471 9.773260497178288,V2 3.175583667031591 30.025750368095725]
```

Neat!

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts.  Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
