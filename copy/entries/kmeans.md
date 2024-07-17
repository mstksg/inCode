---
title: "Haskell Nuggets: k-means"
categories: Haskell
tags: haskell, machine learning
create-time: 2024/07/16 22:55:56
identifier: kmeans
slug: haskell-nugget-kmeans
---

AI is hot, so let's talk about some "classical machine learning" in Haskell
with k-means clustering! Over time my blog has slowly shifted from practical
projects to "high-concept" articles introducing new principles and techniques,
but I think it's lead me to hesitate posting about the simple one-off practical
Haskell solutions that have become my day-to-day doing actual work. So, I'm
going to try to post a bit more about small, practical snippets that
demonstrate some useful Haskell techniques and principles drive how I approach
coding in Haskell overall.

There are a bazillion ways of implementing such a simple algorithm, but this is
how *I'd* do it, as someone who develops almost exclusively in Haskell (or
functional pure languages) in both personal projects and work. It's not the
"right" way or the "best" way, but hopefully it can break beyond the simple toy
projects you'll see in isolation. You'll see how I integrate dependent types,
type-driven development, mutable data structures, and scaling things up with
parallelism.

The Algorithm
-------------

[K-means][] is a method of assigning a bunch of data points and samples into
*k* clusters. For the purpose of this post, we're going to talk about data
points as points in a vector space ("vectors") and clustering as grouping
together clusters of points that are close to each other (using Euclidean/l2
distance).

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
again.

A simple stopping condition would be if none of the k centers move after the
update step.

The algorithm leaves the assigning of the original points undefined, so it's
not deterministic...and it's also not optimal either, since it might converge
on clusters that aren't the best.  But it's simple enough conceptually that
it's taught in every beginner machine learning course.

The Haskell
-----------

We're going to be dealing with vectors and norms between them, so a good thing
to each for is the *[linear][]* library, which offers types for 2D vectors, 3D
vectors, etc. and how to deal with them as points in a vector space.

[linear]: http://hackage.haskell.org/package/linear

Then we want a collection of k cluster centers.  We can use *[vector-sized][]*
for a fixed-size collection of items, `Vector k (V2 Double)` for k 2-D points,
or `Vector k p` for k of any type of points.

So overall, our function will have type:

```haskell
kMeans :: [p] -> Vector k p
```

Which will take a collection of `p` points, and provide the `k` cluster
centers. Note here that we have "return-type polymorphism", where the `k`
(number of items) is determined by what type the user expects the function to
return.  If they want 3 clusters, they will call it expecting `Vector 3 p`.

We can take a list of `p`'s here because all we are going to do is *iterate*
over each one...we don't really care about random access or updates, so it's
really the best we can hope for, asymptotically[^branch].

[^branch]: Yes, yes, linked lists are notoriously bad for the CPU-level cache
and branch prediction, so if we are in a situation where we really cared, using
a contiguous memory data structure (like Storable Vector) might be better.

We have some leeway to how we initialize our initial clusters. One simple
solution is to just assign point 0 to cluster 0, point 1 to cluster, point 2 to
cluster 2, etc., cycling around the clusters.

```haskell
initialClusters :: Num p => [p] -> Vector k p
initialClusters pts = runST do
    sums <- MV.replicate 0
    counts <- MV.replicate 0
    ifor_ pts \i p -> do
      modify sums (+ p) (modulo i)
      modify counts (+ 1) (modulo i)
    liftA2 (/) <$> V.freeze sums <*> V.freeze counts
```

`runST` runs the mutable algorithm where we initialize a vector of point sums
and a vector of point counts. We then iterate over all of the points with their
index, and we add that point to the index of the cluster, modulo `k`. A sized
vector `Vector k a` is indexed by a `Finite k` (an integer from 0 to k-1). So,
`modulo :: Integer -> Finite k` will convert an integer index to the `Finite k`
index type, using modulus to wrap it around if it's too big.

We can actually do a similar loop to assign/bin each point and compute the new
centroids:

```haskell
moveClusters
    :: [p]
    -> Vector k p
    -> Vector k p
moveClusters pts cs0 = runST do
    sums <- MV.replicate 0
    counts <- MV.replicate 0
    for_ pts \p -> do
      let closestIx = V.minIndex (distance p <$> cs0)
      modify sums (+ p) closestIx
      modify counts (+ 1) closestIx
    liftA2 (/) <$> V.freeze sums <*> V.freeze counts
```

And so that's the whole thing:

```haskell
kMeans :: [p] -> Vector k p
kMeans pts = go (initialClusters ps)
  where
    go !cs
        | cs == cs' = cs
        | otherwise = go cs'
      where
        cs' = moveClusters pts cs
```

