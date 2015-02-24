Abstracting over Computation with Free
======================================

Categories
:   Haskell
:   Ramblings
:   Projects
Tags
:   haskell
:   monads
:   free-monads
CreateTime
:   2015/02/24 02:02:55
PostDate
:   Never
Identifier
:   free-random

It's fair enough to say that I'm a little late to the free monad party, but
I still think their power is greatly either misunderstood or underrated or
obscure in Haskell, and this article will be my attempt to throw more examples
of its usage.

Here we're going to construct a type that can represent computations using
randomness or entropy, and we're going to abstract over our entropy source ---
not by swapping out the pseudorandom generator, but by really abstracting over
what our computation really *is* to the barest essentials.
