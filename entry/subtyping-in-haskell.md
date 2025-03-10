Subtyping in Haskell

=====================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/subtyping-in-haskell.html)

It is often said that Haskell does not have subtyping. While it is indeed true
that Haskell doesn't have *ad-hoc* subtyping, you can achieve something similar
with Haskell, RankN types, and some key choice of data types or typeclasses.
And, in many situations, you can build programs around it!

As a simple example, let's redesign the API of the *Control.Monad.Trans.State*
module from the *transformers* package. Here, they define

``` haskell
data StateT s m a = StateT (s -> m (a, s))

runStateT  :: StateT s m a -> s -> m (a, s)
evalStateT :: StateT s m a -> s -> m a
execStateT :: StateT s m a -> s -> m s

get    ::                  StateT s m s
put    :: s             -> StateT s m ()
modify :: (s -> s)      -> StateT s m ()
state  :: (s -> (a, s)) -> StateT s m a

type State s = StateT s Identity

runState  :: State s a  -> s -> (a, s)
evalState :: State s a -> s -> a
execState :: State s a -> s -> s
```

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

