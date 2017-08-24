---
title: Subtyping in Haskell
categories: Haskell, Ramblings
tags: functional programming, haskell, types
create-time: 2017/08/23 00:44:23
date: Never
identifier: subtyping
slug: subtyping-in-haskell
---

It is often said that Haskell does not have subtyping.  While it is indeed true
that Haskell doesn't have *ad-hoc* subtyping, you can achieve something similar
with Haskell, RankN types, and some key choice of data types or typeclasses.
And, in many situations, you can build programs around it!

As a simple example, let's redesign the API of the *Control.Monad.Trans.State*
module from the *transformers* package.  Here, they define

```haskell
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


