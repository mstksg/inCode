{-# LANGUAGE ExistentialQuantification #-}

module ParArrow where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Arrow
import Control.Category
import Control.Concurrent
import Prelude hiding      (id, (.))

data ParArrow a b =                     Pure  (a -> b)
                  | forall z.           Seq   (ParArrow a z)
                                              (ParArrow z b)
                  | forall a1 a2 b1 b2. Par   (a -> (a1, a2))
                                              (ParArrow a1 b1)
                                              (ParArrow a2 b2)
                                              ((b1, b2) -> b)

instance Category ParArrow where
    id    = Pure id
    f . g = Seq g f

instance Arrow ParArrow where
    arr      = Pure
    first f  = f  *** id
    second g = id *** g
    f &&& g  = Par (id &&& id) f g id
    f *** g  = Par id          f g id

collapse :: ParArrow a b -> ParArrow a b
collapse (Seq f g)       =
    case (collapse f, collapse g) of
      (Pure p1, Pure p2)      -> Pure (p1 >>> p2)
      (Seq s1 s2, _)          -> Seq (collapse s1)
                                     (collapse (Seq s2 g))
      (_, Seq s1 s2)          -> Seq (collapse (Seq f s1))
                                     (collapse s2)
      (Pure p, Par l p1 p2 r) -> Par (p >>> l)
                                     (collapse p1) (collapse p2)
                                     r
      (Par l p1 p2 r, Pure p) -> Par l
                                     (collapse p1) (collapse p2)
                                     (r >>> p)
      (Par l p1 p2 r,
       Par l' p1' p2' r')     -> let p1f x = fst . l' . r $ (x, undefined)
                                     p2f x = snd . l' . r $ (undefined, x)
                                     pp1 = collapse (p1 >>> arr p1f >>> p1')
                                     pp2 = collapse (p2 >>> arr p2f >>> p2')
                                 in  Par l pp1 pp2 r'
collapse p = p

collapse_ :: ParArrow a b -> ParArrow a b
collapse_ (Seq f g)       =
    case (collapse_ f, collapse_ g) of
      (Pure p1, Pure p2)      -> Pure (p1 >>> p2)
      (Seq s1 s2, _)          -> Seq (collapse_ s1)
                                     (collapse_ (Seq s2 g))
      (_, Seq s1 s2)          -> Seq (collapse_ (Seq f s1))
                                     (collapse_ s2)
      -- (Pure p, Par l p1 p2 r) -> Par (p >>> l)
      --                                (collapse_ p1) (collapse_ p2)
      --                                r
      -- (Par l p1 p2 r, Pure p) -> Par l
      --                                (collapse_ p1) (collapse_ p2)
      --                                (r >>> p)
      (Par l p1 p2 r,
       Par l' p1' p2' r')     -> let p1f x = fst . l' . r $ (x, undefined)
                                     p2f x = snd . l' . r $ (undefined, x)
                                     pp1 = collapse_ (p1 >>> arr p1f >>> p1')
                                     pp2 = collapse_ (p2 >>> arr p2f >>> p2')
                                 in  Par l pp1 pp2 r'
      (f,g)                   -> Seq f g
collapse_ p = p

data Graph = GPure                  -- Pure function
           | Graph :->: Graph       -- Sequenced arrows
           | Graph :/: Graph        -- Parallel arrows
           deriving Show

analyze' :: ParArrow a b -> Graph
analyze' (Pure _) = GPure
analyze' (Seq f g) = analyze' f :->: analyze' g
analyze' (Par _ f g _) = analyze' f :/: analyze' g

analyze :: ParArrow a b -> Graph
analyze = analyze' . collapse

analyze_ :: ParArrow a b -> Graph
analyze_ = analyze' . collapse_

runPar' :: ParArrow a b -> (a -> IO b)
runPar' = go
  where
    go :: ParArrow a b -> (a -> IO b)
    go (Pure f)      = \x -> putStrLn "P" >> return (f x)
    go (Seq f g)     = go f >=> go g
    go (Par l f g r) = \x -> do
      putStrLn "F"

      fres <- newEmptyMVar
      gres <- newEmptyMVar

      let (fin,gin) = l x
      forkIO $ runPar' f fin >>= putMVar fres
      forkIO $ runPar' g gin >>= putMVar gres

      reses <- (,) <$> takeMVar fres <*> takeMVar gres
      return (r reses)


runPar :: ParArrow a b -> (a -> IO b)
runPar = runPar' . collapse

runPar_ :: ParArrow a b -> (a -> IO b)
runPar_ = runPar' . collapse_
