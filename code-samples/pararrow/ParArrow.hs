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

run :: ParArrow a b -> (a -> IO b)
run = go . collapse
  where
    go :: ParArrow a b -> (a -> IO b)
    go (Pure f)      = \x -> putStrLn "P" >> return (f x)
    go (Seq f g)     = go f >=> go g
    go (Par l f g r) = \x -> do
      putStrLn "F"

      fres <- newEmptyMVar
      gres <- newEmptyMVar

      let (fin,gin) = l x
      forkIO $ run f fin >>= putMVar fres
      forkIO $ run g gin >>= putMVar gres

      reses <- (,) <$> takeMVar fres <*> takeMVar gres
      return (r reses)
