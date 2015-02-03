{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Auto3 where

import Auto
import Auto2
import System.Random
import Control.Applicative
import System.IO
import Control.Monad.Fix
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Trans.State
import Data.Function             (fix)
import Prelude hiding            ((.), id)

newtype AutoM m a b = AConsM { runAutoM :: a -> m (b, AutoM m a b) }

-- | Auto testers
--
testAutoM :: Monad m => AutoM m a b -> [a] -> m ([b], AutoM m a b)
testAutoM a []      = return ([], a)
testAutoM a (x:xs)  = do
    (y , a' ) <- runAutoM a x
    (ys, a'') <- testAutoM a' xs
    return (y:ys, a'')

testAutoM_ :: Monad m => AutoM m a b -> [a] -> m [b]
testAutoM_ a = liftM fst . testAutoM a


-- | Instances
instance Monad m => Category (AutoM m) where
    id    = AConsM $ \x -> return (x, id)
    g . f = AConsM $ \x -> do
              (y, f') <- runAutoM f x
              (z, g') <- runAutoM g y
              return (z, g' . f')

instance Monad m => Functor (AutoM m r) where
    fmap f a = AConsM $ \x -> do
                 (y, a') <- runAutoM a x
                 return (f y, fmap f a')

instance Monad m => Applicative (AutoM m r) where
    pure y    = AConsM $ \_ -> return (y, pure y)
    af <*> ay = AConsM $ \x -> do
                  (f, af') <- runAutoM af x
                  (y, ay') <- runAutoM ay x
                  return (f y, af' <*> ay')

instance Monad m => Arrow (AutoM m) where
    arr f     = AConsM $ \x -> return (f x, arr f)
    first a   = AConsM $ \(x, z) -> do
                  (y, a') <- runAutoM a x
                  return ((y, z), first a')
    second a  = AConsM $ \(z, x) -> do
                  (y, a') <- runAutoM a x
                  return ((z, y), second a')
    a1 *** a2 = AConsM $ \(x1, x2) -> do
                  (y1, a1') <- runAutoM a1 x1
                  (y2, a2') <- runAutoM a2 x2
                  return ((y1, y2), a1' *** a2')
    a1 &&& a2 = AConsM $ \x -> do
                  (y1, a1') <- runAutoM a1 x
                  (y2, a2') <- runAutoM a2 x
                  return ((y1, y2), a1' &&& a2')

instance Monad m => ArrowChoice (AutoM m) where
    left a = AConsM $ \x ->
                 case x of
                   Left l  -> do
                     (l', a') <-runAutoM a l
                     return (Left l', left a')
                   Right r ->
                     return (Right r, left a)

instance MonadFix m => ArrowLoop (AutoM m) where
    loop a = AConsM $ \x -> do
               rec ((y, d), a') <- runAutoM a (x, d)
               return (y, loop a')

autoM :: Monad m => Auto a b -> AutoM m a b
autoM a = AConsM $ \x -> let (y, a') = runAuto a x
                         in  return (y, autoM a')

arrM :: Monad m => (a -> m b) -> AutoM m a b
arrM f = AConsM $ \x -> do
                    y <- f x
                    return (y, arrM f)

replicateGets :: AutoM IO Int String
replicateGets = proc n -> do
    ioString <- arrM (\_ -> getLine) -< ()
    let inpStr = concat (replicate n ioString)
    autoM monoidAccum -< inpStr

logging :: Show b => Auto a b -> AutoM IO a b
logging a = proc x -> do
    y <- autoM a -< x
    arrM (appendFile "log.txt") -< show y ++ "\n"
    id -< y

laggingSummer :: Num a => Auto a a
laggingSummer = sumFrom 0
  where
    sumFrom :: Num a => a -> Auto a a
    sumFrom x0 = ACons $ \x -> (x0, sumFrom (x0 + x))

piTargeter :: Auto Double Double
piTargeter = proc control -> do
    rec let err = control - response
        errSums  <- summer         -< err

        input    <- laggingSummer  -< 0.2 * err + 0.01 * errSums
        response <- blackBoxSystem -< input

    id -< response
  where
    blackBoxSystem = id     -- to simplify things :)

