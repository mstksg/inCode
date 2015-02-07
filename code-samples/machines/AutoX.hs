{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
-- http://blog.jle.im/entry/id/28
--
-- Auto with on/off behavior and effectful stepping.

module AutoX where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Fix
import Prelude hiding      ((.), id)

-- | The AutoX type: Auto with on/off behavior and effectful stepping.
newtype AutoX m a b = AConsX { runAutoX :: a -> m (Maybe b, AutoX m a b) }

-- | Instances
instance Monad m => Category (AutoX m) where
    id    = AConsX $ \x -> return (Just x, id)
    g . f = AConsX $ \x -> do
              (y, f') <- runAutoX f x
              (z, g') <- case y of
                           Just _y -> runAutoX g _y
                           Nothing -> return (Nothing, g)
              return (z, g' . f')

instance Monad m => Functor (AutoX m r) where
    fmap f a = AConsX $ \x -> do
                 (y, a') <- runAutoX a x
                 return (fmap f y, fmap f a')

instance Monad m => Applicative (AutoX m r) where
    pure y    = AConsX $ \_ -> return (Just y, pure y)
    af <*> ay = AConsX $ \x -> do
                  (f, af') <- runAutoX af x
                  (y, ay') <- runAutoX ay x
                  return  (f <*> y, af' <*> ay')

instance Monad m => Arrow (AutoX m) where
    arr f     = AConsX $ \x -> return (Just (f x), arr f)
    first a   = AConsX $ \(x, z) -> do
                  (y, a') <- runAutoX a x
                  return (fmap (,z) y , first a')
    second a  = AConsX $ \(z, x) -> do
                  (y, a') <- runAutoX a x
                  return (fmap (z,) y, second a')
    a1 *** a2 = AConsX $ \(x1, x2) -> do
                  (y1, a1') <- runAutoX a1 x1
                  (y2, a2') <- runAutoX a2 x2
                  return  (liftA2 (,) y1 y2, a1' *** a2')
    a1 &&& a2 = AConsX $ \x -> do
                  (y1, a1') <- runAutoX a1 x
                  (y2, a2') <- runAutoX a2 x
                  return (liftA2 (,) y1 y2, a1' &&& a2')

instance Monad m => ArrowChoice (AutoX m) where
    left a = AConsX $ \x ->
                 case x of
                   Left l  -> do
                     (l', a') <- runAutoX a l
                     return (fmap Left l', left a')
                   Right r ->
                     return (Just (Right r), left a)

instance Monad m => Alternative (AutoX m a) where
    empty     = AConsX $ \_ -> return (Nothing, empty)
    a1 <|> a2 = AConsX $ \x -> do
                  (y1, a1') <- runAutoX a1 x
                  (y2, a2') <- runAutoX a2 x
                  return (y1 <|> y2, a1' <|> a2')


-- urggghh my head hurt so much trying to write this in a clean way using
-- recursive do notation instead of explicit calls to `mfix` and `fix`.
-- Anyone want to submit a pull request? :)
--
-- instance MonadFix m => ArrowLoop (AutoX m) where

-- | Smart constructors
--
-- aCons: Use as you would use `ACons`, but makes an `AutoX`.
aCons :: Monad m => (a -> (b, AutoX m a b)) -> AutoX m a b
aCons a = AConsX $ \x ->
            let (y, aX) = a x
            in  return (Just y, aX)

-- aConsM: Use as you would use `AConsM`, but makes an `AutoX`.
aConsM :: Monad m => (a -> m (b, AutoX m a b)) -> AutoX m a b
aConsM a = AConsX $ \x -> do
             (y, aX) <- a x
             return (Just y, aX)

-- aConsOn: Use as you would use `AConsOn`, but makes an `AutoX`.
aConsOn :: Monad m => (a -> (Maybe b, AutoX m a b)) -> AutoX m a b
aConsOn a = AConsX $ \x ->
              let (y, aX) = a x
              in  return (y, aX)

-- | AutoX Test Autos
--
-- summer: Outputs the sum of all inputs so far.  Demonstrates the usage of
--      the `aCons` smart constructor.
summer :: (Monad m, Num a) => AutoX m a a
summer = sumFrom 0
  where
    sumFrom n = aCons $ \input ->
      let s = n + input
      in  ( s , sumFrom s )

-- arrM: Converts an `a -> m b` into an always-on `AutoX` that just runs
--      the function on the input and outputs the result.  Demonstrates the
--      usage of the `aConsM` smart constructor.
arrM :: Monad m => (a -> m b) -> AutoX m a b
arrM f = aConsM $ \x -> do
                    y <- f x
                    return (y, arrM f)

-- untilA: Lets all values pass through until the first one that satisfies
--      the predicate.  Demonstrates the usage of the `aConsOn` smart
--      constructor.
untilA :: Monad m => (a -> Bool) -> AutoX m a a
untilA p = aConsOn $ \x ->
             if p x
               then (Just x , untilA p)
               else (Nothing, empty   )

