{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module AutoOn where

import Auto
import Control.Monad
import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad.Fix
import Prelude hiding      ((.), id)

newtype AutoOn a b = AConsOn { runAutoOn :: a -> (Maybe b, AutoOn a b) }

autoOn :: Auto a b -> AutoOn a b
autoOn a = AConsOn $ \x ->
             let (y, a') = runAuto a x
             in  (Just y, autoOn a')

arrOn :: (a -> Maybe b) -> AutoOn a b
arrOn f = AConsOn $ \x -> (f x, arrOn f)

fromAutoOn :: AutoOn a b -> Auto a (Maybe b)
fromAutoOn a = ACons $ \x ->
                 let (y, a') = runAutoOn a x
                 in  (y, fromAutoOn a')

-- | Instances
instance Category AutoOn where
    id    = AConsOn $ \x -> (Just x, id)
    g . f = AConsOn $ \x ->
              let (y, f') = runAutoOn f x
                  (z, g') = case y of
                              Just _y -> runAutoOn g _y
                              Nothing -> (Nothing, g)
              in  (z, g' . f')

instance Functor (AutoOn r) where
    fmap f a = AConsOn $ \x ->
                 let (y, a') = runAutoOn a x
                 in  (fmap f y, fmap f a')

instance Applicative (AutoOn r) where
    pure y    = AConsOn $ \_ -> (Just y, pure y)
    af <*> ay = AConsOn $ \x ->
                  let (f, af') = runAutoOn af x
                      (y, ay') = runAutoOn ay x
                  in  (f <*> y, af' <*> ay')

instance Arrow AutoOn where
    arr f     = AConsOn $ \x -> (Just (f x), arr f)
    first a   = AConsOn $ \(x, z) ->
                  let (y, a') = runAutoOn a x
                  in  (fmap (,z) y , first a')
    second a  = AConsOn $ \(z, x) ->
                  let (y, a') = runAutoOn a x
                  in  (fmap (z,) y, second a')
    a1 *** a2 = AConsOn $ \(x1, x2) ->
                  let (y1, a1') = runAutoOn a1 x1
                      (y2, a2') = runAutoOn a2 x2
                  in  (liftA2 (,) y1 y2, a1' *** a2')
    a1 &&& a2 = AConsOn $ \x ->
                  let (y1, a1') = runAutoOn a1 x
                      (y2, a2') = runAutoOn a2 x
                  in  (liftA2 (,) y1 y2, a1' &&& a2')

instance ArrowChoice AutoOn where
    left a = AConsOn $ \x ->
                 case x of
                   Left l  ->
                     let (l', a') = runAutoOn a l
                     in  (fmap Left l', left a')
                   Right r ->
                     (Just (Right r), left a)

instance ArrowLoop AutoOn where
    loop a = AConsOn $ \x ->
               let res = do
                     rec let (myd, a') = runAutoOn a (x, d)
                         (y, d) <- myd
                     return (y, a')
               in  case res of
                     Just (_y, _a') -> (Just _y, loop _a')
                     Nothing        -> (Nothing, loop a)


instance Alternative (AutoOn a) where
    empty     = AConsOn $ \_ -> (Nothing, empty)
    a1 <|> a2 = AConsOn $ \x ->
                  let (y1, a1') = runAutoOn a1 x
                      (y2, a2') = runAutoOn a2 x
                  in  (y1 <|> y2, a1' <|> a2')


onFor :: Int -> AutoOn a a
onFor n = proc x -> do
    i <- autoOn summer -< 1
    if i <= n
      then id    -< x       -- succeed
      else empty -< x       -- fail

filterA :: (a -> Bool) -> AutoOn a a
filterA p = arrOn (\x -> x <$ guard (p x))

untilA :: (a -> Bool) -> AutoOn a a
untilA p = proc x -> do
    stopped <- autoOn (autoFold (||) False) -< p x
    if stopped
      then empty -< x       -- fail
      else id    -< x       -- succeed
    
