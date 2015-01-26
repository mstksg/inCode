{-# LANGUAGE Arrows #-}
{-# LANGUAGE TupleSections #-}

module Auto3 where

import Auto hiding (onFor)
import Auto2
import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Function       (fix)
import Prelude hiding      ((.), id)

onFor :: Int -> Auto a (Maybe a)
onFor n = proc x -> do
    count <- summer -< 1
    id -< if count <= n
            then Just x
            else Nothing

onUntil :: (a -> Bool) -> Auto a (Maybe a)
onUntil p = proc x -> do
    off <- autoFold (||) False -< p x
    id -< if off
            then Nothing
            else Just x

(.?) :: Auto b (Maybe c) -> Auto a (Maybe b) -> Auto a (Maybe c)
g .? f = proc x -> do
    resF <- f -< x
    case resF of
        Nothing -> id -< Nothing
        Just x  -> g  -< x

newtype AutoOn a b = AConsOn { runAutoOn :: a -> (Maybe b, AutoOn a b) }

instance Functor (AutoOn r) where
    fmap f a = AConsOn $ \x ->
                 let (y, a') = runAutoOn a x
                 in  (fmap f y, fmap f a')

instance Category AutoOn where
    id    = AConsOn $ \x -> (Just x, id)
    g . f = AConsOn $ \x ->
              let (y, f') = runAutoOn f x
                  (z, g') = case y of
                              Nothing -> (Nothing, g)
                              Just y' -> runAutoOn g y'
              in  (z, g' . f')

instance Arrow AutoOn where
    arr f     = AConsOn $ \x -> (Just (f x), arr f)
    first a   = AConsOn $ \(x, z) ->
                  let (y, a') = runAutoOn a x
                      res     = fmap (, z) y
                  in  (res, first a')

instance ArrowChoice AutoOn where
    left a = AConsOn $ \x ->
                 case x of
                   Left l  -> let (l', a') = runAutoOn a l
                              in  (fmap Left l', left a')
                   Right r -> (Just (Right r), left a)

