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

newtype AutoOn1 a b = AutoOn1 (Auto a (Maybe b))

instance Category AutoOn1 where
    id                    = AutoOn1 (arr Just)
    AutoOn1 g . AutoOn1 f = AutoOn1 (g .? f)

instance Functor (AutoOn1 a) where
    fmap f (AutoOn1 a) = AutoOn1 (fmap (fmap f) a)

instance Arrow AutoOn1 where
    arr f             = AutoOn1 (fmap Just (arr f))
    first (AutoOn1 f) = AutoOn1 $ proc (x, y) -> do
                                      z <- f -< x
                                      id -< fmap (,y) z

newtype AutoOn2 a b = 
