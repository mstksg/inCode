{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AutoX where


import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Data.Functor.Identity
import Control.Monad.Fix
import Prelude hiding      ((.), id)

newtype AutoX m a b = AConsX { runAutoX :: Maybe a -> m (Maybe b, AutoX m a b) }

instance Monad m => Category (AutoX m) where
    id    = AConsX $ \x -> return (x, id)
    g . f = AConsX $ \x -> do
              (y, f') <- runAutoX f x
              (z, g') <- runAutoX g y
              return (z, g' . f')

instance Monad m => Functor (AutoX m r) where
    fmap f a = AConsX $ \x -> do
                 (y, a') <- runAutoX a x
                 return (fmap f y, fmap f a)
               

instance Monad m => Applicative (AutoX m r) where
    pure x    = AConsX $ \_ -> return (Just x, pure x)
    af <*> ay = AConsX $ \x -> do
                  (f, af') <- runAutoX af x
                  (y, ay') <- runAutoX ay x
                  return (f <*> y, af' <*> ay')

aCons :: Monad m => (a -> (b, AutoX m a b)) -> AutoX m a b
aCons a = AConsX $ \x -> case x of
                           Just _x -> let (y, a') = a _x
                                      in  return (Just y, a')
                           Nothing -> return (Nothing, aCons a)

aConsM :: Monad m => (a -> m (b, AutoX m a b)) -> AutoX m a b
aConsM a = AConsX $ \x -> case x of
                            Just _x -> do
                              (y, a') <- a _x
                              return (Just y, a')
                            Nothing ->
                              return (Nothing, aConsM a)

aConsOn :: Monad m => (a -> (Maybe b, AutoX m a b)) -> AutoX m a b
aConsOn a = AConsX $ \x -> case x of
                             Just _x -> let (y, a') = a _x
                                        in  return (y, a')
                             Nothing -> return (Nothing, aConsOn a)

aConsOnM :: Monad m => (a -> m (Maybe b, AutoX m a b)) -> AutoX m a b
aConsOnM a = AConsX $ \x -> case x of
                              Just _x -> do
                                (y, a') <- a _x
                                return (y, a')
                              Nothing ->
                                return (Nothing, aConsOnM a)

autoXI :: Monad m => (Maybe a -> (Maybe b, AutoX m a b)) -> AutoX m a b
autoXI a = AConsX $ return . a

autoFold :: Monad m => (b -> a -> b) -> b -> AutoX m a b
autoFold op = aCons . foldFrom
  where
    foldFrom x y = (z, aCons (foldFrom z))
      where
        z = x `op` y

-- type Auto a b = AutoX Identity Identity a b
-- type AutoM m a b = AutoX m Identity a b
-- type AutoOn a b = AutoX Identity Maybe a b
-- type AutoOnM m a b = AutoX m Maybe a b

-- class MonadA m f where
--     bindA :: AutoX m f a b -> AutoX m f (f a) b

-- instance Monad m => MonadA m Maybe where
--     bindA a = AConsX $ \x ->
--                 case x of
--                   Just _x -> do
--                     (y, a') <- runAutoX a _x
--                     return (y, bindA a')
--                   Nothing ->
--                     return (Nothing, bindA a)

-- instance (Monad m, MonadA m f, Monad f) => Category (AutoX m f) where
--     id = AConsX $ \x -> return (return x, id)
--     g . f = AConsX $ \x -> do
--               (y, f') <- runAutoX f x
--               (z, g') <- runAutoX (bindA g) y
--               return (undefined, g' . f')
