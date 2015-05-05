{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

-- | http://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015
--
-- Depends on Unfoldable.hs from
-- <https://github.com/mstksg/inCode/blob/master/code-samples/fixvec>

module FVTypeLits where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import Unfoldable
import qualified GHC.Exts as L (IsList(..))

type x > y = CmpNat x y ~ 'GT

data Vec :: Nat -> * -> * where
    Nil  :: Vec 0 a
    (:#) :: a -> Vec (n - 1) a -> Vec n a

infixr 5 :#

deriving instance Show a => Show (Vec n a)

instance Unfoldable (Vec 0) where
    unfold _ _ = Nil

instance Unfoldable (Vec (n - 1), n > 0) => Unfoldable (Vec n) where
    unfold f x0 = let (y, x1) = f x0
                  in  y :# unfold f x1

instance Functor (Vec n) where
    fmap _ Nil       = Nil
    fmap f (x :# xs) = f x :# fmap f xs

instance Applicative (Vec 0) where
    pure _    = Nil
    Nil <*> _ = Nil

instance (Applicative (Vec (n - 1)), n > 0) => Applicative (Vec n) where
    pure x = x :# pure x
    (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)

instance Foldable (Vec 0) where
    foldMap _ Nil = mempty

instance (Foldable (Vec (n - 1)), n > 0) => Foldable (Vec n) where
    foldMap f (x :# xs) = f x <> foldMap f xs

instance Traversable (Vec 0) where
    traverse _ Nil = pure Nil

instance (Traversable (Vec (n - 1)), n > 0) => Traversable (Vec n) where
    traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)

class Index (n :: Nat) v where
    index :: Proxy n -> v a -> a

instance (m > 0) => Index 0 (Vec m) where
    index _ (x :# _) = x

instance forall n m. (Index (n - 1) (Vec (m - 1)), n > 0, m > 0) => Index n (Vec m) where
    index _ (_ :# xs) = index (Proxy :: Proxy (n - 1)) xs

instance (Unfoldable (Vec n), Traversable (Vec n)) => L.IsList (Vec n a) where
    type Item (Vec n a) = a
    fromList xs = case fromListU xs of
                    Nothing -> error "Demanded vector from a list that was too short."
                    Just ys -> ys
    toList      = toList

headV :: (n > 0) => Vec n a -> a
headV (x :# _)  = x

tailV :: (n > 0) => Vec n a -> Vec (n - 1) a
tailV (_ :# xs) = xs

appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV Nil       ys = ys
appendV (x :# xs) ys = x :# appendV xs ys
