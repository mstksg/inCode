{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | http://blog.jle.im/entry/fixed-length-vector-types-in-haskell-2015
--
-- Depends on Unfoldable.hs from
-- <https://github.com/mstksg/inCode/blob/master/code-samples/fixvec>

module FVTypeNats where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Proxy
import Unfoldable
import qualified GHC.Exts as L (IsList(..))

data Nat = Z | S Nat
         deriving Show

type family (x :: Nat) + (y :: Nat) where
    'Z   + y = y
    'S x + y = 'S (x + y)

(+#) :: Nat -> Nat -> Nat       -- types!
Z   +# y = y
S x +# y = S (x +# y)

data Vec :: Nat -> * -> * where
    Nil  :: Vec Z a
    (:#) :: a -> Vec n a -> Vec (S n) a

infixr 5 :#

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)

instance Unfoldable (Vec Z) where
    unfold _ _ = Nil

instance Unfoldable (Vec n) => Unfoldable (Vec (S n)) where
    unfold f x0 = let (y, x1) = f x0
                  in  y :# unfold f x1

instance Functor (Vec n) where
    fmap _ Nil       = Nil
    fmap f (x :# xs) = f x :# fmap f xs

instance Applicative (Vec Z) where
    pure _    = Nil
    Nil <*> _ = Nil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
    pure x = x :# pure x
    (f :# fs) <*> (x :# xs) = f x :# (fs <*> xs)

instance Foldable (Vec Z) where
    foldMap _ Nil = mempty

instance Foldable (Vec n) => Foldable (Vec (S n)) where
    foldMap f (x :# xs) = f x <> foldMap f xs

instance Traversable (Vec Z) where
    traverse _ Nil = pure Nil

instance Traversable (Vec n) => Traversable (Vec (S n)) where
    traverse f (x :# xs) = liftA2 (:#) (f x) (traverse f xs)

class Index (n :: Nat) v where
    index :: Proxy n -> v a -> a

instance Index Z (Vec (S n)) where
    index _ (x :# _) = x

instance forall n m. Index n (Vec m) => Index (S n) (Vec (S m)) where
    index _ (_ :# xs) = index (Proxy :: Proxy n) xs

instance (Unfoldable (Vec n), Traversable (Vec n)) => L.IsList (Vec n a) where
    type Item (Vec n a) = a
    fromList xs = case fromListU xs of
                    Nothing -> error "Demanded vector from a list that was too short."
                    Just ys -> ys
    toList      = Data.Foldable.toList

headV :: Vec (S n) a -> a
headV (x :# _)  = x

tailV :: Vec (S n) a -> Vec n a
tailV (_ :# xs) = xs

appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV Nil       ys = ys
appendV (x :# xs) ys = x :# appendV xs ys

-- | Trick with singletons
data Fin :: Nat -> * where
    FZ :: Fin k
    FS :: Fin k -> Fin (S k)

index' :: Vec (S n) a -> Fin n -> a
index' (x :# _) FZ = x
index' (_ :# xs) (FS n) = index' xs n

