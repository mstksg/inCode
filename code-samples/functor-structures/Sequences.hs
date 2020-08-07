{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Sequences (
    -- Ap(..)
  -- , liftAp
  -- , runAp
    -- CoAp(..)
  -- , liftCoAp
  -- , runCoAp
    Div(..)
  , liftDiv, runDiv
  , Dec(..)
  , liftDec, runDec
  , Conclude(..)
  , InvDay(..)
  , runInvDayApply
  , runInvDayDivise
  , InvNight(..)
  , runInvNightAlt
  , runInvNightDecide
  , Not(..)
  , chainPair
  ) where

import           Control.Applicative
import           Control.Natural
import           Data.Bifunctor
import           Data.Bifunctor.Assoc
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Divisible
import           Data.Functor.Identity
import           Data.Functor.Invariant
import           Data.Functor.Plus
import           Data.HBifunctor
import           Data.HBifunctor.Tensor
import           Data.HFunctor
import           Data.HFunctor.Chain
import           Data.Kind
import           Data.Void

class Contravariant f => Divise f where
    divise :: (a -> (b, c)) -> f b -> f c -> f a

instance Semigroup r => Divise (Op r) where
    divise f (Op g) (Op h) = Op $ \x -> case f x of
      (y, z) -> g y <> h z

data Div :: (Type -> Type) -> Type -> Type where
    Conquer :: Div f a
    Divide  :: (a -> (b, c)) -> f b -> Div f c -> Div f a

liftDiv :: f a -> Div f a
liftDiv x = Divide (,()) x Conquer

runDiv
    :: forall f g a. Divisible g
    => (forall x. f x -> g x)
    -> Div f a
    -> g a
runDiv f = go
  where
    go :: Div f x -> g x
    go = \case
      Conquer       -> conquer
      Divide g x xs -> divide g (f x) (go xs)

instance Contravariant (Div f) where
    contramap f = \case
      Conquer       -> Conquer
      Divide g x xs -> Divide (g . f) x xs

instance Divisible (Div f) where
    conquer  = Conquer
    divide f = \case
      Conquer       -> contramap (snd . f)
      Divide g x xs -> Divide (assoc . first g . f) x
                     . divide id xs

class Contravariant f => Decide f where
    decide :: (a -> Either b c) -> f b -> f c -> f a

class Decide f => Conclude f where
    conclude :: (a -> Void) -> f a

instance Decide (Op r) where
    decide f (Op g) (Op h) = Op $ \r -> case f r of
      Left  x -> g x
      Right y -> h y
instance Conclude (Op r) where
    conclude g = Op (absurd . g)

data Dec :: (Type -> Type) -> Type -> Type where
    Lose :: (a -> Void) -> Dec f a
    Choose   :: (a -> Either b c) -> f b -> Dec f c -> Dec f a

liftDec :: f a -> Dec f a
liftDec x = Choose Left x (Lose id)

decided :: Conclude f => f a -> f b -> f (Either a b)
decided = decide id


runDec
    :: forall f g a. Conclude g
    => (forall x. f x -> g x)
    -> Dec f a
    -> g a
runDec f = go
  where
    go :: Dec f x -> g x
    go = \case
      Lose g    -> conclude g
      Choose g x xs -> decide g (f x) (go xs)

instance Contravariant (Dec f) where
    contramap f = \case
      Lose g    -> Lose (g . f)
      Choose g x xs -> Choose (g . f) x xs

instance Decide (Dec f) where
    decide f = \case
      Lose   g      -> contramap (either (absurd . g) id . f)
      Choose g x xs -> Choose (assoc . first g . f) x
                     . decide id xs
instance Conclude (Dec f) where
    conclude = Lose

data InvDay :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) where
    InvDay
        :: f b
        -> g c
        -> (a -> (b, c))
        -> (b -> c -> a)
        -> InvDay f g a

instance HBifunctor InvDay where
    hbimap f g (InvDay x y h j) = InvDay (f x) (g y) h j

instance Invariant (InvDay f g) where
    invmap f g (InvDay x y h j) = InvDay x y (h . g) (\k -> f . j k)

runInvDayApply
    :: Apply h
    => (f ~> h)
    -> (g ~> h)
    -> InvDay f g ~> h
runInvDayApply f g (InvDay x y _ j) = j <$> f x <.> g y

runInvDayDivise
    :: Divise h
    => (f ~> h)
    -> (g ~> h)
    -> InvDay f g ~> h
runInvDayDivise f g (InvDay x y h _) = divise h (f x) (g y)


data InvNight :: (Type -> Type) -> (Type -> Type) -> (Type -> Type) where
    InvNight
        :: f b
        -> g c
        -> (a -> Either b c)
        -> (Either b c -> a)
        -> InvNight f g a

instance HBifunctor InvNight where
    hbimap f g (InvNight x y h j) = InvNight (f x) (g y) h j

runInvNightAlt
    :: Alt h
    => (f ~> h)
    -> (g ~> h)
    -> InvNight f g ~> h
runInvNightAlt f g (InvNight x y _ j) = fmap (j . Left) (f x) <!> fmap (j . Right) (g y)

runInvNightDecide
    :: Decide h
    => (f ~> h)
    -> (g ~> h)
    -> InvNight f g ~> h
runInvNightDecide f g (InvNight x y h _) = decide h (f x) (g y)


instance Invariant (InvNight f g) where
    invmap f g (InvNight x y h j) = InvNight x y (h . g) (f . j)

newtype Not a = Not { refute :: a -> Void }

instance Invariant Not where
    invmap _ g (Not x) = Not (x . g)

-- instance Invariant (Chain t i f) where
--     invmap = undefined

instance Invariant (Chain InvNight Not f) where
    invmap f g = \case
      Done x  -> Done (invmap f g x )
      More xs -> More (invmap f g xs)

instance Invariant (Chain InvDay Identity f) where
    invmap f g = \case
      Done x  -> Done (invmap f g x )
      More xs -> More (invmap f g xs)

chainPair :: Tensor t i => t f f ~> Chain t i f
chainPair = More . hright inject

-- instance (Invariant i, Invariant (t f (Chain t i f))) => Invariant (Chain t i f) where
--     invmap f g = \case
--       Done x  -> Done (invmap f g x)
--       More xs -> More (invmap f g xs)

-- data Ap :: (Type -> Type) -> (Type -> Type) where
--     Pure   :: a -> Ap f a
--     ConsAp :: ((a, b) -> c)
--            -> f a
--            -> Ap f b
--            -> Ap f c

-- instance Functor (Ap f) where
--     fmap f = \case
--       Pure   x      -> Pure (f x)
--       ConsAp g x xs -> ConsAp (f . g) x xs

-- instance Applicative (Ap f) where
--     pure     = Pure
--     liftA2 f = \case
--       Pure x        -> fmap (f x)
--       ConsAp g x xs -> ConsAp (\(i, (j, k)) -> f (g (i, j)) k) x
--                      . liftA2 (,) xs

-- liftAp :: f a -> Ap f a
-- liftAp x = ConsAp fst x (Pure ())

-- runAp
--     :: forall f g a. Applicative g
--     => (forall x. f x -> g x)
--     -> Ap f a
--     -> g a
-- runAp f = go
--   where
--     go :: Ap f b -> g b
--     go = \case
--       Pure x        -> pure x
--       ConsAp g x xs -> curry g <$> f x <*> go xs

-- data CoAp :: (Type -> Type) -> (Type -> Type) where
--     CoPure   :: CoAp f a
--     ConsCoAp :: (Either a b -> c)
--              -> f a
--              -> CoAp f b
--              -> CoAp f c

-- instance Functor (CoAp f) where
--     fmap f = \case
--       CoPure          -> CoPure
--       ConsCoAp g x xs -> ConsCoAp (f . g) x xs

-- instance Alt (CoAp f) where
--     (<!>) = appendCoAp (either id id)
-- instance Plus (CoAp f) where
--     zero = CoPure

-- appendCoAp
--     :: (Either a b -> c)
--     -> CoAp f a
--     -> CoAp f b
--     -> CoAp f c
-- appendCoAp f = \case
--     CoPure -> fmap (f . Right)
--     ConsCoAp g x xs -> ConsCoAp (f . first g . reEither) x
--                      . appendCoAp id xs
--   where
--     reEither = \case
--       Left  x         -> Left (Left x)
--       Right (Left  y) -> Left  (Right y)
--       Right (Right z) -> Right z

-- liftCoAp :: f a -> CoAp f a
-- liftCoAp x = ConsCoAp (either id absurd) x CoPure

-- runCoAp
--     :: forall f g a. Plus g
--     => (forall x. f x -> g x)
--     -> CoAp f a
--     -> g a
-- runCoAp f = go
--   where
--     go :: CoAp f b -> g b
--     go = \case
--       CoPure          -> zero
--       ConsCoAp g x xs -> (g . Left <$> f x) <!> (g . Right <$> go xs)

-- data ContraAp :: (Type -> Type) -> (Type -> Type) where
--     ContraPure   :: ContraAp f a
--     ConsContraAp :: (c -> (a, b))
--                  -> f a
--                  -> ContraAp f b
--                  -> ContraAp f c

-- instance Contravariant (ContraAp f) where
--     contramap f = \case
--       ContraPure          -> ContraPure
--       ConsContraAp g x xs -> ConsContraAp (g . f) x xs

-- instance Divisible (ContraAp f) where
--     divide f = \case
--       ContraPure -> contramap (snd . f)
--       ConsContraAp g x xs -> \ys ->
--         ConsContraAp ((\((i,j),k) -> (i, (j,k))) . first g . f) x $
--           divide id xs ys
--     conquer = ContraPure

-- instance HFunctor ContraAp where
--     hmap f = \case
--       ContraPure -> ContraPure
--       ConsContraAp g x xs -> ConsContraAp g (f x) (hmap f xs)

-- liftContraAp :: f a -> ContraAp f a
-- liftContraAp x = ConsContraAp (,()) x ContraPure

-- data ContraCoAp :: (Type -> Type) -> (Type -> Type) where
--     ContraCoPure   :: (a -> Void) -> ContraCoAp f a
--     ConsContraCoAp :: (c -> Either a b)
--                    -> f a
--                    -> ContraCoAp f b
--                    -> ContraCoAp f c

-- instance Contravariant (ContraCoAp f) where
--     contramap f = \case
--       ContraCoPure g -> ContraCoPure (g . f)
--       ConsContraCoAp g x xs -> ConsContraCoAp (g . f) x xs

-- -- instance Decidable (ContraCoAp f) where
-- --     lose = ContraCoPure

-- appendContraCoAp
--     :: (c -> Either a b)
--     -> ContraCoAp f a
--     -> ContraCoAp f b
--     -> ContraCoAp f c
-- appendContraCoAp f = \case
--     ContraCoPure g -> contramap (either (absurd . g) id . f)
--     ConsContraCoAp g x xs ->
--       ConsContraCoAp (reEither . first g . f) x . appendContraCoAp id xs
--   where
--     reEither = \case
--       Left (Left  x) -> Left x
--       Left (Right y) -> Right (Left  y)
--       Right z        -> Right (Right z)

-- liftContraCoAp :: f a -> ContraCoAp f a
-- liftContraCoAp x = ConsContraCoAp Left x (ContraCoPure id)

-- -- data InvAp :: (Type -> Type) -> (Type -> Type) where
-- --     InvPure   :: a -> InvAp f a
-- --     ConsInvAp :: ((a, b) -> c)
-- --               -> (c -> (a, b))
-- --               -> f a
-- --               -> InvAp f b
-- --               -> InvAp f c

-- -- instance Invariant (InvAp f) where
-- --     invmap f g = \case
-- --       InvPure x -> InvPure (f x)
-- --       ConsInvAp j k x xs -> ConsInvAp (f . j) (k . g) x xs

-- -- appendInvAp
-- --     :: ((a, b) -> c)
-- --     -> (c -> (a, b))
-- --     -> InvAp f a
-- --     -> InvAp f b
-- --     -> InvAp f c
-- -- appendInvAp f g = \case
-- --     InvPure x -> invmap (f . (x,)) (snd . g)
