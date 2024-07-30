#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE GADTs #-}

module Level0 (Sigma (..), castSigma, Showable (..)) where

import Data.Dynamic
import Data.Kind
import Data.Type.Equality
import Type.Reflection

data Any :: Type where
  MkAny :: a -> Any

anyInt :: Any
anyInt = MkAny (8 :: Int)

anyBool :: Any
anyBool = MkAny True

anyList :: Any
anyList = MkAny ([1, 2, 3] :: [Int])

data Sigma :: (Type -> Type) -> Type where
  MkSigma :: p a -> a -> Sigma p

showIfBool :: Sigma TypeRep -> String
showIfBool (MkSigma tr x) = case testEquality tr (typeRep @Bool) of
  Just Refl -> case x of -- in this branch, we know x is a Bool
    False -> "False"
    True -> "True"
  Nothing -> "Not a Bool"

dynBool :: Sigma TypeRep
dynBool = MkSigma typeRep True

dynInt :: Sigma TypeRep
dynInt = MkSigma typeRep (1 :: Int)

showIfBool' :: Dynamic -> String
showIfBool' dyn = case fromDynamic dyn of
  Just x -> case x of -- in this branch, we know x is a Bool
    False -> "False"
    True -> "True"
  Nothing -> "Not a Bool"

castSigma :: forall a. Typeable a => Sigma TypeRep -> Maybe a
castSigma (MkSigma tr x) = case testEquality tr (typeRep @a) of
  Just Refl -> Just x
  Nothing -> Nothing

data Showable :: Type -> Type where
  WitShowable :: Show a => Showable a

showSigma :: Sigma Showable -> String
showSigma (MkSigma WitShowable x) = show x -- here, we know x is Show

showableBool :: Sigma Showable
showableBool = MkSigma WitShowable True

showableInt :: Sigma Showable
showableInt = MkSigma WitShowable (3 :: Int)

data Proxy a = Proxy

uselessBool :: Sigma Proxy
uselessBool = MkSigma Proxy True

data IsBool :: Type -> Type where
  ItsABool :: IsBool Bool

justABool :: Sigma IsBool
justABool = MkSigma ItsABool False

justAnInt :: Sigma ((:~:) Int)
justAnInt = MkSigma Refl 10

main :: IO ()
main = pure ()
