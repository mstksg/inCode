#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver nightly-2017-08-20 --package finite-typelits --package singletons-2.3.1 -- -Wall

-- {-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- import           Data.Proxy
-- import           GHC.TypeNats
-- import           Prelude hiding        ((++), replicate)
import           Data.Finite
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Singletons.TypeLits
import qualified Data.Vector              as V

data Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
    deriving Show

mkVec_ :: Sing n -> V.Vector a -> Maybe (Vec n a)
mkVec_ s v | V.length v == l = Just (UnsafeMkVec v)
           | otherwise       = Nothing
  where
    l = fromIntegral (fromSing s)

mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVec v | V.length v == l = Just (UnsafeMkVec v)
        | otherwise       = Nothing
  where
    l = fromIntegral (fromSing (sing :: Sing n))

replicate_ :: Sing n -> a -> Vec n a
replicate_ s x = UnsafeMkVec $ V.replicate l x
  where
    l = fromIntegral (fromSing s)

replicate :: KnownNat n => a -> Vec n a
replicate = replicate_ sing

withVec :: V.Vector a -> (forall n. Sing n -> Vec n a -> r) -> r
withVec v0 f = case toSing (fromIntegral (V.length v0)) of
    SomeSing s -> f s (UnsafeMkVec v0)

exactLength_ :: Sing m -> Sing n -> Vec n a -> Maybe (Vec m a)
exactLength_ sM sN v = case sM %~ sN of
    Proved Refl -> Just v
    Disproved _  -> Nothing

exactLength :: (KnownNat m, KnownNat n) => Vec n a -> Maybe (Vec m a)
exactLength = exactLength_ sing sing

generate_ :: Sing n -> (Finite n -> a) -> Vec n a
generate_ s f = withKnownNat s $
    UnsafeMkVec $ V.generate l (f . finite . fromIntegral)
  where
    l = fromIntegral (fromSing s)

-- alternatively, via pattern matching:
generate'_ :: Sing n -> (Finite n -> a) -> Vec n a
generate'_ s@SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
  where
    l = fromIntegral (fromSing s)

generate :: KnownNat n => (Finite n -> a) -> Vec n a
generate = generate_ sing

main :: IO ()
main = return ()
