{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

import Control.Monad.Random
import Data.Binary                  as B
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import GHC.Generics                 (Generic)
import Numeric.LinearAlgebra.Static

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }
  deriving (Show, Generic)

data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:&~) :: KnownNat h
          => !(Weights i h)
          -> !(Network h hs o)
          -> Network i (h ': hs) o
infixr 5 :&~

deriving instance (KnownNat i, KnownNat o) => Show (Network i hs o)

instance (KnownNat i, KnownNat o) => Binary (Weights i o)

randomWeights :: (MonadRandom m, KnownNat i, KnownNat o)
              => m (Weights i o)
randomWeights = do
    s1 :: Int <- getRandom
    s2 :: Int <- getRandom
    let wB = randomVector  s1 Uniform * 2 - 1
        wN = uniformSample s2 (-1) 1
    return $ W wB wN

randomNet' :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNat o)
           => Sing hs -> m (Network i hs o)
randomNet' = \case SNil            ->     O <$> randomWeights
                   SNat `SCons` ss -> (:&~) <$> randomWeights <*> randomNet' ss

randomNet :: forall m i hs o. (MonadRandom m, KnownNat i, SingI hs, KnownNat o)
          => m (Network i hs o)
randomNet = randomNet' sing

putNet :: (KnownNat i, KnownNat o)
       => Network i hs o
       -> Put
putNet = \case O w     -> put w
               w :&~ n -> put w *> putNet n

getNet :: forall i hs o. (KnownNat i, KnownNat o)
       => Sing hs
       -> Get (Network i hs o)
getNet = \case SNil            ->     O <$> get
               SNat `SCons` ss -> (:&~) <$> get <*> getNet ss

instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where
    put = putNet
    get = getNet sing

data OpaqueNet :: Nat -> Nat -> * where
    ONet :: Sing hs -> Network i hs o -> OpaqueNet i o

numHiddens :: OpaqueNet i o -> Int
numHiddens = \case ONet ss _ -> lengthSing ss
  where
    lengthSing :: Sing (hs :: [Nat]) -> Int
    lengthSing = \case SNil         -> 0
                       _ `SCons` ss -> 1 + lengthSing ss

putONet :: (KnownNat i, KnownNat o)
        => OpaqueNet i o
        -> Put
putONet = \case ONet ss net -> do
                  put (fromSing ss)
                  putNet net

getONet :: (KnownNat i, KnownNat o)
        => Get (OpaqueNet i o)
getONet = do
    hs <- get
    case toSing hs of
      SomeSing ss -> do
        n <- getNet ss
        return (ONet ss n)

randomONet :: (MonadRandom m, KnownNat i, KnownNat o)
           => [Integer]
           -> m (OpaqueNet i o)
randomONet hs = case toSing hs of
                  SomeSing ss -> case singInstance ss of
                    SingInstance ->
                      ONet ss <$> randomNet

instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o) where
    put = putONet
    get = getONet

type OpaqueNet' i o r = (forall hs. Sing hs -> Network i hs o -> r) -> r

oNet' :: Sing hs -> Network i hs o -> OpaqueNet' i o r
oNet' s n = \f -> f s n

-- withONet :: OpaqueNet i o -> (forall hs. Sing hs -> Network i hs o -> r) -> r
withONet :: OpaqueNet i o -> OpaqueNet' i o r
withONet = \case ONet s n -> (\f -> f s n)

toONet :: OpaqueNet' i o (OpaqueNet i o) -> OpaqueNet i o
toONet oN' = oN' (\s n -> ONet s n)

putONet' :: (KnownNat i, KnownNat o)
         => OpaqueNet' i o Put
         -> Put
putONet' oN = oN $ \ss net -> do
                      put (fromSing ss)
                      putNet net

getONet' :: (KnownNat i, KnownNat o)
         => (forall hs. Sing hs -> Network i hs o -> Get r)
         -> Get r
--  aka, => OpaqueNet' i o (Get r)
getONet' f = do
    hs <- get
    withSomeSing (hs :: [Integer]) $ \ss -> do
      n <- getNet ss
      f ss n

withRandomONet' :: (MonadRandom m, KnownNat i, KnownNat o)
                => [Integer]
                -> (forall hs. Sing hs -> Network i hs o -> m r)
                -> m r
--         aka, => OpaqueNet' i o (m r)
withRandomONet' hs f = withSomeSing hs $ \ss ->
                       withSingI ss    $ do
                         net <- randomNet
                         f ss net

main :: IO ()
main = do
    putStrLn "What size random net?"
    xs <- readLn
    withSomeSing xs $ \(ss :: Sing (hs :: [Nat])) -> do
      net <- randomNet' ss :: IO (Network 10 hs 3)
      print net
      -- blah blah stuff with our dynamically generated net

-- main :: IO ()
-- main = do
--     putStrLn "What size random net?"
--     hs <- readLn
--     ONet ss (net :: Network 10 hs 3) <- randomONet hs
--     print net
--     -- blah blah stuff with our dynamically generated net

-- main' :: IO ()
-- main' = do
--     putStrLn "What size random net?"
--     hs <- readLn
--     withRandomONet' hs $ \ss (net :: Network 10 hs 3) -> do
--       print net
--       -- blah blah stuff with our dynamically generated net
