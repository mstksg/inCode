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
import Data.Kind

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

hiddenSing :: Network i hs o -> Sing hs
hiddenSing = \case O _      -> SNil
                   _ :&~ n' -> SNat `SCons` hiddenSing n'

hiddenStruct :: Network i hs o -> [Integer]
hiddenStruct = \case O _      -> []
                     _ :&~ n' -> netInp n' : hiddenStruct n'
  where
    netInp :: forall i hs o. KnownNat i
           => Network i hs o
           -> Integer
    netInp _ = natVal (Proxy :: Proxy i)

data OpaqueNet :: Nat -> Nat -> * where
    ONet :: Network i hs o -> OpaqueNet i o

numHiddens :: OpaqueNet i o -> Int
numHiddens = \case ONet n -> go n
  where
    go :: Network i hs o -> Int
    go = \case O _      -> 0
               _ :&~ n' -> 1 + go n'

numHiddens' :: OpaqueNet' i o Int -> Int
numHiddens' oN = oN go
  where
    go :: Network i hs o -> Int
    go = \case O _      -> 0
               _ :&~ n' -> 1 + go n'

putONet :: (KnownNat i, KnownNat o)
        => OpaqueNet i o
        -> Put
putONet = \case ONet net -> do
                  put (hiddenStruct net)
                  putNet net

getONet :: (KnownNat i, KnownNat o)
        => Get (OpaqueNet i o)
getONet = do
    hs <- get
    case toSing hs of
      SomeSing ss -> do
        n <- getNet ss
        return (ONet n)

randomONet :: (MonadRandom m, KnownNat i, KnownNat o)
           => [Integer]
           -> m (OpaqueNet i o)
randomONet hs = case toSing hs of
                  SomeSing ss -> ONet <$> randomNet' ss

instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o) where
    put = putONet
    get = getONet

type OpaqueNet' i o r = (forall hs. Network i hs o -> r) -> r

oNet' :: Network i hs o -> OpaqueNet' i o r
oNet' n = \f -> f n

-- -- withONet :: OpaqueNet i o -> (forall hs. Network i hs o -> r) -> r
-- withONet :: OpaqueNet i o -> OpaqueNet' i o r
-- withONet = \case ONet n -> (\f -> f n)

-- toONet :: OpaqueNet' i o (OpaqueNet i o) -> OpaqueNet i o
-- toONet oN' = oN' (\n -> ONet n)

putONet' :: (KnownNat i, KnownNat o)
         => OpaqueNet' i o Put
         -> Put
putONet' oN = oN $ \net -> do
                      put (hiddenStruct net)
                      putNet net

getONet' :: (KnownNat i, KnownNat o)
         => (forall hs. Network i hs o -> Get r)
         -> Get r
--  aka, => OpaqueNet' i o (Get r)
getONet' f = do
    hs <- get
    withSomeSing (hs :: [Integer]) $ \ss -> do
      n <- getNet ss
      f n

withRandomONet' :: (MonadRandom m, KnownNat i, KnownNat o)
                => [Integer]
                -> (forall hs. Network i hs o -> m r)
                -> m r
--         aka, => [Integer]
--              -> OpaqueNet' i o (m r)
withRandomONet' hs f = withSomeSing hs $ \ss -> do
                         net <- randomNet' ss
                         f net

main :: IO ()
main = do
    putStrLn "What hidden layer structure do you want?"
    hs <- readLn
    ONet (net :: Network 10 hs 3) <- randomONet hs
    print net
    -- blah blah stuff with our dynamically generated net

main' :: IO ()
main' = do
    putStrLn "What hidden layer structure do you want?"
    hs <- readLn
    withRandomONet' hs $ \(net :: Network 10 hs 3) -> do
      print net
      -- blah blah stuff with our dynamically generated net

