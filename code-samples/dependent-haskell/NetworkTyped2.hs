{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           GHC.Generics                 (Generic)
import           Numeric.LinearAlgebra.Static
import Data.Binary as B

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }
  deriving (Generic)

data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:&~) :: KnownNat h
          => !(Weights i h)
          -> !(Network h hs o)
          -> Network i (h ': hs) o
infixr 5 :&~

instance (KnownNat i, KnownNat o) => Binary (Weights i o)

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

sillyGetNet :: (KnownNat i, KnownNat o)
            => Sing hs
            -> Get (Network i hs o)
sillyGetNet ss = withSingI ss get

data OpaqueNet :: Nat -> Nat -> * where
    ONet :: Sing hs -> Network i hs o -> OpaqueNet i o

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
    withSomeSing (hs :: [Integer]) $ \ss -> do
      n <- getNet ss
      return (ONet ss n)

instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o) where
    put = putONet
    get = getONet

main :: IO ()
main = return ()
