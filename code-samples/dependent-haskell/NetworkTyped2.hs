{-# LANGUAGE    DataKinds           #-}
{-# LANGUAGE    DeriveGeneric       #-}
{-# LANGUAGE    GADTs               #-}
{-# LANGUAGE    KindSignatures      #-}
{-# LANGUAGE    LambdaCase          #-}
{-# LANGUAGE    ScopedTypeVariables #-}
{-# LANGUAGE    TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           GHC.Generics                 (Generic)
import           Numeric.LinearAlgebra.Static
import qualified Data.Binary                  as B

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }
  deriving (Generic)

data Network :: Nat -> [Nat] -> Nat -> * where
    O     :: !(Weights i o)
          -> Network i '[] o
    (:&~) :: (KnownNat h, SingI hs)
          => !(Weights i h)
          -> !(Network h hs o)
          -> Network i (h ': hs) o
infixr 5 :&~

instance (KnownNat i, KnownNat o) => B.Binary (Weights i o)

instance (KnownNat i, SingI hs, KnownNat o) => B.Binary (Network i hs o) where
    put = \case O w     -> B.put w
                w :&~ n -> B.put w *> B.put n
    get = go sing
      where
        go :: forall h hs'. KnownNat h
           => Sing hs'
           -> B.Get (Network h hs' o)
        go hs = case hs of
                  SNil            ->     O <$> B.get
                  SNat `SCons` ss -> withSingI ss
                                   $ (:&~) <$> B.get <*> go ss

data OpaqueNet :: Nat -> Nat -> * where
    ONet :: Sing hs -> Network i hs o -> OpaqueNet i o

instance (KnownNat i, KnownNat o) => B.Binary (OpaqueNet i o) where
    put = \case ONet ss net ->
                  withSingI ss $ do
                    B.put (fromSing ss)
                    B.put net
    get = do
      hs <- B.get
      withSomeSing (hs :: [Integer]) $ \ss ->
        withSingI ss $ do
          n <- B.get
          return (ONet ss n)

main :: IO ()
main = return ()
