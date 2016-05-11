#!/usr/bin/env stack
-- stack --resolver lts-5.15 --install-ghc runghc --package hmatrix --package MonadRandom

{-# LANGUAGE StandaloneDeriving #-}

import Numeric.LinearAlgebra.Static
import GHC.TypeLits

data Weights i o = W { wBiases :: !(R o)
                     , wNodes  :: !(L o i)
                     }
deriving instance (KnownNat i, KnownNat o) => Show (Weights i o)

data Network = O !Weights
             | !Weights :&~ !Network
  deriving (Show, Eq)
infixr 5 :&~

main :: IO ()
main = print 1
