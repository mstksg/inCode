#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-10-31 --package singletons --package decidable

{-# LANGUAGE AllowAmbiguousTypes            #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE NoStarIsType                   #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE TypeSynonymInstances           #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.Sigma
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Type.Predicate
-- import           Data.Type.Lens

$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord)
  
  type Board = [[Maybe Piece]]

  emptyBoard :: Board
  emptyBoard = [[Nothing, Nothing, Nothing]
               ,[Nothing, Nothing, Nothing]
               ,[Nothing, Nothing, Nothing]
               ]

  altP :: Piece -> Piece
  altP PX = PO
  altP PO = PX
  |])

-- $(singletonsOnly [d|
--   placeBoard :: N -> N -> Piece -> Board -> Board
--   placeBoard i j p = set (ixList i . ixList j) (Just p)
--   |])


altP_cyclic :: Sing p -> AltP (AltP p) :~: p
altP_cyclic SPX = Refl @'PX
altP_cyclic SPO = Refl @'PO

data InPlay :: Predicate Board
data Update :: Piece -> Board -> Board -> Type

data GameState :: Piece -> Board -> Type where
    -- | The empty board is a valid state
    GSStart
        :: GameState 'PX EmptyBoard
    -- | We can also construct a valid game state if we have:
    GSUpdate
        :: forall p b1 b2. ()
        => InPlay          @@ b1     -- ^ a proof that b1 is in play
        -> Update    p        b1 b2  -- ^ a valid update
        -> GameState p        b1     -- ^ a proof that p, b1 are a valid state
        -- ---------------------------- then
        -> GameState (AltP p)    b2  -- ^ AltP p, b2 is a valid satte
