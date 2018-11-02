#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-11-02 --package singletons --package decidable --package lens-typelevel

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
{-# LANGUAGE PolyKinds                      #-}
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
import           Data.Singletons.Prelude hiding (Not)
import           Data.Singletons.Sigma
import           Data.Singletons.TH
import           Data.Type.Lens
import           Data.Type.Predicate
import           Data.Type.Predicate.Param

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

$(singletonsOnly [d|
  placeBoard :: N -> N -> Piece -> Board -> Board
  placeBoard i j p = set (ixList i . ixList j) (Just p)
  |])


altP_cyclic :: Sing p -> AltP (AltP p) :~: p
altP_cyclic SPX = Refl @'PX
altP_cyclic SPO = Refl @'PO

data InPlay :: Predicate Board

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
        -> GameState (AltP p)    b2  -- ^ `AltP p`, b2 is a valid satte

data Update :: Piece -> Board -> Board -> Type where
    MkUpdate
        :: forall i j p b. ()
        => Coord '(i, j) b 'Nothing         -- ^ If the item at (i, j) in b is Nothing
        -- ------------------------------------- then
        -> Update p b (PlaceBoard i j p b)  -- ^ Placing `Just p` at i, j is a valid update

data Sel :: N -> [k] -> k -> Type where
    -- | The first item in a list is at index ''Z'
    SelZ :: Sel 'Z (a ': as) a
    SelS :: Sel     n        as  a  -- ^ If item `a` is at index `n` in list `as`
         -- ---------------------------- then
         -> Sel ('S n) (b ': as) a  -- ^ Item `a` is at index `S n` in list `b : as`

data Coord :: (N, N) -> [[k]] -> k -> Type where
    (:$:) :: forall i j rows row p. ()
          => Sel i rows row         -- ^ If the ith list in `rows` is `row`
          -> Sel j row  p           -- ^ And the jth item in `row` is `p`
          -- --------------------------- then
          -> Coord '(i, j) rows p   -- ^ The item at (i, j) is `p`

type InBounds    n = Found (TyPP (Sel n))

type OutOfBounds n = Not (InBounds n)

-- | A view of a coordinate and a board.  Either:
data Pick :: (N, N, Board) -> Type where
    -- | We are out of bounds in x
    PickOoBX   :: OutOfBounds i @@ b                         -> Pick '(i, j, b)
    -- | We are in-bounds in x, but out of bounds in y
    PickOoBY   :: Sel i b row        -> OutOfBounds j @@ row -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, but spot is taken by `p`.
    -- We include `Sing p` in this constructor to potentially provide
    -- feedback to the user on what piece is already in the spot.
    PickPlayed :: Coord '(i, j) b ('Just p) -> Sing p        -> Pick '(i, j, b)
    -- | We are in-bounds in x, in-bounds in y, and spot is clear
    PickValid  :: Coord '(i, j) b 'Nothing                   -> Pick '(i, j, b)

instance SingI n => Decidable (InBounds n) where
    decide = inBounds sing

inBounds :: Sing n -> Sing xs -> Decision (InBounds n @@ xs)
inBounds = \case
    SZ -> \case
      SNil         -> inBounds_znil
      x `SCons` xs -> inBounds_zcons x xs
    SS n -> \case
      SNil         -> inBounds_snil n
      x `SCons` xs -> inBounds_scons n x xs

inBounds_znil
    :: Decision (InBounds 'Z @@ '[])
inBounds_znil = Disproved $ \(_ :&: s) -> case s of {}

inBounds_zcons
    :: Sing x
    -> Sing xs
    -> Decision (InBounds 'Z @@ (x ': xs))
inBounds_zcons x _ = Proved (x :&: SelZ)

inBounds_snil
    :: Sing n
    -> Decision (InBounds ('S n) @@ '[])
inBounds_snil _ = Disproved $ \(_ :&: s) -> case s of {}

inBounds_scons
    :: Sing n
    -> Sing x
    -> Sing xs
    -> Decision (InBounds ('S n) @@ (x ': xs))
inBounds_scons n _ xs = case inBounds n xs of
    Proved (y :&: s) ->       -- if xs has y in its n spot
      Proved (y :&: SelS s)   -- then (x : xs) has y in its (S n) spot
    -- v is a disproof that an item is in n spot in xs
    Disproved v      -> Disproved $
      \(y :&: s) ->      -- suppose we had item y in (S n) spot in (x : xs)
        case s of
          SelS s' ->     -- this would mean that item y is in n spot in xs
            v (y :&: s') -- however, v disproves this.

pick
    :: Sing ijb
    -> Pick ijb
pick (STuple3 (Sing :: Sing i) (Sing :: Sing j) b) = case decide @(InBounds i) b of
    Proved (row :&: selX) -> case decide @(InBounds j) row of
      Proved (p :&: selY) -> case p of
        SNothing -> PickValid   (selX :$: selY)
        SJust p' -> PickPlayed  (selX :$: selY) p'
      Disproved vY -> PickOoBY selX vY
    Disproved vX -> PickOoBX vX

instance Provable (TyPred Pick) where
    prove = pick
