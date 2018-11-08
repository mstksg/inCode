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

import           Control.Monad
import           Data.Kind
import           Data.List
import           Data.Singletons
import           Data.Singletons.Prelude hiding (Not)
import           Data.Singletons.Sigma
import           Data.Singletons.TH
import           Data.Type.Lens
import           Data.Type.Predicate
import           Data.Type.Predicate.Param
import           Text.Read

$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord, Show)
  
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

play
    :: forall i j p b. ()
    => InPlay @@ b
    -> Coord '(i, j) b 'Nothing
    -> GameState p b
    -> GameState (AltP p) (PlaceBoard i j p b)
play r c = GSUpdate r (MkUpdate c)

data SelFound :: N -> Predicate [k]
type instance Apply (SelFound n) (xs :: [k]) = Σ k (TyPred (Sel n xs))

type OutOfBounds n = Not (SelFound n)

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

selFound
    :: Sing n
    -> Sing xs
    -> Decision (SelFound n @@ xs)
selFound = \case
    SZ -> \case
      SNil         -> selFound_znil
      x `SCons` xs -> selFound_zcons x xs
    SS n -> \case
      SNil         -> selFound_snil n
      x `SCons` xs -> selFound_scons n x xs

noEmptySel :: Sel n '[] a -> Void
noEmptySel = \case {}
            -- ^ we handle all 0 of the valid patterns for Sel n '[] a

selFound_znil
    :: Decision (SelFound 'Z @@ '[])
selFound_znil = Disproved $ \(_ :&: s) -> noEmptySel s

selFound_zcons
    :: Sing x
    -> Sing xs
    -> Decision (SelFound 'Z @@ (x ': xs))
selFound_zcons x _ = Proved (x :&: SelZ)

selFound_snil
    :: Sing n
    -> Decision (SelFound ('S n) @@ '[])
selFound_snil _ = Disproved $ \(_ :&: s) -> noEmptySel s

selFound_scons
    :: Sing n
    -> Sing x
    -> Sing xs
    -> Decision (SelFound ('S n) @@ (x ': xs))
selFound_scons n _ xs = case selFound n xs of
    Proved (y :&: s) ->       -- if xs has y in its n spot
      Proved (y :&: SelS s)   -- then (x : xs) has y in its (S n) spot
    Disproved v      -> Disproved $ -- v is a disproof that an item is in n spot in xs
      \(y :&: s) ->      -- suppose we had item y in (S n) spot in (x : xs)
        case s of
          SelS s' ->     -- this would mean that item y is in n spot in xs
            v (y :&: s') -- however, v disproves this.

selFoundTest1 :: SelFound 'Z @@ '[ 'True, 'False ]
selFoundTest1 = STrue :&: SelZ
                       -- ^ Sel 'Z '[ 'True, 'False ] 'True

selFoundTest2 :: SelFound ('S 'Z) @@ '[ 'True, 'False ]
selFoundTest2 = SFalse :&: SelS SelZ
                        -- ^ Sel ('S 'Z) '[ 'True, 'False ] 'False

pick
    :: forall i j b. ()
    => Sing i
    -> Sing j
    -> Sing b
    -> Pick '(i, j, b)
pick i j b = case selFound i b of
    Proved (row :&: selX) -> case selFound j row of
      Proved (p :&: selY) ->
        let c = selX :$: selY
        in  case p of
              SNothing -> PickValid   c     -- p is 'Nothing
              SJust q  -> PickPlayed  c q   -- p is 'Just q
      Disproved vY -> PickOoBY selX vY    -- vY :: SelFound j @@ row -> Void
                                          -- vY :: Not (SelFound j) @@ row
                                          -- vY :: OutOfBounds j @@ row
    Disproved vX -> PickOoBX vX   -- vX :: SelFound i @@ b   -> Void
                                  -- vX :: Not (SelFound i) @@ b
                                  -- vX :: OutOfBounds i @@ b

intToN :: Int -> Maybe N
intToN n = case compare n 0 of
    LT -> Nothing
    EQ -> Just Z
    GT -> S <$> intToN (n - 1)

getN :: String -> IO N
getN prompt = do
    putStrLn $ "Enter non-negative integer " ++ prompt ++ ":"
    res <- getLine
    case intToN =<< readMaybe res of
      Nothing -> putStrLn "Bad." >> getN prompt
      Just n  -> pure n

printBoard :: Board -> IO ()
printBoard = mapM_ $ putStrLn . intercalate "|" . map showPiece
  where
    showPiece Nothing   = "   "
    showPiece (Just PX) = " X "
    showPiece (Just PO) = " O "

simplePlayIO :: IO ()
simplePlayIO = simplePlayIO' SPX sEmptyBoard GSStart

simplePlayIO'
    :: Sing p
    -> Sing b
    -> GameState p b
    -> IO ()
simplePlayIO' p b gs = do
    printBoard $ FromSing b
    FromSing i <- getN "for row"
    FromSing j <- getN "for column"
    case pick i j b of
      PickOoBX _ -> do
        putStrLn "Out of bounds in rows.  Try again."
        simplePlayIO' p b gs
      PickOoBY _ _ -> do
        putStrLn "Out of bounds in cols.  Try again."
        simplePlayIO' p b gs
      PickPlayed _ q -> do
        putStrLn $ "Already played by " ++ show (fromSing q) ++ ". Try again."
        simplePlayIO' p b gs
      PickValid c -> do
        putStrLn "Success!"
        let p'  = sAltP p                 -- update player (enforced by `play`)
            b'  = sPlaceBoard i j p b     -- update board  (enforced by `play`)
            gs' = play undefined c gs     -- update game state
        simplePlayIO' p' b' gs'
