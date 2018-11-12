#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-11-02 --package singletons --package decidable --package lens-typelevel

{-# LANGUAGE BlockArguments                 #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE QuantifiedConstraints          #-}
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
import           Data.List
import           Data.Singletons
import           Data.Singletons.Prelude hiding      (Not, All)
import           Data.Singletons.Prelude.List hiding (All, Any, Null)
import           Data.Singletons.Sigma
import           Data.Singletons.TH
import           Data.Type.Lens
import           Data.Type.Predicate
import           Data.Type.Predicate.Auto
import           Data.Type.Predicate.Param
import           Data.Type.Universe
import           Text.Read

$(singletons [d|
  data Piece = PX | PO
    deriving (Eq, Ord, Show)

  data Result = ResCats
              | ResWin Piece
    deriving (Show, Eq)
  
  type Board = [[Maybe Piece]]

  diagonal :: [[a]] -> [a]
  diagonal []          = []
  diagonal ((x:_):xss) = x : diagonal (map (drop 1) xss)

  lines :: [[a]] -> [[a]]
  lines xs = concat [ xs
                    , transpose xs
                    , [diagonal xs, diagonal (reverse xs)]
                    ]

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

data VicWit :: [Maybe Piece] -> Piece -> Type where
    VicWit :: All [] (EqualTo ('Just p)) @@ as
           -> VicWit ('Just p ': as) p

type Victory = TyPP VicWit

type Winner = LinesSym0 `PPMap` AnyMatch [] Victory

type Cats = All [] (All [] IsJust)

data GameOverWit :: Board -> Result -> Type where
    GOVictory :: Winner b @@ p
              -> GameOverWit b ('ResWin p)
    GOCats    :: Not (Found Winner) @@ b
              -> Cats @@ b
              -> GameOverWit b 'ResCats

type GameOver = TyPP GameOverWit

-- | A predicate that a game is still in play
type InPlay = Not (Found GameOver)

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

type SelFound    n = (Found (TyPP (Sel n)) :: Predicate [k])
type OutOfBounds n = (Not (SelFound n)     :: Predicate [k])

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

noEmptySel :: Sel n '[] a -> Void
noEmptySel = \case {}
            -- ^ we handle all 0 of the valid patterns for Sel n '[] a

instance SingI n => Decidable (SelFound n) where
    decide = go sing
      where
        go  :: Sing m
            -> Decide (SelFound m)
        go = \case
          SZ -> \case
            SNil         -> Disproved \(_ :&: s) -> noEmptySel s
            x `SCons` _  -> Proved (x :&: SelZ)
          SS n -> \case
            SNil         -> Disproved \(_ :&: s) -> noEmptySel s
            _ `SCons` xs -> case go n xs of
              Proved (y :&: s) -> Proved (y :&: SelS s)
              Disproved v -> Disproved \(y :&: SelS s) -> v (y :&: s)

instance Provable (TyPred Pick) where
    prove (STuple3 i0 j0 b0) = go i0 j0 b0
      where
        go  :: forall i j b. ()
            => Sing i
            -> Sing j
            -> Sing b
            -> Pick '(i, j, b)
        go Sing Sing b = case decide @(SelFound i) b of
            Proved (row :&: selX) -> case decide @(SelFound j) row of
              Proved (p :&: selY) ->
                let c = selX :$: selY
                in  case p of
                      SNothing -> PickValid   c
                      SJust q  -> PickPlayed  c q
              Disproved vY -> PickOoBY selX vY
            Disproved vX -> PickOoBX vX

instance Decidable (Found Victory) where
    decide = \case
      SNil -> Disproved \case
        _ :&: v -> case v of {}
      SNothing `SCons` _ -> Disproved \case
        _ :&: v -> case v of {}
      SJust (x@Sing :: Sing p) `SCons` xs -> case decide @(All [] (EqualTo ('Just p))) xs of
        Proved p    -> Proved $ x :&: VicWit p
        Disproved r -> Disproved \case
          _ :&: VicWit a -> r a

instance Auto (Not (Found Victory)) ('Nothing ': as) where
    auto (_ :&: w) = case w of {}

instance Decidable (Found GameOver) where
    decide b = case search @Winner b of
      Proved (p :&: v) -> Proved $ SResWin p :&: GOVictory v
      Disproved r      -> case decide @Cats b of
        Proved c     -> Proved $ SResCats :&: GOCats r c
        Disproved r' -> Disproved \case
          SResWin p :&: GOVictory v -> r $ p :&: v
          SResCats  :&: GOCats _ c  -> r' c

-- | The empty board is in-play.
startInPlay :: InPlay @@ EmptyBoard
startInPlay = \case
    SResWin p :&: GOVictory v -> noVictor (p :&: v)
    SResCats  :&: GOCats _ c  -> noCats   c
  where
    noVictor :: Not (Found Winner) @@ EmptyBoard
    noVictor = autoNot @_ @(Found Winner)  @EmptyBoard
    noCats   :: Not Cats @@ EmptyBoard
    noCats   = mapRefuted allComp
                $ autoNotAll @IsJust $ IZ :? IZ

intToN :: Int -> Maybe N
intToN n = case compare n 0 of
    LT -> Nothing
    EQ -> Just Z
    GT -> S <$> intToN (n - 1)

getN :: String -> IO N
getN prompt = do
    putStrLn $ "Enter non-negative integer for " ++ prompt ++ ":"
    res <- getLine
    case intToN =<< readMaybe res of
      Nothing -> putStrLn "Bad." >> getN prompt
      Just n  -> pure n

printBoard :: Board -> IO ()
printBoard = mapM_ $ putStrLn . intercalate "|" . map showPiece
  where
    showPiece Nothing   = " _ "
    showPiece (Just PX) = " X "
    showPiece (Just PO) = " O "

simplePlayIO :: IO ()
simplePlayIO = simplePlayIO' SPX sEmptyBoard startInPlay GSStart

simplePlayIO'
    :: Sing p
    -> Sing b
    -> InPlay @@ b
    -> GameState p b
    -> IO ()
simplePlayIO' p b ip gs = do
    printBoard $ FromSing b
    FromSing i <- getN "row"
    FromSing j <- getN "column"
    case prove @(TyPred Pick) (STuple3 i j b) of
      PickOoBX _ -> do
        putStrLn "Out of bounds in rows.  Try again."
        simplePlayIO' p b ip gs
      PickOoBY _ _ -> do
        putStrLn "Out of bounds in cols.  Try again."
        simplePlayIO' p b ip gs
      PickPlayed _ q -> do
        putStrLn $ "Already played by " ++ show (fromSing q) ++ ". Try again."
        simplePlayIO' p b ip gs
      PickValid c -> do
        putStrLn "Success!"
        let p'  = sAltP p                 -- update player (enforced by `play`)
            b'  = sPlaceBoard i j p b     -- update board  (enforced by `play`)
            gs' = play ip c gs            -- update game state
        case decide @(Found GameOver) b' of
            Proved (r :&: _) -> do
              printBoard $ FromSing b'
              putStrLn $ case r of
                SResCats    -> "Cat's game!"
                SResWin SPX -> "X wins!"
                SResWin SPO -> "O wins!"
            Disproved ip'    -> do
              simplePlayIO' p' b' ip' gs'
