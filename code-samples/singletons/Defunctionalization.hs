#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16

{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Data.Kind

data TyFun a b
type a ~> b = TyFun a b -> Type

infixr 0 ~>

type family Apply (f :: a ~> b) (x :: a) :: b

type f @@ a = Apply f a

infixl 9 @@

data Id :: a ~> a
type instance Apply Id x = x

data Not :: Bool ~> Bool
type instance Apply Not 'False = 'True
type instance Apply Not 'True  = 'False

data TyCon1
        :: (j -> k)     -- ^ take a type constructor
        -> (j ~> k)     -- ^ return a defunctionalization symbol
-- alternatively
-- data TyCon1 (t :: j -> k) :: j ~> k

type instance Apply (TyCon1 t) a = t a

type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq, Ord)

type family MergeState (s :: DoorState) (t :: DoorState) :: DoorState where
    MergeState 'Opened t       = t
    MergeState 'Closed 'Opened = 'Closed
    MergeState 'Closed 'Closed = 'Closed
    MergeState 'Closed 'Locked = 'Locked
    MergeState 'Locked t       = 'Locked

data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
type instance Apply MergeStateSym0 s = MergeStateSym1 s

data MergeStateSym1 :: DoorState -> DoorState ~> DoorState
type instance Apply (MergeStateSym1 s) t = MergeState s t

type MergeStateSym2 s t = MergeState s t

type MergeStateList ss = Foldr MergeStateSym0 'Opened ss

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)
infixr 5 :<#

mergeDoor :: Door s -> Door t -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds

data FoldrSym0 :: (j ~> k ~> k) ~> k ~> [j] ~> k
type instance Apply FoldrSym0 f = FoldrSym1 f

data FoldrSym1 :: (j ~> k ~> k) -> k ~> [j] ~> k
type instance Apply (FoldrSym1 f) z = FoldrSym2 f z

data FoldrSym2 :: (j ~> k ~> k) -> k -> [j] ~> k
type instance Apply (FoldrSym2 f z) xs = Foldr f z xs

type FoldrSym3 f z xs = Foldr f z xs

type MergeStateListSym0 = FoldrSym2 MergeStateSym0 'Opened

main :: IO ()
main = putStrLn "hi"
