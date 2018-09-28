#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-12.9 --package singletons

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)

  class MyEnum a where
    mySucc :: a -> a
    myPred :: a -> a
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

$(singletons [d|
  data Pass = Obstruct | Allow
  passState :: DoorState -> Pass
  passState Opened = Allow
  passState Closed = Obstruct
  passState Locked = Obstruct

  mergeState :: DoorState -> DoorState -> DoorState
  mergeState Opened x      = x
  mergeState Closed Opened = Closed
  mergeState Closed Closed = Closed
  mergeState Closed Locked = Locked
  mergeState Locked _      = Locked

  foldr' :: (a -> b -> b) -> b -> [a] -> b
  foldr' _ z []     = z
  foldr' f z (x:xs) = f x (foldr' f z xs)
  |])

knock :: (PassState s ~ 'Obstruct) => Door s -> IO ()
knock _ = putStrLn "Knock knock!"

tryKnock :: Sing s -> Door s -> IO ()
tryKnock s = case sPassState s of
    SObstruct -> knock
    SAllow    -> \_ -> putStrLn "Cannot knock."

mergeDoor :: Door s -> Door t -> Door (MergeState s t)
mergeDoor (UnsafeMkDoor x) (UnsafeMkDoor y) = UnsafeMkDoor (x ++ " and " ++ y)

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (MkSomeDoor s d) (MkSomeDoor t e) =
        MkSomeDoor (sMergeState s t) (mergeDoor d e)

data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]
    HCons :: Door s -> Hallway ss -> Hallway (s ': ss)

mergeHallway :: Sing ss -> Hallway ss -> SomeDoor
mergeHallway = \case
    SNil -> \case
      HEnd -> MkSomeDoor SOpened (UnsafeMkDoor "End")
    s `SCons` ss -> \case
      HCons d ds -> case mergeHallway ss ds of
        MkSomeDoor ss ds' -> MkSomeDoor (sMergeState s ss) (mergeDoor d ds')

mergeHallway' :: Hallway ss -> Door (Foldr' MergeStateSym0 'Opened ss)
mergeHallway' = \case
    HEnd       -> UnsafeMkDoor @Opened "End"
    HCons d ds -> mergeDoor d (mergeHallway' ds)

main :: IO ()
main = return ()
