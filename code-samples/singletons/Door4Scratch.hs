#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-09-29 --package singletons

{-# LANGUAGE AllowAmbiguousTypes            #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}
{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

import           Data.Kind
-- import           Data.Singletons
-- -- import           Data.Singletons.Prelude hiding (And, Or, )
-- import           Data.Singletons.TH hiding (Foldr)
-- import           Data.Void

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq, Ord)

-- $(singletons [d|
--   data DoorState = Opened | Closed | Locked
--     deriving (Show, Eq, Ord)
--   |])

-- type family MergeState (s :: DoorState) (t :: DoorState) :: DoorState where
--     MergeState s t = Max s t

-- type family Foldr (f :: j -> k -> k) (z :: k) (xs :: [j]) :: k where
--     Foldr f z '[]       = z
--     Foldr f z (x ': xs) = f x (Foldr f z xs)

-- -- type family MergeStates (ss :: [DoorState]) :: DoorState where
-- --     MergeStates ss = Foldr MergeState 'Opened ss

-- type family ListToList (xs :: [k]) :: [k] where
--     ListToList ss = Foldr '(:) '[] ss

data TyFun a b
type a ~> b = TyFun a b -> Type

infixr 0 ~>

type family Apply (f :: a ~> b) (x :: a) :: b
type f @@ x = Apply f x

infixl 9 @@

data Id :: a ~> a
type instance Apply Id x = x

data Not :: Bool ~> Bool
type instance Apply Not 'True  = 'False
type instance Apply Not 'False = 'True

type family And (x :: Bool) (y :: Bool) :: Bool where
    And 'False x = 'False
    And 'True  x = x

data AndSym0 :: Bool ~> (Bool ~> Bool)
type instance Apply AndSym0 x = AndSym1 x

data AndSym1 x :: Bool ~> Bool
type instance Apply (AndSym1 x) y = And x y

type family Foldr (f :: j ~> k ~> k) (z :: k) (xs :: [j]) :: k where
    Foldr f z '[]       = z
    Foldr f z (x ': xs) = (f @@ x) @@ Foldr f z xs


type MergeStates ss = Foldr MergeStateSym0 'Opened ss
-- type family MergeStates (ss :: [DoorState]) :: DoorState where
--     MergeStates ss = Foldr MergeStateSym0 'Opened ss

type family MergeState (s :: DoorState) (t :: DoorState) :: DoorState where
    MergeState s t = s

data MergeStateSym0 :: DoorState ~> DoorState ~> DoorState
type instance Apply MergeStateSym0 s = MergeStateSym1 s

data MergeStateSym1 :: DoorState -> DoorState ~> DoorState
type instance Apply (MergeStateSym1 s) t = MergeState s t

data family Sing (a :: k)

data Sigma k :: (k ~> Type) -> Type where
    (:&:) :: Sing x -> (f @@ x) -> Sigma k f

foo :: Sing MergeStateSym0
foo = sing

main :: IO ()
main = return ()
