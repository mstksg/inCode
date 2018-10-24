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
{-# LANGUAGE NoStarIsType                   #-}
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
import           Data.Singletons
import           Data.Singletons.Prelude hiding    (And, Or, sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr)
import           Data.Singletons.Sigma
import           Data.Singletons.TH hiding         (sFoldr, FoldrSym0, FoldrSym1, FoldrSym2, FoldrSym3, Foldr, sFold, Fold)
import           Data.Singletons.TypeLits

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq, Ord)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

$(singletons [d|
  mergeState :: DoorState -> DoorState -> DoorState
  mergeState = max
  |])

mergeDoor
    :: Door s
    -> Door t
    -> Door (MergeState s t)
mergeDoor d e = UnsafeMkDoor $ doorMaterial d ++ " and " ++ doorMaterial e

type SomeDoor = Sigma DoorState (TyCon1 Door)

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    dsSing :&: mkDoor dsSing mat

mergeSomeDoor :: SomeDoor -> SomeDoor -> SomeDoor
mergeSomeDoor (s :&: d) (t :&: e) =
    sMergeState s t :&: mergeDoor d e

data Hallway :: [DoorState] -> Type where
    HEnd  :: Hallway '[]        -- ^ end of the hallway, a stretch with no
                                --   doors
    (:<#) :: Door s
          -> Hallway ss
          -> Hallway (s ': ss)  -- ^ A door connected to a hallway is a new
                                --   hallway, and we track the door's state
                                --   in the list of hallway door states
infixr 5 :<#

$(singletons [d|
  mergeStateList :: [DoorState] -> DoorState
  mergeStateList []     = Opened               -- ^ the identity of mergeState
  mergeStateList (s:ss) = s `mergeState` mergeStateList ss
  |])

collapseHallway :: Hallway ss -> Door (MergeStateList ss)
collapseHallway HEnd       = mkDoor SOpened "End of Hallway"
collapseHallway (d :<# ds) = d `mergeDoor` collapseHallway ds

type SomeHallway = Sigma [DoorState] (TyCon1 Hallway)

collapseSomeHallway :: SomeHallway -> SomeDoor
collapseSomeHallway (ss :&: d) = sMergeStateList ss
                             :&: collapseHallway d

$(singletons [d|
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr _ z []     = z
  foldr f z (x:xs) = f x (foldr f z xs)
  |])

-- | COMMENT THIS OUT IF YOU WANT TO RUN ON SINGLETONS < 2.5 OR GHC 8.4
$(singletons [d|
  fold :: Monoid b => [b] -> b
  fold []     = mempty
  fold (x:xs) = x <> fold xs

  instance Semigroup DoorState where
      (<>) = mergeState
  instance Monoid DoorState where
      mempty  = Opened
      mappend = (<>)
  |])

collapseHallway'
    :: Hallway ss
    -> Door (Fold ss)
collapseHallway' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway' (d :<# ds) = d `mergeDoor` collapseHallway' ds

collapseSomeHallway' :: SomeHallway -> SomeDoor
collapseSomeHallway' (ss :&: d) =
        sFold ss
    :&: collapseHallway' d

-- | END OF SINGLETONS-2.5 ONLY SECTON

collapseHallway''
    :: Hallway ss
    -> Door (FoldrSym2 MergeStateSym0 'Opened @@ ss)
collapseHallway'' HEnd       = UnsafeMkDoor "End of Hallway"
collapseHallway'' (d :<# ds) = d `mergeDoor` collapseHallway'' ds

collapseSomeHallway'' :: SomeHallway -> SomeDoor
collapseSomeHallway'' (ss :&: d) =
        sFoldr (singFun2 @MergeStateSym0 sMergeState) SOpened ss
     -- or
     -- sFoldr (sing @MergeStateSym0) SOpened ss
    :&: collapseHallway'' d


-- Exercises

-- | Supplementary definitions
data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)

  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])


-- | 1.

mergedIsKnockable
    :: Knockable s
    -> Knockable t
    -> Knockable (MergeState s t)
mergedIsKnockable = \case
    KnockClosed -> \case
      KnockClosed -> KnockClosed
      KnockLocked -> KnockLocked    -- GHC makes sure we use KnockLocked!
    KnockLocked -> \case
      KnockClosed -> KnockLocked
      KnockLocked -> KnockLocked
    -- and -Werror=incomplete-patterns makes sure we handle all cases

-- | 2.
$(singletons [d|
  append :: [a] -> [a] -> [a]
  append []     ys = ys
  append (x:xs) ys = x : append xs ys
  |])

appendHallways
    :: Hallway ss
    -> Hallway ts
    -> Hallway (Append ss ts)
appendHallways = \case
    HEnd     -> id
    d :<# ds -> \es -> d :<# appendHallways ds es

appendSomeHallways
    :: SomeHallway
    -> SomeHallway
    -> SomeHallway
appendSomeHallways (ss :&: ds) (ts :&: es)
      = sAppend ss ts
    :&: appendHallways ds es

-- | 3.

-- Auto-promotion, Knockable
$(singletons [d|
  type KnockableDoor1 s = (Knockable s, Door s)
  |])

type SomeKnockableDoor1 = Sigma DoorState KnockableDoor1Sym0

-- Manual family, Knockable
data KnockableDoor2 :: DoorState ~> Type
type instance Apply KnockableDoor2 s = (Knockable s, Door s)

type SomeKnockableDoor2 = Sigma DoorState KnockableDoor2

-- Auto-promotion, via Pass
$(singletons [d|
  type KnockableDoor3 s = (StatePass s :~: 'Obstruct, Door s)
  |])

type SomeKnockableDoor3 = Sigma DoorState KnockableDoor3Sym0

-- Manual family, via Pass
data KnockableDoor4 :: DoorState ~> Type
type instance Apply KnockableDoor4 s = (StatePass s :~: 'Obstruct, Door s)

type SomeKnockableDoor4 = Sigma DoorState KnockableDoor4

-- | 4.
data AlmostHalfOf :: Nat -> Nat ~> Type
type instance Apply (AlmostHalfOf n) m = n :~: (m * 2 + 1)

type IsOdd n = Sigma Nat (AlmostHalfOf n)

sevenIsOdd :: IsOdd 7
sevenIsOdd = SNat @3 :&: Refl @7

-- | 5.
type Map f xs = Foldr (TyCon2 (:) .@#@$$$ f) '[] xs

-- | 6.
mkSomeHallway :: [SomeDoor] -> SomeHallway
mkSomeHallway []               = SNil :&: HEnd
mkSomeHallway ((s :&: d) : ds) = case mkSomeHallway ds of
    ss :&: hw -> (s `SCons` ss) :&: (d :<# hw)
