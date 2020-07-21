#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package singletons

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TH

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

doorStatus :: Sing s -> Door s -> DoorState
doorStatus SOpened _ = Opened
doorStatus SClosed _ = Closed
doorStatus SLocked _ = Locked

lockAnyDoor :: Sing s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor
    SClosed -> lockDoor
    SLocked -> id

doorStatus_ :: SingI s => Door s -> DoorState
doorStatus_ = doorStatus sing

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor = MkSomeDoor

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor s d) = case s of
    SOpened -> Just . fromDoor_ $ closeDoor d
    SClosed -> Nothing
    SLocked -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds = case toSing ds of
    SomeSing s -> fromDoor s . mkDoor s

withDoor :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
withDoor ds m f = withSomeSing ds $ \s -> f s (mkDoor s m)

main :: IO ()
main = return ()

-- Exercises

data OldSomeDoor :: Type where
    OldMkSomeDoor :: DoorState -> String -> OldSomeDoor

toOld :: SomeDoor -> OldSomeDoor
toOld (MkSomeDoor s d) = OldMkSomeDoor (fromSing s) (doorMaterial d)

fromOld :: OldSomeDoor -> SomeDoor
fromOld (OldMkSomeDoor ds m) = mkSomeDoor ds m

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
    | n `mod` 2 == 1 = Just (UnsafeMkDoor m)
    | otherwise      = Nothing

unlockSomeDoor :: Int -> Door 'Locked -> SomeDoor
unlockSomeDoor n d = case unlockDoor n d of
                       Nothing -> fromDoor_ d
                       Just d' -> fromDoor_ d'

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor_ sing
  where
    openAnyDoor_ :: Sing s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor_ = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n

openAnySomeDoor :: Int -> SomeDoor -> SomeDoor
openAnySomeDoor n sd@(MkSomeDoor s d) = withSingI s $
    case openAnyDoor n d of
      Nothing -> sd
      Just d' -> fromDoor_ d'

data List a = Nil | Cons a (List a)

data SList :: List a -> Type where
    SNil  :: SList 'Nil
    SCons :: Sing x -> SList xs -> SList ('Cons x xs)

type instance Sing = SList

instance SingKind k => SingKind (List k) where
    type Demote (List k) = List (Demote k)

    fromSing :: Sing (xs :: List k) -> List (Demote k)
    fromSing = \case
      SNil         -> Nil
      SCons sx sxs -> Cons (fromSing sx) (fromSing sxs)

    toSing :: List (Demote k) -> SomeSing (List k)
    toSing = \case
      Nil       -> SomeSing SNil
      Cons x xs -> withSomeSing x  $ \sx ->
                   withSomeSing xs $ \sxs ->
        SomeSing (SCons sx sxs)
