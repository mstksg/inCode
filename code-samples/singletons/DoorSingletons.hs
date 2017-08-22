#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver nightly-2017-07-31 --package singletons

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

import Data.Singletons
import Data.Kind
import Data.Singletons.TH

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: Door s

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor UnsafeMkDoor = UnsafeMkDoor

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor UnsafeMkDoor = UnsafeMkDoor

openDoor :: Door 'Closed -> Door 'Opened
openDoor UnsafeMkDoor = UnsafeMkDoor

doorStatus :: Sing s -> Door s -> DoorState
doorStatus = \case
    SOpened -> -- in this branch, `s` is `'Opened`
        \_ -> Opened
    SClosed -> -- in this branch, `s` is `'Closed`
        \_ -> Closed
    SLocked -> -- in this branch, `s` is `'Locked`
        \_ -> Locked

lockAnyDoor :: Sing s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor
    SClosed -> lockDoor
    SLocked -> id

doorStatus_ :: SingI s => Door s -> DoorState
doorStatus_ = doorStatus sing

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

mkDoor :: Sing s -> Door s
mkDoor = \case
    SOpened -> UnsafeMkDoor
    SClosed -> UnsafeMkDoor
    SLocked -> UnsafeMkDoor

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

closeSomeDoor :: SomeDoor -> Maybe SomeDoor
closeSomeDoor = \case
    MkSomeDoor SOpened d -> Just $ MkSomeDoor SClosed (closeDoor d)
    MkSomeDoor SClosed _ -> Nothing
    MkSomeDoor SLocked _ -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = MkSomeDoor SLocked (lockAnyDoor s d)

mkSomeDoor :: DoorState -> SomeDoor
mkSomeDoor = \case
    Opened -> MkSomeDoor SOpened (mkDoor SOpened)
    Closed -> MkSomeDoor SClosed (mkDoor SClosed)
    Locked -> MkSomeDoor SLocked (mkDoor SLocked)

main :: IO ()
main = return ()

