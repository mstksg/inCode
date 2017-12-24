#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.0

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}

import Data.Kind

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = (UnsafeMkDoor m)

data SingDS :: DoorState -> Type where
    SOpened :: SingDS 'Opened
    SClosed :: SingDS 'Closed
    SLocked :: SingDS 'Locked

lockAnyDoor :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor sng door = case sng of
    SOpened -> lockDoor (closeDoor door) -- in this branch, s is 'Opened
    SClosed -> lockDoor door             -- in this branch, s is 'Closed
    SLocked -> door                      -- in this branch, s is 'Locked

fromSingDS :: SingDS s -> DoorState
fromSingDS SOpened = Opened
fromSingDS SClosed = Closed
fromSingDS SLocked = Locked

doorStatus :: SingDS s -> Door s -> DoorState
doorStatus s _ = fromSingDS s

class SingDSI s where
    singDS :: SingDS s

instance SingDSI 'Opened where
    singDS = SOpened
instance SingDSI 'Closed where
    singDS = SClosed
instance SingDSI 'Locked where
    singDS = SLocked

lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS

doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS

withSingDSI :: SingDS s -> (SingDSI s => r) -> r
withSingDSI sng x = case sng of
    SOpened -> x
    SClosed -> x
    SLocked -> x

lockAnyDoor__ :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor__ s d = withSingDSI s (lockAnyDoor_ d)

mkDoor :: SingDS s -> String -> Door s
mkDoor _ = UnsafeMkDoor

main :: IO ()
main = return ()
