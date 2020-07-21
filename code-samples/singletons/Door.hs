#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

import           Data.Kind

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

data Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }
                  -- requires -XDataKinds

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

data SDoorState :: DoorState -> Type where
    SOpened :: SDoorState 'Opened
    SClosed :: SDoorState 'Closed
    SLocked :: SDoorState 'Locked

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor sng door = case sng of
    SOpened -> lockDoor (closeDoor door) -- in this branch, s is 'Opened
    SClosed -> lockDoor door             -- in this branch, s is 'Closed
    SLocked -> door                      -- in this branch, s is 'Locked

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus s _ = fromSDoorState s

class SingDSI s where
    singDS :: SDoorState s

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

withSingDSI :: SDoorState s -> (SingDSI s => r) -> r
withSingDSI sng x = case sng of
    SOpened -> x
    SClosed -> x
    SLocked -> x

lockAnyDoor__ :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor__ s d = withSingDSI s (lockAnyDoor_ d)

mkDoor :: SDoorState s -> String -> Door s
mkDoor _ = UnsafeMkDoor

main :: IO ()
main = return ()
