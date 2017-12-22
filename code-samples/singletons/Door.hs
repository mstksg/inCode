#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver nightly-2017-08-20

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeInType     #-}

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

lockAnyDoor :: SingDS s -> (Door s -> Door 'Locked)
lockAnyDoor = \case
    SOpened -> lockDoor . closeDoor  -- in this branch, s is 'Opened
    SClosed -> lockDoor              -- in this branch, s is 'Closed
    SLocked -> id                    -- in this branch, s is 'Locked

fromSingDS :: SingDS s -> DoorState
fromSingDS = \case
    SOpened -> Opened
    SClosed -> Closed
    SLocked -> Locked

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
withSingDSI s x = case s of
    SOpened -> x
    SClosed -> x
    SLocked -> x

lockAnyDoor__ :: SingDS s -> Door s -> Door 'Locked
lockAnyDoor__ s d = withSingDSI s (lockAnyDoor_ d)

mkDoor :: SingDS s -> String -> Door s
mkDoor = \case
    SOpened -> UnsafeMkDoor
    SClosed -> UnsafeMkDoor
    SLocked -> UnsafeMkDoor

data SomeDoor :: Type where
    MkSomeDoor ::
      { someDoorState :: SingDS s
      , someDoorDoor  :: Door s
      } -> SomeDoor

closeSomeOpenedDoor :: SomeDoor -> Maybe (Door 'Closed)
closeSomeOpenedDoor = \case
    MkSomeDoor SOpened d -> Just (closeDoor d)
    MkSomeDoor SClosed d -> Nothing
    MkSomeDoor SLocked _ -> Nothing

lockAnySomeDoor :: SomeDoor -> Door 'Locked
lockAnySomeDoor (MkSomeDoor s d) = lockAnyDoor s d

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
    Opened -> MkSomeDoor SOpened . mkDoor SOpened
    Closed -> MkSomeDoor SClosed . mkDoor SClosed
    Locked -> MkSomeDoor SLocked . mkDoor SLocked

withDoorState :: DoorState -> (forall s. SingDS s -> r) -> r
withDoorState = \case
    Opened -> \f -> f SOpened
    Closed -> \f -> f SClosed
    Locked -> \f -> f SLocked

withDoor :: DoorState -> String -> (forall s. SingDS s -> Door s -> r) -> r
withDoor s m f = withDoorState s $ \sds ->
                    f sds (UnsafeMkDoor m)

main :: IO ()
main = return ()
