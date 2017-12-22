

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

