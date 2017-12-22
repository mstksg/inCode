


openAnySomeDoor :: Int -> SomeDoor -> Maybe (Door 'Opened)
openAnySomeDoor n (MkSomeDoor s d) = withSingI s $
    openAnyDoor n d

withSomeDoor :: SomeDoor -> (forall s. Sing s -> Door s -> r) -> r
withSomeDoor sd f = case sd of
    MkSomeDoor s d -> f s d

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

closeSomeDoor :: SomeDoor -> Maybe SomeDoor
closeSomeDoor = \case
    MkSomeDoor SOpened d -> Just $ MkSomeDoor SClosed (closeDoor d)
    MkSomeDoor SClosed _ -> Nothing
    MkSomeDoor SLocked _ -> Nothing

lockAnySomeDoor :: SomeDoor -> SomeDoor
lockAnySomeDoor (MkSomeDoor s d) = MkSomeDoor SLocked (lockAnyDoor s d)

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor = \case
    Opened -> MkSomeDoor SOpened . mkDoor SOpened
    Closed -> MkSomeDoor SClosed . mkDoor SClosed
    Locked -> MkSomeDoor SLocked . mkDoor SLocked
