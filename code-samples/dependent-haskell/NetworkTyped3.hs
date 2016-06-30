
runSomeNet :: KnownNat i
           => SomeNet
           -> R i
           -> (forall o. KnownNat o => R o -> r)
           -> Maybe r
runSomeNet n x f = case n of
                     SNet n' ->
                       case exactLength x of
                         Just x' -> Just (f (runNet n' x'))
                         Nothing -> Nothing

