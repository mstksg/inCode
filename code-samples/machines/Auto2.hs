module Auto where

newtype Stream b = SCons { runStream :: (b, Stream b) }

newtype Auto a b = ACons { runAuto :: a -> (b, Auto a b) }

