#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver nightly-2018-09-29 --package singletons

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding (And, Or)
import           Data.Singletons.TH
import           Data.Void

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

data SomeDoor :: Type where
    MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing ->
    MkSomeDoor dsSing (UnsafeMkDoor mat)

data Knockable :: DoorState -> Type where
    KnockClosed :: Knockable 'Closed
    KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

class Provable p a where
    auto :: p a

instance Provable Knockable 'Closed where
    auto = KnockClosed

instance Provable Knockable 'Locked where
    auto = KnockLocked

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
    SOpened -> Disproved $ \case {}         -- s ~ 'Opened
    SClosed -> Proved KnockClosed           -- s ~ 'Closed
    SLocked -> Proved KnockLocked           -- s ~ 'Locked

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}             -- empty pattern match

knockSomeDoor
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
    Proved k    -> knock k d
    Disproved _ -> putStrLn "No knocking allowed!"

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq)

  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])

knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorP
    :: SomeDoor     -- ^ status not known until you pattern match at runtime
    -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
    SObstruct -> knockP d                        -- ^ `StatePass s ~ 'Obstruct`
    SAllow    -> putStrLn "No knocking allowed!" -- ^ `StatePass s ~ 'Allow`

main :: IO ()
main = return ()

-- Exercises

-- | 1. The predicate `SDoorState` (and really, the predicate for 
-- `Sing` for any specific kind instance) is essentially `const True`, the
-- "always-true" predicate.
--
decideDoorState :: Sing s -> Decision (SDoorState s)
decideDoorState = Proved

-- | 2.
refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s
refuteRefuteKnockable rrK =
    case isKnockable (sing @s) of   -- sing @_ @s for singletons-2.4.1 and earlier
      Proved    k  -> k
      Disproved rK -> absurd (rrK rK)

-- | 3.
data And :: (k -> Type) -> (k -> Type) -> (k -> Type) where
    And :: p a -> q a -> And p q a

data Or :: (k -> Type) -> (k -> Type) -> (k -> Type) where
    OrLeft  :: p a -> Or p q a
    OrRight :: q a -> Or p q a

decideAnd
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (And p q a)
decideAnd dP dQ x = case dP x of
    Proved    pP -> case dQ x of
      Proved    pQ -> Proved (And pP pQ)
      Disproved vQ -> Disproved $ \(And _ pQ) -> vQ pQ
    Disproved vP -> Disproved $ \(And pP _) -> vP pP

decideOr
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (Or p q a)
decideOr dP dQ x = case dP x of
    Proved    pP -> Proved (OrLeft pP)
    Disproved vP -> case dQ x of
      Proved    pQ -> Proved (OrRight pQ)
      Disproved vQ -> Disproved $ \case
        OrLeft  pP -> vP pP
        OrRight pQ -> vQ pQ

knockableNotOpened
    :: forall s. SingI s
    => Refuted (And Knockable ((:~:) 'Opened) s)
knockableNotOpened (And k o) = case k of
    KnockClosed -> case o of {} -- no constructor of type ('Opened :~: 'Closed)
    KnockLocked -> case o of {} -- no constructor of type ('Opened :~: 'Locked)

knockableOrOpened
    :: forall s. SingI s
    => Or Knockable ((:~:) 'Opened) s
knockableOrOpened = case sing @s of
    SOpened -> OrRight Refl
    SClosed -> OrLeft KnockClosed
    SLocked -> OrLeft KnockLocked

-- | 4.
knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)
knockedRefute = \case
    KnockClosed -> \case {}
    KnockLocked -> \case {}

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened)
    -> Knockable s
refuteKnocked v = case sing @s of   -- sing @_ @s for singletons-2.4.1 and earlier
    SOpened -> absurd (v Refl)
    SClosed -> KnockClosed
    SLocked -> KnockLocked

-- | 5.
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =
    case sStatePass s %~ SObstruct of
      Proved r    -> knockRefl r d
      Disproved _ -> putStrLn "No knocking allowed!"

-- | 6.
$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
  |])

knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =
    case sInvertPass (sStatePass s) of
      SObstruct -> putStrLn "No knocking allowed!"
      SAllow    -> knockInv d

-- | 7.
$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])

instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed

instance PCycle DoorState where
    type Next 'Opened = 'Closed
    type Next 'Closed = 'Locked
    type Next 'Locked = 'Opened

    type Prev 'Opened = 'Locked
    type Prev 'Closed = 'Opened
    type Prev 'Locked = 'Closed

instance SCycle DoorState where
    sNext = \case
      SOpened -> SClosed
      SClosed -> SLocked
      SLocked -> SOpened

    sPrev = \case
      SOpened -> SLocked
      SClosed -> SOpened
      SLocked -> SClosed
