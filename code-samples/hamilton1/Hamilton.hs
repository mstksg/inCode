#!/usr/bin/env stack
-- stack runghc --resolver lts-9 --package ad --package hmatrix --package vector-sized

-- | Source file accompanying
-- https://blog.jle.im/entry/hamiltonian-dynamics-in-haskell.html

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

import           Data.Foldable
import           Data.Maybe
import           GHC.TypeLits
import           Numeric.AD
import           Numeric.LinearAlgebra.Static
import qualified Control.Comonad              as C
import qualified Control.Comonad.Cofree       as C
import qualified Data.Vector.Generic.Sized    as VG
import qualified Data.Vector.Sized            as V

-- | A @'System' m n@ represents a system parameterized by @n@ generalized
-- coordinates, moving in an "underlying" @m@-dimensional cartesian
-- coordinate system.
data System m n = System
    { sysInertia       :: R m                         -- ^ 'm' vector
    , sysCoords        :: R n -> R m                  -- ^ f
    , sysJacobian      :: R n -> L m n                -- ^ J_f
    , sysHessian       :: R n -> V.Vector n (L m n)   -- ^ H_f
    , sysPotential     :: R n -> Double               -- ^ U
    , sysPotentialGrad :: R n -> R n                  -- ^ grad U
    }

-- | A system's position in configuration space
data Config n = Config
    { confPositions  :: R n
    , confVelocities :: R n
    }
  deriving Show

-- | A system's position in phase space
data Phase n = Phase
    { phasePositions :: R n
    , phaseMomenta   :: R n
    }
  deriving Show

-- | Given a position in @n@-dimensional generalized coordinates, return
-- the position in the underlying @m@-dimensional cartesian coordinate
-- system.
underlyingPosition
    :: System m n
    -> R n
    -> R m
underlyingPosition = sysCoords

-- | Give the system's generalized momenum, given its position in
-- configuration space.
momenta
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Config n
    -> R n
momenta s (Config q v) = tr j #> mHat #> j #> v
  where
    j    = sysJacobian s q
    mHat = diag (sysInertia s)

-- | Convert a position in configuration space to a position in phase space
toPhase
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Config n
    -> Phase n
toPhase s c = Phase (confPositions c) (momenta s c)

-- | Convert a sized-vector vector to an hmatrix vector
vec2r :: KnownNat n => V.Vector n Double -> R n
vec2r = fromJust . create . VG.fromSized . VG.convert

-- | Convert an hmatrix vector to a sized-vector vector
r2vec :: KnownNat n => R n -> V.Vector n Double
r2vec = VG.convert . fromJust . VG.toSized . extract

-- | Convert a sized-vector nested vector to an hmatrix matrix
vec2l :: (KnownNat m, KnownNat n) => V.Vector m (V.Vector n Double) -> L m n
vec2l = fromJust . (\rs -> withRows rs exactDims) . toList . fmap vec2r

-- | Shift around the Hessian into the shape expected
rehessian :: (KnownNat m, KnownNat n) => V.Vector m (L n n) -> V.Vector n (L m n)
rehessian = fmap (fromJust . (\rs -> withRows rs exactDims) . toList)
          . sequenceA
          . fmap (fromJust . V.fromList . toRows)

-- | Make a system given its inertias, coordinate functions, and potential
-- energy function
mkSystem
    :: (KnownNat m, KnownNat n)
    => R m
    -> (forall a. RealFloat a => V.Vector n a -> V.Vector m a)
    -> (forall a. RealFloat a => V.Vector n a -> a)
    -> System m n
mkSystem m f u = System
                  -- < convert from | actual thing | convert to >
    { sysInertia       =                         m
    , sysCoords        =      vec2r .            f . r2vec
    , sysJacobian      =      vec2l .   jacobian f . r2vec
    , sysHessian       = rehessian
                       . fmap vec2l .   hessianF f . r2vec
    , sysPotential     =                         u . r2vec
    , sysPotentialGrad =      vec2r .       grad u . r2vec
                  -- < convert from | actual thing | convert to >
    }

-- | Equations of motion for a system at a given position in phase space
hamilEqns
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Phase n
    -> (R n, R n)       -- dq/dt and dp/dt
hamilEqns s (Phase q p) = (dqdt, dpdt)
  where
    j       = sysJacobian s q
    trj     = tr j
    mHat    = diag (sysInertia s)
    kHat    = trj <> mHat <> j
    kHatInv = inv kHat
    dqdt    = kHatInv #> p
    dpdt    = vec2r bigUglyThing - sysPotentialGrad s q
      where
        bigUglyThing =
          fmap (\j2 -> -p <.> kHatInv #> trj #> mHat #> j2 #> kHatInv #> p)
               (sysHessian s q)

-- | Step a system's position through phase space using Euler integration
stepEuler
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ q(t) and p(t)
    -> Phase n          -- ^ q(t + dt) and p(t + dt)
stepEuler s dt ph@(Phase q p) = Phase (q + konst dt * dq) (p + konst dt * dp)
  where
    (dq, dp) = hamilEqns s ph

-- | Iterate Euler's method to repeatedly step a system through phase space
runSystem
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ initial phase
    -> [Phase n]        -- ^ progression of the system using Euler integration
runSystem s dt = go
  where
    go p0 = p0 : go (stepEuler s dt p0)

-- | A simple particle in 2D cartesian space under gravity
simpleSystem :: System 2 2
simpleSystem = mkSystem (vec2 5 5) id pot
  where
    -- potential energy of a gravity field
    -- U(x,y) = 9.8 * y
    pot :: RealFloat a => V.Vector 2 a -> a
    pot xy = 9.8 * (xy `V.index` 1)

-- | An initial position in configuration space, representing a particle
-- at <0,0> with initial velocity <1,3>
simpleConfig0 :: Config 2
simpleConfig0 = Config
    { confPositions  = vec2 0 0
    , confVelocities = vec2 1 3
    }

simpleMain :: IO ()
simpleMain =
    mapM_ (disp 2 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem simpleSystem 0.1 (toPhase simpleSystem simpleConfig0)


-- | A pendulum system, parameterized by its angle clockwise from
-- equilibrium
pendulum :: System 2 1
pendulum = mkSystem (vec2 5 5) coords pot      -- 5kg particle
  where
    -- <x,y> = <-0.5 sin(theta), -0.5 cos(theta)>
    -- pendulum of length 0.25
    coords :: RealFloat a => V.Vector 1 a -> V.Vector 2 a
    coords (V.head->theta) = fromJust
                           . V.fromList
                           $ [- 0.25 * sin theta, - 0.25 * cos theta]
    -- potential energy of gravity field
    -- U(x,y) = 9.8 * y
    pot :: RealFloat a => V.Vector 1 a -> a
    pot q = 9.8 * (coords q `V.index` 1)

-- | An initial pendulum position in configuration space, representing
-- a pendulum at equilibrium with initial angular velocity 0.1 rad/sec
-- clockwise
pendulumConfig0 :: Config 1
pendulumConfig0 = Config
    { confPositions  = 0
    , confVelocities = 0.1
    }

pendulumMain :: IO ()
pendulumMain =
    mapM_ (disp 3 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem pendulum 0.1 (toPhase pendulum pendulumConfig0)

main :: IO ()
main = pendulumMain

