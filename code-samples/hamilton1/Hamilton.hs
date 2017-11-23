#!/usr/bin/env stack
-- stack runghc --resolver lts-9 --package ad --package hmatrix --package vector-sized

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

data System m n = System
    { sysInertia       :: R m                         -- ^ 'm' vector
    , sysCoords        :: R n -> R m                  -- ^ f
    , sysJacobian      :: R n -> L m n                -- ^ J_f
    , sysJacobian2     :: R n -> V.Vector n (L m n)   -- ^ grad (J_f)
    , sysPotential     :: R n -> Double               -- ^ U
    , sysPotentialGrad :: R n -> R n                  -- ^ grad U
    }

data Config n = Config
    { confPositions  :: R n
    , confVelocities :: R n
    }
  deriving Show

data Phase n = Phase
    { phasePositions :: R n
    , phaseMomenta   :: R n
    }
  deriving Show

underlyingPosition :: System m n -> R n -> R m
underlyingPosition = sysCoords

momenta :: (KnownNat n, KnownNat m) => System m n -> Config n -> R n
momenta s (Config q v) = tr j #> mHat #> j #> v
  where
    j    = sysJacobian s q
    mHat = diag (sysInertia s)

toPhase :: (KnownNat n, KnownNat m) => System m n -> Config n -> Phase n
toPhase s c = Phase (confPositions c) (momenta s c)

jacobian2
    :: (Traversable f, Functor g, RealFloat a)
    => (forall b. RealFloat b => f b -> g b)
    -> f a
    -> g (f (f a))
jacobian2 f = (fmap . fmap . fmap) C.extract  -- ^ take 2nd deriv
            . (fmap . fmap) C.unwrap          -- ^ drap 1st deriv
            . fmap C.unwrap                   -- ^ drop 0th deriv
            . jacobians f                     -- ^ create list of jacobians

vec2r :: KnownNat n => V.Vector n Double -> R n
vec2r = fromJust . create . VG.fromSized . VG.convert

r2vec :: KnownNat n => R n -> V.Vector n Double
r2vec = VG.convert . fromJust . VG.toSized . extract

vec2l :: (KnownNat m, KnownNat n) => V.Vector m (V.Vector n Double) -> L m n
vec2l = fromJust . (\rs -> withRows rs exactDims) . toList . fmap vec2r

rejacobi :: (KnownNat m, KnownNat n) => V.Vector m (L n n) -> V.Vector n (L m n)
rejacobi = fmap (fromJust . (\rs -> withRows rs exactDims) . toList)
         . sequenceA
         . fmap (fromJust . V.fromList . toRows)

mkSystem
    :: (KnownNat m, KnownNat n)
    => R m
    -> (forall a. RealFloat a => V.Vector n a -> V.Vector m a)
    -> (forall a. RealFloat a => V.Vector n a -> a)
    -> System m n
mkSystem m f u = System
    { sysInertia       =                        m
    , sysCoords        =      vec2r .           f . r2vec
    , sysJacobian      =      vec2l . jacobian  f . r2vec
    , sysJacobian2     = rejacobi
                       . fmap vec2l . jacobian2 f . r2vec
    , sysPotential     =                        u . r2vec
    , sysPotentialGrad =      vec2r .      grad u . r2vec
    }

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
               (sysJacobian2 s q)

stepEuler
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ q(t) and p(t)
    -> Phase n          -- ^ q(t + dt) and p(t + dt)
stepEuler s dt ph@(Phase q p) = Phase (q + konst dt * dq) (p + konst dt * dp)
  where
    (dq, dp) = hamilEqns s ph

runSystem
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ initial phase
    -> [Phase n]        -- ^ progression of the system using Euler integration
runSystem s dt = go
  where
    go p0 = p0 : go (stepEuler s dt p0)

simpleSystem :: System 2 2
simpleSystem = mkSystem (vec2 5 5) id pots
  where
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 2 a -> a
    pots xy = 9.8 * (xy `V.index` 1)

simpleConfig0 :: Config 2
simpleConfig0 = Config { confPositions  = vec2 0 0
                       , confVelocities = vec2 1 3
                       }

simpleMain :: IO ()
simpleMain =
    mapM_ (disp 2 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem simpleSystem 0.1 (toPhase simpleSystem simpleConfig0)


pendulum :: System 2 1
pendulum = mkSystem (vec2 5 5) coords pots      -- 5kg particle
  where
    -- <x,y> = <-0.5 sin(theta), -0.5 cos(theta)>
    -- pendulum of length 0.25
    coords :: RealFloat a => V.Vector 1 a -> V.Vector 2 a
    coords (V.head->theta) = fromJust
                           . V.fromList
                           $ [- 0.25 * sin theta, - 0.25 * cos theta]
    -- U(x,y) = 9.8 * y
    pots :: RealFloat a => V.Vector 1 a -> a
    pots q = 9.8 * (coords q `V.index` 1)

pendulumConfig0 :: Config 1
pendulumConfig0 = Config { confPositions  = 0
                         , confVelocities = 0.1
                         }

pendulumMain :: IO ()
pendulumMain =
    mapM_ (disp 3 . phasePositions)  -- position with 2 digits of precision
  . take 25                          -- 25 steps
  $ runSystem pendulum 0.1 (toPhase pendulum pendulumConfig0)

main :: IO ()
main = pendulumMain

