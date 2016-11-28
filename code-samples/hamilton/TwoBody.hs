{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

import           Data.Maybe
import           Numeric.Hamilton
import           Numeric.LinearAlgebra.Static
import qualified Data.Vector.Sized            as V

twoBody :: System 4 2
twoBody = mkSystem masses coordinates potential
  where
    masses :: R 4
    masses = vec4 10 10 1 1
    coordinates
        :: Floating a
        => V.Vector 2 a
        -> V.Vector 4 a
    coordinates (V2 r θ) = V4 (r1 * cos θ) (r1 * sin θ)
                              (r2 * cos θ) (r2 * sin θ)
      where
        r1 =   r *  1 / 11
        r2 = - r * 10 / 11
    potential
        :: Fractional a
        => V.Vector 2 a
        -> a
    potential (V2 r _) = - 10 / r       -- G = 1

pattern V2 :: a -> a -> V.Vector 2 a
pattern V2 x y <- (V.toList->[x,y])
  where
    V2 x y = fromJust (V.fromList [x,y])

pattern V4 :: a -> a -> a -> a -> V.Vector 4 a
pattern V4 x y z a <- (V.toList->[x,y,z,a])
  where
    V4 x y z a = fromJust (V.fromList [x,y,z,a])

config0 :: Config 2
config0 = Cfg (vec2 2   0)  -- initial positions
              (vec2 0 0.5)  -- initial velocities

phase0 :: Phase 2
phase0 = toPhase twoBody config0

evolution :: [Phase 2]
evolution = evolveHam' twoBody phase0 [0,0.1 .. 1]

evolution' :: [Phase 2]
evolution' = iterate (stepHam 0.1 twoBody) phase0

positions :: [R 2]
positions = phsPositions <$> evolution'

main :: IO ()
main = withRows (take 25 positions) (disp 4)


