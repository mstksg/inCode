{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

import           Data.Maybe
import           Numeric.Hamilton
import           Numeric.LinearAlgebra.Static
import qualified Data.Vector.Sized            as V

doublePendulum :: System 4 2
doublePendulum = mkSystem' masses coordinates potential
  where
    masses :: R 4
    masses = vec4 1 1 2 2
    coordinates
        :: Floating a
        => V.Vector 2 a
        -> V.Vector 4 a
    coordinates (V2 θ1 θ2) = V4 (sin θ1)            (-cos θ1)
                                (sin θ1 + sin θ2/2) (-cos θ1 - cos θ2/2)
    potential
        :: Num a
        => V.Vector 4 a
        -> a
    potential (V4 _ y1 _ y2) = (y1 + 2 * y2) * 5    -- assuming g = 5

pattern V2 :: a -> a -> V.Vector 2 a
pattern V2 x y <- (V.toList->[x,y])
  where
    V2 x y = fromJust (V.fromList [x,y])

pattern V4 :: a -> a -> a -> a -> V.Vector 4 a
pattern V4 x y z a <- (V.toList->[x,y,z,a])
  where
    V4 x y z a = fromJust (V.fromList [x,y,z,a])

config0 :: Config 2
config0 = Cfg (vec2 1 0  )  -- initial positions
              (vec2 0 0.5)  -- initial velocities

phase0 :: Phase 2
phase0 = toPhase doublePendulum config0

evolution :: [Phase 2]
evolution = evolveHam' doublePendulum phase0 [0,0.1 .. 1]

evolution' :: [Phase 2]
evolution' = iterate (stepHam 0.1 doublePendulum) phase0

positions :: [R 2]
positions = phsPositions <$> evolution'

main :: IO ()
main = withRows (take 25 positions) (disp 4)

