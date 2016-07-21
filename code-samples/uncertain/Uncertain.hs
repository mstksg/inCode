#!/usr/bin/env stack
-- stack --resolver lts-5.15 --install-ghc runghc --package linear --package ad

{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE ViewPatterns    #-}

module Uncertain where

import Data.Foldable          (toList)
import Data.Function          (on)
import Data.Ord               (comparing)
import Linear                 (V2(..), V3(..))
import Numeric.AD.Mode.Sparse
import Numeric.AD.Mode.Tower

data Uncert a = Un { uMean :: !a
                   , uVar  :: !a
                   }

(+/-) :: Num a => a -> a -> Uncert a
x +/- dx = Un x (dx*dx)

exact :: Num a => a -> Uncert a
exact x = x +/- 0

-- [GHC 7.10:]
-- pattern (:+/-) :: () => Floating a => a -> a -> Uncert a
-- [GHC 8.0:]
-- pattern (:+/-) :: Floating a => a -> a -> Uncert a
pattern x :+/- dx <- Un x (sqrt->dx)
  where
    x :+/- dx = Un x (dx*dx)

uStdev :: Floating a => Uncert a -> a
uStdev (_ :+/- dx) = dx

liftU :: Fractional a
      => (forall s. AD s (Tower a) -> AD s (Tower a))
      -> Uncert a
      -> Uncert a
liftU f (Un x vx) = Un y vy
  where
    fx:dfx:ddfx:_ = diffs0 f x
    y             = fx + ddfx * vx / 2
    vy            = dfx^2 * vx

diag :: [[a]] -> [a]
diag = \case
    []        -> []
    []   :yss -> diag (drop 1 <$> yss)
    (x:_):yss -> x : diag (drop 1 <$> yss)

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)

liftUF :: (Traversable f, Fractional a)
       => (forall s. f (AD s (Sparse a)) -> AD s (Sparse a))
       -> f (Uncert a)
       -> Uncert a
liftUF f us = Un y vy
  where
    xs          =         uMean <$> us
    vxs         = toList (uVar  <$> us)
    (fx, hgrad) = hessian' f xs
    dfxs        = fst <$> hgrad
    hess        = snd <$> hgrad
    y           = fx + partials / 2
      where
        partials = dot vxs
                 . diag
                 $ toList (fmap toList hess) -- from f (f a) to [[a]]
    vy          = dot vxs
                $ toList ((^2) <$> dfxs)

liftU2 :: Fractional a
       => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
       -> Uncert a
       -> Uncert a
       -> Uncert a
liftU2 f x y = liftUF (\(V2 x' y') -> f x' y') (V2 x y)

liftU3 :: Fractional a
       => (forall s. AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a) -> AD s (Sparse a))
       -> Uncert a
       -> Uncert a
       -> Uncert a
       -> Uncert a
liftU3 f x y z = liftUF (\(V3 x' y' z') -> f x' y' z') (V3 x y z)

instance Fractional a => Num (Uncert a) where
    (+)         = liftU2 (+)
    (*)         = liftU2 (*)
    (-)         = liftU2 (-)
    negate      = liftU negate
    abs         = liftU abs
    signum      = liftU signum
    fromInteger = exact . fromInteger

instance Fractional a => Fractional (Uncert a) where
    recip        = liftU recip
    (/)          = liftU2 (/)
    fromRational = exact . fromRational

instance Floating a => Floating (Uncert a) where
    pi      = exact pi
    exp     = liftU exp
    log     = liftU log
    sqrt    = liftU sqrt
    (**)    = liftU2 (**)
    logBase = liftU2 logBase
    sin     = liftU sin
    cos     = liftU cos
    asin    = liftU asin
    acos    = liftU acos
    atan    = liftU atan
    sinh    = liftU sinh
    cosh    = liftU cosh
    asinh   = liftU asinh
    acosh   = liftU acosh
    atanh   = liftU atanh

instance Eq a => Eq (Uncert a) where
    (==) = (==) `on` uMean
    (/=) = (/=) `on` uMean

instance Ord a => Ord (Uncert a) where
    compare = comparing uMean

instance (Fractional a, Real a) => Real (Uncert a) where
    toRational = toRational . uMean

instance RealFrac a => RealFrac (Uncert a) where
    properFraction x = (n, d)
      where
        d    = liftU (snd' . properFraction) x
        n    = fst . properFraction $ uMean x
        snd' :: (Int, b) -> b
        snd' = snd
    truncate = truncate . uMean
    round    = round    . uMean
    ceiling  = ceiling  . uMean
    floor    = floor    . uMean

instance RealFloat a => RealFloat (Uncert a) where
    floatRadix      = floatRadix     . uMean
    floatDigits     = floatDigits    . uMean
    floatRange      = floatRange     . uMean
    decodeFloat     = decodeFloat    . uMean
    exponent        = exponent       . uMean
    isNaN           = isNaN          . uMean
    isInfinite      = isInfinite     . uMean
    isDenormalized  = isDenormalized . uMean
    isNegativeZero  = isNegativeZero . uMean
    isIEEE          = isIEEE         . uMean
    encodeFloat a b = exact (encodeFloat a b)
    significand     = liftU significand
    atan2           = liftU2 atan2

uNormalize :: (Floating a, RealFrac a)
           => Uncert a
           -> Uncert a
uNormalize u = x' :+/- dx'
  where
    x :+/- dx = u
    uncert    :: Int
    uncert    = negate . floor . logBase 10 $ dx
    rounder   = 10 ** fromIntegral uncert
    roundTo   = (/ rounder) . fromIntegral . round' . (* rounder)
    x'        = roundTo x
    dx'       = roundTo dx
    round'    :: RealFrac a => a -> Integer
    round'    = round

instance (Show a, Floating a, RealFrac a) => Show (Uncert a) where
    showsPrec d u = showParen (d > 5) $
                        showsPrec 6 x
                      . showString " +/- "
                      . showsPrec 6 dx
      where
        x :+/- dx = uNormalize u

main :: IO ()
main = do
    let x = 14.6 +/- 0.8
        y = 31   +/- 2
    mapM_ putStrLn
      $ [ "[ x = " ++ show x ++ " ]"
        , "[ y = " ++ show y ++ " ]"
        , "---"
        , "x + y        => " ++ show (x + y)
        , "x * y        => " ++ show (x * y)
        , "sqrt (x + y) => " ++ show (sqrt (x + y))
        , "logBase y x  => " ++ show (logBase y x)
        , "log (x**y)   => " ++ show (log (x**y))
        ]

