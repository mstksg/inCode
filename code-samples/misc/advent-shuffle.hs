#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package finite-typelits --package groups --package heredoc

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Finite
import           Text.Heredoc
import           Data.Group
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeNats

data Affine n = Aff
    { aScale :: Finite n
    , aShift :: Finite n
    }

runPerm :: KnownNat n => Affine n -> Finite n -> Finite n
runPerm (Aff a b) x = a * x + b

parseAffine :: KnownNat n => String -> Affine n
parseAffine str = case words str of
    "cut":n:_           -> Aff                1  (-modulo (read n))
    "deal":"into":_     -> Aff        (negate 1)          maxBound
    "deal":"with":_:n:_ -> Aff (modulo (read n))                 0

instance KnownNat n => Semigroup (Affine n) where
    Aff a' b' <> Aff a b = Aff (a' * a) (a' * b + b')

instance KnownNat n => Monoid (Affine n) where
    mempty = Aff 1 0

-- | Group instance only works if n is prime
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = a ^ (natVal (Proxy @n) - 2)
        b' = negate (a' * b)

-- | Part 1: Given a permutation list, find the place where 2019 ends up
part1 :: [Affine 10007] -> Finite 10007
part1 perms = runPerm bigPerm 2019
  where
    bigPerm = mconcat perms

-- | Part 2: Given a permutation list, find the index that will end up at 2020
part2 :: [Affine 119315717514047] -> Finite 119315717514047
part2 perms = runPerm (invert biiigPerm) 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm

-- | The permutation list for my advent of code account, parsed from
-- `myPuzzleInput`
myShuffles :: KnownNat n => [Affine n]
myShuffles = reverse (map parseAffine myPuzzleInput)

-- | The randomized puzzle input for my advent of code account
myPuzzleInput :: [String]
myPuzzleInput = lines
  [str|cut 181
      |deal with increment 61
      |cut -898
      |deal with increment 19
      |cut -1145
      |deal with increment 35
      |cut 3713
      |deal with increment 8
      |deal into new stack
      |cut -168
      |deal with increment 32
      |cut -3050
      |deal with increment 74
      |cut 7328
      |deal with increment 38
      |deal into new stack
      |deal with increment 11
      |cut 5419
      |deal with increment 34
      |cut 7206
      |deal with increment 53
      |cut 4573
      |deal into new stack
      |deal with increment 50
      |cut -1615
      |deal with increment 9
      |cut -4772
      |deal with increment 66
      |cut 9669
      |deal into new stack
      |deal with increment 2
      |cut 5003
      |deal with increment 46
      |cut -3368
      |deal into new stack
      |cut 1276
      |deal with increment 19
      |cut 530
      |deal with increment 57
      |cut 8914
      |deal with increment 41
      |cut -6173
      |deal with increment 44
      |cut -2173
      |deal with increment 55
      |deal into new stack
      |cut 5324
      |deal with increment 58
      |cut 592
      |deal with increment 17
      |cut 8744
      |deal with increment 10
      |cut -5679
      |deal into new stack
      |deal with increment 37
      |cut 1348
      |deal with increment 30
      |cut -8824
      |deal with increment 54
      |deal into new stack
      |cut -1263
      |deal with increment 29
      |deal into new stack
      |deal with increment 13
      |cut -9682
      |deal with increment 19
      |cut 8665
      |deal with increment 42
      |cut 3509
      |deal with increment 57
      |cut 7536
      |deal with increment 42
      |cut -1391
      |deal with increment 25
      |deal into new stack
      |deal with increment 49
      |cut 7942
      |deal with increment 49
      |cut -9595
      |deal with increment 59
      |cut 9964
      |deal with increment 22
      |deal into new stack
      |cut 5436
      |deal into new stack
      |cut 4605
      |deal into new stack
      |deal with increment 36
      |cut -2667
      |deal with increment 49
      |cut 4727
      |deal into new stack
      |cut 2236
      |deal with increment 66
      |cut 8098
      |deal into new stack
      |deal with increment 62
      |deal into new stack
      |deal with increment 70
      |cut -9110
      |]
  
