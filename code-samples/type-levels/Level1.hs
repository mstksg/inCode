#! /usr/bin/env -S nix develop --command runghc -Wall

module Level1 (HList) where

import Data.Kind
import Data.Maybe
import Data.Type.Equality
import Level0
import Type.Reflection

-- | We can define this in terms of Haskell's built-in list
type HList p = [Sigma p]

data Method = HTTP | HTTPS

indexHList :: Int -> HList p -> Maybe (Sigma p)
indexHList 0 [] = Nothing
indexHList 0 (x : _) = Just x
indexHList n (_ : xs) = indexHList (n - 1) xs

-- | Expects a String, an Int, then a Method.
mkConnection :: HList TypeRep -> IO ()
mkConnection args = pure () -- something with host, port, and method
  where
    host :: Maybe String
    host = castSigma' =<< indexHList 0 args
    port :: Maybe Int
    port = castSigma' =<< indexHList 1 args
    method :: Maybe Method
    method = castSigma' =<< indexHList 2 args

findValueOfType :: Typeable a => HList TypeRep -> Maybe a
findValueOfType = listToMaybe . mapMaybe castSigma'

-- | Expects a String, an Int, then a Method, in any order.
mkConnectionAnyOrder :: HList TypeRep -> IO ()
mkConnectionAnyOrder args = pure ()
  where
    host :: Maybe String
    host = findValueOfType args
    port :: Maybe Int
    port = findValueOfType args
    method :: Maybe Method
    method = findValueOfType args


showableStuff :: HList Showable
showableStuff = [MkSigma WitShowable (1 :: Int), MkSigma WitShowable True]

showAll :: HList Showable -> [String]
showAll = map showSigma
  where
    showSigma (MkSigma WitShowable x) = show x

sigmaToHList :: Sigma TypeRep -> Maybe (HList TypeRep)
sigmaToHList = castSigma'

hlistToSigma :: HList TypeRep -> Sigma TypeRep
hlistToSigma = MkSigma typeRep

main :: IO ()
main = pure ()
