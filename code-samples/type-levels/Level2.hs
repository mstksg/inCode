#! /usr/bin/env -S nix develop --command runghc -Wall

module Level2 () where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Kind

data SomeList :: (Type -> Type) -> Type where
  MkSomeList :: p a -> [a] -> SomeList p

-- | An Ord counterpart for Showable
data Comparable :: Type -> Type where
  WitOrd :: Ord a => Comparable a

monotonic :: Ord a => [a] -> Bool
monotonic [] = True
monotonic (x : xs) = go x xs
  where
    go y [] = True
    go y (z : zs) = (y <= z) && go z zs

monotonicSomeList :: SomeList Comparable -> Bool
monotonicSomeList (MkSomeList WitOrd xs) = monotonic xs

getItems :: IO (SomeList Comparable)
getItems = do
  putStrLn "would you like to provide int or bool?"
  ans <- getLine
  case map toLower ans of
    "int" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    "bool" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    _ -> throwIO $ userError "no"

getAndAnalyze :: IO ()
getAndAnalyze = do
  MkSomeList WitOrd xs <- getItems
  putStrLn $ "Got " ++ show (length xs) ++ " items."
  let isMono = monotonic xs
      isRevMono = monotonic (reverse xs)
  when isMono $
    putStrLn "The items are monotonic."
  when (isMono && isRevMono) $ do
    putStrLn "The items are monotonic both directions."
    putStrLn "This means the items are all identical."

main :: IO ()
main = pure ()
