#! /usr/bin/env -S nix develop --command runghc -Wall

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Level2 () where

import Control.Exception
import Data.Type.Equality
import Control.Monad
import Data.Char
import Data.Kind
import Level0
import Level1
import Type.Reflection

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
  putStrLn "would you like to provide int or bool or string?"
  ans <- getLine
  case map toLower ans of
    "int" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    "bool" -> MkSomeList WitOrd <$> replicateM 3 (readLn @Int)
    "string" -> MkSomeList WitOrd <$> replicateM 3 getLine
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

-- | Behavior depends on what is given.
--
-- * If it's a list of Bools, returns if they are all True
-- * If it's a list of Ints, returns if their sum is greater than 50
-- * If it's a list of Doubles, returns if their sum is greater than 5.0
-- * If it's a list of Strings, returns if it contains a "hello"
processList :: SomeList TypeRep -> Bool
processList (MkSomeList tr xs)
    | Just Refl <- testEquality tr (typeRep @Bool)   = and xs
    | Just Refl <- testEquality tr (TypeRep @Int)    = sum xs > 50
    | Just Refl <- testEquality tr (TypeRep @Double) = sum xs > 5.0
    | Just Refl <- testEquality tr (TypeRep @String) = "hello" `elem` xs
    | otherwise = False

hlistToSomeList :: HList TypeRep -> Maybe (SomeList TypeRep)
hlistToSomeList = \case
  [] -> Nothing
  MkSigma tr x : xs -> MkSomeList tr . (x :) <$> traverse (castSigma tr) xs

someListToHList :: SomeList TypeRep -> HList TypeRep
someListToHList (MkSomeList tr xs) = MkSigma tr <$> xs

sigmaToHList :: Sigma TypeRep -> Maybe (SomeList TypeRep)
sigmaToHList (MkSigma tr xs) = do
  App tcon telem <- Just tr 
  Refl <- testEquality (typeRepKind telem) (typeRep @Type)
  Refl <- testEquality tcon (typeRep @[])
  pure $ MkSomeList telem xs

hlistToSigma :: SomeList TypeRep -> Sigma TypeRep
hlistToSigma (MkSomeList tr xs) = MkSigma (typeRep @[] `App` tr) xs

main :: IO ()
main = pure ()
