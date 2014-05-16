{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- Load it up inside GHCI and give follow along with the post!
--
-- λ: :l maybe.hs
--
-- http://blog.jle.im/entry/inside-my-world

module InsideMaybe where

import Data.Maybe
import Control.Applicative ((<$>))

-- | Various functions used in the post

-- divideMaybe: Takes two integers and returns -- possibly -- their integer
--      quotient. It succeeds if the denominator is not zero, and fails if
--      it is.
divideMaybe :: Int -> Int -> Maybe Int
divideMaybe _ 0 = Nothing
divideMaybe x y = Just (x `div` y)

-- headMaybe: Takes a list and returns -- possibly -- its first element.
--      Fails if the list is empty, and succeeds with the first element
--      otherwise.
headMaybe :: [a] -> Maybe a
headMaybe []    = Nothing
headMaybe (x:_) = Just x

-- halveMaybe: Takes an integer and returns -- possibly -- its half.  Fails
--      if it is an odd number.
halveMaybe :: Int -> Maybe Int
halveMaybe x | x `mod` 2 == 0 = Just (x `div` 2)
             | otherwise      = Nothing

addThree :: Int -> Int
addThree = (+ 3)

square :: Int -> Int
square = (^ 2)

showInt :: Int -> String
showInt = show

-- you can type ID's by just typing 1, 2, 3, etc.
newtype ID  = ID Int
            deriving (Show, Num, Eq, Ord, Integral, Real, Enum)

data Person = John | Sarah | Susan
            deriving (Show, Eq, Enum, Bounded)

age :: Person -> Int
age John  = 27
age Sarah = 32
age Susan = 25

personFromId :: ID -> Maybe Person
personFromId (ID i) | i <= 3 && i > 0 = Just (toEnum (i-1))
                    | otherwise       = Nothing

ageFromId :: ID -> Maybe Int
ageFromId i = (inMaybe age) (personFromId i)

halfOfAge :: ID -> Maybe Int
halfOfAge i = (liftInput halveMaybe) (ageFromId i)

-- | The Maybe utility functions we defined in the post

certaintify :: Maybe a -> a
certaintify (Just x) = x
certaintify Nothing  = error "Nothing was there, you fool!"

certaintifyWithDefault :: a -> Maybe a -> a
certaintifyWithDefault _ (Just x) = x
certaintifyWithDefault d Nothing  = d

inMaybe :: (a -> b) -> (Maybe a -> Maybe b)
inMaybe f = liftedF
  where
    liftedF (Just x) = Just (f x)
    liftedF Nothing  = Nothing

liftInput :: (a -> Maybe b) -> (Maybe a -> Maybe b)
liftInput f = liftedF
  where
    liftedF Nothing  = Nothing
    liftedF (Just x) = f x

