module Main where

import Control.Monad             (guard, mfilter)
import Control.Monad.Trans.State
import Data.List                 (foldl')

select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

asNumber :: [Int] -> Int
asNumber = foldl' (\t o -> t*10 + o) 0

-- |
--     S E N D
-- +   M O R E
-- -----------
--   M O N E Y
--
main :: IO ()
main = print . flip evalStateT [0..9] $ do
    s <- StateT select
    e <- StateT select
    n <- StateT select
    d <- StateT select
    m <- StateT select
    o <- StateT select
    r <- StateT select
    y <- StateT select
    guard $ s /= 0 && m /= 0
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard $ send + more == money
    return (send, more, money)

-- faster than select, but doesn't preserve order
select' :: [a] -> [(a,[a])]
select' = go []
  where
   go xs [] = []
   go xs (y:ys) = (y,xs++ys) : go (y:xs) ys

-- faster; switch this with `main` to test
main' :: IO ()
main' = print . flip evalStateT [0..9] $ do
    s <- mfilter (/= 0) $ StateT select'
    m <- mfilter (/= 0) $ StateT select'
    e <- StateT select'
    n <- StateT select'
    d <- StateT select'
    o <- StateT select'
    r <- StateT select'
    y <- StateT select'
    let send  = asNumber [s,e,n,d]
        more  = asNumber [m,o,r,e]
        money = asNumber [m,o,n,e,y]
    guard $ send + more == money
    return (send, more, money)
