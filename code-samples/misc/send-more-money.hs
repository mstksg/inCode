module Main where

import Control.Monad             (guard)
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

-- ghci> :l send-more-money
-- ghci> roulette
--
roulette :: [Int]
roulette = evalStateT (go 1) [True,False,False,False,False,False]
  where
    go :: Int -> StateT [Bool] [] Int
    go i = do
      shot <- StateT select
      if shot
        then return i
        else go $ i + 1

