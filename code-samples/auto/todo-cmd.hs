-- |
--
-- Recommended to run with cabal sandboxes:
--
-- $ cabal sandbox init
-- $ cabal install auto
-- $ cabal exec runghc todo-cmd.hs
-- 
module Main where

import Control.Auto
import Control.Auto.Interval
import Control.Monad
import Data.IntMap           (IntMap)
import Data.Maybe
import Prelude hiding        ((.), id)
import Text.Read
import Todo
import qualified Data.IntMap as IM

-- | Parse a string input.
parseInp :: String -> Maybe TodoInp
parseInp = p . words
  where
    p ("A":xs)   = Just (IAdd (unwords xs))
    p ("D":n:_)  = onId n CDelete
    p ("C":n:_)  = onId n (CComplete True)
    p ("U":n:_)  = onId n (CComplete False)
    p ("P":n:_)  = onId n CPrune
    p ("M":n:xs) = onId n (CModify (unwords xs))
    p _          = Nothing

    onId :: String -> TaskCmd -> Maybe TodoInp
    onId "*" te = Just (IAll te)
    onId n   te = (`ITask` te) <$> readMaybe n

-- | Just for command line testing use, turning the IntMap into a String.
formatTodo :: IntMap Task -> String
formatTodo = unlines . map format . IM.toList
  where
    format (n, Task desc compl) = concat [ show n
                                         , ". ["
                                         , if compl then "X" else " "
                                         , "] "
                                         , desc
                                         ]

main :: IO ()
main = do
    putStrLn "Enter command! 'A descr' or '[D/C/U/P/M] [id/*]'"
    void . interactAuto $ -- interactAuto takes an Interval; run forever
                          toOn
                          -- default value on bad command
                        . fromBlips "Bad command!"
                          -- run `formatTodo <$> todoApp` on emitted commands
                        . perBlip (formatTodo <$> todoApp)
                          -- emit when input is parseable
                        . emitJusts parseInp
