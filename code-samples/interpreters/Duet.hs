#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.3 --package MonadPrompt --package pointedlist --package lens --package type-combinators

{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

import           Control.Lens
import           Control.Monad.Fail
import           Control.Monad.Prompt
import           Control.Monad.State
import           Data.Char
import           Data.Kind
import           Data.Maybe
import           Data.Type.Disjunction
import qualified Data.List.PointedList as P
import qualified Data.Map              as M

type Addr = Either Char Int

data Op = OSnd Addr
        | ORcv Char
        | OJgz Addr Addr
        | OBin (Int -> Int -> Int) Char Addr

parseOp :: String -> Op
parseOp inp = case words inp of
    "snd":c    :_   -> OSnd (addr c)
    "set":(x:_):y:_ -> OBin (const id) x (addr y)
    "add":(x:_):y:_ -> OBin (+)        x (addr y)
    "mul":(x:_):y:_ -> OBin (*)        x (addr y)
    "mod":(x:_):y:_ -> OBin mod        x (addr y)
    "rcv":(x:_):_   -> ORcv x
    "jgz":x    :y:_ -> OJgz (addr x) (addr y)
    _               -> error "Bad parse"
  where
    addr :: String -> Addr
    addr [c] | isAlpha c = Left c
    addr str = Right (read str)

parseProgram :: String -> P.PointedList Op
parseProgram = fromJust . P.fromList . map parseOp . lines

data Mem :: Type -> Type where
    MGet :: Char -> Mem Int
    MSet :: Char -> Int -> Mem ()
    MJmp :: Int -> Mem ()
    MPk  :: Mem Op

data Com :: Type -> Type where
    CSnd :: Int -> Com ()
    CRcv :: Int -> Com Int

type Duet = Prompt (Mem :|: Com)

dGet :: Char -> Duet Int
dGet = prompt . L . MGet

dSet :: Char -> Int -> Duet ()
dSet r = prompt . L . MSet r

dJmp :: Int -> Duet ()
dJmp = prompt . L . MJmp

dPk :: Duet Op
dPk = prompt (L MPk)

dSnd :: Int -> Duet ()
dSnd = prompt . R . CSnd

dRcv :: Int -> Duet Int
dRcv = prompt . R . CRcv

stepProg :: Duet ()
stepProg = dPk >>= \case
    OSnd x -> do
      dSnd =<< addrVal x
      dJmp 1
    OBin f x y -> do
      yVal <- addrVal y
      xVal <- dGet    x
      dSet x $ f xVal yVal
      dJmp 1
    ORcv x -> do
      y <- dRcv =<< dGet x
      dSet x y
      dJmp 1
    OJgz x y -> do
      xVal <- addrVal x
      dJmp =<< if xVal > 0
                 then addrVal y
                 else return 1
  where
    addrVal (Left r ) = dGet r
    addrVal (Right x) = return x

data ProgState = PS { _psTape :: P.PointedList Op
                    , _psRegs :: M.Map Char Int
                    }
makeClassy ''ProgState

interpMem
    :: (MonadState s m, MonadFail m, HasProgState s)
    => Mem a
    -> m a
interpMem = \case
    MGet c   -> use (psRegs . at c . non 0)
    MSet c x -> psRegs . at c . non 0 .= x
    MJmp n   -> do
      Just t' <- P.moveN n <$> use psTape
      psTape .= t'
    MPk      -> use (psTape . P.focus)

main :: IO ()
main = putStrLn "hi"
