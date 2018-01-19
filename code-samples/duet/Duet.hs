#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-10.3 --package operational --package pointedlist --package lens --package type-combinators --package transformers-0.5.5.0 --package mtl --package containers -- -Wall

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonadFailDesugaring    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Fail
import           Control.Monad.Operational
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Char
import           Data.Kind
import           Data.Maybe
import           Data.Type.Disjunction
import qualified Control.Monad.Trans.Accum as A
import qualified Data.List.PointedList     as P
import qualified Data.Map                  as M

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
    MJmp :: Int  -> Mem ()
    MPk  :: Mem Op

data Com :: Type -> Type where
    CSnd :: Int -> Com ()
    CRcv :: Int -> Com Int

type Duet = Program (Mem :|: Com)

dGet :: Char -> Duet Int
dGet = singleton . L . MGet

dSet :: Char -> Int -> Duet ()
dSet r = singleton . L . MSet r

dJmp :: Int -> Duet ()
dJmp = singleton . L . MJmp

dPk :: Duet Op
dPk = singleton (L MPk)

dSnd :: Int -> Duet ()
dSnd = singleton . R . CSnd

dRcv :: Int -> Duet Int
dRcv = singleton . R . CRcv

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

data ProgState = PS
    { _psTape :: P.PointedList Op
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

class (Monad m, Monoid w) => MonadAccum w m | m -> w where
    add  :: w -> m ()
    look :: m w

interpComA
    :: (MonadAccum (Last Int) m, MonadWriter (First Int) m)
    => Com a
    -> m a
interpComA = \case
    CSnd x ->
      add (Last (Just x))
    CRcv x -> do
      unless (x == 0) $ do      -- don't rcv if the register parameter is 0
        Last lastSent <- look
        tell (First lastSent)
      return x

stepA :: MaybeT (StateT ProgState (WriterT (First Int) (A.Accum (Last Int)))) ()
stepA = interpretWithMonad (interpMem >|< interpComA) stepProg

partA :: P.PointedList Op -> Maybe Int
partA ops = getFirst
          . flip A.evalAccum mempty
          . execWriterT
          . flip runStateT (PS ops M.empty)
          . runMaybeT
          $ many stepA

data Thread = T
    { _tState   :: ProgState
    , _tBuffer  :: [Int]
    }
makeClassy ''Thread

instance HasProgState Thread where
    progState = tState

interpComB
    :: (MonadWriter [Int] m, MonadFail m, MonadState Thread m)
    => Com a
    -> m a
interpComB = \case
    CSnd x -> tell [x]
    CRcv _ -> do
      x:xs <- use tBuffer
      tBuffer .= xs
      return x

stepB :: MaybeT (State (Thread, Thread)) Int
stepB = do
    outA <- execWriterT . zoom _1 $
      many $ interpretWithMonad (interpMem >|< interpComB) stepProg
    outB <- execWriterT . zoom _2 $
      many $ interpretWithMonad (interpMem >|< interpComB) stepProg
    _1 . tBuffer .= outB
    _2 . tBuffer .= outA
    guard . not $ null outA && null outB
    return $ length outB

partB :: P.PointedList Op -> Int
partB ops = maybe (error "`many` cannot fail") sum
          . flip evalState s0
          . runMaybeT
          $ many stepB
  where
    s0 = ( T (PS ops (M.singleton 'p' 0)) []
         , T (PS ops (M.singleton 'p' 1)) []
         )

main :: IO ()
main = do
    print $ partA (parseProgram testProg)
    print $ partB (parseProgram testProg)

testProg :: String
testProg = unlines
    [ "set i 31"
    , "set a 1"
    , "mul p 17"
    , "jgz p p"
    , "mul a 2"
    , "add i -1"
    , "jgz i -2"
    , "add a -1"
    , "set i 127"
    , "set p 826"
    , "mul p 8505"
    , "mod p a"
    , "mul p 129749"
    , "add p 12345"
    , "mod p a"
    , "set b p"
    , "mod b 10000"
    , "snd b"
    , "add i -1"
    , "jgz i -9"
    , "jgz a 3"
    , "rcv b"
    , "jgz b -1"
    , "set f 0"
    , "set i 126"
    , "rcv a"
    , "rcv b"
    , "set p a"
    , "mul p -1"
    , "add p b"
    , "jgz p 4"
    , "snd a"
    , "set a b"
    , "jgz 1 3"
    , "snd b"
    , "set f 1"
    , "add i -1"
    , "jgz i -11"
    , "snd a"
    , "jgz f -16"
    , "jgz a -19"
    ]

instance (Monoid w, Monad m) => MonadAccum w (A.AccumT w m) where
    add = A.add
    look = A.look

instance MonadAccum w m => MonadAccum w (MaybeT m) where
    add = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (StateT s m) where
    add = lift . add
    look = lift look

instance (Monoid v, MonadAccum w m) => MonadAccum w (WriterT v m) where
    add = lift . add
    look = lift look
