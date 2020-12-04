#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package free --package functor-combinators --package vinyl

{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}

import           Control.Monad.Free.TH
import           Control.Monad.Free

data PipeF i o a where
    YieldF :: o -> PipeF i o ()
    AwaitF :: PipeF i o i
    
      -- YieldF o a
    -- | AwaitF (Maybe i -> a)
  -- deriving Functor
    
-- makeFree ''PipeF
               
type Pipe i o = Free (PipeF i o)

comp :: Pipe b c y -> Pipe a b x -> Pipe a c y
comp = \case
    Pure x          -> \_ -> Pure x
    Free (YieldF o) -> \u -> Free $ YieldF _
    -- $ YieldF o (comp x u)
    -- Free (AwaitF f  ) -> \case
    --   Pure y            -> comp (f Nothing ) (Pure y)
    --   Free (YieldF o y) -> comp (f (Just o)) y
    --   Free (AwaitF g  ) -> Free $ AwaitF (comp (Free (AwaitF f)) . g)

-- comp :: Pipe b c y -> Pipe a b x -> Pipe a c y
-- comp = \case
--     Pure x            -> \_ -> Pure x
--     Free (YieldF o x) -> \u -> Free $ YieldF o (comp x u)
--     Free (AwaitF f  ) -> \case
--       Pure y            -> comp (f Nothing ) (Pure y)
--       Free (YieldF o y) -> comp (f (Just o)) y
--       Free (AwaitF g  ) -> Free $ AwaitF (comp (Free (AwaitF f)) . g)

-- comp :: Pipe b c y -> Pipe a b x -> Pipe a c y
-- comp = \case
--     Pure x            -> \_ -> Pure x
--     Free (YieldF o x) -> \u -> Free $ YieldF o (comp x u)
--     Free (AwaitF f  ) -> \case
--       Pure y            -> comp (f Nothing ) (Pure y)
--       Free (YieldF o y) -> comp (f (Just o)) y
--       Free (AwaitF g  ) -> Free $ AwaitF (comp (Free (AwaitF f)) . g)

(.|) :: Pipe a b c -> Pipe b c y -> Pipe a c y
f .| g = comp g f


main :: IO ()
main = pure ()
