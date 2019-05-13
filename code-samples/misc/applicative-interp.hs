#!/usr/bin/env stack
-- stack --install-ghc ghci --package free --package transformers --package optparse-applicative --package kan-extensions

{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

import           Data.Functor.Coyoneda
import           Data.Functor.Day
import           Data.Functor.Identity
import           Data.Functor.Kan.Lan
import           Data.Functor.Yoneda
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           Options.Applicative

type Summary = Const [String]

data Arg a = Arg
    { argName  :: String
    , argHelp  :: String
    , argRead  :: ReadM a
    }
  deriving Functor

data Opt a = Opt
    { optFlag  :: String
    , optHelp  :: String
    , optMeta  :: String
    , optRead  :: ReadM a
    }
  deriving Functor

data Switch :: Type -> Type where
    Switch
      :: { switchFlag :: String
         , switchHelp :: String
         }
      -> Switch Bool

argSummary :: Arg a -> Summary a
argSummary Arg{..} = Const [ argName ++ ": " ++ argHelp ]

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
       help    argHelp
    <> metavar argName

optSummary :: Opt a -> Summary a
optSummary Opt{..} = Const
    [ "--" ++ optFlag ++ " " ++ optMeta ++ ": " ++ optHelp ]

optParser :: Opt a -> Parser a
optParser Opt{..} = option optRead $
       long optFlag
    <> help optHelp
    <> metavar optMeta

switchSummary :: Switch a -> Summary a
switchSummary Switch{..} = Const
    [ "--" ++ switchFlag ++ ": " ++ switchHelp ]

switchParser :: Switch a -> Parser a
switchParser Switch{..} = switch $
       long switchFlag
    <> help switchHelp

nameArg :: Arg String
nameArg = Arg
    { argName = "<name>"
    , argHelp = "A person's name"
    , argRead = str
    }

ageOpt :: Opt Int
ageOpt = Opt
    { optFlag = "age"
    , optHelp = "A person's age"
    , optMeta = "<int>"
    , optRead = auto
    }

petsSwitch :: Switch Bool
petsSwitch = Switch
    { switchFlag = "pets"
    , switchHelp = "Has pets"
    }

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
