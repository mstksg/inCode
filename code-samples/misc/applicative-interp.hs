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
    { optString :: String
    , optHelp   :: String
    , optMeta   :: String
    , optRead   :: ReadM a
    }
  deriving Functor

data Flag :: Type -> Type where
    Flag
      :: { flagString :: String
         , flagHelp   :: String
         }
      -> Flag Bool

argSummary :: Arg a -> Summary a
argSummary Arg{..} = Const [ argName ++ ": " ++ argHelp ]

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
       help    argHelp
    <> metavar argName

optSummary :: Opt a -> Summary a
optSummary Opt{..} = Const
    [ "--" ++ optString ++ " " ++ optMeta ++ ": " ++ optHelp ]

optParser :: Opt a -> Parser a
optParser Opt{..} = option optRead $
       long optString
    <> help optHelp
    <> metavar optMeta

flagSummary :: Flag a -> Summary a
flagSummary Flag{..} = Const
    [ "--" ++ flagString ++ ": " ++ flagHelp ]

flagParser :: Flag a -> Parser a
flagParser Flag{..} = switch $
       long flagString
    <> help flagHelp

nameArg :: Arg String
nameArg = Arg
    { argName = "<name>"
    , argHelp = "A person's name"
    , argRead = str
    }

ageOpt :: Opt Int
ageOpt = Opt
    { optString = "age"
    , optHelp   = "A person's age"
    , optMeta   = "<int>"
    , optRead   = auto
    }

petsFlag :: Flag Bool
petsFlag = Flag
    { flagString = "pets"
    , flagHelp   = "Has pets"
    }

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
