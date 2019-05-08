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

data OptType :: Type -> Type where
    -- ^ Contains a "name" for the argument, and a reader
    OTRequired :: String -> ReadM a -> OptType a
    -- ^ Contains a "name" for the argument, and a reader
    OTOptional :: String -> ReadM a -> OptType (Maybe a)
    OTSwitch   :: OptType Bool

data Opt a = Opt
    { optFlag  :: String
    , optHelp  :: String
    , optType  :: Coyoneda OptType a    -- ^ Coyoneda so we can be a Functor
    }
  deriving Functor

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
        help    argHelp
     <> metavar argName

argSummary :: Arg a -> Summary a
argSummary Arg{..} = Const [ argName ++ ": " ++ argHelp ]

otRequired :: String -> ReadM a -> Coyoneda OptType a
otRequired n = liftCoyoneda . OTRequired n

otOptional :: String -> ReadM a -> Coyoneda OptType (Maybe a)
otOptional n = liftCoyoneda . OTOptional n

otSwitch :: Coyoneda OptType Bool
otSwitch = liftCoyoneda OTSwitch

optSummary :: forall a. Opt a -> Summary a
optSummary Opt{..} = lowerCoyoneda $ hoistCoyoneda go optType
  where
    go :: OptType x -> Summary x
    go = \case
      OTRequired n _ -> Const
        [ "--" ++ optFlag ++ " " ++ n ++ ": " ++ optHelp ]
      OTOptional n _ -> Const
        [ "[--" ++ optFlag ++ " " ++ n ++ "]: " ++ optHelp ]
      OTSwitch -> Const
        [ "[--" ++ optFlag ++ "]: " ++ optHelp ]

optParser :: Opt a -> Parser a
optParser Opt{..} = lowerCoyoneda $ hoistCoyoneda go optType
  where
    go :: OptType x -> Parser x
    go = \case
      OTRequired n r -> option r $
           long optFlag
        <> help optHelp
        <> metavar n
      OTOptional n r -> optional $ option r $
           long optFlag
        <> help optHelp
        <> metavar n
      OTSwitch       -> switch $
           long optFlag
        <> help optHelp

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
    , optType = otRequired "<int>" auto
    }

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
