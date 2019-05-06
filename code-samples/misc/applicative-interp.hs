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

import           Control.Monad.Morph
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

data Arg a = Arg
    { argName  :: String
    , argHelp  :: String
    , argRead  :: ReadM a
    }
  deriving Functor

data OptType :: Type -> Type where
    OTRequired :: ReadM a -> OptType a
    OTOptional :: ReadM a -> Maybe (a -> String, a) -> OptType (Maybe a)
    OTSwitch   :: OptType Bool

data Opt a = Opt
    { optLong  :: String
    , optShort :: Maybe Char
    , optMeta  :: String
    , optHelp  :: String
    , optType  :: Coyoneda OptType a
    }
  deriving Functor

argParser :: Arg a -> Parser a
argParser Arg{..} = argument argRead $
        help    argHelp
     <> metavar argName


optParser :: Opt a -> Parser a
optParser Opt{..} = lowerCoyoneda $ hoistCoyoneda go optType
  where
    go :: OptType x -> Parser x
    go = \case
      OTRequired r   -> option r mods
      OTOptional r d -> optional $ option r $
            mods
         <> foldMap (\(f,x) -> value x <> showDefaultWith f) d
      OTSwitch       -> switch $ long optLong
                              <> foldMap short optShort
                              <> help optHelp
    mods :: Mod OptionFields x
    mods = long optLong
        <> foldMap short optShort
        <> help optHelp
        <> metavar optMeta

nameArg :: Arg String
nameArg = Arg
    { argName = "<name>"
    , argHelp = "A person's name"
    , argRead = str
    }

testParser :: Parser a -> String -> IO a
testParser p = handleParseResult
             . execParserPure defaultPrefs (info (p <**> helper) mempty)
             . words
