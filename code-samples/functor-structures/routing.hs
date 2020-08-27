#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter --package functor-combinators-0.3.6.0 --package aeson --package vinyl-0.13.0 --package contravariant --package scientific --package text --package semigroupoids --package free --package invariant --package aeson-better-errors --package kan-extensions

{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import           Control.Applicative.Free
import           Control.Monad
import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Decide
import           Data.Functor.Contravariant.Divisible.Free
import           Data.Functor.Plus
import           Data.HFunctor
import           Data.HFunctor.HTraversable
import           Data.HFunctor.Interpret
import           Data.HFunctor.Route
import           Data.Scientific
import qualified Data.Aeson                                as Aeson
import qualified Data.Aeson.BetterErrors                   as A
import qualified Data.Aeson.Types                          as Aeson
import qualified Data.Text                                 as T
import qualified Data.Text.Prettyprint.Doc                 as PP

data Schema a =
      RecordType  (PreT  Ap  Field  a)
    | SumType     (PostT Dec Choice a)
    | SchemaLeaf  (Primitive a)

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }

data Primitive a =
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)

pString :: Primitive String
pString = PString id Just

pInt :: Primitive Int
pInt = PNumber fromIntegral toBoundedInteger

pBool :: Primitive Bool
pBool = PBool id Just

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

customerSchema :: Schema Customer
customerSchema = SumType . PostT $
    decide (\case CPerson x y -> Left (x, y); CBusiness x -> Right x)
      (injectPost (uncurry CPerson) Choice
        { choiceName = "Person"
        , choiceValue = RecordType . PreT $ (,)
            <$> injectPre fst Field { fieldName = "Name", fieldValue = SchemaLeaf pString }
            <*> injectPre snd Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
      )
      (injectPost CBusiness         Choice
        { choiceName = "Person"
        , choiceValue = RecordType . inject $
            Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
      )

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          htoList (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          htoList choiceDoc cs
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
  where
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc Field{..} = schemaDoc fieldName fieldValue
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc Choice{..} = schemaDoc choiceName choiceValue
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ _ -> "string"
      PNumber _ _ -> "number"
      PBool   _ _ -> "bool"

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance Monad f => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"

type ErrType = String

schemaParser :: Schema a -> A.Parse ErrType a
schemaParser = \case
    RecordType fs -> interpret fieldParser fs
    SumType    cs -> postPlusT choiceParser cs
    SchemaLeaf p  -> primParser p
  where
    fieldParser :: Field a -> A.Parse String a
    fieldParser Field{..} = A.key (T.pack fieldName) (schemaParser fieldValue)
    choiceParser :: Choice a -> A.Parse String a
    choiceParser Choice{..} = do
      tag <- A.key "tag" A.asString
      unless (tag == choiceName) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser choiceValue
    primParser :: Primitive a -> A.Parse String a
    primParser = \case
      PString _ f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber _ f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool _ f -> A.withBool $
        maybe (Left "error validating bool") Right . f

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    RecordType fs -> Aeson.object . getOp (preDivisibleT fieldToValue fs)
    SumType    cs -> getOp (interpret choiceToValue cs)
    SchemaLeaf p  -> primToValue p
  where
    fieldToValue :: Field a -> Op [Aeson.Pair] a
    fieldToValue Field{..} = Op $ \x ->
        [T.pack fieldName Aeson..= schemaToValue fieldValue x]
    choiceToValue :: Choice a -> Op Aeson.Value a
    choiceToValue Choice{..} = Op $ \x -> Aeson.object
        [ "tag"      Aeson..= T.pack choiceName
        , "contents" Aeson..= schemaToValue choiceValue x
        ]
    primToValue :: Primitive a -> a -> Aeson.Value
    primToValue = \case
      PString f _ -> \x -> Aeson.String (T.pack (f x))
      PNumber f _ -> \x -> Aeson.Number (f x)
      PBool   f _ -> \x -> Aeson.Bool   (f x)

testRoundTrip
    :: Schema a
    -> a
    -> Either (A.ParseError String) a
testRoundTrip sch = A.parseValue (schemaParser sch) . schemaToValue sch

main :: IO ()
main = pure ()

