#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter --package functor-combinators-0.3.3.0 --package aeson --package vinyl-0.13.0 --package contravariant --package scientific --package text --package semigroupoids --package free --package invariant --package aeson-better-errors

{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE EmptyCase                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# OPTIONS_GHC -Wall                 #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import           Control.Monad
import           Data.Functor.Contravariant
import           Data.Functor.Invariant.TH
import           Data.Functor.Plus
import           Data.HFunctor
import           Data.HFunctor.Chain
import           Data.HFunctor.Interpret
import           Data.Scientific
import           GHC.Generics
import qualified Data.Aeson                              as Aeson
import qualified Data.Aeson.BetterErrors                 as A
import qualified Data.Aeson.Types                        as Aeson
import qualified Data.Functor.Invariant.Day              as ID
import qualified Data.Functor.Invariant.Night            as IN
import qualified Data.Text                               as T
import qualified Data.Text.Prettyprint.Doc               as PP

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }
  deriving Generic

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }
  deriving Generic

data Schema a =
      SumType     (IN.NightChain Choice a)
    | RecordType  (ID.DayChain Field a)
    | SchemaLeaf  (Primitive a)
  deriving Generic

data Primitive a =
      PString (a -> String)     (String     -> Maybe a)
    | PNumber (a -> Scientific) (Scientific -> Maybe a)
    | PBool   (a -> Bool)       (Bool       -> Maybe a)
  deriving Generic

deriveInvariant ''Choice
deriveInvariant ''Field
deriveInvariant ''Schema
deriveInvariant ''Primitive

pString :: Primitive String
pString = PString id Just

pInt :: Primitive Int
pInt = PNumber fromIntegral toBoundedInteger

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

mySchema :: Schema Customer
mySchema = SumType $
    swerve (\case CPerson x y -> Left (x,y); CBusiness x -> Right x) (uncurry CPerson) CBusiness
        (inject Choice
          { choiceName  = "Person"
          , choiceValue = RecordType $ gathered
              (inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString })
              (inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    })
          }
        )
        (inject Choice
          { choiceName  = "Business"
          , choiceValue = RecordType $
              inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt }
          }
        )

swerve
    :: (a -> Either b c)
    -> (b -> a)
    -> (c -> a)
    -> IN.NightChain f b
    -> IN.NightChain f c
    -> IN.NightChain f a
swerve f g h x y = appendChain (IN.Night x y f g h)

gather
    :: (a -> (b, c))
    -> (b -> c -> a)
    -> ID.DayChain f b
    -> ID.DayChain f c
    -> ID.DayChain f a
gather f g x y = appendChain (ID.Day x y f g)

gathered
    :: ID.DayChain f a
    -> ID.DayChain f b
    -> ID.DayChain f (a, b)
gathered = gather id (,)

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          icollect (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) (ID.chainAp fs)
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          icollect choiceDoc (IN.chainDec cs)
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

schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    SumType    cs -> getOp (IN.runContraNightChain choiceToValue cs)
    RecordType fs -> Aeson.object
                   . getOp (ID.runContraDayChain fieldToValue fs)
    SchemaLeaf p  -> primitiveToValue p
  where
    choiceToValue :: Choice x -> Op Aeson.Value x
    choiceToValue Choice{..} = Op $ \x -> Aeson.object
      [ "tag"      Aeson..= T.pack choiceName
      , "contents" Aeson..= schemaToValue choiceValue x
      ]
    fieldToValue :: Field x -> Op [Aeson.Pair] x
    fieldToValue Field{..} = Op $ \x ->
        [T.pack fieldName Aeson..= schemaToValue fieldValue x]
    primitiveToValue :: Primitive x -> x -> Aeson.Value
    primitiveToValue = \case
      PString f _ -> Aeson.String . T.pack . f
      PNumber f _ -> Aeson.Number . f
      PBool   f _ -> Aeson.Bool . f

parseSchema
    :: Schema a
    -> A.Parse String a
parseSchema = \case
    SumType    cs -> IN.runCoNightChain parseChoice cs
    RecordType fs -> ID.runCoDayChain parseField fs
    SchemaLeaf p  -> parsePrimitive p
  where
    parseChoice :: Choice b -> A.Parse String b
    parseChoice Choice{..} = do
      tag <- A.key "tag" A.asString
      unless (tag == choiceName) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ parseSchema choiceValue
    parseField :: Field b -> A.Parse String b
    parseField Field{..} = A.key (T.pack fieldName) (parseSchema fieldValue)
    parsePrimitive :: Primitive b -> A.Parse String b
    parsePrimitive = \case
      PString _ f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber _ f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool _ f -> A.withBool $
        maybe (Left "error validating bool") Right . f

testRoundTrip
    :: Schema a
    -> a
    -> Either (A.ParseError String) a
testRoundTrip sch = A.parseValue (parseSchema sch) . schemaToValue sch

main :: IO ()
main = pure ()

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance (Monad f) => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"
instance Monad m => Apply (A.ParseT e m) where
    (<.>) = (<*>)
