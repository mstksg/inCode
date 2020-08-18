#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter --package functor-combinators-0.3.5.1 --package aeson --package vinyl-0.13.0 --package contravariant --package scientific --package text --package semigroupoids --package free --package invariant --package aeson-better-errors --package kan-extensions

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Monad
import           Data.Functor.Contravariant
import           Data.Functor.Invariant.DecAlt
import           Data.Functor.Invariant.DivAp
import           Data.Functor.Plus
import           Data.HFunctor
import           Data.HFunctor.Interpret
import           Data.Scientific
import qualified Data.Aeson                    as Aeson
import qualified Data.Aeson.BetterErrors       as A
import qualified Data.Aeson.Types              as Aeson
import qualified Data.Text                     as T
import qualified Data.Text.Prettyprint.Doc     as PP

data Schema a =
      RecordType  (DivAp  Field  a)
    | SumType     (DecAlt Choice a)
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

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

customerSchema :: Schema Customer
customerSchema = SumType $
    swerve (\case CPerson x y -> Left (x,y); CBusiness x -> Right x)
           (uncurry CPerson)
           CBusiness
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

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          icollect (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) (divApAp fs)
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          icollect choiceDoc (decAltDec cs)
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p
  where
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc (Field name val) = schemaDoc name val
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc (Choice name val) = schemaDoc name val
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
    RecordType fs -> Aeson.object
                   . getOp (runContraDivAp  fieldToValue  fs)
    SumType    cs -> getOp (runContraDecAlt choiceToValue cs)
    SchemaLeaf p  -> primToValue p
  where
    choiceToValue :: Choice x -> Op Aeson.Value x
    choiceToValue (Choice name val) = Op $ \x -> Aeson.object
      [ "tag"      Aeson..= T.pack name
      , "contents" Aeson..= schemaToValue val x
      ]
    fieldToValue :: Field x -> Op [Aeson.Pair] x
    fieldToValue (Field name val) = Op $ \x ->
        [T.pack name Aeson..= schemaToValue val x]
    primToValue :: Primitive x -> x -> Aeson.Value
    primToValue = \case
      PString f _ -> Aeson.String . T.pack . f
      PNumber f _ -> Aeson.Number . f
      PBool   f _ -> Aeson.Bool . f

schemaParser
    :: Schema a
    -> A.Parse String a
schemaParser = \case
    RecordType fs -> runCoDivAp  fieldParser  fs
    SumType    cs -> runCoDecAlt choiceParser cs
    SchemaLeaf p  -> primParser p
  where
    choiceParser :: Choice b -> A.Parse String b
    choiceParser (Choice name val) = do
      tag <- A.key "tag" A.asString
      unless (tag == name) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser val
    fieldParser :: Field b -> A.Parse String b
    fieldParser (Field name val) = A.key (T.pack name) (schemaParser val)
    primParser :: Primitive b -> A.Parse String b
    primParser = \case
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
testRoundTrip sch = A.parseValue (schemaParser sch) . schemaToValue sch

main :: IO ()
main = pure ()

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance (Monad f) => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"
instance Monad m => Apply (A.ParseT e m) where
    (<.>) = (<*>)
