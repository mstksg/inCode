#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package aeson-better-errors --package prettyprinter --package semigroupoids --package scientific --package text --package functor-combinators --package vinyl --package invariant --package contravariant --package free --package assoc --package bytestring

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Applicative
import           Control.Applicative.Free
import           Control.Applicative.ListF
import           Control.Monad
import           Data.ByteString.Lazy      (ByteString)
import           Data.Functor.Compose
import           Data.Functor.Plus
import           Data.HFunctor
import           Data.HFunctor.Interpret
import           Data.Scientific
import qualified Data.Aeson.BetterErrors   as A
import qualified Data.Text                 as T
import qualified Data.Text.Prettyprint.Doc as PP

data Schema a =
      RecordType  (Ap Field a)
    | SumType     (ListF Choice a)
    | SchemaLeaf  (Primitive a)
  deriving Functor

data Field a = Field
    { fieldName  :: String
    , fieldValue :: Schema a
    }
  deriving Functor

data Choice a = Choice
    { choiceName  :: String
    , choiceValue :: Schema a
    }
  deriving Functor

data Primitive a =
      PString (String -> Maybe a)
    | PNumber (Scientific -> Maybe a)
    | PBool   (Bool -> Maybe a)
  deriving Functor

pString :: Primitive String
pString = PString Just

pInt :: Primitive Int
pInt = PNumber toBoundedInteger

pBool :: Primitive Bool
pBool = PBool Just

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

customerSchema :: Schema Customer
customerSchema = SumType $
      inject Choice
        { choiceName  = "Person"
        , choiceValue = RecordType $
            CPerson
              <$> liftAp Field { fieldName = "Name", fieldValue = SchemaLeaf pString }
              <*> liftAp Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
  <!> inject Choice
        { choiceName  = "Business"
        , choiceValue = RecordType $
            CBusiness
              <$> liftAp Field { fieldName = "Employees", fieldValue = SchemaLeaf pInt }
        }

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    SumType cs -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep . map choiceDoc . runListF $ cs
      ]
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep . getConst $
            runAp (Const . (:[]) . ("*" PP.<+>) . PP.indent 2 . fieldDoc) fs
      ]
    SchemaLeaf p ->
              PP.pretty (title <> ":")
        PP.<+> primDoc p
  where
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc Choice{..} = schemaDoc choiceName choiceValue
    fieldDoc :: Field x -> PP.Doc a
    fieldDoc Field{..} = schemaDoc fieldName fieldValue
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ -> "string"
      PNumber _ -> "number"
      PBool   _ -> "bool"

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance (Monad f) => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"
instance Monad m => Apply (A.ParseT e m) where
    (<.>) = (<*>)

type ErrType = String

schemaParser
    :: Schema a
    -> A.Parse ErrType a
schemaParser = \case
    RecordType fs -> interpret fieldParser fs
    SumType    cs -> interpret choiceParser cs
    SchemaLeaf p  -> primitiveParser p
  where
    fieldParser :: Field b -> A.Parse String b
    fieldParser Field{..} = A.key (T.pack fieldName) (schemaParser fieldValue)
    choiceParser :: Choice b -> A.Parse String b
    choiceParser Choice{..} = do
      tag <- A.key "tag" A.asString
      unless (tag == choiceName) $
        A.throwCustomError "Tag does not match"
      A.key "contents" $ schemaParser choiceValue
    primitiveParser :: Primitive b -> A.Parse String b
    primitiveParser = \case
      PString f -> A.withString $
        maybe (Left "error validating string") Right . f
      PNumber f -> A.withScientific $
        maybe (Left "error validating number") Right . f
      PBool f -> A.withBool $
        maybe (Left "error validating bool") Right . f

parseSchema :: Schema a -> ByteString -> Either (A.ParseError ErrType) a
parseSchema sc = A.parse (schemaParser sc)

main :: IO ()
main = pure ()
