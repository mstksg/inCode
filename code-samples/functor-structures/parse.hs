#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter --package functor-combinators-0.3.5.1 --package aeson-better-errors --package vinyl-0.13.0 --package containers --package scientific --package text --package semigroupoids --package bytestring --package free

{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Control.Applicative.Free
import           Control.Applicative.ListF
import           Control.Monad
import           Data.ByteString.Lazy      (ByteString)
import           Data.Functor.Plus
import           Data.HFunctor
import           Data.HFunctor.Interpret
import           Data.List
import           Data.Scientific
import qualified Data.Aeson.BetterErrors   as A
import qualified Data.Map                  as M
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
              <$> inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString }
              <*> inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
        }
  <!> inject Choice
        { choiceName  = "Business"
        , choiceValue = RecordType $
            CBusiness
              <$> inject Field { fieldName = "Employees", fieldValue = SchemaLeaf pInt }
        }

schemaDoc
    :: String       -- ^ name
    -> Schema x     -- ^ schema
    -> PP.Doc a
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          icollect (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          icollect choiceDoc cs
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
      PString _ -> "string"
      PNumber _ -> "number"
      PBool   _ -> "bool"

instance Monad f => Alt (A.ParseT e f) where
    (<!>) = (A.<|>)
instance Monad f => Plus (A.ParseT String f) where
    zero  = A.throwCustomError "No options were validated"

type ErrType = String

schemaParser :: Schema a -> A.Parse ErrType a
schemaParser = \case
    SumType    cs -> interpret choiceParser cs
    RecordType fs -> interpret fieldParser fs
    SchemaLeaf p  -> primParser p

choiceParser :: Choice a -> A.Parse String a
choiceParser (Choice name val) = do
  tag <- A.key "tag" A.asString
  unless (tag == name) $
    A.throwCustomError "Tag does not match"
  A.key "contents" $ schemaParser val

fieldParser :: Field a -> A.Parse String a
fieldParser (Field name val) = A.key (T.pack name) (schemaParser val)

primParser :: Primitive a -> A.Parse String a
primParser = \case
  PString f -> A.withString $
    maybe (Left "error validating string") Right . f
  PNumber f -> A.withScientific $
    maybe (Left "error validating number") Right . f
  PBool f -> A.withBool $
    maybe (Left "error validating bool") Right . f

schemaParser2 :: Schema a -> A.Parse ErrType a
schemaParser2 = \case
    RecordType fs -> interpret fieldParser fs
    SumType    cs -> do
      let schemaMap = M.fromList 
            [ (nm, vl) | Choice nm vl <- runListF cs ]
      tag <- A.key "tag" A.asString
      case M.lookup tag schemaMap of
        Nothing -> A.throwCustomError $
                "tag " <> tag <> " not recognized: Expected one of "
             <> intercalate ", " (M.keys schemaMap)
        Just sc -> A.key "contents" (schemaParser2 sc)
    SchemaLeaf p  -> primParser p

parseSchema :: Schema a -> ByteString -> Either (A.ParseError ErrType) a
parseSchema sc = A.parse (schemaParser sc)

parseSchema2 :: Schema a -> ByteString -> Either (A.ParseError ErrType) a
parseSchema2 sc = A.parse (schemaParser2 sc)

main :: IO ()
main = pure ()
