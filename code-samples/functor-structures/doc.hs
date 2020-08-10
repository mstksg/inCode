#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall          #-}

import qualified Data.Text.Prettyprint.Doc as PP

data Schema =
      RecordType  [Field]
    | SumType     [Choice]
    | SchemaLeaf  Primitive
  deriving Show

data Field = Field
    { fieldName  :: String
    , fieldValue :: Schema
    }
  deriving Show

data Choice = Choice
    { choiceName  :: String
    , choiceValue :: Schema
    }
  deriving Show

data Primitive =
      PString
    | PNumber
    | PBool
  deriving Show

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

customerSchema :: Schema
customerSchema = SumType
    [ Choice
        { choiceName  = "Person"
        , choiceValue = RecordType
            [ Field { fieldName = "Name", fieldValue = SchemaLeaf PString }
            , Field { fieldName = "Age" , fieldValue = SchemaLeaf PNumber }
            ]
        }
    , Choice
        { choiceName  = "Business"
        , choiceValue = RecordType
            [ Field { fieldName = "Employees", fieldValue = SchemaLeaf PNumber } ]
        }
    ]

schemaDoc
    :: String       -- ^ name
    -> Schema       -- ^ schema
    -> PP.Doc x
schemaDoc title = \case
    RecordType fs -> PP.vsep [
        PP.pretty ("{" <> title <> "}")
      , PP.indent 2 . PP.vsep $
          map (\fld -> "*" PP.<+> PP.indent 2 (fieldDoc fld)) fs
      ]
    SumType cs    -> PP.vsep [
        PP.pretty ("(" <> title <> ")")
      , "Choice of:"
      , PP.indent 2 . PP.vsep $
          map choiceDoc cs
      ]
    SchemaLeaf p  -> PP.pretty (title <> ":")
              PP.<+> primDoc p

fieldDoc :: Field -> PP.Doc x
fieldDoc Field{..} = schemaDoc fieldName fieldValue

choiceDoc :: Choice -> PP.Doc x
choiceDoc Choice{..} = schemaDoc choiceName choiceValue

primDoc :: Primitive -> PP.Doc x
primDoc = \case
  PString -> "string"
  PNumber -> "number"
  PBool   -> "bool"

main :: IO ()
main = pure ()

