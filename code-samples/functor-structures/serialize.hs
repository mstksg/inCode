#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package prettyprinter --package functor-combinators-0.3.6.0 --package aeson --package vinyl-0.13.0 --package contravariant --package scientific --package text --package semigroupoids --package free

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor.Contravariant
import           Data.Functor.Contravariant.Decide
import           Data.Functor.Contravariant.Divisible
import           Data.Functor.Contravariant.Divisible.Free
import           Data.HFunctor
import           Data.HFunctor.HTraversable
import           Data.HFunctor.Interpret
import           Data.Scientific
import           GHC.Generics
import qualified Data.Aeson                                as Aeson
import qualified Data.Aeson.Types                          as Aeson
import qualified Data.Text                                 as T
import qualified Data.Text.Prettyprint.Doc                 as PP

data Schema a =
      RecordType  (Div Field  a)
    | SumType     (Dec Choice a)
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
      PString (a -> String)
    | PNumber (a -> Scientific)
    | PBool   (a -> Bool)

instance Contravariant Choice where
    contramap f ch = ch
      { choiceValue = contramap f (choiceValue ch) }
instance Contravariant Field where
    contramap f fld = fld
      { fieldValue = contramap f (fieldValue fld) }
instance Contravariant Schema where
    contramap f = \case
      SumType    x -> SumType    (contramap f x)
      RecordType x -> RecordType (contramap f x)
      SchemaLeaf x -> SchemaLeaf (contramap f x)
instance Contravariant Primitive where
    contramap f = \case
      PString g -> PString (g . f)
      PNumber g -> PNumber (g . f)
      PBool   g -> PBool   (g . f)

pString :: Primitive String
pString = PString id

pInt :: Primitive Int
pInt = PNumber fromIntegral

pBool :: Primitive Bool
pBool = PBool id

data Customer =
      CPerson   { cpName :: String, cpAge :: Int }
    | CBusiness { cbEmployees :: Int }
  deriving Show

customerSchema :: Schema Customer
customerSchema = SumType $
    decide (\case CPerson x y -> Left (x, y); CBusiness x -> Right x)
      (inject Choice
        { choiceName = "Person"
        , choiceValue = RecordType $ divided
            (inject Field { fieldName = "Name", fieldValue = SchemaLeaf pString })
            (inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    })
        }
      )
      (inject Choice
        { choiceName  = "Business"
        , choiceValue = RecordType $
            inject Field { fieldName = "Age" , fieldValue = SchemaLeaf pInt    }
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
    fieldDoc (Field name val) = schemaDoc name val
    choiceDoc :: Choice x -> PP.Doc a
    choiceDoc (Choice name val) = schemaDoc name val
    primDoc :: Primitive x -> PP.Doc a
    primDoc = \case
      PString _ -> "string"
      PNumber _ -> "number"
      PBool   _ -> "bool"


schemaToValue
    :: Schema a
    -> a
    -> Aeson.Value
schemaToValue = \case
    SumType    cs -> getOp (interpret choiceToValue cs)
    RecordType fs -> Aeson.object . getOp (interpret fieldToValue fs)
    SchemaLeaf p  -> primToValue p

choiceToValue :: Choice a -> Op Aeson.Value a
choiceToValue (Choice name val) = Op $ \x -> Aeson.object
    [ "tag"      Aeson..= T.pack name
    , "contents" Aeson..= schemaToValue val x
    ]

fieldToValue :: Field a -> Op [Aeson.Pair] a
fieldToValue (Field name val) = Op $ \x ->
    [T.pack name Aeson..= schemaToValue val x]

primToValue :: Primitive a -> a -> Aeson.Value
primToValue = \case
  PString f -> \x -> Aeson.String (T.pack (f x))
  PNumber f -> \x -> Aeson.Number (f x)
  PBool   f -> \x -> Aeson.Bool   (f x)

main :: IO ()
main = pure ()
