{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ExprStage3 where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality
import GHC.TypeLits (KnownSymbol, Symbol, sameSymbol, symbolVal)
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

type data Ty
  = TInt
  | TBool
  | TString
  | TRecord [(Symbol, Ty)]
  | TSum [(Symbol, Ty)]
  | Ty :-> Ty

infixr 0 :->

infixr 5 :&

data Rec :: (k -> Type) -> [k] -> Type where
  RNil :: Rec f '[]
  (:&) :: f x -> Rec f xs -> Rec f (x : xs)

data Index :: [k] -> k -> Type where
  IZ :: Index (x : xs) x
  IS :: Index xs x -> Index (y : xs) x

class ListIx (l :: Symbol) (xs :: [(Symbol, Ty)]) (a :: Ty) | l xs -> a where
  listIx :: Index xs (l ::: a)

instance ListIx l (l ::: a : xs) a where
  listIx = IZ

instance {-# OVERLAPPABLE #-} ListIx l xs a => ListIx l (m ::: b : xs) a where
  listIx = IS (listIx @l)

indexRec :: Index xs x -> Rec f xs -> f x
indexRec = \case
  IZ -> \(x :& _) -> x
  IS i -> \(_ :& xs) -> indexRec i xs

traverseRec :: Applicative m => (forall x. f x -> m (g x)) -> Rec f xs -> m (Rec g xs)
traverseRec _ RNil = pure RNil
traverseRec f (x :& xs) = (:&) <$> f x <*> traverseRec f xs

type (:::) l a = '(l, a)

infixr 6 :::

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
  STRecord :: Rec STyField as -> STy (TRecord as)
  STSum :: Rec STyField as -> STy (TSum as)
  STFun :: STy a -> STy b -> STy (a :-> b)

data STyField :: (Symbol, Ty) -> Type where
  STyField :: KnownSymbol l => STy a -> STyField (l ::: a)

sField :: forall l a. KnownSymbol l => STy a -> STyField (l ::: a)
sField = STyField

data Prim :: Ty -> Type where
  PInt :: Int -> Prim TInt
  PBool :: Bool -> Prim TBool
  PString :: String -> Prim TString

data Op :: Ty -> Ty -> Ty -> Type where
  OPlus :: Op TInt TInt TInt
  OTimes :: Op TInt TInt TInt
  OLte :: Op TInt TInt TBool
  OAnd :: Op TBool TBool TBool

data Expr :: Ty -> Type where
  EPrim :: Prim t -> Expr t
  EVar :: STy t -> String -> Expr t
  ELambda :: STy a -> String -> Expr b -> Expr (a :-> b)
  EApply :: Expr (a :-> b) -> Expr a -> Expr b
  EOp :: Op a b c -> Expr a -> Expr b -> Expr c
  ERecord :: Rec ExprField as -> Expr (TRecord as)
  EAccess :: KnownSymbol l => Expr (TRecord as) -> Index as (l ::: a) -> Expr a
  EChoice :: KnownSymbol l => Index as (l ::: a) -> Expr a -> Expr (TSum as)
  ECase :: Expr (TSum as) -> Rec (ExprHandler b) as -> Expr b

data ExprField :: (Symbol, Ty) -> Type where
  EField :: KnownSymbol l => Expr a -> ExprField (l ::: a)

data ExprHandler :: Ty -> (Symbol, Ty) -> Type where
  EHandler :: KnownSymbol l => STy a -> String -> Expr b -> ExprHandler b (l ::: a)

eField :: forall l -> KnownSymbol l => Expr a -> ExprField (l ::: a)
eField l = EField @l

eAccess ::
  forall (l :: Symbol) ->
  (KnownSymbol l, ListIx l as a) =>
  Expr (TRecord as) ->
  Expr a
eAccess l e = EAccess @l e (listIx @l)

eChoice ::
  forall (l :: Symbol) ->
  (KnownSymbol l, ListIx l as a) =>
  Expr a ->
  Expr (TSum as)
eChoice l e = EChoice @l (listIx @l) e

eHandler :: forall l -> KnownSymbol l => STy a -> String -> Expr b -> ExprHandler b (l ::: a)
eHandler l = EHandler @l

fifteen :: Expr TInt
fifteen =
  EApply
    (ELambda STInt "x" (EOp OTimes (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))

makeRecordExample :: Expr (TRecord ["value" ::: TInt, "label" ::: TString])
makeRecordExample =
  ERecord
    ( eField "value" (EPrim (PInt 7))
        :& eField "label" (EPrim (PString "found"))
        :& RNil
    ) ::
    Expr (TRecord '["value" ::: TInt, "label" ::: TString])

recordExample :: Expr TInt
recordExample = EOp OPlus (EAccess @"value" makeRecordExample IZ) (EPrim (PInt 1))

autoRecordExample :: Expr TInt
autoRecordExample =
  EOp
    OPlus
    ( eAccess
        "value"
        ( ERecord
            ( eField "value" (EPrim (PInt 7))
                :& eField "label" (EPrim (PString "found"))
                :& RNil
            ) ::
            Expr (TRecord '["value" ::: TInt, "label" ::: TString])
        )
    )
    (EPrim (PInt 1))

namedAccessExample :: Expr TString
namedAccessExample =
  eAccess
    "label"
    ( ERecord
        ( eField "value" (EPrim (PInt 7))
            :& eField "label" (EPrim (PString "found"))
            :& RNil
        ) ::
        Expr (TRecord '["value" ::: TInt, "label" ::: TString])
    )

namedChoiceExample :: Expr (TSum '["Found" ::: TInt, "Missing" ::: TString])
namedChoiceExample =
  eChoice "Missing" (EPrim (PString "not here"))

makeSumExample :: Expr (TSum '["Found" ::: TInt, "Missing" ::: TString])
makeSumExample =
  EChoice @"Found" IZ (EPrim (PInt 7)) ::
    Expr (TSum '["Found" ::: TInt, "Missing" ::: TString])

sumExample :: Expr TInt
sumExample =
  ECase
    makeSumExample
    ( eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
        :& eHandler "Missing" STString "message" (EPrim (PInt 0))
        :& RNil
    )

autoSumExample :: Expr TInt
autoSumExample =
  ECase
    (eChoice "Found" (EPrim (PInt 7)) :: Expr (TSum '["Found" ::: TInt, "Missing" ::: TString]))
    ( eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
        :& eHandler "Missing" STString "message" (EPrim (PInt 0))
        :& RNil
    )

badCaseBranchExample :: Expr TInt
badCaseBranchExample =
  ECase
    (EChoice @"Found" IZ (EPrim (PInt 7)) :: Expr (TSum '["Found" ::: TInt, "Missing" ::: TString]))
    ( eHandler "Found" STInt "value" (EOp OPlus (EVar STInt "missing") (EPrim (PInt 1)))
        :& eHandler "Missing" STString "message" (EOp OPlus (EVar STInt "message") (EPrim (PInt 1)))
        :& RNil
    )

badCaseBranchResult :: Maybe String
badCaseBranchResult =
  showEValue <$> eval M.empty badCaseBranchExample

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Rec EValueField as -> EValue (TRecord as)
  EVSum :: Index as (l ::: a) -> EValue a -> EValue (TSum as)
  EVFun :: (EValue a -> Maybe (EValue b)) -> EValue (a :-> b)

data EValueField :: (Symbol, Ty) -> Type where
  EVField :: EValue a -> EValueField (l ::: a)

data SomeValue = forall t. SomeValue (STy t) (EValue t)

showEValue :: EValue t -> String
showEValue = \case
  EVInt n -> show n
  EVBool b -> show b
  EVString s -> show s
  EVRecord _ -> "<record>"
  EVSum _ _ -> "<sum>"
  EVFun _ -> "<function>"

prettyExpr :: Expr t -> PP.Doc ann
prettyExpr = ppExpr False

ppExpr :: Bool -> Expr t -> PP.Doc ann
ppExpr paren = \case
  EPrim p -> ppPrim p
  EVar _ v -> PP.pretty v
  ELambda _ n body ->
    wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
  EApply f x ->
    wrap $ ppExpr True f <+> ppExpr True x
  EOp o x y ->
    wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
  ERecord xs ->
    PP.encloseSep "{ " " }" ", " (ppFields xs)
  EAccess @l e _ ->
    ppExpr True e <> "." <> PP.pretty (symbolVal (Proxy @l))
  EChoice @l _ x ->
    wrap $ PP.pretty (symbolVal (Proxy @l)) <+> ppExpr True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr False x <+> "of"
        , PP.encloseSep "{ " " }" "; " (ppHandlers hs)
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

ppFields :: Rec ExprField xs -> [PP.Doc ann]
ppFields RNil = []
ppFields (EField @l x :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> "=" <+> ppExpr False x) : ppFields xs

ppHandlers :: Rec (ExprHandler b) xs -> [PP.Doc ann]
ppHandlers RNil = []
ppHandlers (EHandler @l _ n body :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> PP.pretty n <+> "->" <+> ppExpr False body) : ppHandlers xs

ppPrim :: Prim t -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool b -> if b then "true" else "false"
  PString s -> PP.pretty (show s)

ppOp :: Op a b c -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

instance TestEquality STy where
    testEquality = sameTy

instance TestEquality STyField where
    testEquality = sameField

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
  STRecord as -> \case
    STRecord bs -> do
      Refl <- sameFields as bs
      Just Refl
    _ -> Nothing
  STSum as -> \case
    STSum bs -> do
      Refl <- sameFields as bs
      Just Refl
    _ -> Nothing
  STFun a b -> \case
    STFun c d -> do
      Refl <- sameTy a c
      Refl <- sameTy b d
      Just Refl
    _ -> Nothing

sameField :: STyField x -> STyField y -> Maybe (x :~: y)
sameField (STyField @l tx) (STyField @m ty) = do
    Refl <- sameSymbol (Proxy @l) (Proxy @m)
    Refl <- sameTy tx ty
    Just Refl

sameFields :: Rec STyField xs -> Rec STyField ys -> Maybe (xs :~: ys)
sameFields RNil RNil = Just Refl
sameFields (x :& xs) (y :& ys) = do
  Refl <- sameField x y
  Refl <- sameFields xs ys
  Just Refl
sameFields _ _ = Nothing

eval :: Map String SomeValue -> Expr t -> Maybe (EValue t)
eval env = \case
  EPrim (PInt n) -> pure (EVInt n)
  EPrim (PBool b) -> pure (EVBool b)
  EPrim (PString s) -> pure (EVString s)
  EVar t v -> do
    SomeValue t' v' <- M.lookup v env
    Refl <- sameTy t t'
    pure v'
  ELambda ta n body ->
    pure $ EVFun $ \x -> eval (M.insert n (SomeValue ta x) env) body
  EApply f x -> do
    EVFun g <- eval env f
    x' <- eval env x
    g x'
  EOp o x y -> case o of
    OPlus -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a + b))
    OTimes -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVInt (a * b))
    OLte -> do
      EVInt a <- eval env x
      EVInt b <- eval env y
      pure (EVBool (a <= b))
    OAnd -> do
      EVBool a <- eval env x
      EVBool b <- eval env y
      pure (EVBool (a && b))
  ERecord xs ->
    EVRecord <$> traverseRec (evalField env) xs
  EAccess e i -> do
    EVRecord xs <- eval env e
    case indexRec i xs of
      EVField v -> pure v
  EChoice i x ->
    EVSum i <$> eval env x
  ECase x hs -> do
    EVSum i v <- eval env x
    case indexRec i hs of
      EHandler t n body -> eval (M.insert n (SomeValue t v) env) body

evalField :: Map String SomeValue -> ExprField x -> Maybe (EValueField x)
evalField env (EField v) = EVField <$> eval env v

main :: IO ()
main = putStrLn $ maybe "<error>" showEValue (eval M.empty fifteen)
