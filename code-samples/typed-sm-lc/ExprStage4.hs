{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module ExprStage4 where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
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
  (:&) :: f x -> Rec f xs -> Rec f (x ': xs)

data Index :: [k] -> k -> Type where
  IZ :: Index (x ': xs) x
  IS :: Index xs x -> Index (y ': xs) x

class ListIx (l :: Symbol) (xs :: [(Symbol, Ty)]) (a :: Ty) | l xs -> a where
  listIx :: Index xs (l ::: a)

instance ListIx l (l ::: a ': xs) a where
  listIx = IZ

instance {-# OVERLAPPABLE #-} ListIx l xs a => ListIx l (m ::: b ': xs) a where
  listIx = IS (listIx @l)

indexRec :: Index xs x -> Rec f xs -> f x
indexRec = \case
  IZ -> \(x :& _) -> x
  IS i -> \(_ :& xs) -> indexRec i xs

mapRec :: (forall x. f x -> g x) -> Rec f xs -> Rec g xs
mapRec _ RNil = RNil
mapRec f (x :& xs) = f x :& mapRec f xs

type (:::) l a = '(l, a)

infixr 6 :::

data Prim :: Ty -> Type where
  PInt :: Int -> Prim TInt
  PBool :: Bool -> Prim TBool
  PString :: String -> Prim TString

data Op :: Ty -> Ty -> Ty -> Type where
  OPlus :: Op TInt TInt TInt
  OTimes :: Op TInt TInt TInt
  OLte :: Op TInt TInt TBool
  OAnd :: Op TBool TBool TBool

data Expr :: [(Symbol, Ty)] -> Ty -> Type where
  EPrim :: Prim t -> Expr vs t
  EVar :: Index vs (n ::: t) -> Expr vs t
  ELambda :: KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
  EApply :: Expr vs (a :-> b) -> Expr vs a -> Expr vs b
  EOp :: Op a b c -> Expr vs a -> Expr vs b -> Expr vs c
  ERecord :: Rec (ExprField vs) as -> Expr vs (TRecord as)
  EAccess :: KnownSymbol l => Expr vs (TRecord as) -> Index as (l ::: a) -> Expr vs a
  EChoice :: KnownSymbol l => Index as (l ::: a) -> Expr vs a -> Expr vs (TSum as)
  ECase :: Expr vs (TSum as) -> Rec (ExprHandler vs b) as -> Expr vs b

eLambda :: forall n -> KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
eLambda n = ELambda @n

eField :: forall l -> KnownSymbol l => Expr vs a -> ExprField vs (l ::: a)
eField l = EField @l

eAccess ::
  forall l ->
  (KnownSymbol l, ListIx l as a) =>
  Expr vs (TRecord as) ->
  Expr vs a
eAccess l e = EAccess @l e (listIx @l)

eChoice ::
  forall l ->
  (KnownSymbol l, ListIx l as a) =>
  Expr vs a ->
  Expr vs (TSum as)
eChoice l e = EChoice @l (listIx @l) e

eHandler ::
  forall n ->
  forall l ->
  (KnownSymbol n, KnownSymbol l) =>
  Expr (n ::: a ': vs) b ->
  ExprHandler vs b (l ::: a)
eHandler n l = EHandler @n @l

eVar :: forall n -> ListIx n vs a => Expr vs a
eVar n = EVar (listIx @n)

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Rec EValueField as -> EValue (TRecord as)
  EVSum :: Index as (l ::: a) -> EValue a -> EValue (TSum as)
  EVFun :: (EValue a -> EValue b) -> EValue (a :-> b)

data EValueField :: (Symbol, Ty) -> Type where
  EVField :: EValue a -> EValueField '(l, a)

data ExprField :: [(Symbol, Ty)] -> (Symbol, Ty) -> Type where
  EField :: KnownSymbol l => Expr vs a -> ExprField vs '(l, a)

data ExprHandler :: [(Symbol, Ty)] -> Ty -> (Symbol, Ty) -> Type where
  EHandler :: (KnownSymbol n, KnownSymbol l) => Expr (n ::: a ': vs) b -> ExprHandler vs b (l ::: a)

fifteen :: Expr '[] TInt
fifteen =
  EApply
    (eLambda "x" (EOp OTimes (eVar "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))

plusThree :: Expr '[] (TInt :-> TInt)
plusThree =
  eLambda "x" (EOp OPlus (eVar "x") (EPrim (PInt 3)))

recordExample :: Expr '[] TInt
recordExample =
  EOp
    OPlus
    ( eAccess
        "value"
        ( ERecord
            ( eField "value" (EPrim (PInt 7))
                :& eField "label" (EPrim (PString "found"))
                :& RNil
            ) ::
            Expr '[] (TRecord '["value" ::: TInt, "label" ::: TString])
        )
    )
    (EPrim (PInt 1))

sumExample :: Expr '[] TInt
sumExample =
  ECase
    (eChoice "Found" (EPrim (PInt 7)) :: Expr '[] (TSum '["Found" ::: TInt, "Missing" ::: TString]))
    ( eHandler "value" "Found" (EOp OPlus (eVar "value") (EPrim (PInt 1)))
        :& eHandler "message" "Missing" (EPrim (PInt 0))
        :& RNil
    )

showEValue :: EValue t -> String
showEValue = \case
  EVInt n -> show n
  EVBool b -> show b
  EVString s -> show s
  EVRecord _ -> "<record>"
  EVSum _ _ -> "<sum>"
  EVFun _ -> "<function>"

data NameField :: (Symbol, Ty) -> Type where
  NameField :: KnownSymbol l => NameField (l ::: a)

prettyExpr :: Expr '[] t -> PP.Doc ann
prettyExpr = ppExpr RNil False

ppExpr :: Rec NameField vs -> Bool -> Expr vs t -> PP.Doc ann
ppExpr names paren = \case
  EPrim p -> ppPrim p
  EVar i -> case indexRec i names of
    NameField @n -> PP.pretty (symbolVal (Proxy @n))
  ELambda @n body ->
    wrap $
      "\\" <> PP.pretty (symbolVal (Proxy @n))
        <+> "->"
        <+> ppExpr (NameField @n :& names) False body
  EApply f x ->
    wrap $ ppExpr names True f <+> ppExpr names True x
  EOp o x y ->
    wrap $ ppExpr names True x <+> ppOp o <+> ppExpr names True y
  ERecord xs ->
    PP.encloseSep "{ " " }" ", " (ppFields names xs)
  EAccess @l e _ ->
    ppExpr names True e <> "." <> PP.pretty (symbolVal (Proxy @l))
  EChoice @l _ x ->
    wrap $ PP.pretty (symbolVal (Proxy @l)) <+> ppExpr names True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr names False x <+> "of"
        , PP.encloseSep "{ " " }" "; " (ppHandlers names hs)
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

ppFields :: Rec NameField vs -> Rec (ExprField vs) xs -> [PP.Doc ann]
ppFields _ RNil = []
ppFields names (EField @l x :& xs) =
  (PP.pretty (symbolVal (Proxy @l)) <+> "=" <+> ppExpr names False x) : ppFields names xs

ppHandlers :: Rec NameField vs -> Rec (ExprHandler vs b) xs -> [PP.Doc ann]
ppHandlers _ RNil = []
ppHandlers names (EHandler @n @l body :& xs) =
  ( PP.pretty (symbolVal (Proxy @l))
      <+> PP.pretty (symbolVal (Proxy @n))
      <+> "->"
      <+> ppExpr (NameField @n :& names) False body
  )
    : ppHandlers names xs

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

eval :: Rec EValueField vs -> Expr vs t -> EValue t
eval env = \case
  EPrim (PInt n) -> EVInt n
  EPrim (PBool b) -> EVBool b
  EPrim (PString s) -> EVString s
  EVar i -> case indexRec i env of
    EVField v -> v
  ELambda body ->
    EVFun $ \x -> eval (EVField x :& env) body
  EApply f x -> case eval env f of
    EVFun g -> g (eval env x)
  EOp o x y -> case (o, eval env x, eval env y) of
    (OPlus, EVInt a, EVInt b) -> EVInt (a + b)
    (OTimes, EVInt a, EVInt b) -> EVInt (a * b)
    (OLte, EVInt a, EVInt b) -> EVBool (a <= b)
    (OAnd, EVBool a, EVBool b) -> EVBool (a && b)
  ERecord xs ->
    EVRecord $ mapRec (\(EField x) -> EVField (eval env x)) xs
  EAccess e i -> case eval env e of
    EVRecord xs -> case indexRec i xs of
      EVField v -> v
  EChoice i x -> EVSum i (eval env x)
  ECase x hs -> case eval env x of
    EVSum i y -> case indexRec i hs of
      EHandler h -> eval (EVField y :& env) h

main :: IO ()
main = putStrLn (showEValue (eval RNil fifteen))
