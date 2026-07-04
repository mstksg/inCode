{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ExprStage4 where

import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol)

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
eLambda n x = ELambda @n x

eHandler :: forall n -> (KnownSymbol n, KnownSymbol l) => Expr (n ::: a ': vs) b -> ExprHandler vs b (l ::: a)
eHandler _ = EHandler

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Rec EValueField as -> EValue (TRecord as)
  EVSum :: Index as (l ::: a) -> EValue a -> EValue (TSum as)
  EVFun :: (EValue a -> EValue b) -> EValue (a :-> b)

data EValueField :: (Symbol, Ty) -> Type where
  EVField :: KnownSymbol l => EValue a -> EValueField '(l, a)

data ExprField :: [(Symbol, Ty)] -> (Symbol, Ty) -> Type where
  EField :: KnownSymbol l => Expr vs a -> ExprField vs '(l, a)

data ExprHandler :: [(Symbol, Ty)] -> Ty -> (Symbol, Ty) -> Type where
  EHandler ::
    (KnownSymbol n, KnownSymbol l) =>
    Expr (n ::: a ': vs) b ->
    ExprHandler vs b (l ::: a)

fifteen :: Expr '[] TInt
fifteen =
  EApply
    (eLambda "x" (EOp OTimes (EVar IZ) (EPrim (PInt 3))))
    (EPrim (PInt 5))

recordExample :: Expr '[] TInt
recordExample =
  EOp
    OPlus
    ( EAccess
        ( ERecord
            ( EField (EPrim (PInt 7))
                :& EField (EPrim (PString "found"))
                :& RNil
            ) ::
            Expr '[] (TRecord '["value" ::: TInt, "label" ::: TString])
        )
        IZ
    )
    (EPrim (PInt 1))

sumExample :: Expr '[] TInt
sumExample =
  ECase
    (EChoice IZ (EPrim (PInt 7)) :: Expr '[] (TSum '["Found" ::: TInt, "Missing" ::: TString]))
    ( eHandler "value" (EOp OPlus (EVar IZ) (EPrim (PInt 1)))
        :& eHandler "message" (EPrim (PInt 0))
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
