{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Stage4 where

import Data.Kind (Type)
import GHC.TypeLits (KnownSymbol, Symbol)

type data Ty
  = TInt
  | TBool
  | TString
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

eLambda :: forall n -> KnownSymbol n => Expr (n ::: a ': vs) b -> Expr vs (a :-> b)
eLambda n x = ELambda @n x

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVFun :: (EValue a -> EValue b) -> EValue (a :-> b)

data EValueField :: (Symbol, Ty) -> Type where
  EVField :: KnownSymbol l => EValue a -> EValueField '(l, a)

fifteen :: Expr '[] TInt
fifteen =
  EApply
    (eLambda "x" (EOp OTimes (EVar IZ) (EPrim (PInt 3))))
    (EPrim (PInt 5))

showEValue :: EValue t -> String
showEValue = \case
  EVInt n -> show n
  EVBool b -> show b
  EVString s -> show s
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

main :: IO ()
main = putStrLn (showEValue (eval RNil fifteen))
