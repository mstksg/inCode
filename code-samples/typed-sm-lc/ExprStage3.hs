{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}

module ExprStage3 where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Type.Equality

type data Ty
  = TInt
  | TBool
  | TString
  | TRecord
  | Ty :-> Ty

infixr 0 :->

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
  STRecord :: STy TRecord
  STFun :: STy a -> STy b -> STy (a :-> b)

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
  ERecord :: Map String SomeExpr -> Expr TRecord
  EAccess :: STy t -> Expr TRecord -> String -> Expr t

fifteen :: Expr TInt
fifteen =
  EApply
    (ELambda STInt "x" (EOp OTimes (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))

data EValue :: Ty -> Type where
  EVInt :: Int -> EValue TInt
  EVBool :: Bool -> EValue TBool
  EVString :: String -> EValue TString
  EVRecord :: Map String SomeValue -> EValue TRecord
  EVFun :: (EValue a -> Maybe (EValue b)) -> EValue (a :-> b)

data SomeExpr where
  SomeExpr :: STy t -> Expr t -> SomeExpr

data SomeValue where
  SomeValue :: STy t -> EValue t -> SomeValue

showEValue :: EValue t -> String
showEValue = \case
  EVInt n -> show n
  EVBool b -> show b
  EVString s -> show s
  EVRecord _ -> "<record>"
  EVFun _ -> "<function>"

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
  STRecord -> \case STRecord -> Just Refl; _ -> Nothing
  STFun a b -> \case
    STFun c d -> do
      Refl <- sameTy a c
      Refl <- sameTy b d
      Just Refl
    _ -> Nothing

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
    EVRecord <$> traverse evalField xs
  EAccess t e k -> do
    EVRecord xs <- eval env e
    SomeValue t' v' <- M.lookup k xs
    Refl <- sameTy t t'
    pure v'
  where
    evalField (SomeExpr t v) = do
      v' <- eval env v
      pure (SomeValue t v')

main :: IO ()
main = putStrLn $ maybe "<error>" showEValue (eval M.empty fifteen)
