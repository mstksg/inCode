{-# LANGUAGE LambdaCase #-}

module ExprStage2 where

import Data.Map (Map)
import qualified Data.Map as M

data Prim = PInt Int | PBool Bool | PString String
  deriving (Eq, Show)

data Op = OPlus | OTimes | OLte | OAnd
  deriving (Eq, Show)

data Expr
  = EPrim Prim
  | EVar String
  | ELambda String Expr
  | EApply Expr Expr
  | EOp Op Expr Expr
  deriving (Eq, Show)

fifteen :: Expr
fifteen = ELambda "x" (EOp OTimes (EVar "x") (EPrim (PInt 3))) `EApply` EPrim (PInt 5)

normalize :: Map String Expr -> Expr -> Maybe Expr
normalize env = \case
  EPrim p -> pure (EPrim p)
  EVar v -> M.lookup v env >>= normalize env
  ELambda n x -> pure (ELambda n x)
  EApply f x -> normalize env f >>= \case
    ELambda n u -> do
      x' <- normalize env x
      normalize (M.insert n x' env) u
    f' -> EApply f' <$> normalize env x
  EOp o x y -> do
    u <- normalize env x
    v <- normalize env y
    case (u, v) of
      (EPrim x', EPrim y') -> case (x', y') of
        (PInt a, PInt b) -> case o of
          OPlus -> pure (EPrim (PInt (a + b)))
          OTimes -> pure (EPrim (PInt (a * b)))
          OLte -> pure (EPrim (PBool (a <= b)))
          OAnd -> Nothing
        (PBool a, PBool b) -> case o of
          OAnd -> pure (EPrim (PBool (a && b)))
          _ -> Nothing
        _ -> Nothing
      (x', y') -> pure $ EOp o x' y'

main :: IO ()
main = print (normalize M.empty fifteen)
