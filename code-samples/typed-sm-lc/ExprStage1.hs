{-# LANGUAGE LambdaCase #-}

module ExprStage1 where

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
  | ERecord (Map String Expr)
  | EAccess Expr String
  | EChoice String Expr
  | ECase Expr (Map String (String, Expr))
  deriving (Eq, Show)

fifteen :: Expr
fifteen = ELambda "x" (EOp OTimes (EVar "x") (EPrim (PInt 3))) `EApply` EPrim (PInt 5)

recordExample :: Expr
recordExample =
  EOp
    OPlus
    ( EAccess
        ( ERecord $
            M.fromList
              [ ("value", EPrim (PInt 7)),
                ("label", EPrim (PString "found"))
              ]
        )
        "value"
    )
    (EPrim (PInt 1))

sumExample :: Expr
sumExample =
  ECase
    (EChoice "Found" (EPrim (PInt 7)))
    ( M.fromList
        [ ("Found", ("value", EOp OPlus (EVar "value") (EPrim (PInt 1)))),
          ("Missing", ("message", EPrim (PInt 0)))
        ]
    )

normalize :: Map String Expr -> Expr -> Expr
normalize env = \case
  EPrim p -> EPrim p
  EVar v -> case M.lookup v env of
    Nothing -> error "Variable not defined"
    Just x -> normalize env x
  ELambda n x -> ELambda n x
  EApply f x -> case normalize env f of
    ELambda n u -> normalize (M.insert n (normalize env x) env) u
    _ -> error "Type error"
  EOp o x y -> case (normalize env x, normalize env y) of
    (EPrim x', EPrim y') -> case (x', y') of
      (PInt a, PInt b) -> case o of
        OPlus -> EPrim (PInt (a + b))
        OTimes -> EPrim (PInt (a * b))
        OLte -> EPrim (PBool (a <= b))
        OAnd -> error "Type error"
      (PBool a, PBool b) -> case o of
        OAnd -> EPrim (PBool (a && b))
        _ -> error "Type error"
      _ -> error "Type error"
    (x', y') -> EOp o x' y'
  ERecord xs -> ERecord (M.map (normalize env) xs)
  EAccess e k -> case normalize env e of
    ERecord xs -> case M.lookup k xs of
      Just v -> normalize env v
      Nothing -> error "Field not found"
    _ -> error "Type error"
  EChoice tag x -> EChoice tag (normalize env x)
  ECase x hs -> case normalize env x of
    EChoice tag payload -> case M.lookup tag hs of
      Just (n, body) -> normalize (M.insert n payload env) body
      Nothing -> error "Missing case branch"
    _ -> error "Type error"

main :: IO ()
main = print (normalize M.empty fifteen)
