{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExprStage1 where

import Data.Map (Map)
import qualified Data.Map as M
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

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

badTypeExample :: Expr
badTypeExample =
  EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))

badLookupExample :: Expr
badLookupExample =
  EAccess
    (ERecord (M.fromList [("value", EPrim (PInt 7))]))
    "label"

badTypeResult :: Maybe Expr
badTypeResult =
  normalize M.empty badTypeExample

badLookupResult :: Maybe Expr
badLookupResult =
  normalize M.empty badLookupExample

prettyExpr :: Expr -> PP.Doc ann
prettyExpr = ppExpr False

ppExpr :: Bool -> Expr -> PP.Doc ann
ppExpr paren = \case
  EPrim p -> ppPrim p
  EVar v -> PP.pretty v
  ELambda n body ->
    wrap $ "\\" <> PP.pretty n <+> "->" <+> ppExpr False body
  EApply f x ->
    wrap $ ppExpr True f <+> ppExpr True x
  EOp o x y ->
    wrap $ ppExpr True x <+> ppOp o <+> ppExpr True y
  ERecord xs ->
    PP.encloseSep "{ " " }" ", " $
      [PP.pretty k <+> "=" <+> ppExpr False v | (k, v) <- M.toList xs]
  EAccess e k ->
    ppExpr True e <> "." <> PP.pretty k
  EChoice tag x ->
    wrap $ PP.pretty tag <+> ppExpr True x
  ECase x hs ->
    wrap $
      PP.sep
        [ "case" <+> ppExpr False x <+> "of"
        , PP.encloseSep "{ " " }" "; " $
            [ PP.pretty tag <+> PP.pretty n <+> "->" <+> ppExpr False body
            | (tag, (n, body)) <- M.toList hs
            ]
        ]
  where
    wrap
      | paren = PP.parens
      | otherwise = id

ppPrim :: Prim -> PP.Doc ann
ppPrim = \case
  PInt n -> PP.pretty n
  PBool b -> if b then "true" else "false"
  PString s -> PP.pretty (show s)

ppOp :: Op -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

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
  ERecord xs -> ERecord <$> traverse (normalize env) xs
  EAccess e k -> do
    ERecord xs <- normalize env e
    M.lookup k xs
  EChoice tag x -> EChoice tag <$> normalize env x
  ECase x hs -> do
    EChoice tag payload <- normalize env x
    (n, body) <- M.lookup tag hs
    normalize (M.insert n payload env) body

main :: IO ()
main = print (normalize M.empty fifteen)
