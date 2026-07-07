{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ExprStage1 where

import Control.Applicative (empty)
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, gets, modify, state)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (isJust)
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

plusThree :: Expr
plusThree = ELambda "x" (EOp OPlus (EVar "x") (EPrim (PInt 3)))

badTypeExample :: Expr
badTypeExample =
  EOp OAnd (EPrim (PInt 1)) (EPrim (PInt 2))

badLookupExample :: Expr
badLookupExample =
  EAccess
    (ERecord (M.fromList [("value", EPrim (PInt 7))]))
    "label"

data CheckedType
  = CTInt
  | CTBool
  | CTString
  | CTFun CheckedType CheckedType
  | CTRecord (Map String CheckedType)
  | CTSum String CheckedType
  | CTVar Int
  deriving (Eq, Show)

check :: Map String CheckedType -> Expr -> Maybe CheckedType
check env e =
  evalStateT (resolve =<< infer env e) initialCheckState

data CheckState = CheckState
  { nextVar :: Int,
    substitutions :: Map Int CheckedType
  }

initialCheckState :: CheckState
initialCheckState = CheckState {nextVar = 0, substitutions = M.empty}

type Check = StateT CheckState Maybe

fresh :: Check CheckedType
fresh =
  state $ \s ->
    let n = nextVar s
     in (CTVar n, s {nextVar = n + 1})

infer :: Map String CheckedType -> Expr -> Check CheckedType
infer env = \case
  EPrim p -> pure (checkPrim p)
  EVar v -> liftMaybe (M.lookup v env)
  ELambda n body -> do
    argTy <- fresh
    bodyTy <- infer (M.insert n argTy env) body
    pure (CTFun argTy bodyTy)
  EApply f x -> do
    fTy <- infer env f
    xTy <- infer env x
    resultTy <- fresh
    unify fTy (CTFun xTy resultTy)
    resolve resultTy
  EOp o x y -> do
    xTy <- infer env x
    yTy <- infer env y
    checkOp o xTy yTy
  ERecord xs ->
    CTRecord <$> traverse (infer env) xs
  EAccess e k -> do
    eTy <- resolve =<< infer env e
    case eTy of
      CTRecord xs -> liftMaybe (M.lookup k xs)
      _ -> empty
  EChoice tag x ->
    CTSum tag <$> infer env x
  ECase x hs -> do
    xTy <- resolve =<< infer env x
    case xTy of
      CTSum tag payload -> do
        (n, body) <- liftMaybe (M.lookup tag hs)
        infer (M.insert n payload env) body
      _ -> empty

checkPrim :: Prim -> CheckedType
checkPrim = \case
  PInt {} -> CTInt
  PBool {} -> CTBool
  PString {} -> CTString

checkOp :: Op -> CheckedType -> CheckedType -> Check CheckedType
checkOp = \case
  OPlus -> checkIntOp
  OTimes -> checkIntOp
  OLte -> \x y -> CTBool <$ requireCompatible CTInt x y
  OAnd -> \x y -> CTBool <$ requireCompatible CTBool x y
  where
    checkIntOp x y = CTInt <$ requireCompatible CTInt x y

requireCompatible :: CheckedType -> CheckedType -> CheckedType -> Check ()
requireCompatible expected x y =
  unify expected x *> unify expected y

unify :: CheckedType -> CheckedType -> Check ()
unify x y = do
  x' <- resolve x
  y' <- resolve y
  case (x', y') of
    (CTVar n, CTVar m)
      | n == m -> pure ()
    (CTVar n, ty) -> bindVar n ty
    (ty, CTVar n) -> bindVar n ty
    (CTInt, CTInt) -> pure ()
    (CTBool, CTBool) -> pure ()
    (CTString, CTString) -> pure ()
    (CTFun a b, CTFun c d) -> unify a c *> unify b d
    (CTRecord xs, CTRecord ys) -> unifyRecords xs ys
    (CTSum tx xPayload, CTSum ty yPayload)
      | tx == ty -> unify xPayload yPayload
    _ -> empty

bindVar :: Int -> CheckedType -> Check ()
bindVar n ty = do
  occurs <- occursIn n ty
  guard (not occurs)
  modifySubstitutions (M.insert n ty)

occursIn :: Int -> CheckedType -> Check Bool
occursIn n ty = do
  ty' <- resolve ty
  case ty' of
    CTVar m -> pure (n == m)
    CTFun a b -> (||) <$> occursIn n a <*> occursIn n b
    CTRecord xs -> or <$> traverse (occursIn n) (M.elems xs)
    CTSum _ payload -> occursIn n payload
    CTInt -> pure False
    CTBool -> pure False
    CTString -> pure False

unifyRecords :: Map String CheckedType -> Map String CheckedType -> Check ()
unifyRecords xs ys =
  traverse_ unifyField (M.keys xs)
  where
    unifyField k =
      case (M.lookup k xs, M.lookup k ys) of
        (Just x, Just y) -> unify x y
        _ -> empty

resolve :: CheckedType -> Check CheckedType
resolve = \case
  CTVar n -> do
    subs <- gets substitutions
    case M.lookup n subs of
      Nothing -> pure (CTVar n)
      Just ty -> resolve ty
  CTFun a b -> CTFun <$> resolve a <*> resolve b
  CTRecord xs -> CTRecord <$> traverse resolve xs
  CTSum tag payload -> CTSum tag <$> resolve payload
  CTInt -> pure CTInt
  CTBool -> pure CTBool
  CTString -> pure CTString

modifySubstitutions :: (Map Int CheckedType -> Map Int CheckedType) -> Check ()
modifySubstitutions f =
  modify $ \s -> s {substitutions = f (substitutions s)}

liftMaybe :: Maybe a -> Check a
liftMaybe =
  maybe empty pure

data EValue
  = EVInt Int
  | EVBool Bool
  | EVString String
  | EVFun (EValue -> Maybe EValue)
  | EVRecord (Map String EValue)
  | EVChoice String EValue

instance Show EValue where
  show = \case
    EVInt n -> "EVInt " ++ show n
    EVBool b -> "EVBool " ++ show b
    EVString s -> "EVString " ++ show s
    EVFun {} -> "EVFun <function>"
    EVRecord xs -> "EVRecord " ++ show xs
    EVChoice tag x -> "EVChoice " ++ show tag ++ " (" ++ show x ++ ")"

badTypeResult :: Maybe EValue
badTypeResult =
  eval M.empty badTypeExample

badLookupResult :: Maybe EValue
badLookupResult =
  eval M.empty badLookupExample

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
  PBool True -> "true"
  PBool False -> "false"
  PString s -> PP.pretty (show s)

ppOp :: Op -> PP.Doc ann
ppOp = \case
  OPlus -> "+"
  OTimes -> "*"
  OLte -> "<="
  OAnd -> "&&"

eval :: Map String EValue -> Expr -> Maybe EValue
eval env = \case
  EPrim p -> evalPrim p
  EVar v -> M.lookup v env
  ELambda n body -> pure (EVFun (\x -> eval (M.insert n x env) body))
  EApply f x -> eval env f >>= \case
    EVFun f' -> eval env x >>= f'
    _ -> Nothing
  EOp o x y -> do
    u <- eval env x
    v <- eval env y
    case (u, v) of
      (EVInt a, EVInt b) -> case o of
        OPlus -> pure (EVInt (a + b))
        OTimes -> pure (EVInt (a * b))
        OLte -> pure (EVBool (a <= b))
        OAnd -> Nothing
      (EVBool a, EVBool b) -> case o of
        OAnd -> pure (EVBool (a && b))
        _ -> Nothing
      _ -> Nothing
  ERecord xs -> EVRecord <$> traverse (eval env) xs
  EAccess e k -> do
    EVRecord xs <- eval env e
    M.lookup k xs
  EChoice tag x -> EVChoice tag <$> eval env x
  ECase x hs -> do
    EVChoice tag payload <- eval env x
    (n, body) <- M.lookup tag hs
    eval (M.insert n payload env) body

evalPrim :: Prim -> Maybe EValue
evalPrim = \case
  PInt n -> pure (EVInt n)
  PBool b -> pure (EVBool b)
  PString s -> pure (EVString s)

isValid :: Expr -> Bool
isValid =
  isJust . check M.empty

main :: IO ()
main = print (eval M.empty fifteen)
