{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeOperators #-}

module ExprStage3 where

import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Type.Equality
import GHC.TypeLits (Symbol)

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

traverseRec :: Applicative m => (forall x. f x -> m (g x)) -> Rec f xs -> m (Rec g xs)
traverseRec _ RNil = pure RNil
traverseRec f (x :& xs) = (:&) <$> f x <*> traverseRec f xs

type (:::) l a = '(l, a)

infixr 6 :::

data STy :: Ty -> Type where
  STInt :: STy TInt
  STBool :: STy TBool
  STString :: STy TString
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
  ERecord :: Rec ExprField as -> Expr (TRecord as)
  EAccess :: Expr (TRecord as) -> Index as (l ::: a) -> Expr a
  EChoice :: Index as (l ::: a) -> Expr a -> Expr (TSum as)
  ECase :: Expr (TSum as) -> Rec (ExprHandler b) as -> Expr b

data ExprField :: (Symbol, Ty) -> Type where
  EField :: Expr a -> ExprField (l ::: a)

data ExprHandler :: Ty -> (Symbol, Ty) -> Type where
  EHandler :: STy a -> String -> Expr b -> ExprHandler b (l ::: a)

fifteen :: Expr TInt
fifteen =
  EApply
    (ELambda STInt "x" (EOp OTimes (EVar STInt "x") (EPrim (PInt 3))))
    (EPrim (PInt 5))

recordExample :: Expr TInt
recordExample =
  EOp
    OPlus
    ( EAccess
        ( ERecord
            ( EField (EPrim (PInt 7))
                :& EField (EPrim (PString "found"))
                :& RNil
            ) ::
            Expr (TRecord '["value" ::: TInt, "label" ::: TString])
        )
        IZ
    )
    (EPrim (PInt 1))

sumExample :: Expr TInt
sumExample =
  ECase
    (EChoice IZ (EPrim (PInt 7)) :: Expr (TSum '["Found" ::: TInt, "Missing" ::: TString]))
    ( EHandler STInt "value" (EOp OPlus (EVar STInt "value") (EPrim (PInt 1)))
        :& EHandler STString "message" (EPrim (PInt 0))
        :& RNil
    )

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

sameTy :: STy a -> STy b -> Maybe (a :~: b)
sameTy = \case
  STInt -> \case STInt -> Just Refl; _ -> Nothing
  STBool -> \case STBool -> Just Refl; _ -> Nothing
  STString -> \case STString -> Just Refl; _ -> Nothing
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
