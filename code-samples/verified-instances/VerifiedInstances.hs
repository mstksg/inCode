#!/usr/bin/env stack
-- stack --resolver lts-11 --install-ghc runghc --package singletons

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding (SCons, SNil, NilSym0)
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Type.Equality
import           Prelude hiding           (Monoid, Functor, Monad)

$(singletons [d|
  data List a = Nil | Cons a (List a)
      deriving (Show, Eq)

  infixr 5 `Cons`

  appendList :: List a -> List a -> List a
  appendList Nil         ys = ys
  appendList (Cons x xs) ys = Cons x (appendList xs ys)

  mapList :: (a -> b) -> List a -> List b
  mapList _ Nil         = Nil
  mapList f (Cons x xs) = Cons (f x) (mapList f xs)

  pureList :: a -> List a
  pureList x = Cons x Nil

  concatMapList :: (a -> List b) -> List a -> List b
  concatMapList _ Nil         = Nil
  concatMapList f (Cons x xs) = f x `appendList` concatMapList f xs

  data N = Z | S N
    deriving (Show)

  plus :: N -> N -> N
  plus Z     y = y
  plus (S x) y = S (plus x y)

  data Option a = None | Some a
      deriving (Show, Eq)

  mapOption :: (a -> b) -> Option a -> Option b
  mapOption _ None     = None
  mapOption f (Some x) = Some (f x)

  bindOption :: Option a -> (a -> Option b) -> Option b
  bindOption None     _ = None
  bindOption (Some x) f = f x
  |])

class Semigroup a where
    type (x :: a) <> (y :: a) :: a

    (%<>) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x <> y)

    appendAssoc
        :: Sing (x :: a)
        -> Sing (y :: a)
        -> Sing (z :: a)
        -> ((x <> y) <> z) :~: (x <> (y <> z))

infixr 6 <>
(<>)
    :: forall a. (SingKind a, Semigroup a)
    => Demote a
    -> Demote a
    -> Demote a
x <> y = withSomeSing x $ \sX ->
           withSomeSing y $ \sY ->
             fromSing (sX %<> sY)

instance Semigroup (List a) where
    type xs <> ys = AppendList xs ys
    (%<>) = sAppendList

    appendAssoc = \case
      SNil       -> \_ _ -> Refl
      SCons _ xs -> \ys zs ->
        case appendAssoc xs ys zs of
          Refl -> Refl

instance Semigroup N where
    type xs <> ys = Plus xs ys
    (%<>) = sPlus

    appendAssoc = \case
      SZ -> \_ _ -> Refl
      SS x -> \y z ->
        case appendAssoc x y z of
          Refl -> Refl

instance Semigroup a => Semigroup (Option a) where
    type None   <> y      = y
    type x      <> None   = x
    type Some x <> Some y = Some (x <> y)

    SNone   %<> y       = y
    x       %<> SNone   = x
    SSome x %<> SSome y = SSome (x %<> y)

    appendAssoc = \case
        SNone   -> \_ _ -> Refl
        SSome x -> \case
          SNone -> \_ -> Refl
          SSome y -> \case
            SNone -> Refl
            SSome z ->
              case appendAssoc x y z of
                Refl -> Refl

class Semigroup a => Monoid a where
    type Empty a :: a

    sEmpty :: Sing (Empty a)

    emptyIdentLeft
        :: Sing x
        -> (Empty a <> x) :~: x

    emptyIdentRight
        :: Sing x
        -> (x <> Empty a) :~: x

empty
    :: forall a. (SingKind a, Monoid a)
    => Demote a
empty = fromSing sEmpty

instance Monoid (List a) where
    type Empty (List a) = Nil

    sEmpty = SNil
    emptyIdentLeft _ = Refl
    emptyIdentRight  = \case
      SNil -> Refl
      SCons _ xs ->
        case emptyIdentRight xs of
          Refl -> Refl

instance Monoid N where
    type Empty N = Z

    sEmpty = SZ
    emptyIdentLeft _ = Refl
    emptyIdentRight  = \case
      SZ -> Refl
      SS x -> case emptyIdentRight x of
        Refl -> Refl

instance Semigroup a => Monoid (Option a) where
    type Empty (Option a) = None

    sEmpty = SNone
    emptyIdentLeft  _ = Refl
    emptyIdentRight _ = Refl

class Functor f where
    type Fmap a b (g :: a ~> b) (x :: f a) :: f b

    sFmap
        :: Sing (g            :: a ~> b)
        -> Sing (x            :: f a   )
        -> Sing (Fmap a b g x :: f b   )

    -- | fmap id x == x
    fmapId
        :: Sing (x :: f a)
        -> Fmap a a IdSym0 x :~: x

    -- | fmap f (fmap g x) = fmap (f . g) x
    fmapCompose
        :: Sing (g :: b ~> c)
        -> Sing (h :: a ~> b)
        -> Sing (x :: f a   )
        -> Fmap b c g (Fmap a b h x) :~: Fmap a c (((:.$) @@ g) @@ h) x

instance Functor Option where
    type Fmap a b g x = MapOption g x

    sFmap = sMapOption
    fmapId = \case
      SNone   -> Refl
      SSome _ -> Refl

    fmapCompose _ _ = \case
      SNone   -> Refl
      SSome _ -> Refl

instance Functor List where
    type Fmap a b g x = MapList g x

    sFmap = sMapList
    fmapId = \case
      SNil       -> Refl
      SCons _ xs ->
        case fmapId xs of
          Refl -> Refl

    fmapCompose g h = \case
      SNil -> Refl
      SCons _ xs ->
        case fmapCompose g h xs of
          Refl -> Refl

class Functor f => Monad f where
    type Return a   (x :: a) :: f a
    type Bind   a b (m :: f a) (g :: a ~> f b) :: f b

    sReturn
        :: Sing (x          :: a  )
        -> Sing (Return a x :: f a)

    sBind
        :: Sing (m            :: f a     )
        -> Sing (g            :: a ~> f b)
        -> Sing (Bind a b m g :: f b     )

    -- | (return x >>= f) == f x
    returnIdentLeft
        :: Sing (x :: a)
        -> Sing (g :: a ~> f b)
        -> Bind a b (Return a x) g :~: (g @@ x)

    -- | (m >>= return) == m
    returnIdentRight
        :: Sing (m :: f a)
        -> Bind a a m ReturnSym0 :~: m

    -- | m >>= (\x -> f x >>= h) == (m >>= f) >>= h
    bindCompose
        :: Sing (m :: f a)
        -> Sing (g :: a ~> f b)
        -> Sing (h :: b ~> f c)
        -> Bind a c m (KCompSym2 a b c g h) :~: Bind b c (Bind a b m g) h

data ReturnSym0 :: a ~> f a
type instance Apply (ReturnSym0 :: a ~> f a) (x :: a) = Return a x

type KComp a b c (g :: a ~> f b) (h :: b ~> f c) (x :: a) = Bind b c (g @@ x) h
data KCompSym2 a b c g h :: (a ~> f c)
type instance Apply (KCompSym2 a b c g h :: a ~> f c) (x :: a) = KComp a b c g h x

sKComp
    :: forall f a b c g h x. Monad f
    => Sing (g :: a ~> f b)
    -> Sing (h :: b ~> f c)
    -> Sing (x :: a)
    -> Sing (KCompSym2 a b c g h @@ x :: f c)
sKComp g h x = sBind (unSingFun1 g x) h

return
    :: forall f a. (SingKind a, SingKind (f a), Monad f)
    => Demote a
    -> Demote (f a)
return x = withSomeSing x $ \sX -> fromSing (sReturn sX)

instance Monad Option where
    type Return a   x   = Some x
    type Bind   a b m g = BindOption m g

    sReturn = SSome
    sBind   = sBindOption

    returnIdentLeft _ _ = Refl
    returnIdentRight = \case
      SNone   -> Refl
      SSome x -> case sReturn x of
        SSome _ -> Refl
    bindCompose = \case
      SNone   -> \_ _ -> Refl
      SSome _ -> \_ _ -> Refl

instance Monad List where
    type Return a   x   = PureList x
    type Bind   a b m g = ConcatMapList g m

    sReturn   = sPureList
    sBind x f = sConcatMapList f x

    returnIdentLeft x g = case sReturn x of
      SCons y SNil -> case emptyIdentRight (unSingFun1 g y) of
        Refl -> Refl

    returnIdentRight = \case
      SNil       -> Refl
      SCons _ xs -> case returnIdentRight xs of
        Refl -> Refl

    bindCompose = \case
      SNil       -> \_ _ -> Refl
      SCons x xs -> \g h -> case bindCompose xs g h of
        Refl -> case unSingFun1 g x of
          SNil       -> Refl
          SCons y ys ->
            let gxs  = sConcatMapList g xs
                hgxs = sConcatMapList h gxs
                hy   = unSingFun1 h y
                hys  = sConcatMapList h ys
            in  case distribConcatMap h ys gxs of
                  Refl -> case appendAssoc hy hys hgxs of
                    Refl -> Refl

distribConcatMap
    :: Sing (g  :: a ~> List b)
    -> Sing (xs :: List a)
    -> Sing (ys :: List a)
    -> ConcatMapList g (xs <> ys) :~: (ConcatMapList g xs <> ConcatMapList g ys)
distribConcatMap g = \case
    SNil -> \_ -> Refl
    SCons x xs -> \ys ->
      case distribConcatMap g xs ys of
        Refl ->
          let gx    = unSingFun1 g x
              cmgxs = sConcatMapList g xs
              cmgys = sConcatMapList g ys
          in  case appendAssoc gx cmgxs cmgys of
                Refl -> Refl

main :: IO ()
main = do
    print $ ((1::Integer) `Cons` 2 `Cons` Nil) <> (3 `Cons` 4 `Cons` Nil)
    print $ S (S Z) <> S Z
    print $ Some (S Z) <> Some (S (S (S Z)))
    print $ None       <> Some (S (S (S Z)))
