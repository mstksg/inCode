#!/usr/bin/env stack
-- stack --install-ghc runghc --package refined-0.2.3.0 --resolver nightly-2018-06-06 -- -Wall -O2

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}

import           Data.Bifunctor     (first)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Numeric.Natural
import           Refined hiding     (NonEmpty)
import qualified Data.Set           as S

-- Challenge: write a `match` for the "init and last" sum decomposition
-- using only one fold and no partial functions or booleans.

-- | Difference list
type Diff a = [a] -> [a]

matchInitLast :: [a] -> Either () ([a], a)
matchInitLast = (fmap . first) ($[])    -- "extract" the difference list
              . foldl' go (Left ())
  where
    go  :: Either () (Diff a, a)
        -> a
        -> Either () (Diff a, a)
    go (Left  _      ) x = Right (id       , x)
    go (Right (ys, y)) x = Right (ys . (y:), x)

-- Challenge: compose prisms and lenses

data Lens' s a = forall q. Lens'
    { split   :: s -> (a, q)
    , unsplit :: (a, q) -> s
    }

(.&.) :: Lens' a b
      -> Lens' b c
      -> Lens' a c
Lens' splitX unsplitX .&. Lens' splitY unsplitY = Lens'
    { split   = \x ->
        let (y, q) = splitX x
            (z, r) = splitY y
        in  (z, (q, r))
    , unsplit = \(z, (q, r)) ->
        let y = unsplitY (z, r)
            x = unsplitX (y, q)
        in  x
    }

data Prism' s a = forall q. Prism'
    { match  :: s -> Either a q
    , inject :: Either a q -> s
    }

(.|.) :: Prism' a b
      -> Prism' b c
      -> Prism' a c
Prism' matchX injectX .|. Prism' matchY injectY = Prism'
    { match  = \x ->
        case matchX x of
          Left y  -> case matchY y of
            Left z  -> Left z
            Right r -> Right (Right r)
          Right q -> Right (Left q)
    , inject = \case
        Left z          -> injectX (Left (injectY (Left z )))
        Right (Left  q) -> injectX (Right q)
        Right (Right r) -> injectX (Left (injectY (Right r)))
    }

-- Utility

view :: Lens' s a -> (s -> a)
view Lens'{..} = fst . split

set :: Lens' s a -> (a -> s -> s)
set Lens'{..} newVal x = case split x of
    (_, q) -> unsplit (newVal, q)      -- "replace" the `a`

overL :: Lens' s a -> (a -> a) -> (s -> s)
overL Lens'{..}  f = unsplit . first f . split   -- instance Bifunctor (,)

preview :: Prism' s a -> (s -> Maybe a)
preview Prism'{..} x = case match x of
    Left  y -> Just y
    Right _ -> Nothing

review  :: Prism' s a -> (a -> s)
review Prism'{..} = inject . Left

overP :: Prism' s a -> (a -> a) -> (s -> s)
overP Prism'{..} f = inject . first f . match    -- instance Bifunctor Either

-- Miscellaneous lenses

data Person = P
    { _pName :: String
    , _pAge  :: Int
    }

pName :: Lens' Person String
pName = Lens'
    { split   = \(P n a) -> (n, a)
    , unsplit = \(n, a)  -> P n a
    }

pAge :: Lens' Person Int
pAge = Lens'
    { split   = \(P n a) -> (a, n)
    , unsplit = \(a, n)  -> P n a
    }

identityL :: Lens' a a
identityL = Lens'
    { split   = \x      -> (x, ())
    , unsplit = \(x, _) -> x
    }

united :: Lens' a ()
united = Lens'
    { split   = \x       -> ((), x)
    , unsplit = \((), x) -> x
    }

mysteryLens1 :: Lens' (Either a a) Bool
mysteryLens1 = Lens'
    { split   = \case
        Left  x -> (False, x)
        Right x -> (True , x)
    , unsplit = \case
        (False, x) -> Left  x
        (True , x) -> Right x
    }

mysteryLens2 :: Lens' (Either a a) a
mysteryLens2 = Lens'
    { split   = \case
        Left  x -> (x, False)
        Right x -> (x, True )
    , unsplit = \case
        (x, False) -> Left  x
        (x, True ) -> Right x
    }

flipEither :: Either a a -> Either a a
flipEither = overL mysteryLens1 not

isRight :: Either a a -> Bool
isRight = view mysteryLens1

fromEither :: Either a a -> a
fromEither = view mysteryLens2

mapEither :: (a -> a) -> Either a a -> Either a a
mapEither = overL mysteryLens2

type CharButNotA = Char

containsA :: Lens' (S.Set Char) Bool
containsA = Lens'
    { split   = \s ->
        ( 'a' `S.member` s
        , 'a' `S.delete` s      :: S.Set CharButNotA
        )
    , unsplit = \case
        (False, s) -> s
        (True , s) -> 'a' `S.insert` (s :: S.Set CharButNotA)
    }

-- Miscellaneous prisms

data Shape = Circle  Double           -- radius
           | RegPoly Natural Double   -- number of sides, length of sides

data Void           -- no constructors, no valid inhabitants

absurd :: Void -> a     -- A useful helper function when working with `Void`
absurd = \case -- empty case statement because we have
               -- no constructors of 'Void' we need to
               -- match on

_Circle :: Prism' Shape Double
_Circle = Prism'
    { match  = \case
        Circle  r    -> Left r
        RegPoly n s  -> Right (n, s)
    , inject = \case
        Left   r     -> Circle r
        Right (n, s) -> RegPoly n s
    }

_RegPoly :: Prism' Shape (Natural, Double)
_RegPoly = Prism'
    { match  = \case
        Circle  r    -> Right r
        RegPoly n s  -> Left (n, s)
    , inject = \case
        Left  (n, s) -> RegPoly n s
        Right  r     -> Circle r
    }

_Nil :: Prism' [a] ()
_Nil = Prism'
    { match  = \case
        []              -> Left ()
        x:xs            -> Right (x :| xs)
    , inject = \case
        Left _          -> []
        Right (x :| xs) -> x:xs
    }

_Cons :: Prism' [a] (NonEmpty a)
_Cons = Prism'
    { match  = \case
        []              -> Right ()
        x:xs            -> Left (x :| xs)
    , inject = \case
        Left  (x :| xs) -> x:xs
        Right _         -> []
    }

_Nil' :: Prism' [a] ()
_Nil' = Prism'
    { match  = \xs -> if null xs
        then Left  ()
        else Right (init xs, last xs)
    , inject = \case
        Left _        -> []
        Right (xs, x) -> xs ++ [x]
    }

_Snoc :: Prism' [a] ([a], a)
_Snoc = Prism'
    { match  = \xs -> if null xs
        then Right ()
        else Left  (init xs, last xs)
    , inject = \case
        Left  (xs, x) -> xs ++ [x]
        Right _       -> []
    }

identityP :: Prism' a a
identityP = Prism'
    { match = Left
    , inject = \case
        Left  x -> x
        Right v -> absurd v
    }


_Void :: Prism' a Void
_Void = Prism'
    { match = Right
    , inject = \case
        Left  v -> absurd v
        Right x -> x
    }

type Not4 = Refined (NotEqualTo 4) Int

only4 :: Prism' Int ()
only4 = Prism'
    { match  = \n -> case refineFail n of
        Nothing -> Left ()
        Just x  -> Right (x :: Not4)
    , inject = \case
        Left  _ -> 4
        Right x -> unrefine x
    }

refined4 :: Prism' Int Not4
refined4 = Prism'
    { match  = \n -> case refineFail n of
        Nothing -> Right ()
        Just x  -> Left x
    , inject = \case
        Left  x -> unrefine x
        Right _ -> 4
    }

isEqualTo4 :: Int -> Bool   -- Checks if a value is 4
isEqualTo4 = isJust . preview only4

four :: Int     -- Is simply `4`
four = review only4 ()

makeNot4 :: Int -> Maybe Not4
makeNot4 = preview refined4

fromNot4 :: Not4 -> Int
fromNot4 = review refined4

onlyA :: Prism' Char ()
onlyA = Prism'
    { match  = \case
        'a' -> Left ()
        x   -> Right (x :: CharButNotA)
    , inject = \case
        Left  _ -> 'a'
        Right x -> x        -- Right contains a CharButNotA
    }

main :: IO ()
main = return ()
