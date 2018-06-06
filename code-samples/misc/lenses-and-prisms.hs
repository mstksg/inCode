#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-11.9 -- -Wall -O2

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}

import           Data.Bifunctor (first)
import           Data.List      (foldl')

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

main :: IO ()
main = return ()
