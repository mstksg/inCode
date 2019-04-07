#!/usr/bin/env stack
-- stack --install-ghc ghci --package free --package transformers

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

import           Control.Alternative.Free
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.List

data Prim a = Prim Char a
  deriving Functor

type RegExp = Alt Prim

-- | charAs: Parse a given character as a given constant result.
charAs :: Char -> a -> RegExp a
charAs c x = liftAlt (Prim c x)     -- liftAlt lets us use the underlying
                                    -- functor Prim in RegExp, analogous
                                    -- to liftFM from earlier

-- | char: Parse a given character as itself.
char :: Char -> RegExp Char
char c = charAs c c

-- | string: Parse a given string as itself.
string :: String -> RegExp String
string = traverse char              -- neat, huh

testRegExp_ :: RegExp ()
testRegExp_ = void $ (char 'a' <|> char 'b')
                  *> many (string "cd")
                  *> char 'e'

testRegExp :: RegExp Int
testRegExp = (char 'a' <|> char 'b')
          *> (length <$> many (string "cd"))
          <* char 'e'

testRegExpDo :: RegExp Int
testRegExpDo = do
    char 'a' <|> char 'b'
    cds <- many (string "cd")
    char 'e'
    pure (length cds)

processPrim :: Prim a -> StateT String Maybe a
processPrim (Prim c x) = do
    d:ds <- get
    guard (c == d)
    put ds
    pure x

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)

match1 :: RegExp a -> String -> Maybe a
match1 l xs = asum [ matchPrefix l ys | ys <- tails xs ]

matchAlts :: RegExp a -> String -> Maybe a
matchAlts (Alt ls) xs = asum [ matchChain l xs | l <- ls  ]

matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap _          _   ) []     = Nothing
matchChain (Ap (Prim c x) next) (d:ds)
    | c == d    = matchAlts (($ x) <$> next) ds
    | otherwise = Nothing
matchChain (Pure x)             _      = Just x

match2 :: RegExp a -> String -> Maybe a
match2 l xs = asum [ matchAlts l ys | ys <- tails xs ]
