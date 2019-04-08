#!/usr/bin/env stack
-- stack --install-ghc ghci --package free --package transformers

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

import           Control.Alternative.Free
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe

data Prim a = Prim Char a
  deriving Functor

type RegExp = Alt Prim

-- | charAs: Parse a given character as a given constant result.
charAs :: Char -> a -> RegExp a
charAs c x = liftAlt (Prim c x)     -- liftAlt lets us use the underlying
                                    -- functor Prim in RegExp

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

digit :: RegExp Int
digit = asum [ charAs (intToDigit i) i | i <- [0..9] ]

bracketDigit :: RegExp Int
bracketDigit = char '[' *> digit <* char ']'

processPrim :: Prim a -> StateT String Maybe a
processPrim (Prim c x) = do
    d:ds <- get
    guard (c == d)
    put ds
    pure x

matchPrefix :: RegExp a -> String -> Maybe a
matchPrefix re = evalStateT (runAlt processPrim re)

matchAlts :: RegExp a -> String -> Maybe a
matchAlts (Alt res) xs = asum [ matchChain re xs | re <- res ]

matchChain :: AltF Prim a -> String -> Maybe a
matchChain (Ap (Prim c x) next) cs = case cs of
    []  -> Nothing
    d:ds | c == d    -> matchAlts (($ x) <$> next) ds
         | otherwise -> Nothing
matchChain (Pure x)             _      = Just x

matches :: RegExp a -> String -> [a]
matches re = mapMaybe (matchPrefix re) . tails

firstMatch :: RegExp a -> String -> Maybe a
firstMatch re = listToMaybe . matches re
