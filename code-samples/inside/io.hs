-- Load it up inside GHCI and give follow along with the post!
--
-- ghci> :l io.hs
--
-- Or run it:
--
-- $ runghc io.hs
--
-- Before starting anything on ghci
--
-- ghci> :set -XNoMonomorphismRestriction
--
-- to get rid of many potential frustrations.
--
-- http://blog.jle.im/entry/inside-my-world

module InsideIO where

wc :: String -> IO Int
wc fp = fmap (length . lines) (readFile fp)

main :: IO ()
main = print =<< wc =<< getLine
