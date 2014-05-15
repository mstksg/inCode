-- Load it up inside GHCI and give follow along with the post!
--
-- λ: :l io.hs
--
-- Or run it:
--
-- $ runghc io.hs
--
-- http://blog.jle.im/entry/inside-my-world

module InsideIO where

wc :: String -> IO Int
wc fp = fmap (length . lines) (readFile fp)

main :: IO ()
main = print =<< wc =<< getLine
