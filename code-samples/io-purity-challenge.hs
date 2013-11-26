-- Demonstrating IO objects and purity.  (>>=) is an operator that composes
-- two IO actions into one, by taking the result of the left hand side and
-- using it as a parameter to the right hand side.
--
-- The program apparently acts differently every time you run it.  How is
-- it still pure?
--
-- To test/run:
-- $ runhaskell ./io-purity-challenge.hs
--
-- http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity#purity-challenged


--  getStringFromStdin: returns a computation that represents the act of
--      getting a string from stdin
getStringFromStdin :: IO String
getStringFromStdin = getLine

--  main: The IO object that we agree that the compiler will actaully
--      compile.
main :: IO ()
main = getStringFromStdin >>= (\result -> print result)
