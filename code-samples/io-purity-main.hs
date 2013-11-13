-- Demonstrating IO objects and purity.  getStringFromStdin and printFibN
-- n both return IO actions; we choose one of the actions and call it
-- "main", which the compiler will compile into a binary, or run
-- interpreted.
--
-- To test/run:
-- $ runghc ./io-purity-main.hs
--
-- http://blog.jle.im/entry/the-compromiseless-reconciliation-of-i-o-and-purity#the-main-point


--  getStringFromStdin: returns a computation that represents the act of
--  fib n: the nth Fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

--      getting a string from stdin.  or rather, a series of instructions on
--      interacting with the computer and generating a String.
getStringFromStdin :: IO String
getStringFromStdin = getLine

--  printFibN: returns a computation that represents the act of printing the
--      nth Fibonacci number to stdout and returns () (Nothing).  or rather,
--      a series of instruction on interacting with the computer to get it to
--      print a Fibonacci number and returning nothing.
printFibN :: Int -> IO ()
printFibN n = print (fib n)

--  main: The IO object that we agree that the execution environment will
--      execute.
main :: IO ()
main = printFibN 10
