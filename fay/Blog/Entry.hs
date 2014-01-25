{-# LANGUAGE PackageImports, CPP #-}

module Web.Blog.Fay.Entry where

#ifdef FAY
import Prelude
#else
import "fay-base" Prelude
#endif

main :: Fay ()
main = putStrLn "hello world!"
