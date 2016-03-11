{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

import "base" Prelude
import Fay
import Data.Default

--                      , NoImplicitPrelude
--                      , PackageImports

main :: IO ()
main = do
    print =<< compileFile def "fay/test.hs"
