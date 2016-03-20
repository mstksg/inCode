import Control.Monad.IO.Class
import Data.Char
import Data.Foldable
import Data.String
import Development.Shake
import Development.Shake.FilePath

opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Loud
                    , shakeThreads   = 0
                    }

psReq :: FilePath -> FilePath
psReq f = "_purescript" </> (f <.> "js")

psExes :: [String]
psExes = ["entry"]

main :: IO ()
main = do
    shakeArgs opts $ do
      want ["build"]

      "build" ~> do
        need (psReq <$> psExes)
        unit $ cmd "stack run -- blog-build" "build"

      "rebuild" ~> do
        need ["purescript"]
        unit $ cmd "stack run -- blog-build" "rebuild"

      "purescript" ~>
        need (psReq <$> psExes)
      
      "_purescript/*.js" %> \out -> do
        let exName = capitalize $ takeBaseName out
        need ["app-purescript" </> exName <.> "purs"]
        unit $ cmd "pulp build"
                   "--main" exName
                   "--src-path" "app-purescript"
                   "--to" out

      "clean" ~> do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter "_purescript" ["//*"]
        unit $ cmd "stack run -- blog-build" "clean"

capitalize [] = []
capitalize (c:cs) = toUpper c : cs
