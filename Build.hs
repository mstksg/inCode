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

ghcjsReq :: FilePath -> FilePath
ghcjsReq f = "_ghcjs" </> (f <.> "jsexe") </> "all.js"

ghcjsExes :: [String]
ghcjsExes = ["blog-javascript-entry"]

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

      ghcjsReq "blog-javascript-entry" %> \out -> do
        need ["app-ghcjs/Entry.hs"]
        () <- cmd "stack --stack-yaml stack-ghcjs.yaml build"
        Stdout bindir <- cmd "stack --stack-yaml stack-ghcjs.yaml path --local-install-root"
        let bindir' = concat (lines bindir) </> "bin/blog-javascript-entry.jsexe"
        copyFile' (bindir' </> "all.js") (ghcjsReq "blog-javascript-entry")

      "clean" ~> do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter "_ghcjs" ["//*"]
        removeFilesAfter "_purescript" ["//*"]
        unit $ cmd "stack run -- blog-build" "clean"

capitalize [] = []
capitalize (c:cs) = toUpper c : cs
