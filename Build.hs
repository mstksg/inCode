import Control.Monad.IO.Class
import Data.Foldable
import Data.String
import Development.Shake
import Development.Shake.FilePath

opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Loud
                    , shakeThreads   = 0
                    }


-- jsexeFiles :: [FilePath]
-- jsexeFiles = [ "runmain.js", "rts.js", "lib.js", "out.js", "all.js" ]

ghcjsReq :: FilePath -> FilePath
ghcjsReq f = "_ghcjs" </> (f <.> "jsexe") </> "all.js"

ghcjsExes :: [String]
ghcjsExes = ["blog-javascript-entry"]

main :: IO ()
main = do
    shakeArgs opts $ do
      want ["build"]

      "build" ~> do
        need (ghcjsReq <$> ghcjsExes)
        unit $ cmd "stack run -- blog-build" "build"

      "rebuild" ~> do
        need (ghcjsReq <$> ghcjsExes)
        unit $ cmd "stack run -- blog-build" "rebuild"

      ghcjsReq "blog-javascript-entry" %> \out -> do
        need ["app-ghcjs/Entry.hs"]
        () <- cmd "stack --stack-yaml stack-ghcjs.yaml build"
        Stdout bindir <- cmd "stack --stack-yaml stack-ghcjs.yaml path --local-install-root"
        let bindir' = concat (lines bindir) </> "bin/blog-javascript-entry.jsexe"
        copyFile' (bindir' </> "all.js") (ghcjsReq "blog-javascript-entry")

      "clean" ~> do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter "_ghcjs" ["//*"]
        unit $ cmd "stack run -- blog-build" "clean"

