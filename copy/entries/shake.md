Shake: Task Automation and Shell Scripting in Haskell
=====================================================

Categories
:   Haskell
Tags
:   shake
:   shell scripting
WriteTime
:   2013/08/24 19:31:22
PostDate
:   2013/09/24 22:36:03

As someone who comes from a background with ruby and rake, I'm used to
powerful task management systems with expressive dependency.
`Make` is a favorite tool of mine when I'm working on projects with people who
don't use ruby, and when I'm working on ruby projects I never go far without
starting a good `Rakefile`.  The two tools provied a perfect DSL for setting
up systems of tasks that had complicated file and task dependencies.

As I was starting to learn Haskell and building larger-scale Haskell projects,
I began to look for alternatives in Haskell.  Was there a Haskel counterpart
to Ruby's `rake`, Node's `jake`, Java's `ant`?

It turns out that by far the most established answer is a library known as
[Shake], (maintained by Neil Mitchell of [hoogle] fame).  So far it's served
pretty well.  Its documentation is written from the perspective of chiefly
using it as a build tool (more "make" than "rake"), so here is a quick primer
on how to get started using it as a task management system.

-------

First, the Shakefile from this blog.


~~~haskell
import Development.Shake
import Text.Pandoc
import Control.Monad
import Control.Monad.Loops
import Control.Monad.IO.Class
import System.INotify

opts = shakeOptions { shakeFiles    = ".shake/"
                    , shakeProgress = progressSimple }

(~>) = phony

binPath = "cabal-dev/bin/"
cmdBin = cmd . (++) binPath
cmdCabal = cmd "cabal-dev"
systemBin = system' . (++) binPath

main :: IO ()
main = shakeArgs opts $ do
  want ["build"]

  "clean" ~> removeFilesAfter ".shake" ["//*"]

  "build" ~>
    need [ "cabal-dev/bin/blog" ]

  "launch" ~> do
    need ["build"]
    systemBin "blog" []

  "migrate" ~> do
    need ["cabal-dev/bin/blog-scripts-migrate"]
    cmdBin "blog-scripts-migrate"

  "seed" ~> do
    need ["cabal-dev/bin/blog-scripts-seed"]
    cmdBin "blog-scripts-seed"

  "view-src" ~> do
    fs <- srcFiles
    liftIO $ print fs

  "watch-copy" ~>
    cmd "scripts/watch_copy.sh"


  "cabal-dev/bin/blog" *> \_ -> do
    fs <- srcFiles
    need fs
    cmdCabal ["install"]

  "cabal-dev/bin/blog-scripts-migrate" *> \_ -> do
    need ["build"]
    return ()

  "cabal-dev/bin/blog-scripts-seed" *> \_ -> do
    need ["build"]
    return ()

srcFiles :: Action [FilePath]
srcFiles = getDirectoryFiles ""
  [ "blog.cabal"
  , "src/*.hs"
  , "src/scripts/*.hs"
  , "src/Web/Blog/*.hs"
  , "src/Web/Blog/*/*.hs"
  , "src/Web/Blog/*/*/*.hs"
  , "src/Development/Blog/*.hs"
  , "src/Development/Blog/*/*.hs" ]
~~~

And that's it!

[Shake]: http://hackage.haskell.org/package/shake "Shake"
[hoogle]: http://haskell.org/hoogle "hoogle"
