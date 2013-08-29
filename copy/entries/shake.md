Shake: Task Automation and Shell Scripting in Haskell
=====================================================

Categories
:   Haskell
Tags
:   shake
:   shell scripting
CreateTime
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

Okay, let's start!

Sample Shakefile
----------------

First, the Shakefile from this blog.

~~~haskell
{-# LANGUAGE OverloadedStrings #-}

-- import Network.Wai
-- import Web.Blog.Database
import Control.Monad.IO.Class
import Development.Blog.Util
import Network.Wai.Middleware.Headers
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Web.Blog.Routes
import Web.Scotty

main :: IO ()
main = scotty 4288 $ do

  liftIO startupHelpers

  middleware logStdoutDev
  middleware $ addHeaders [("Cache-Control","max-age=86400")]
  middleware $ staticPolicy (noDots >-> addBase "static")
  middleware $ staticPolicy (noDots >-> addBase "tmp/static")
  middleware $ addHeaders [("Cache-Control","max-age=0")]

  route
~~~

And that's it!

[Shake]: http://hackage.haskell.org/package/shake "Shake"
[hoogle]: http://haskell.org/hoogle "hoogle"
