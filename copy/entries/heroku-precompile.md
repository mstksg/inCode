Deploying Large Haskell Apps to Heroku by Precompiling
======================================================

Categories
:   Haskell
Tags
:   web development
:   heroku
CreateTime
:   2013/09/23 23:13:10
PostDate
:   Never
Identifier
:   heroku-precompile

The [blog engine][engine] that this blog is run on was written in Haskell.
Now, blog engines are not particularly complicated applications by any
stretch. Haskell, however, is a bit unique in comparison to the trendy ruby,
python, or php applications of the current day.  Haskell is a *compiled
language*.  Whatever the server runs has to be compiled to the specific
architecture of the server.  It cannot be run as-is with an interpreter.

<!-- Now, blog engines are not particularly complicated applications by any -->
<!-- stretch.  They're only one step above the Pastie Clone and the Todo List, and -->
<!-- if you don't expect to be able to add posts between deploys, are even simpler -->
<!-- in that they may be *completely static* and every page pre-compiled to html -->
<!-- before it even reaches the server. -->

No matter -- Heroku's brilliant [Cedar Stack][cedar] was built from the start
to support things like this.  And by using a well-made buildpack (methods
outlined [here][method1] and [here][method2]), deploying Haskell to Heroku is,
in principle, as straightforward as deploying any other interpreted language.
In principle.

This blog is a Haskell web app of unremarkable complexity and
[reasonable dependencies][dependencies].  However, when deploying using a
build pack, Cedar **times out** while downloading and compiling the app's
dependencies through cabal.  It appears that Cedar apps, as of September 2013,
have a hard-coded timeout of **fifteen minutes** --- any app that takes longer
than fifteen minutes to set up on the server is completely out of luck.

The only solutions, therefore, for an app with enough dependencies as to
require more than fifteen minutes to both download and compile, is to either
play an ugly dance of slowly committing more and more dependencies to force
cabal to install in small bursts and increments, or to compile the app to a
native binary that can be run on Heroku's servers out of the box.

This article is about the second option.

[engine]: https://github.com/mstksg/blog
[cedar]: https://devcenter.heroku.com/articles/cedar
[method1]: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html#deploying-to-heroku
[method2]: http://blog.begriffs.com/2013/08/deploying-yesod-to-heroku-with-postgres.html
[dependencies]: https://github.com/mstksg/blog/blob/master/blog.cabal#L20-52


The Problem
-----------

Yeah


