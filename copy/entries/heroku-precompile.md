Deploying Medium/Large Haskell Apps to Heroku by Precompiling
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

If you do a search on how to deploy Haskell apps to Heroku these days, chances
are you are going to find the very elegant method (here's
[one solution][method1], and [another][method2]) involving leveraging Heroku's
powerful [Cedar stack][cedar] and having Heroku use `cabal install` to
download and compile your app and all of its dependencies into a native binary
on the server itself.  It's a rather beautiful solution to the problem of
a truly polyglot automated production server.

[method1]: http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html#deploying-to-heroku
[method2]: http://blog.begriffs.com/2013/08/deploying-yesod-to-heroku-with-postgres.html
[cedar]: https://devcenter.heroku.com/articles/cedar

The [blog engine][engine] that runs this blog is written in Haskell.  When I
tried to deploy it using those steps, I encountered a rather frustrating
roadblock:

[engine]: https://github.com/mstksg/blog

Heroku enforces a hard time-out limit of **fifteen minutes** for all of its
apps to compile and deploy.  And because *cabal* needs time to download and
compile every dependency, a typical non-trivial app (like a blog) would reach
this limit very quickly with only a [modest amount][deps] of dependencies.

[deps]: https://github.com/mstksg/blog/blob/master/blog.cabal#L20-52

I did some searching on this and asked around on the irc channel, but I was
not able to find any real-world examples of *non-trivial* apps being deployed
to Haskell using this method.  It seemed like most articles simply deployed a
toy project, and left it at that.  (On that note, if anyone has actually had
success with this, or knows someone who has, please let me know)

Until Heroku's time-out limit can be adjusted or bypassed, the only real
solution (besides incrementally pushing dependencies with a buildpack that
caches --- a solution even uglier and less practical) is to pre-compile your
binary to an architecture that Heroku supports.

Unfortunately, most tutorials on this are out-of-date and from the time before
these Haskell buildpacks were even written.  Let's try to see how we can get a
medium- to large- scale Haskell app on Heroku in 2013.

