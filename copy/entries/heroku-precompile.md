Deploying Medium/Large Haskell Apps to Heroku by Precompiling
=============================================================

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

How Heroku's Deploy/Proc System Works
-------------------------------------

Here's a quick refresher for those who are not certain how the Cedar stack
manages the app's file structure, its web workers, and procedures.

When you deploy an app to Heroku, Heroku first downloads and installs all
dependencies (through a process specified by the buildpack given) and then
[compiles][slug] the application's entire directory structure into a highly
compressed image for reasons of fast distribution.  This image is known as a
"slug".

[slug]: https://devcenter.heroku.com/articles/slug-compiler

There are two hard limits to this process --- the timeout limit (which we are
concerned with), which is set at **15 minutes**, and the compressed filesize
limit, which is set at **300 MB**.

To actually launch the application (and its associated processes), Heroku
looks to the *Procfile*, a file given at the root of the application
directory structure, and executes all commands listed.  The very basic
`Procfile` would look like this, for (as an example) a ruby project:

~~~yaml
# Procfile
web: ./application.rb
~~~

It's a structure very similar to the [YAML specification][yaml] for an
associative array.  The keys are the names of the processes, and the values
are the "command" that corresponds to launching that process.  The `web`
process is launched automatically, and the others can be triggered either
through Heroku's web interface or its command line toolkit.

[yaml]: http://en.wikipedia.org/wiki/YAML

[Neil Middleton][neil] (a Heroku engineer) offers
[a more detailed walkthrough][procfile] of how to craft a useful *Procfile*,
and the possibilities one can realize with one.

[neil]: http://www.neilmiddleton.com/
[procfile]: http://www.neilmiddleton.com/the-procfile-is-your-friend/

For our cases, it seems simple then how to deploy to Heroku via pre-compiling:
simply compile the binary on a similar architecture, commit the compiled
binary to version control (an admittedly ungraceful process), and specify in
the *Procfile* the command to execute that binary.

Let's walk through this.

Compiling Your Binary
---------------------











