Deploying Medium to Large Haskell Apps to Heroku by Precompiling

=================================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on October 7, 2013.
> [Read online!](https://blog.jle.im/entry/deploying-medium-to-large-haskell-apps-to-heroku.html)

**UPDATE**: This post was written in 2013, where the options available to
someone looking to host a Haskell site on Heroku were fairly limited. It's (as
of the time of writing this) 2015 now and things have changed. Check out [the
comments](http://blog.jle.im/entry/deploying-medium-to-large-haskell-apps-to-heroku#disqus_thread)
for two good alternatives to this that are working today!

Consider the rest of this article obsolete, or look here if none of the
solutions given in the comments work :)

## Old Article (Written 2013)

If you do a search on how to deploy Haskell apps to Heroku these days, chances
are you are going to find the very elegant method (here's [one
solution](http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html#deploying-to-heroku),
and
[another](http://blog.begriffs.com/2013/08/deploying-yesod-to-heroku-with-postgres.html))
involving leveraging Heroku's powerful [Cedar
stack](https://devcenter.heroku.com/articles/cedar) and having Heroku use
`cabal install` to download and compile your app and all of its dependencies
into a native binary on the server itself. It's a rather beautiful solution to
the problem of a truly polyglot automated production server.

The [blog engine](https://github.com/mstksg/blog) that runs this blog is written
in Haskell. When I tried to deploy it using those steps, I encountered a rather
frustrating roadblock:

Heroku enforces a hard time-out limit of **fifteen minutes** for all of its apps
to compile and deploy. And because *cabal* needs time to download and compile
every dependency, a typical non-trivial app (like a blog) would reach this limit
very quickly with only a [modest amount of
dependencies](https://github.com/mstksg/blog/blob/master/blog.cabal#L20-52).

I did some searching on this and asked around on the irc channel, but I was not
able to find any real-world examples of *non-trivial* apps being deployed to
Haskell using this method. It seemed like most articles simply deployed a toy
project, and left it at that. (On that note, if anyone has actually had success
with this, or knows someone who has, please let me know)

Until Heroku's time-out limit can be adjusted or bypassed, the only real
solution (besides incrementally pushing dependencies with a buildpack that
caches --- a solution even uglier and less practical) is to pre-compile your
binary to an architecture that Heroku supports.

There are some tutorials on this already, but few are up to date and
flexible/comprehensive, so here is my shot.

As an **important note**: this method is a lot less elegant and maintanable (in
principle) than the recommended buildpack method; only use it if you are
absolutely certain that buildpacks won't work for you.

Also note that this assumes that your app is already configured to work on
Heroku --- that it doesn't modify the filesystem after the deploy (so no
*sqlite*), it uses the `$DATABASE_URL` environment variable to establish
database connections, it uses the `$PATH` environment variable to choose the
port to listen to, and other small things you just have to worry about that is
out of the scope of this post.

Much of this post is owed to [this article on the Yesod
wiki](https://github.com/yesodweb/yesod/wiki/Deploying-Yesod-Apps-to-Heroku).

## Compiling Your Binary

### The Virtual Machine

First of all, we need to find ourselves a machine with the same architecture as
the Heroku virtual machines. For most people, it is not practical to go out and
buy a physical machine that you can use for yourself, so we're going to be
setting up a virtual one here.

A lot of this is going to be verbatim from [this
reference](https://github.com/yesodweb/yesod/wiki/Setting-up-a-virtual-machine%2C-using-VirtualBox-and-Vagrant),
with a few updates.

1.  You're going to need
    [VirtualBox](https://www.virtualbox.org/wiki/Downloads), from Oracle. Most
    modern work on virtual machines leverage this great tool.

2.  Install [vagrant](http://downloads.vagrantup.com/), a really convenient
    wrapper around the raw VirtualBox functionality that integrates things like
    build scripts and configurations into an easy-to-use package.

3.  Clone/download the
    [vagrant-haskell-heroku](https://bitbucket.org/puffnfresh/vagrant-haskell-heroku)
    project from BitBucket. This is the vagrant project that will set up
    everything you need to build and deploy to Heroku --- an installation of
    GHC, the Haskell Platform, and the Heroku Toolbelt, all on a 64 bit Ubuntu
    10.04 image.

4.  Edit the `Vagrantfile`; on the line starting with `chef.json.merge!`, change
    the values to the versions of [GHC](http://www.haskell.org/ghc/) and
    [Haskell Platform](http://www.haskell.org/platform/) you will be using.

    ``` ruby
    chef.json.merge!({ :ghc_version => '7.4.4',
                       :haskell_platform_version => '2012.4.0.0'})
    ```

    I'm using `ghc-7.6.3` to develop, and as of October 2013, the lastest stable
    Haskell Platform is `2013.2.0.0`.

5.  Launch your virtual machine with

    ``` bash
    $ vagrant up
    ```

    This will launch the VM and install the given versions of GHC and the
    Haskell Platform. You are mostly good to go now -- log onto your machine
    using

    ``` bash
    $ vagrant ssh
    ```

    If this doesn't work, try installing `libgc-dev`. `vagrant ssh` should send
    you into an "ssh" session on your VM. Once there, let's run some basic
    bookkeeping/updating that isn't handled by the vagrant project:

    ``` bash
    $ sudo apt-get update
    $ sudo apt-get upgrade
    $ sudo apt-get install git-core
    $ cabal update
    ```

And you should have a fresh virtual machine compatible with Heroku ready to
build your project on.

### Building Your Project

At this point, there are many ways to proceed. Building is now more or less the
same as if you were building on your own production server. Here is one way to
go about it.

1.  Get your project files onto your guest (virtual) machine.

    -   If your projects are on a version control repository like
        [Github](https://www.github.com) or any accessible server, then getting
        your files on your guest machine and keeping them up-to-date is as easy
        as a `git pull` (substitute git for whatever version control you use).

    -   If not, the directory that your vagrant files are stored in is actually
        mounted onto the file system of the guest machine. You can access it at
        `/vagrant` on the guest machine.

        This is really handy for transferring things like ssh keys, but you can
        also use this to get your project files onto your guest machine. Simply
        copy them over somehow onto a folder in the vagrant directory, and you
        can then access them on your virtual machine and do what you want with
        them.

        However, if you want to keep your files up to date, you'll have to do
        this manually.

    -   If you are using version control like *git*, but you aren't hosting it
        on a server (and why not? you can even [host a repo server locally on
        your own machine](https://github.com/sitaramc/gitolite).), see if you
        can use a local folder as a repository source.

        For *git*, this is pretty simple. You only need to run `git init --bare`
        on the folder you want to use as the repository, and add the local
        filesystem path as the remote url. There are many resources explaining
        this process in detail, like [this
        post](http://www.jedi.be/blog/2009/05/06/8-ways-to-share-your-git-repository/)
        and [this
        one](http://treeleaf.be/blog/2011/03/creating-a-new-git-repository-on-a-local-file-system/)

2.  Build the executable. This is the same as on any machine. However, I
    strongly recommend using some kind of sandboxing system like
    [cabal-dev](http://hackage.haskell.org/package/cabal-dev), or cabal 1.18's
    built-in sandboxing, just to make sure you don't run into any problems in
    the future.

    ``` bash
    #   using cabal-dev
    $ cabal install cabal-dev
    #   you can add ~/.cabal/bin to your $PATH if you want
    $ ~/.cabal/bin/cabal-dev install
    ```

    If any of your cabal packages require developer libraries to be installed on
    your machine (anything involving Postgres comes to mind), you'll need to be
    sure that they are installed. A simple `apt-get` should take care of this
    for all relevant packages.

    After everything downloads, builds, installs, etc., your executable will be
    created as `dist/build/app-name/app-name`.

    This will also be a good test as to whether or not you specified your
    dependencies in your `.cabal` file properly.

## Deploying it all

Almost there! Your binary is now compiled; how are you going to deploy it to
Heroku?

1.  First, you have to commit your binary to version control. Some people
    recommend using a separate branch for this, but because your guest machine's
    project directory is kind of a transient thing, this really isn't that
    necessary.

    You can simply forcefully add the file to git as it is, because chances are
    you have it already in your `.gitignore`:

    ``` bash
    $ git add -f dist/build/app-name/app-name
    ```

    Alternatively, you can create a `bin/` folder and copy the executable there.
    It really doesn't make a difference, except that you don't have to modify
    your `.gitignore`.

2.  Now, you need to create your `Procfile` --- this specifies the processes
    that Heroku will be executing.

    ``` yaml
    # Procfile
    web: # system command to launch your server
    ```

    For some web servers, it is simply the path to the executable; for some
    frameworks like *Yesod*, you need to specify the flag `-p $PORT`, because
    Heroku specifies the port you are to listen to via the environment variable.

3.  Heroku requires every project to have *some* buildpack. Because the actual
    web processes are specified in your `Procfile`, buildpacks won't interfere
    with any actual execution of your server.

    There are three easy ways to do this --- you can either use a blank
    `requirements.txt` (the easiest way) to act like a *Python* app, a valid but
    empty `package.json` to act like a *Node.js* app, or a valid but empty
    `Gemfile` and `Gemfile.lock` combination to act like a *ruby* app.

    But hey, if you use any node packages or gems or python packages in your
    project, then you can actually use this to your advantage! I personally use
    *[compass](http://compass-style.org/)* a lot for their extensions to *sass*,
    so adding it is as simple as using a *Gemfile* --- just like in any normal
    ruby app.

    If you want to mix and match libraries from different languages/ecosystems,
    you can use the [multi](https://github.com/ddollar/heroku-buildpack-multi)
    buildpack and have Heroku check for packages in all of the normal package
    managers of the languages you specify.

4.  Configure your [Heroku Toolbelt](https://toolbelt.heroku.com/), and deploy.

    ``` bash
    #   create your app
    $ heroku create appname
    #   and, after making sure everything is set up, committed, and in order...
    $ git push heroku master
    ```

    If you have done everything right, this should be successful. Hooray!

5.  Make sure your `web` process is running properly. You can do this by going
    to <https://dashboard.heroku.com/apps>, clicking on your app, and making
    sure under **Dynos** that the check box next to "web" is checked off.

And that should be it!

## After the Deploy

Your app should be running successfully now! Probably. Maybe. If you run into
any problems, let me know in the comments. But to preempt any issues that might
arise, here are some things that it might be important to pay attention to.

-   If you are using Heroku's Postgres instances (and you should, they are
    amazing and probably more reliable than anything you could host yourself on
    cheap, for free), you will have to make sure to [configure them
    properly](https://devcenter.heroku.com/articles/heroku-postgresql).

    If you have problems making a connection, you can try the
    [heroku](http://hackage.haskell.org/package/heroku) package on Hackage and
    integrate it with your database connection backends.

-   You are probably going to want to automate your entire re-deploy process ---
    the pull, the build/install, the copying of the executable, the committing
    of the binary to version control, and the deploy to Heroku.

    You can use your favorite task management system, like *Make*, *Rake*, or
    even *Shake* (see my [brief tutorial on
    Shake](/entry/shake-task-automation-and-shell-scripting-in-haskell))

    Your basic workflow should consist of pushing your project files to your
    repository on your host machine, and `vagrant ssh`-ing onto your guest
    machine and executing one or two commands to automate the entire re-deploy
    process.

-   Be aware of good virtual machine management practices. Suspend your machine
    whenever you are not using it:

    ``` bash
    #   suspend
    $ vagrant suspend
    #   resume
    $ vagrant resume
    ```

    and you will also prevent things from getting hairy in case of a system
    crash on the host side.

Good luck developing for the web on the Haskell platform, and welcome to the
club!

--------------------------------------------------------------------------------

Hi, thanks for reading! You can reach me via email at <justin@jle.im>, or at
twitter at [\@mstk](https://twitter.com/mstk)! This post and all others are
published under the [CC-BY-NC-ND
3.0](https://creativecommons.org/licenses/by-nc-nd/3.0/) license. Corrections
and edits via pull request are welcome and encouraged at [the source
repository](https://github.com/mstksg/inCode).

If you feel inclined, or this post was particularly helpful for you, why not
consider [supporting me on Patreon](https://www.patreon.com/justinle/overview),
or a [BTC donation](bitcoin:3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU)? :)

