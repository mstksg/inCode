---
title: "I nixified my blog"
categories: Meta
tags: haskell, nix, purescript
create-time: 2023/12/30 11:38:08
identifier: nixify-blog
slug: i-nixified-my-blog
---

Hi all! It's been a while (almost three years) since my last post.  But it's a
new year, a new me -- and I just recently started working at a new job writing
Haskell where everything is built and deployed using [nix][].  Aside from the
bliss that comes from being able to write Haskell as a day job, I also enjoyed
the push to finally dive into the nix ecosystem for my general personal
computing, which many of my friends have been subtly (or not so subtly) pushing
me to look into for a long time. And I have to say, I'm hooked!  I get a lot of
similar joy using nix to organize my projects as I did when I was first
learning Haskell.  My path to re-organizing all of my personal projects to
using nix has lead me back to one of my more "longer-running" pieces of legacy
code -- my 10 year old blog.  So, this is a post from a naive new nix user on
how I converted my blog deployment and building from a manual multi-stage build
process into an automatic nix-with-cachix deploy -- and some future things I
would be hoping to investigate!

[nix]: https://nixos.org/

How Did We Get Here
-------------------

If you want to just jump right into the nixification, feel free to skip this
section, which gives the context of the state of my blog before and what needed
to change.

The [development history][] of my blog (which just turned 10 a few months ago,
actually) has gone through a few stages: (which you might be able to track [on
github][])

[development history]: https://blog.jle.im/entries/category/@meta.html
[on github]: https://github.com/mstksg/inCode

1. A simple *[scotty][]* server on Heroku backed by a cloud-hosted postgresql
   database.  My first ever non-trivial Haskell project!
2. Using *[fay][]* to compile to javascript for interactivity and
   post-processing in blog posts.
3. Converting *fay* to *ghcjs*, to write javascript in Haskell.
4. Switching from "classic cabal" to *[stack][]* for development and building.
5. Using *[shake][]* for build/deploy scripts.
6. Converting *ghcjs* to *[purescript][]*, because I didn't want to bundle 1.5
   MB of Haskell Runtime just to generate a table of contents.
7. The biggest change: moving from *scotty* to *[hakyll][]*, from a simple HTTP
   server to a static site generation.
8. Deploying to github pages within a build script, using a custom haskell tool
   I had written a long time ago.
8. Moving from yaml for configuration to *[dhall][]*.

[scotty][]: https://hackage.haskell.org/package/scotty
[fay][]: https://hackage.haskell.org/package/fay
[purescript][]: https://www.purescript.org/
[stack][]: https://docs.haskellstack.org/en/stable/
[hakyll][]: https://jaspervdj.be/hakyll/
[dhall][]: https://dhall-lang.org/
[shake][]: https://shakebuild.com/

As you can see, I sort of used my blog as a playground to apply new ideas I've
learned and to build familiarity with them, and I have some fun refactoring
every couple of years because Haskell is so fun to refactor.

However, the current state of things is kind of a monster to build.  First, you
have to have *purescript* AND *npm* AND *bower* (for general javascript
deployment) installed.  Then the build script (the Shakefile) compiles
javascript and purescript into the working directory, then calls hakyll which
does the rest.  It's a really finicky system that involves having the correct
javascript ecosystem dependencies already installed -- and also uses a
deprecated system for building purescript.  At least we had *stack* to manage
having the correct ghc version.

Looking at this in 2023, there are a few things that had to change:

1. The purescript ecosystem has moved onto *[spago][]* for dependency
   management.
2. I have been moving most of my projects from stack back to pure cabal using
   the new modern tooling
3. I have moved dev environments a few times and every time it is a huge pain
   to remember the exact set of things that need to be installed for all stages
   to build and work correctly.  I'd like to move from an ad-hoc build script
   dependent on the environment into a single tool that manages everything.
4. I wanted to get automatic deployments to github pages running instead of
   having to manually deploy.

[spago]: https://github.com/purescript/spago

Some of these don't necessarily require nix, but I figured that this would be a
good opportunity to check everything off and seeing what nix had to offer.
