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

In this post I'll also be explaining a bit of *nix*, so hopefully it's
accessible if you are curious like I was too!

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

[scotty]: https://hackage.haskell.org/package/scotty
[fay]: https://hackage.haskell.org/package/fay
[purescript]: https://www.purescript.org/
[stack]: https://docs.haskellstack.org/en/stable/
[hakyll]: https://jaspervdj.be/hakyll/
[dhall]: https://dhall-lang.org/
[shake]: https://shakebuild.com/

As you can see, I sort of used my blog as a playground to apply new ideas I've
learned and to build familiarity with them, and I have some fun refactoring
every couple of years because Haskell is so fun to refactor.

However, the current state of things is kind of a monster to build.  First, you
have to have purescript *and* npm *and* bower (for general javascript
deployment) installed.  Then the build script (the Shakefile) compiles
javascript and purescript into the working directory, then calls hakyll which
does the rest. It's a really finicky system that involves having the correct
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

The Game Plan
-------------

Going into this, the general workflow would be:

1.  Create a *nix derivation* for all of the purescript scripts, that generates
    a single javascript file each.
2.  Create a nix derivation for the *hakyll* static site generator binary
3.  Create a nix derivation for the full static site itself: it will assemble
    all of the compiled purescript files from step 1, the source
    files (markdown, static images, etc.) from the repo, and then run the
    hakyll binary on it.
4.  Create a github actions workflow to automatically build the full static
    site generation workflow.
5.  Use *[cachix][]* to ensure that all derivations where the dependencies
    don't change will not need to be re-built.
6.  If step 4 completes, automatically push the derivation's outputs to the
    *gh-pages* branch.

[cachix]: https://www.cachix.org/

One cool thing about cachix is that you get build caching for free for any
derivation you hand-write across all of your environments.  You don't need to
register your "package" to any sort of registry or set up custom cloud caching
in every situation.

### What is a Derivation?

The main character in all of these steps is the *nix derivation*.  In my mental
model, a nix derivation is a recipe for building "something" -- sometimes, it's
a binary, and in our case it's often just going to be a collection of files.  A
derivation contains a list of pinned reproducible dependency derivations --- so
it's all reproducible down the whole chain.  In our case:

*   The final derivation (the static site's html files and contents) *depends*
    on the hakyll binary derivation and the compiled purescript derivation
*   The hakyll binary derivation depends on a pinned ghc, cabal, and a
    derivation for every haskell library dependency.
*   The compiled purescript derivation depends on the purescript
    compiler (purs), as well as all purescript library dependencies

When we say "depends", it just means that uses what the derivation outputs in
one way or another.  The final static site derivation uses the *binary*
outputted by the hakyll binary derivation.  It also uses the *javascript
files* outputted by the compiled purescript derivation.  And its *output* is
just the static pages and contents of the site.

One major leap for me was realizing that derivations just created output files.
In my mind, before this, I thought that a derivation was something "executable"
--- when you first get into nix and nixos, "derivation" and "executable" can
sometimes appear synonymous.  To install a new executable in nixos (like *vim*
or any other linux program), you installed its derivation.  However, this was a
misconception. It's just a *convention* that if your derivation is meant to
provide an executable, you put the binary file in the `./bin/` subdirectory of
your output files.  In our case, our output files are sometimes binaries (which
we put in `./bin/`), but they're also sometimes static files, which we put in
`./dist/` or `./js/` or wherever we want. It's just a "convention" that the
`./bin` subdirectory of your output contains a binary that the caller would
want in their PATH, and so nix automatically puts the `./bin` of any dependency
derivation in the PATH/execution environment for convenience.  At least, that's
my mental model and how I understand it --- I am definitely new to this, so if
I'm incorrect at any point, feel free to leave a comment.

### Prior Art

While researching this further, I noticed that this approach is similar to that
of *[hakyll-nix-template][]*: have a derivation for the hakyll binary, then a
derivation for the final website, then push to gh-pages with cachix to ensure
caching haskell dependencies.  Because of this, I was able to use a lot of code
from that repository to help figure things out for my own path.  So, a special
thanks to [Robert Pearce][].

[hakyll-nix-template]: https://github.com/rpearce/hakyll-nix-template
[Robert Pearce]: https://github.com/rpearce

### Shortcomings: Static Artifact Caching

There is one shortcoming to this approach that I consider very significant --
using nix with *cachix* ensures that we don't redo work at the derivation
level.  So, if the purescript files don't change, we won't re-build the
compiled javascript.  If the haskell files don't change, we won't re-build the
hakyll binary.  But for a static site, we also have to build the site based on
the input files --- mostly, markdown files.  For my blog, this is not trivial
-- each markdown post gets compiled and outputted into many different formats
(latex, pdf, html multiple times), and actually gets re-combined with re-usable
code snippets from different folders.  In addition, we build a page for each of
the many tags, a paginated homepage, a granular history page, and an rss feed.
On a whole, a build from scratch takes about five minutes.

I'm not sure if this can be optimized on the Haskell side, but it's still kind
of wasteful in principle, because *hakyll* is smart enough to only re-build
what is needed, if it's run in its normal mode of operation.  If you update
only a single post, hakyll will only update the pages that are effected --- the
home page, the rss feed, and any tag pages it appears on, etc.  It knows not to
re-generate any single blog post pages or pdf files for different blog posts
that aren't affected by the new update.

If you're using github actions in the normal way, you might be able to take
advantage of this with github actions caches?  It's unclear to me how change
detection interacts with this. However, in the "nix style", if we really want
to view the final generated site as a *nix derivation* (and not just, ie, a
build script), it's a little more complicated to make this fit into the
conceptual model and make sure cache invalidation etc. works correctly.

What would be really ideal is if these units of cached data (individual web
pages, etc.) could be managed and invalidated by nix itself --- maybe each blog
post's outputs (the html, pdf, post-processed markdown, latex) could be a
derivation, and so these derivations won't have to be re-built if the blog post
is not changed.  There seems to be a few options in terms of "purely nix"
static site generators that could do this I think, but that's not really an
option to me because the blog at this point is pretty huge, logic-wise, and
also I would rather write the blog in Haskell in the end still.

Maybe in the future I could figure out how to modify hakyll to use the nix
cache for its caching and invalidation, instead of its own bespoke method?  I
wonder if that's even a viable option.  If anyone has any insight, this newbie
would really appreciate hearing!
