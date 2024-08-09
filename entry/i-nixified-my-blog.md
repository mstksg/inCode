I nixified my blog

===================

> Originally posted by [Justin Le](https://blog.jle.im/) on January 1, 2023.
> [Read online!](https://blog.jle.im/entry/i-nixified-my-blog.html)

Happy new year! It's been a while (almost three years) since my last post. But
it's a new year, a new me -- and I just recently started working at [a new
job](https://github.com/anduril) writing Haskell where everything is built and
deployed using [nix](https://nixos.org/). Aside from the bliss that comes from
being able to write Haskell as a day job, I also enjoyed the push to finally
dive into the nix ecosystem for my general personal computing, which many of my
friends have been subtly (or not so subtly) pushing me to look into for a long
time. And I have to say, I'm hooked! I get a lot of similar joy using nix to
organize my projects as I did when I was first learning Haskell. My path to
re-organizing all of my personal projects to using nix has lead me back to one
of my more "longer-running" pieces of legacy code -- my 10 year old blog. So,
this is a post from a naive new nix user on how I converted my blog deployment
and building from a manual multi-stage build process into an automatic
nix-with-cachix deploy -- and some future things I would be hoping to
investigate!

In this post I'll also be explaining a bit of *nix*, so hopefully it's
accessible if you are curious like I was too. However, it's *not* a tutorial ---
instead, it's a high-level overview of the concepts that I put together to
achieve the goal.

## How Did We Get Here

The [development history](https://blog.jle.im/entries/category/@meta.html) of my
blog (which just turned 10 a few months ago, actually) has gone through a few
stages: (which you might be able to track [on
github](https://github.com/mstksg/inCode))

1.  A simple *[scotty](https://hackage.haskell.org/package/scotty)* server on
    Heroku backed by a cloud-hosted postgresql database. My first ever
    non-trivial Haskell project!
2.  Using *[fay](https://hackage.haskell.org/package/fay)* to compile to
    javascript for interactivity and post-processing in blog posts.
3.  Converting *fay* to *ghcjs*, to write javascript in Haskell.
4.  Switching from "classic cabal" to
    *[stack](https://docs.haskellstack.org/en/stable/)* for development and
    building.
5.  Using *[shake](https://shakebuild.com/)* for build/deploy scripts.
6.  Converting *ghcjs* to *[purescript](https://www.purescript.org/)*, because I
    didn't want to bundle 1.5 MB of Haskell Runtime just to generate a table of
    contents.
7.  The biggest change: moving from *scotty* to
    *[hakyll](https://jaspervdj.be/hakyll/)* (a static site generator), from a
    simple HTTP server to a static site generation.
8.  Deploying to github pages within a build script, using a custom haskell tool
    I had written a long time ago. I honestly don't remember where the site was
    hosted before github pages, but I remember having to configure my own CDN
    and https for a my hosted static website and at some point I wondered
    "why?".
9.  Moving from yaml for configuration to *[dhall](https://dhall-lang.org/)*.

As you can see, I sort of use my blog as a playground to apply new ideas I'm
learning build familiarity with them, and I have some fun refactoring every
couple of years because Haskell is so fun to refactor.

However, the current state of things is kind of a monster to build. First, you
have to have purescript *and* npm *and* bower (for general javascript
deployment) installed. Then the build script (the Shakefile) compiles javascript
and purescript into the working directory, then calls hakyll which generates the
actual site files. It's a really finicky system that involves having the correct
javascript ecosystem dependencies already installed -- and also uses a
deprecated system for building purescript. At least we had *stack* to manage
having the correct ghc version.

Looking at this in 2023, there are a few things that had to change:

1.  The purescript ecosystem has moved onto
    *[spago](https://github.com/purescript/spago)* for dependency management.
2.  I have been moving most of my projects from stack back to pure cabal using
    the new modern tooling
3.  I have moved dev environments a few times and every time it is a huge pain
    to remember the exact set of things that need to be installed for all stages
    to build and work correctly. I'd like to move from an ad-hoc build script
    dependent on the environment into a single tool that manages everything.
4.  I wanted to get automatic deployments to github pages running instead of
    having to manually deploy.

Some of these don't necessarily require nix, but I figured that this would be a
good opportunity to check everything off and seeing what nix had to offer.

## The Game Plan

Going into this, the general workflow would be:

1.  Create a *nix derivation* for all of the purescript scripts, that generates
    a single javascript file each.
2.  Create a nix derivation for the *hakyll* static site generator binary
3.  Create a nix derivation for the full static site itself: it will assemble
    all of the compiled purescript files from step 1, the source files
    (markdown, static images, etc.) from the repo, and then run the hakyll
    binary on it.
4.  Create a github actions workflow to automatically build the full static site
    generation workflow.
5.  Use *[cachix](https://www.cachix.org/)* to ensure that all derivations where
    the dependencies don't change will not need to be re-built.
6.  If step 4 completes, automatically push the derivation's outputs to the
    *gh-pages* branch.
7.  Establish *local development environments* that nix can set up for us:
    1.  Haskell development -- with haskell language server, cabal for
        intermediate building, etc.
    2.  Purescript development -- I've structured each interactive blog post's
        purescript source as its own spago project, so we actually need to
        generate a separate development environment (with purescript language
        server, etc.) for each project. I don't expect to be working on more
        than one at a given time.
    3.  *Writing* -- with the hakyll binary available for intermediate caching
        for fast builds and updates and hakyll's local preview server, and the
        *purescript* compiler available for fast compilation for the scripts
        driving interactive blog posts.

One cool thing about cachix is that you get build caching for free for any
derivation you hand-write across all of your environments. You don't need to
register your "package" to any sort of registry or set up custom cloud caching
in every situation.

### What is a Derivation?

The main character in all of these steps is the *nix derivation*. In my mental
model, a nix derivation is a recipe for building "something" -- sometimes, it's
a binary, and in our case it's often just going to be a collection of files. A
derivation contains a list of pinned reproducible dependency derivations --- so
it's all reproducible down the whole chain. In our case:

-   The final derivation (the static site's html files and contents) *depends*
    on the hakyll binary derivation and the compiled purescript derivation
-   The hakyll binary derivation depends on a pinned ghc, cabal, and a
    derivation for every haskell library dependency.
-   The compiled purescript derivation depends on the purescript compiler
    (purs), as well as all purescript library dependencies

When we say "depends", it just means that uses what the derivation outputs in
one way or another. The final static site derivation uses the *binary* outputted
by the hakyll binary derivation. It also uses the *javascript files* outputted
by the compiled purescript derivation. And its *output* is just the static pages
and contents of the site.

One major leap for me was realizing that derivations just created output files.
In my mind, before this, I thought that a derivation was something "executable"
--- when you first get into nix and nixos, "derivation" and "executable" can
sometimes appear synonymous. To install a new executable in nixos (like *vim* or
any other linux program), you installed its derivation. However, this was a
misconception. It's just a *convention* that if your derivation is meant to
provide an executable, you put the binary file in the `./bin/` subdirectory of
your output files. In our case, our output files are sometimes binaries (which
we put in `./bin/`), but they're also sometimes static files, which we put in
`./dist/` or `./js/` or wherever we want. It's just a "convention" that the
`./bin` subdirectory of your output contains a binary that the caller would want
in their PATH, and so nix automatically puts the `./bin` of any dependency
derivation in the PATH/execution environment for convenience. At least, that's
my mental model and how I understand it --- I am definitely new to this, so if
I'm incorrect at any point, feel free to leave a comment.

### What is a development environment?

Very important to this system is the local development environment that lets us
quickly re-build the site (and also lets us use hakyll's built-in preview server
features). I have a little less refined of a mental model for these than I do
for the derivation --- but apparently it's just a derivation that sets up a
shell with everything we need in the PATH? In any case, we need to set one up
here to drop us in a Haskell development environment to get all of our favorite
Haskell development tools -- like Haskell Language Server and cabal -- and a
snapshot of all of our Haskell dependencies. And then we need one that just
gives us the hakyll binary, which has interactive development features like
directory watching, local servers, and incremental builds/cache invalidation.

### Prior Art

While researching this further, I noticed that this approach is similar to that
of *[hakyll-nix-template](https://github.com/rpearce/hakyll-nix-template)*: have
a derivation for the hakyll binary, then a derivation for the final website,
then push to gh-pages with cachix to ensure caching haskell dependencies.
Because of this, I was able to use a lot of code from that repository to help
figure things out for my own path. So, a special thanks to [Robert
Pearce](https://github.com/rpearce)!

### Shortcomings: Static Artifact Caching

There is one shortcoming to this approach that I consider very significant --
using nix with *cachix* ensures that we don't redo work at the derivation level.
So, if the purescript files don't change, we won't re-build the compiled
javascript. If the haskell files don't change, we won't re-build the hakyll
binary. But for a static site, we also have to build the site based on the input
files --- mostly, markdown files. For my blog, this is not trivial -- each
markdown post gets compiled and outputted into many different formats (latex,
pdf, html multiple times), and actually gets re-combined with re-usable code
snippets from different folders. In addition, we build a page for each of the
many tags, a paginated homepage, a granular history page, and an rss feed. On a
whole, a build from scratch takes about five minutes. This is a significant
downgrade --- in the past, I could build and deploy almost instantaneously
(under a minute).

I'm not sure if this can be optimized on the Haskell side, but it's still kind
of wasteful in principle, because *hakyll* is smart enough to only re-build what
is needed, if it's run in its normal mode of operation. If you update only a
single post, hakyll will only update the pages that are effected --- the home
page, the rss feed, and any tag pages it appears on, etc. It knows not to
re-generate any single blog post pages or pdf files for different blog posts
that aren't affected by the new update.

If you're using github actions in the normal way, you might be able to take
advantage of this with github actions caches? It's unclear to me how change
detection interacts with this. However, in the "nix style", if we really want to
view the final generated site as a *nix derivation* (and not just, ie, a build
script), it's a little more complicated to make this fit into the conceptual
model and make sure cache invalidation etc. works correctly.

What would be really ideal is if these units of cached data (individual web
pages, etc.) could be managed and invalidated by nix itself --- maybe each blog
post's outputs (the html, pdf, post-processed markdown, latex) could be a
derivation, and so these derivations won't have to be re-built if the blog post
is not changed. There seems to be a few options in terms of "purely nix" static
site generators that could do this I think, but that's not really an option to
me because the blog at this point is pretty huge, logic-wise, and also I would
rather write the blog in Haskell in the end still.

Maybe in the future I could figure out how to modify hakyll to use the nix cache
for its caching and invalidation, instead of its own bespoke method? I wonder if
that's even a viable option. If anyone has any insight, this newbie would really
appreciate hearing!

In any case, for now, we can still have fast local development building (more on
that later), so it isn't super painful to actually write test. However, the
deployment lag is still kind of annoying --- and the "principal" of it (the
wasted work) does still bother me.

## Assembling the pieces

Again, this isn't a tutorial --- here, I'm going to explain to explain the
components I put together at a high level.

We're going to structure this in terms of a nix flake, which I'm using to:

-   Pin dependencies and package sets
-   Describe buildable derivations and the development environments in a single
    file, which we can run with `nix build` and `nix develop` commands.

For reference, the "final" nix flake described in this post [can be found on
github](https://github.com/mstksg/inCode/blob/bf444098b8773b1b47c0b2e66ca1682cdc5f674d/flake.nix).

### Haskell

For Haskell, I ended up using
*[haskell.nix](https://input-output-hk.github.io/haskell.nix/#haskellnix)*,
which is what I also what we use at work. It does this neat thing where it
parses a cabal file and uses *that* to generate the derivation needed --- which
includes the dependency derivations for all haskell library derivations. In this
way, it can cache all external dependencies. It also lets us solve for which
version of each dependency we need. And, by freezing it with flake lock, it also
allows us to freeze all of the versions of each dependency in a way that works.
There might be other tools that allow this, but I went with haskell.nix because
I'm already familiar with it. It also has a nice development environment offered
that contains haskell language server with all of the right package versions and
artifacts.

Armed with this, we can easily generate a derivation for the hakyll binary, by
passing the cabal file to the `haskell-nix.project'` function provided by
haskell.nix. Now step 1 is complete!

Note we also get our development environment as well -- you get it with the
`devShell` property!

### Purescript

For Purescript, I ended up using [purifix](https://github.com/purifix/purifix),
which does a similar thing with *haskell.nix* but for the *spago.yaml* file,
which is how modern purescript projects declare their dependencies. Purifix also
has this mode of operation where it auto-detects if you're working in a
monorepo-style project with multiple binaries, and gives you a *separate
derivation* for each binary (or, compiled .js file)! And also a separate
development environment for each binary, too. This works well for me because I
structure the javascript for each interactive blog post as its own project. So,
I can export an entire "project" (blog post file) to a bundled file easily, and
also lead development environments for specific blog posts as well.

For each project, it provides a `bundle-app` derivation that is literally just
the generated bundled-up single javascript file. This is exactly the derivation
we need for the final part where we pull everything together.

However, it also gives us an easy way to access all of the derivations of all of
the dependencies (the `globs` property it adds onto its derivations) --- which
is very useful for our final integrated development environment which needs to
do incremental builds quickly.

A note -- finding all of these useful properties and derivations (outside of the
main ones) does take a bit of trial and error -- digging through `nix repl` to
explore the derivation contents, and the source code of these projects.

### Static Site derivation

Putting it all together is the final derivation to generate the actual site:

``` nix
web = pkgs.stdenv.mkDerivation {
  name = "inCode";
  buildInputs = [ inCode.haskell ] ++ lib.attrValues inCode.purescript;
  srcs = [
    ./code-samples
    ./config
    ./copy
    ./css
    ./js
    ./latex
    ./scss
    ./static
  ];
  unpackPhase = ''
    for srcFile in $srcs; do
      cp -a $srcFile/. $(stripHash $srcFile)
    done

    mkdir _purescript
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList
          (name: value: ''cp ${value.bundle-app} _purescript/${name}.js'')
          inCode.purescript
        )
     }
  '';
  buildPhase = ''
    ${inCode.haskell}/bin/inCode-build build --verbose
  '';
  installPhase = ''
    mkdir -p "$out/dist"
    cp -a _site/. "$out/dist"
  '';
};
```

This is the only "hand-built" derivation. Hopefully it's legible enough:

1.  The `buildInputs` attribute tells us what derivations give us the
    binaries/generated files in scope for us to do our job.
    -   `inCode.haskell` is the derivation that gives us the hakyll binary, from
        running *haskell.nix* on the haskell hakyll project.
    -   `lib.attrValues inCode.purescript` gives us each of the derivations of
        all of the purescript projects, one for each spago sub-project (and each
        blog post/page that uses purescript). In this case, it gives us the
        actual compiled javascript bundle that the site generator expects.
2.  `srcs` is the actual source files that the static site generator uses --- in
    this case, the code samples, configuration files, markdown files for the
    actual blog posts in `./copy`, the static files, etc.
3.  `unpackPhase` is the shell script to get things ready for the hakyll site
    generator to run:
    -   First, copy all of the source files into the temporary build directory
    -   Then, copy all of the compiled javascript binaries (from the
        `bundle-app` property of each derivation) into the `_purescript` folder
        in the temporary build directory, where the static site generator
        expects to find it.
4.  `buildPhase` is the shell script to actually run the static site generator
    on the work directory that we carefully unpacked. Note that we refer to the
    binary using the derivation variable, `${inCode.haskell}` and knowing that
    it is found under the `bin` folder, as convention states.
5.  The `installPhase` copies what the site generated (in the `_site` directory
    of the temporary work folder) into `$out`, which is the place we put the
    "results".

### Github Action

The final github action is pretty standard --- it pulls together the actions:

1.  `cachix/install-nix-action`: Install nix
2.  `cachix/cachix-action`: Use the given cachix cache and upload the artifacts
    to it after building
3.  Actually run `nix build` on the static site derivation to generate the
    static files. In the process, it will either re-build (if anything changes)
    the dependency haskell and purescfript derivations, or pull it straight from
    cachix if nothing changed.
4.  `crazy-max/ghaction-github-pages` to push the files to the `gh-pages` branch

This was taken pretty much verbatim from *hakyll-nix-template*.

### Development Environment for Writing

Now, because we don't have page-level caching for nix (and hakyll manages its
own caching), we have to bite the bullet and create a customized development
environment where we can locally deploy hakyll's cached incremental builds.
Again, this will be a hand-written environment, so let's think about what we
want. We need "temporary" directories that hakyll will use to (1) find the
source files, (2) store its cache, and (3) output the static site. Because of
this, if we make any changes to blog posts while we are in our development
environment, hakyll will not do a full re-build --- it'll still have its
development-environment-scoped cache. We can also use hakyll's built in local
preview server and filesystem watch-and-rebuild features. We also want to do
incremental builds for purescript development as well, and we need to make sure
that this temporary environment also has an incremental build cache area for
`purs`. While we *could* do this without any temporary directories by just using
the working directory for this, it is kind of nice to be able to have this all
take place in a temporary folder that will go away after we exit the development
environment.

Unfortunately, the hakyll binary isn't aware of `purs` and purescript --- it
just receives the compiled bundles. So, we need a way for the user to run a
command to re-build the purescript dependencies if they have changed it. There
are a few ways we can do this (maybe have nix initiate a "watcher" that
automatically incrementally recompiles, or wrap our hakyll binary in a wrapper
shell script that triggers a re-build). However, the simplest way I could think
of (that also does the job) is for the development environment to provide a
command/shell script that just runs `purs` with the right arguments.

``` nix
rebuild-js =
  let
    buildSingleDep = name: value:
      let
        srcGlob = "purescript/${name}/src/**/*.purs";
        buildDir = "$HAKYLL_DIR/_purescript-build/${name}";
        mainModule = "${buildDir}/Main/index.js";
        outFile = "$HAKYLL_DIR/_purescript/${name}.js";
      in
      ''
        mkdir -p ${buildDir}
        purs compile ${toString value.globs} ${srcGlob} -o ${buildDir}
        chmod -R +w ${buildDir}
        echo "import {main} from '${mainModule}'; main()" | esbuild --bundle --outfile=${outFile} --format=iife
      '';
  in
  pkgs.writeShellScriptBin
    "rebuild-js"
    ''
      mkdir -p "$HAKYLL_DIR/_purescript";
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList buildSingleDep inCode.purescript)}
    '';
```

`writeShellScriptBin` is a convenient way to provide a derivation that *only*
contains a single shell script in its `bin/` subdir. So if we include it as a
dependency in our development environment, this shell script will be in scope.
Keep in mind that the hakyll binary expects the output bundles in the
`_purescript` directory.

In the script, `$HAKYLL_DIR` is our temporary working directory. We iterate over
each of the derivations in `inCode.purescript`, and for each one, we create a
temporary build directory and use `purs` to compile all of the source files for
the project (using the `globs` property that `purifix` gives us, which contains
globs of all the source files of the dependencies as well, which are found in
the global nix cache). And then we use `esbuild` to manually generate the final
bundle in the output directory.

And, to assemble the initial working directory, we have to:

1.  Copy all of the compiled purescript bundles into the temporary directory,
    from the derivation based on the current state at initialization time.
2.  Simlink all of the source files (markdown, etc.) in the user's working
    directory to the temporary working directory, so any changes to the working
    directory will also be reflected in the temporary directory.
3.  Provide all of the build tools through `nativeBuildInputs`.
4.  Provide the user-accessible commands through `packages`.

``` nix
devShells.default = pkgs.mkShell {
  shellHook = ''
    export HAKYLL_DIR=$(mktemp -d)
    echo "Available commands: rebuild-js inCode-build"
    echo "Hakyll working directory: \$HAKYLL_DIR"

    mkdir -p $HAKYLL_DIR/_purescript
    ${lib.concatStringsSep "\n"
    (lib.mapAttrsToList
      (name: value:
          ''
          cp -a ${value.deps}/output/. purescript/output
          chmod -R +w purescript/output
          cp ${value.bundle-app} $HAKYLL_DIR/_purescript/${name}.js
          ''
      )
      inCode.purescript
    )}
    chmod -R +w $HAKYLL_DIR/_purescript

    for srcDir in code-samples config copy css js latex scss static; do
      ln -s "$PWD/$srcDir" $HAKYLL_DIR
    done
  '';
  nativeBuildInputs = [ pkgs.esbuild pkgs.purescript ]
    ++ haskellFlake.devShell.nativeBuildInputs
    ++ lib.attrValues inCode.purescript
    ++ map (value: value.develop.buildInputs) (lib.attrValues inCode.purescript);
  packages = [
    rebuild-js
    inCode.haskell
  ];
};
```

## Conclusions

Overall, I still feel like I'm hacking things together and figuring things out
as I go. So far, all I have done is automate a set-up that I have been doing
manually, in hopefully a way that is better for long-term maintainability.
However, hopefully as I learn more, I start to open up new doors and new ways of
thinking that enable to me to do things I haven't been able to do before.

Thank you for reading this --- whether you are looking to get started into nix,
or are an experienced veteran, I hope you were able to get something out of this
post. And, for those with experience, I would definitely appreciate any advice
concerning the things described in this post --- things I could have done
better, alternative approaches, or ways to be more idiomatic/maintainable.

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

