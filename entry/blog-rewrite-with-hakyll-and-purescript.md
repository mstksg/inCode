Blog Rewrite with Hakyll and Purescript

========================================

> Originally posted by [Justin Le](https://blog.jle.im/) on March 25, 2016.
> [Read online!](https://blog.jle.im/entry/blog-rewrite-with-hakyll-and-purescript.html)

It's been almost a year since my last post! Things have been a bit hectic with
research and related things, and with the unrelenting academia publishing cycle,
any time I can get to write or explore has been a nice escape.

Admittedly, I've also run into some friction updating my blog because it was a
compiled web server with some delicate dependencies and required environment
configuration to build/deploy. It was written/built at a time when a lot of the
infrastructure we have now in the Haskell ecosystem either wasn't there, or
wasn't mature. We didn't have easy [Heroku
deployment](https://haskellonheroku.com/), and we didn't have great tools like
[stack](http://haskellstack.org/) to let us create reproducible builds. One of
my [first
posts](http://blog.jle.im/entry/deploying-medium-to-large-haskell-apps-to-heroku.html)
in 2013 was actually about hoops to jump through *just* to get a simple Heroku
deployment. I've had to maintain a virtual machine just to compile and push
changes!

My blog was one of my first Haskell projects ever, and if I had started it now,
in 2016, things would definitely be a bit different. By this point, it's been
long enough and the slight inconveniences have been building up enough that I
thought it'd be time to sit down and finally migrate my "first large-ish Haskell
project" and bring it into modern times, by using
[hakyll](https://jaspervdj.be/hakyll/) and
[purescript](http://www.purescript.org/). Here are my thoughts and observations
on how the migration went, with insight on Haskell migrations in general!

My blog engine is open-source, and the [source for this specific
instance](https://github.com/mstksg/inCode) is up on github, for those
interested in checking it out!

## Hakyll

To be honest, there was little actual practical reasons why my site wasn't
static to begin with. The main reason, feature-wise, was for me to be able to
schedule blog posts and updates without requiring me to actually re-render and
re-push every time I wanted to make a post. The real underlying reason, however,
was that this blog was my first major Haskell project, and I wanted to take the
opportunity to be able to learn how to interface with databases in Haskell.

Now that that learning impetus is behind me, I felt free to throw it all out the
window and rewrite things to be a completely 100% static site!

[Hakyll](https://jaspervdj.be/hakyll/) was great; it's basically like a very
specialized *make*-like tool for building sites. It takes a bit of time to get
used to "thinking in Hakyll" --- generating standalone pages instead of just
ones based off of files, getting used to the identifier/snapshot system --- but
once you do, things go pretty smoothly. I started thinking about snapshots as
customized "object files" that you can leave behind in the process of creating
pages that other pages can use. Hakyll manages all the dependencies for you, so
pages that depend on the things left from other pages will be sequenced
properly, and rebuilding your website only requires rebuilding pages that depend
on files you changed. Neat!

Before, I had gotten the impression that Hakyll was mostly for generating
"simple", pre-built blog layouts, but I was able to use Hakyll (without much
friction, at all) to generate the complex, intricate, and arbitrary site map
that I had designed for my
[scotty](http://hackage.haskell.org/package/scotty)-based blog. I definitely
recommend it for any static site generating needs, blogs or not.

An unexpected consequence of the static-site-hosted-by-github-pages approach,
however, is that I don't have any control over MIME types anymore (or 301
redirects), so I had to do some migrations to move pages over to ".html" and set
up redirects and stuff (and get redirects to work with google analytics), but
those were made super simple with Hakyll.

## Refactoring Haskell Code

One thing that did not disappoint me was how *easy* and *painless* it is to
refactor Haskell code. This is something I always trumpet/brag about Haskell,
and getting the opportunity to actually refactor a major-ish codebase.

And, yes, I was not disappointed! For the most part, I already had my html
templates, CSS, static javascript, etc. in place. All of the mechanisms were
extremely modular and very easy to port. The type system made sure everything
fit together well at the boundaries. They also instantly told me what did what,
and ensured that sweeping changes in my code were safe. The "if it compiles, it
works" mantra served me greatly here. I can't even begin to imagine migrating
one of my old ruby projects in the same way. With this, I was confident that my
compiled code was correct and did what I wanted. The types were a guide and also
a avenue of insight into my 3-years-removed past self.

Thanks to the types, I was able to pick up something I hadn't touched in 3
years, figure out how all things fit together, and completely gut everything
apart and use them for a new build system ... with compile-time assurances that
I didn't do anything incorrectly!

It's hard for me to really explain how amazing the feeling of refactoring
Haskell code is. I used to dread refactors and migrations, but now I look
forward to them and find any opportunity to do one! :D It's something that's
difficult to convey the sublime joy of until you actually try it, so I recommend
trying it some day :)

## Purescript

### on Fay

With my [last major blog
update](http://blog.jle.im/entry/blog-engine-updates-markdown-preprocessor-fay-scripts.html#fay),
I ported all of my one-off javascript scripts to fay. This time around, I
figured I'd move away from [fay](https://github.com/faylang/fay/wiki), because
it was slightly clunky to build/get working/integrate in the way that GHCJS
spoiled me to be accustomed to. In the future, I might return ... but at this
point in time, Fay seems a bit awkward in the ecosystem. GHCJS lets you use the
full power of Haskell (including all of *base*'s concurrency mechanisms and
almost every library on hackage), at the expense of creating large and
unreadable javascript blobs.

Fay seemed like just a *weaker* GHCJS to me, but in all the ways that mattered.
It doesn't have all of the awesome GHC things that make modern Haskell what it
is (not just the lack of base's identical API, but also ... no typeclasses? Lens
abstractions? Hackage libraries?), so almost all of my normal Haskell
programming flow is thrown out the window. It's a subset of Haskell, but lacks
most of the tools people use to write *actual* Haskell like they'd write
everyday. The generated javascript blobs are still decently opaque.

So, if you're going to be spending your time writing something that is like
Haskell, but forces you to write it in a way that is nothing like any actual
Haskell code you'd normally write... why even bother keeping up with Haskell
semantics and Haskell compatibility? Why not break out and try something new and
fresh, unbound by Haskell and compatibility issues?[^1][^2]

### on Purescript

With that mindset, I looked at *[purescript](http://www.purescript.org/)*, which
is a language that's inspired by Haskell, with a lot of Haskell features we use
every day, and throws in things we all wish we had in Haskell, like extensible
records!

(Note --- I *did* rewrite all of my fay in GHCJS at first. This resulted in a
javascript blob that was *1.4 MB* in size for just a bunch of small DOM
manipulation scripts. Definitely not practical, unfortunately!)

I liked that purescript was able to throw away a lot of warts in the Haskell
ecosystem, with a cleaner typeclass hierarchy and just a lot of design decisions
"done right", that we'd all change in Haskell if we could. And extensible
records being built into the language is quite refreshing; not having to deal
with fancy GADT's in Haskell was a nice step back from the craziness that is
type-level programming in Haskell. Alongside all of that, I was also able to
rely and seamlessly use a lot of Haskell idioms that we all know and love, like
lenses and traversals and compositions.

At many moments, I felt like writing in Purescript felt like writing in *the
language that Haskell should have been*.

But one of my favorite aspects about purescript ended up being the sheer beauty
and conciseness of the generated javascript. Look at how[^3]:

``` purescript
appendTopLinks doc = do
    hs <- querySelectorAll headers (documentToParentNode doc)
    flip traverseNodeList_ hs \h -> do
      topLink <- createElement "a" doc
      let topLinkNode = elementToNode topLink
      setAttribute "href" "#title" topLink
      setClassName "top-link" topLink
      setTextContent "top" topLinkNode
      appendChild topLinkNode (elementToNode h)
      return unit
```

gets translated to:

``` javascript
var appendTopLinks = function (doc) {
    return function __do() {
        var v = querySelectorAll(headers)(documentToParentNode(doc))();
        return flip(traverseNodeList_(monadEffEff))(v)(function (h) {
            return function __do() {
                var v1 = createElement("a")(doc)();
                var topLinkNode = elementToNode(v1);
                setAttribute("href")("#title")(v1)();
                setClassName("top-link")(v1)();
                setTextContent("top")(topLinkNode)();
                appendChild(topLinkNode)(elementToNode(h))();
                return unit;
            };
        })();
    };
};
```

And it's not just the IO-based imperative code that looks nice, either.
Everything gets compiled to clean, readable javascript that you'd be happy to
import in your node/normal javascript project.

The total exported javascript blob is only *88 kB*, even smaller than fay's *100
kB* output (but not significantly so), and much smaller than GHCJS's *1.4
MB*[^4] output (which, to be fair, has to also contain the entire Haskell
runtime, implementing Haskell semantics, as well).

Interestingly enough, the *original* raw javacript I wrote in 2013 came out to
about the same size, about *80 kB*. (Well, it is about *2 kB* of actual script,
but it utilized all of *jquery*, which implements a lot of the functionality.)
Getting comparable filesizes to jquery bundles is something that's pretty
impressive to me!

I'd recommend purescript to anyone who has to write simple javascript *scripts*
and wants to do it in a sane, beautiful language. I still use *ghcjs* for actual
*applications*, for now, because I still love Haskell and its ecosystem, along
with the free data type sharing and code re-usage. But for small scripts like
these, purescript might just be the ideal and perfect solution!

You can check out [the actual purescript
script](https://github.com/mstksg/inCode/blob/28f6a5da4c83356c4be87067ab88171879c68784/app-purescript/Entry.purs)
on github!

## Conclusions

My main takeways ---

1.  I will never be able to never work on a Haskell project/application without
    *stack* again (how did we even survive before *stack*?)
2.  Hakyll is a fun little library that is a great specialized *make* for
    building static websites
3.  Refactoring Haskell is an amazing experience; I would recommend it to anyone
    to try it out at least once in their lives
4.  *Purescript* is an amazing and beautiful technology that I had the pleasure
    of learning during this process, and generates elegant, readable javascript
    scripts.

This reflection post has been to help me organize my thoughts, but I hope they
can be useful for those of you looking for new technologies to learn and ways to
implement/approach your stack or next programming project, as well!

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

[^1]: I definitely don't mean to bash on *fay* here! It definitely has its role
    and place in the ecosystem. It's for my specific application that I was
    looking for an alternative with.

[^2]: There's another thing here that I skipped over slightly --
    [Haste](http://haste-lang.org/). I haven't had much experience with it
    myself, but for this purpose, I decided to jump into something not-Haskell
    and try out something new!

[^3]: Unfortunately,
    *[highlighting-kate](https://github.com/jgm/highlighting-kate)* doesn't yet
    support purescript syntax highlighting?

[^4]: A previous version of this post claimed that the javascript bundle was
    *140 MB*, instead of *1.4 MB*. My bad!

