---
title: Rewriting my blog using Hakyll, GHCJS
categories: Meta
tags: ghcjs, haskell, hakyll
create-time: 2016/03/13 18:37:38
date: never
identifier: hakyll-rewrite
slug: rewriting-my-blog-using-hakyll-ghcjs
---

It's been almost a year since my last post!  Things have been a bit hectic with
research and related things, and with the unrelenting publishing cycle, any
time I can get to write or explore has been a great escape.

Admittedly, I've also run into some friction updating my blog because it was a
compiled web server with some delicate dependencies and required environment
configuration to build/deploy.  It was written/built at a time when a lot of
the infrastructure we have now in the Haskell ecosystem either wasn't there, or
wasn't mature.  We didn't have easy [Heroku deployment][heroku], and we didn't
have great tools like [stack][] to let us create reproducible builds.  One of
my [first posts][heroku-post] in 2013 was actually about hoops to jump through
*just* to get a simple Heroku deployment.  I've had to maintain a virtual
machine just to compile and push changes!

[heroku]: https://haskellonheroku.com/
[stack]: http://haskellstack.org/
[heroku-post]: http://blog.jle.im/entry/deploying-medium-to-large-haskell-apps-to-heroku.html

My blog was one of my first Haskell projects ever, and if I had started it now,
in 2016, things would definitely be a bit different.  But, it's been long
enough and the slight inconveniences have been building up enough that I
thought it'd be time to sit down and finally migrate my "first large-ish
Haskell project" and bring it into modern times, by using [hakyll][] and
[ghcjs][].  Here are my thoughts and observations on how the migration went,
with insight on Haskell migrations in general!

[hakyll]: https://jaspervdj.be/hakyll/
[ghcjs]: https://github.com/ghcjs/ghcjs

Hakyll
------

To be fair, there was little actual practical reasons why my site wasn't static
to begin with.  The main reason, feature-wise, was for me to be able to
schedule blog posts and updates without requiring me to actually re-render and
re-push every time I wanted to make a post.  But, the real underlying reason
was that it was my first Haskell project, and I wanted to take the opportunity
to be able to learn how to interface with databases in Haskell.

Now that that learning process is behind me, I felt free to throw it all out
the window and rewrite things to be a completely 100% static site!
