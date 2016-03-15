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

Admittedly I've also run into some friction updating my blog because it was a
compiled web server with some delicate dependencies and required environment
configuration to build/deploy.  It was written/built at a time when a lot of
the infrastructure we have now in the Haskell ecosystem either wasn't there, or
wasn't mature.  We didn't have easy Heroku deployment, and --- more importantly
--- we didn't have tools like [stack][]

[stack]: http://haskellstack.org
