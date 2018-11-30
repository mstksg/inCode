---
title: Tries using Recursion Schemes
categories: Haskell, Tutorials
tags: haskell, recursion schemes
create-time: 2018/11/29 17:45:04
series: Beginner/Intermediate Haskell Projects
identifier: trie
slug: tries-using-recursion-schemes
---

Not too long ago, I was browsing the [prequel memes subreddit][r/prequelmemes]
--- a community built around creative ways of remixing and re-contextualizing
quotes from the cinematic corpus of the three Star Wars prequel movies --- when
I noticed that a fad was in progress [constructing tries based on quotes as
keys][meme] indexing stills from the movie corresponding to those quotes.

[r/prequelmemes]: https://www.reddit.com/r/PrequelMemes
[meme]: https://www.reddit.com/r/PrequelMemes/comments/9w59t4/i_expanded_it/

This inspired me to try playing around with some tries myself, and it gave me
an excuse to play around with *[recursion-schemes][]* (one of my favorite
Haskell libraries).  If you haven't heard about it yet, *recursion-schemes*
(and the similar library *[data-fix][]*) abstracts over common recursive
functions written on recursive data types.  It exploits the fact that a lot of
recursive functions for different recursive data types all really follow the
same pattern and gives us powerful tools for writing cleaner and safer code.

[recursion-schemes]: https://hackage.haskell.org/package/recursion-schemes
[data-fix]: https://hackage.haskell.org/package/data-fix

Recursion schemes is a perfect example of those amazing accidents that happen
throughout the Haskell ecosystem: an extremely "theoretically beautiful"
abstraction that also happens to be extremely useful for writing industrially
rigorous code.

Tries are a common intermediate-level data type, and recursion-schemes is a
common intermediate-level library.  So, as a fun intermediate-level Haskell
project, let's build a trie data type in Haskell based on recursion-schemes, to
see what it has to offer!  The resulting data type will definitely not be a
"toy" --- it'll be something you can actually use to build meme diagrams of
your own!

Trie
----

A [trie][] (prefix tree) is a classic example of a simple yet powerful data
type most people encounter in school (I remember being introduced to it through
a project implementing a boggle solver).

[trie]: https://en.wikipedia.org/wiki/Trie
