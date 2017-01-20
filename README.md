inCode
======

Static site generator backend for [my blog][].  See [this post][hakyll-post]
for more information.

[my blog]: https://blog.jle.im
[hakyll-post]: https://blog.jle.im/entry/blog-rewrite-with-hakyll-and-purescript.html

Building
--------

Requirements:

1.  [stack][]
2.  [bower][]

[stack]: https://docs.haskellstack.org/en/stable/README/
[bower]: https://bower.io

Commands:

~~~bash
$ git clone https://github.com/mstksg/blog
$ cd blog
$ stack install
$ stack run ./Build.hs
~~~
