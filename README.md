inCode
======

[![Join the chat at https://gitter.im/mstksg/inCode](https://badges.gitter.im/mstksg/inCode.svg)](https://gitter.im/mstksg/inCode?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Static site generator backend for [my blog][].  See [this post][hakyll-post]
for more information.

[my blog]: https://blog.jle.im
[hakyll-post]: https://blog.jle.im/entry/blog-rewrite-with-hakyll-and-purescript.html

Building
--------

Requirements:

1.  [stack][]
2.  [bower][]
3.  [pulp][]

[stack]: https://docs.haskellstack.org/en/stable/README/
[bower]: https://bower.io
[pulp]: https://github.com/bodil/pulp

Commands:

~~~bash
$ git clone https://github.com/mstksg/blog
$ cd blog
$ stack install
$ ./Build.hs
~~~
