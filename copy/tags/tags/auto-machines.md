auto-machines
=============

Posts about the "automation arrow" type, an implementation of the mathematical
"mealy machine".

For example, one possible implementation:

~~~haskell
data Auto a b = MkAuto { runAuto :: a -> (b, Auto a b) }
~~~

This is here to contrast things with [posts about the *library* "auto"][auto].

[auto]: http://blog.jle.im/entries/tagged/auto


