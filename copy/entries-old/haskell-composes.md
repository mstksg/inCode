An open list of functions to compose functions in Haskell
=========================================================

Categories
:   Haskell
:   Lists
Tags
:   haskell
CreateTime
:   2015/01/24 16:00:38
PostDate
:   Never
Identifier
:   haskell-composes

Hi all, just a fun post here :)  I've been telling myself for a long time to
compile a list of all the ways you can compose two functions, `(a -> b)` and
`(b -> c)` using functions in base and common libraries (and their simple
manipulations). There are an embarassingly large amount of them, and I'm sure
that I'll find more over time.  If any of you have suggestions, feel free to
leave a comment or find me on [twitter][] or #haskell on freenode as *jle`* :)

[twitter]: https://twitter.com/mstk "Twitter"

1.  `(.)` (Prelude)
2.  `fmap` (Prelude)
3.  `(<$>)` (Data.Functor)
4.  `liftA` (Control.Applicative)
5.  `liftM` (Control.Monad)
6.  `(.)` (Control.Category)
7.  `(<<<)` (Control.Category)
8.  `flip (>>>)` (Control.Category)
9.  `(<<^)` (Control.Arrow)
10. `(^<<)` (Control.Arrow)
11. `flip (^>>)` (Control.Arrow)
12. `flip (>>^)` (Control.Arrow)
13. `rmap` (Data.Profunctor)
14. `dimap id` (Data.Profunctor)
15. `over mapped` (Control.Lens)
16. `(mapped %~)` (Control.Lens)

So, am I missing any?


