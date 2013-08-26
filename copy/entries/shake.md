Shake: Task Automation and Shell Scripting in Haskell
=====================================================

Categories
:   Haskell
Tags
:   shake
:   shell scripting
WriteTime
:   2013/08/24 19:31:22
PostDate
:   2013/09/24 22:36:03

As someone who comes from a background with ruby and rake, I'm used to
powerful task management systems with expressive dependency.  `Make` is a
favorite tool of mine when I'm working on projects with people who don't use
ruby, and when I'm working on ruby projects I never go far without starting a
good `Rakefile`.  The two tools provied a perfect DSL for setting up systems
of tasks that had complicated file and task dependencies.

As I was starting to learn Haskell and building larger-scale Haskell projects,
I began to look for alternatives in Haskell.  Was there a Haskel counterpart
to Ruby's `rake`, Node's `jake`, Java's `ant`?

At the end of it all I settled into a

-------

<!-- And unlike some of other people in the Haskell community, I -->
<!-- consider ruby, as a language, to be the king of scripting -- maybe describable -->
<!-- as a DSL for systems scripting with a glove-like fit. -->
