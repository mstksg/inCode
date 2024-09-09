Roll your own Holly Jolly streaming combinators with Free

==========================================================

> Originally posted by [Justin Le](https://blog.jle.im/) on December 12, 2020.
> [Read online!](https://blog.jle.im/entry/holly-jolly-streaming-combinators.html)

Hi! Welcome, if you're joining us from the great [Advent of Haskell
2020](https://adventofhaskell.com/) event! Feel free to grab a hot chocolate and
sit back by the fireplace. I'm honored to be able to be a part of the event this
year; it's a great initiative and harkens back to the age-old Haskell tradition
of bite-sized Functional Programming "advent calendars". I remember when I was
first learning Haskell, [Ollie Charles' 24 Days of Hackage
series](https://ocharles.org.uk/pages/2012-12-01-24-days-of-hackage.html) was
one of my favorite series that helped me really get into the exciting world of
Haskell and the all the doors that functional programming can open.

All of the posts this year have been great --- they range from insightful
reflections on the nature of Haskell and programming in Haskell, or also on
specific language features. This post is going to be one of the "project-based"
ones, where we walk through and introduce a solidly *intermediate* Haskell
technique as it applies to building a useful general toolset. I'm going to be
exploring the "functor combinator style" where you identify the interface you
want, associate it with a common Haskell typeclass, pick your primitives, and
automatically get the ability to imbue your primitives with the structure you
need. I've talked about this previously with:

1.  [Applicative regular
    expressions](https://blog.jle.im/entry/free-alternative-regexp.html)
2.  [The functor
    combinatorpedia](https://blog.jle.im/entry/functor-combinatorpedia.html)
3.  [Bidirectional
    serializers](https://blog.jle.im/entries/series/+enhancing-functor-structures.html)
4.  [Composable
    interpreters](https://blog.jle.im/entry/interpreters-a-la-carte-duet.html)

and I wanted to share a recent application I have been able to use apply it with
where just *thinking* about the primitives gave me almost all the functionality
I needed for a type: composable streaming combinators. This specific application
is also very applicable to integrate into any [composable effects
system](https://www.stephendiehl.com/posts/decade.html#algebraic-effect-systems),
since it's essentially a monadic interface.

In a way, this post could also be seen as capturing the spirit of the holidays
by reminiscing about the days of yore --- looking back at one of the more
exciting times in modern Haskell's development, where competing composable
streaming libraries were at the forefront of practical innovation. The dust has
settled on that a bit, but it every time I think about composable streaming
combinators, I do get a bit nostalgic :)

This post is written for an *intermediate* Haskell audience, and will assume you
have a familiarity with monads and monadic interfaces, and also a little bit of
experience with monad transformers. Note --- there are many ways to arrive at
the same result, but this post is more of a demonstration of a certain style and
approach that has benefited my greatly in the past.

All of the code in this page [can be found online at
github](https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs)!

## Dreaming of an Effectful Christmas

The goal here is to make a system of composable pipes that are "pull-based", so
we can process data as it is read in from IO only as we need it, and never do
more work than we need to do up-front or leak memory when we stop using it.

So, the way I usually approach things like these is: "dress for the interface
you want, not the one you have." It involves:

1.  Thinking of the `m a` you want and how you would want to combine it/use it.
2.  Express the primitive actions of that thing
3.  Use some sort of free structure or effects system to enhance that primitive
    with the interface you are looking for.

So, let's imagine our type!

``` haskell
type Pipe i o m a = ...
```

where a `Pipe i o m a` represents a pipe component where:

-   `i`: the type of the input the pipe expects from upstream
-   `o`: the type of the output the pipe will be yielding upstream
-   `m`: the monad that the underlying actions live in
-   `a`: the overall result of the pipe once it has terminated.

One nice thing about this setup is that by picking different values for the type
parameters, we can already get a nice classification for interesting subtypes:

1.  If `i` is `()` (or universally quantified[^1]) --- a `Pipe () o m a` --- it
    means that the pipe doesn't ever expect any sort of information upstream,
    and so can be considered a "source" that keeps on churning out values.

2.  If `o` is `Void` (or universally quantified) --- a `Pipe i Void m a` --- it
    means that the pipe will never yield anything downstream, because `Void` has
    no inhabitants that could possibly be yielded.

    ``` haskell
    data Void
    ```

    This means that it acts like a "sink" that will keep on eating `i` values
    without ever outputting anything downstream.

3.  If `i` is `()` and `o` is `Void` (or they are both universally quantified),
    then the pipe doesn't expect any sort of information upstream, and also
    won't ever yield anything downstream... a `Pipe () Void m a` is just an
    `m     a`! In the biz, we often call this an "effect".

4.  If `a` is `Void` (or universally quantified) --- a `Pipe i o m Void` --- it
    means that the pipe will never terminate, since `Void` has no inhabitants
    that could it could possibly produce upon termination.

To me, I think it embodies a lot of the nice principles about the "algebra" of
types that can be used to reason with inputs and outputs. Plus, it allows us to
unify sources, sinks, and non-terminating pipes all in one type!

Now let's think of the interface we want. We want to be able to:

``` haskell
-- | Yield a value `o` downstream
yield :: o -> Pipe i o m ()

-- | Await a value `i` upstream
await :: Pipe i o m (Maybe i)

-- | Terminate immediately with a result value
return :: a -> Pipe i o m a

-- | Sequence pipes one-after-another:
-- "do this until it terminates, then that one next"
(>>) :: Pipe i o m a -> Pipe i o m b -> Pipe i o m b

-- | In fact let's just make it a full fledged monad, why not?  We're designing
-- our dream interface here.
(>>=) :: Pipe i o m a -> (a -> Pipe i o m b) -> Pipe i o m b

-- | A pipe that simply does action in the underlying monad and terminates with
-- the result
lift :: m a -> Pipe i o m a

-- | Compose pipes, linking the output of one to the input of the other
(.|) :: Pipe i j m a -> Pipe j o m b -> Pipe i o m b

-- | Finally: run it all on a pipe expecting no input and never yielding:
runPipe :: Pipe () Void m a -> m a
```

This looks like a complicated list...but actually most of these come from
ubiquitous Haskell typeclasses like `Monad` and `Applicative`. We'll see how
this comes into play later, when we learn how to get these instances for our
types for free. This makes the actual "work" we have to do very small.

So, these are going to be implementing "conduit-style" streaming combinators,
where streaming actions are monadic, and monadic sequencing represents "do this
after this one is done." Because of this property, they work well as
*pull-based* pipes: yields will block until a corresponding await can accept
what is yielded.

### Put on those Christmas Sweaters

"Dress for the interface you want, not the one you have". So let's pretend we
already implemented this interface...what could we do with it?

Well, can write simple sources like "yield the contents from a file
line-by-line":

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L65-L72

sourceHandleIO :: Handle -> Pipe i String IO ()
sourceHandleIO handle = do
    res <- lift $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out
        sourceHandle handle
```

Note that because the `i` is universally quantified, it means that we know that
`sourceFile` never ever awaits or touches any input: it's purely a source.

We can even write a simple sink, like "await and print the results to stdout as
they come":

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L83-L90

sinkStdoutIO :: Pipe String o IO ()
sinkStdoutIO = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        lift $ putStrLn x
        sinkStdout
```

And maybe we can write a pipe that takes input strings and converts them to all
capital letters and re-yields them:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L101-L108

toUpperPipe :: Monad m => Pipe String String m ()
toUpperPipe = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        yield (map toUpper x)
        toUpperPipe
```

And we can maybe write a pipe that stops as soon as it reads the line `STOP`.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L110-L119

untilSTOP :: Monad m => Pipe String String m ()
untilSTOP = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x
        | x == "STOP" -> pure ()
        | otherwise   -> do
            yield x
            untilSTOP
```

`untilSTOP` is really sort of the crux of what makes these streaming systems
useful: we only pull items from the file as we need it, and `untilSTOP` will
stop pulling anything as soon as we hit `STOP`, so no IO will happen anymore if
the upstream sink does IO.

### Our Ideal Program

Now ideally, we'd want to write a program that lets us compose the above pipes
to read from a file and output its contents to stdout, until it sees a STOP
line:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L121-L126

samplePipeIO :: Handle -> Pipe i o IO ()
samplePipeIO handle =
       sourceHandleIO handle
    .| untilSTOP
    .| toUpperPipe
    .| sinkStdoutIO
```

## Setting up our Stockings

Step 2 of our plan was to identify the primitive actions we want. Looking at our
interface, it seems like the few things that let us "create" a `Pipe` from
scratch (instead of combining existing ones) are:

``` haskell
yield  :: o -> Pipe i o m ()
await  :: Pipe i o m (Maybe i)
lift   :: m a -> Pipe i o m a
return :: a   -> Pipe i o m a
```

However, we can note that `lift` and `return` can be gained just from having a
`Monad` and `MonadTrans` instance. So let's assume we have those instances.

``` haskell
class Monad m where
    return :: a -> m a

class MonadTrans p where
    lift :: m a -> p m a
```

The functor combinator plan is to identify your primitives, and let free
structures give you the instances (in our case, `Monad` and `MonadTrans`) you
need for them.

So this means we only need two primitives: `yield` and `await`. Then we just
throw them into some machinery that gives us a free `Monad` and `MonadTrans`
structure, and we're golden :)

In the style of the *[free](https://hackage.haskell.org/package/free)* library,
we'd write base functions to get an ADT that describes the primitive actions:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L22-L25

data PipeF i o a =
    YieldF o a
  | AwaitF (Maybe i -> a)
    deriving Functor
```

The general structure of the base functor style is to represent each primitive
as a constructor: include any inputs, and then a continuation on what to do if
you had the result.

For example:

1.  For `YieldF`, you need an `o` to be able to yield. The second field should
    really be the continuation `() -> a`, since the result is `()`, but that's
    equivalent to `a` in Haskell.
2.  For `AwaitF`, you don't need any parameters to await, but the continuation
    is `Maybe i -> a` since you need to specify how to handle the `Maybe i`
    result.

(This is specifically the structure that
*[free](https://hackage.haskell.org/package/free)* expects, but this principle
can be ported to any algebraic effects system.)

### A Christmas Surprise

And now for the last ingredient: we can use the `FreeT` type from
*[Control.Monad.Trans.Free](https://hackage.haskell.org/package/free/docs/Control-Monad-Trans-Free.html)*,
and now we have our pipe interface, with a `Monad` and `MonadTrans` instance!

``` haskell
type Pipe i o = FreeT (PipeF i o)
```

This takes our base functor and imbues it with a full `Monad` and `MonadTrans`
instance:

``` haskell
lift :: m a -> FreeT (PipeF i o) m a
lift :: m a -> Pipe i o m a

return :: a -> FreeT (PipeF i o) m a
return :: a -> Pipe i o m a

(>>)  :: Pipe i o m a -> Pipe i o m b -> Pipe i o m b
(>>=) :: Pipe i o m a -> (a -> Pipe i o m b) -> Pipe i o m b
```

That's the essence of the free structure: it *adds* to our base functor
(`PipeF`) exactly the structure it needs to be able to implement the instances
it is free on. And it's all free as in beer! :D

As a bonus gift, we also get a `MonadIO` instance from `FreeT`, as well:

``` haskell
liftIO :: MonadIO m => IO a -> FreeT (PipeF i o) m a
liftIO :: MonadIO m => IO a -> Pipe i o m a
```

Now we just need our functions to lift our primitives to `Pipe`, using
`liftF :: f a -> FreeT f m a`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L29-L33

yield :: Monad m => o -> Pipe i o m ()
yield x = liftF $ YieldF x ()

await :: Monad m => Pipe i o m (Maybe i)
await = liftF $ AwaitF id
```

(these things you can usually just fill in using type tetris, filling in values
with typed holes into they typecheck).

Note that all of the individual pipes we had planned work as-is! And we can even
even make `sourceHandle` and `sinkStdout` work for any
`MonadIO m => Pipe i o m a`, because of the unexpected surprise Christmas gift
we got (the `MonadIO` instance and
`liftIO :: MonadIO m => IO a -> Pipe i o u m a`). Remember, `MonadIO m` is
basically any `m` that supports doing IO.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L74-L119

sourceHandle :: MonadIO m => Handle -> Pipe i String m ()
sourceHandle handle = do
    res <- liftIO $ tryJust (guard . isEOFError) (hGetLine handle)
    case res of
      Left  _   -> return ()
      Right out -> do
        yield out
        sourceHandle handle

sinkStdout :: MonadIO m => Pipe String o m ()
sinkStdout = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        liftIO $ putStrLn x
        sinkStdout

toUpperPipe :: Monad m => Pipe String String m ()
toUpperPipe = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x  -> do
        yield (map toUpper x)
        toUpperPipe

untilSTOP :: Monad m => Pipe String String m ()
untilSTOP = do
    inp <- await
    case inp of
      Nothing -> pure ()
      Just x
        | x == "STOP" -> pure ()
        | otherwise   -> do
            yield x
            untilSTOP
```

That's because using `FreeT`, we imbue the structure required to do monadic
chaining (do notation) and MonadTrans (`lift`) and MonadIO (`liftIO`) for free!

To "run" our pipes, we can use `FreeT`'s "interpreter" function. This follows
the same pattern as for many free structures: specify how to handle each
individual base functor constructor, and it then gives you a handler to handle
the entire thing.

``` haskell
iterT
    :: (PipeF i o (m a) -> m a)  -- ^ given a way to handle each base functor constructor ...
    -> Pipe i o m a -> m a       -- ^ here's a way to handle the whole thing
```

So let's write our base functor handler. Remember that we established earlier we
can only "run" a `Pipe () Void m a`: that is, pipes where `await` can always be
fed with no information (`()`) and no `yield` is ever called (because you cannot
yield with `Void`, a type with no inhabitants). We can directly translate this
to how we handle each constructor:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L57-L60

handlePipeF :: PipeF () Void (m a) -> m a
handlePipeF = \case
    YieldF o _ -> absurd o
    AwaitF f   -> f (Just ())
```

And so we get our full `runPipe`:

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L62-L63

runPipe :: Monad m => Pipe () Void m a -> m a
runPipe = iterT handlePipeF
```

I think this process exemplifies most of the major beats when working with free
structures:

1.  Define the base functor
2.  Allow the free structure to imbue the proper structure over your base
    functor
3.  Write your interpreter to interpret the constructors of your base functor,
    and the free structure will give you a way to interpret the entire
    structure.

### The Final Ornament

If you look at the list of all the things we wanted, we're still missing one
thing: pipe composition/input-output chaining. That's because it isn't a
primitive operation (like yield or await), and it wasn't given to us for free by
our free structure (`FreeT`, which gave us monadic composition and monad
transformer ability). So with how we have currently written it, there isn't any
way of getting around writing `(.|)` manually. So let's roll up our sleeves and
do the (admittedly minimal amount of) dirty work.

Let's think about the semantics of our pipe chaining. We want to never do more
work than we need to do, so we'll be "pull-based": for `f .| g`, try running `g`
as much as possible until it awaits anything from `f`. Only then do we try doing
`f`.

To implement this, we're going to have to dig in a little bit to the
implementation/structure of `FreeT`:

``` haskell
newtype FreeT f m a = FreeT
    { runFreeT :: m (FreeF f a (FreeT f m a)) }

data FreeF f a b
      Pure a
    | Free (f b)
```

This does look a little complicated, and on the face of it, it can be a bit
intimidating. And why is there a second internal data type?

Well, you can think of `FreeF f a b` as being a fancy version of
`Either a (f b)`. And the implementation of `FreeT` is saying that `FreeT f m a`
is *an m-action* that produces `Either a (FreeT f m a)`. So for example,
`FreeT f IO a` is an IO action that produces *either* the `a` (we're done, end
here!) or a `f (FreeT f m a))` (we have to handle an `f` here!)

``` haskell
newtype FreeT f m a = FreeT
    { runFreeT :: m (Either a (f (FreeT f m a))) }
```

At the top level, `FreeT` is an action in the underlying monad (just like
`MaybeT`, `ExceptT`, `StateT`, etc.). Let's take that into account and write our
implementation (with a hefty bit of help from the typechecker and typed holes)!
Remember our plan: for `f .| g`, *start unrolling `g`* until it needs anything,
and then ask `f` when it does.

``` haskell
(.|)
    :: Monad m
    => Pipe a b m x         -- ^ pipe from a -> b
    -> Pipe b c m y         -- ^ pipe from b -> c
    -> Pipe a c m y         -- ^ pipe from a -> c
pf .| pg = do
    gRes <- lift $ runFreeT pg          -- 1
    case gRes of
      Pure x            -> pure x       -- 2
      Free (YieldF o x) -> do           -- 3
        yield o
        pf .| x
      Free (AwaitF g  ) -> do           -- 4
        fRes <- lift $ runFreeT pf
        case fRes of
          Pure _            -> pure () .| g Nothing     -- 5
          Free (YieldF o y) -> y       .| g (Just o)    -- 6
          Free (AwaitF f  ) -> do                       -- 7
            i <- await
            f i .| FreeT (pure gRes)
```

Here are some numbered notes and comments:

1.  Start unrolling the downstream pipe `pg`, in the underlying monad `m`!
2.  If `pg` produced `Pure x`, it means we're done pulling anything. The entire
    pipe has terminated, since we will never need anything again. So just quit
    out with `pure x`.
3.  If `pg` produced `Free (YieldF o x)`, it means it's yielding an `o` and
    continuing on with `x`. So let's just yield that `o` and move on to the
    composition of `pf` with the next pipe `x`.
4.  If `pg` produced `Free (AwaitF g)`, now things get interesting. We need to
    unroll `pf` until it yields some `Maybe b`, and feed that to
    `g :: Maybe b     -> Pipe b c m y`.
5.  If `pf` produced `Pure y`, that means it was done! The upstream terminated,
    so the downstream will have to terminate as well. So `g` gets a `Nothing`,
    and we move from there. Note we have to compose with a dummy pipe `pure ()`
    to make the types match up properly.
6.  If `pf` produced `YieldF o y`, then we have found our match! So give
    `g     (Just o)`, and now we recursively compose the next pipe (`y`) with
    the that `g` gave us.
7.  If `pf` produced `AwaitF f`, then we're in a bind, aren't we? We now have
    two layers waiting for something further upstream. So, we await from *even
    further* upstream; when we get it, we feed it to `f` and then compose
    `f i     :: Pipe a b m x` with `pg`'s result (wrapping up `gRes` back into a
    `FreeT`/`Pipe` so the types match up).

Admittedly (!) this is the "ugly" part of this derivation: sometimes we just
can't get everything for free. But getting the Monad, Applicative, Functor,
MonadTrans, etc. instances is probably nice enough to justify this inconvenience
:) And who knows, there might be a free structure that I don't know about that
gives us all of these *plus* piping for free.

### Christmas Miracle

It runs!

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L128-L133

samplePipe :: Handle -> Pipe i o IO ()
samplePipe handle =
       sourceHandle handle
    .| untilSTOP
    .| toUpperPipe
    .| sinkStdout
```

    $ cat testpipefile.txt
    hello
    world
    STOP
    okay
    goodbye

``` haskell
ghci> withFile "testpipefile.txt" ReadMode $ \handle ->
        runPipe (samplePipe handle)
-- HELLO
-- WORLD
```

Smooth as silk :D

## Takeways for a Happy New Year

Most of this post was thought up when I needed[^2] a tool that was *sort of*
like conduit, *sort of* like pipes, *sort of* like the other libraries...and I
thought I had to read up on the theory of pipes and iteratees and trampolines
and fancy pants math stuff to be able to make anything useful in this space. I
remember being very discouraged when I read about this stuff as a wee new
Haskeller, because the techniques seemed so foreign and out of the range of my
normal Haskell experience.

However, I found a way to maintain a level head somehow, and just thought ---
"ok, I just need a monad (trans) with two primitive actions: await, and yield.
Why don't I just make an await and yield and get automatic `Monad` and
`MonadTrans` instances with the appropriate free structure?"

As we can see...this works just fine! We only needed to implement one extra
thing (`.|`) to get the interface of our dreams. Of course, for a real
industrial-strength streaming combinator library, we might need to be a bit more
careful. But for my learning experience and use case, it worked perfectly.

The next time you need to make some monad that might seem exotic, try this out
and see if it works for you :)

Happy holidays, and merry Christmas!

## Exercises

Click on the links in the corner of the text boxes for solutions! (or just check
out [the source
file](https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs))

1.  An `Pipe i o m a` "takes" `i` and "produces" `o`, so it should make sense to
    make pre-map and post-map functions:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L148-L151

    postMap :: Monad m => (o -> o') -> Pipe i o m a -> Pipe i o' m a

    preMap :: Monad m => (i' -> i) -> Pipe i o m a -> Pipe i' o m a
    ```

    That pre-maps all inputs the pipe would receive, and post-maps all of the
    values it yields.

    Hint: This actually is made a lot simpler to write with the handy
    `transFreeT` combinator, which lets you swap out/change the base functor:

    ``` haskell
    transFreeT
        :: (forall a. f a -> g a)     -- ^ polymorphic function to edit the base functor
        -> FreeT f m b
        -> FreeT g m b

    transFreeT
        :: (forall a. PipeF i o a -> PipeF i' o' a)  -- ^ polymorphic function to edit the base functor
        -> Pipe i  o  m a
        -> Pipe i' o' m a
    ```

    We could then write pre-map and post-map function on `PipeF` and translate
    them to `Pipe` using `transFreeT`:

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L140-L152

    postMapF :: (o -> o') -> PipeF i o a -> PipeF i o' a

    preMapF :: (i' -> i) -> PipeF i o a -> PipeF i' o a

    postMap :: Monad m => (o -> o') -> Pipe i o m a -> Pipe i o' m a
    postMap f = transFreeT (postMapF f)

    preMap :: Monad m => (i' -> i) -> Pipe i o m a -> Pipe i' o m a
    preMap f = transFreeT (preMapF f)
    ```

2.  One staple of a streaming combinator system is giving you a disciplined way
    to handle resources allocations like file handlers and properly close them
    on completion. Our streaming combinator system has no inherent way of doing
    this within its structure, but we can take advantage of the
    *[resourcet](https://hackage.haskell.org/package/resourcet)* package to
    handle it for us.

    Basically, if we run our pipes over `ResourceT IO` instead of normal `IO`,
    we get an extra action `allocate`:

    ``` haskell
    allocate
        :: IO a             -- ^ get a handler
        -> (a -> IO ())     -- ^ close a handler
        -> ResourceT IO (ResourceKey, a)

    -- example
    allocate (openFile fp ReadMode) hClose
        :: ResourceT IO (ResourceKey, Handler)
    ```

    We can use this in our pipe to open a handler from a filename, and rest
    assured that the file handler will be closed when we eventually
    `runResourceT :: ResourceT IO a -> IO a` our pipe.

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L155-L165

    sourceFile :: MonadIO m => FilePath -> Pipe i String (ResourceT m) ()

    samplePipe2 :: FilePath -> Pipe i o (ResourceT IO) ()
    samplePipe2 fp =
           sourceFile fp
        .| untilSTOP
        .| toUpperPipe
        .| hoistFreeT lift sinkStdout
    ```

    ``` haskell
    ghci> runResourceT . runPipe $ samplePipe2 "testpipefile.txt"
    -- HELLO
    -- WORLD
    ```

3.  Let's say we modified our `PipeF` slightly to take another parameter `u`,
    the result type of the upstream pipe.

    ``` haskell
    data PipeF i o u a =
        YieldF o a
      | AwaitF (Either u i -> a)

    type Pipe i o u = FreeT (PipeF i o u)

    await :: Pipe i o m (Either u i)
    await = liftF $ AwaitF id
    ```

    So now `await` would be fed `i` things yielded from upstream, but sometimes
    you'd get a `Left` indicating that the upstream pipe has terminated.

    What would be the implications if `u` is `Void`?

    ``` haskell
    type CertainPipe i o = Pipe i o Void
    ```

    What could you do in a `CertainPipe i o m a` that you couldn't normally do
    with our `Pipe i o m a`?

4.  We mentioned earlier that a "source" could have type

    ``` haskell
    type Source = Pipe ()
    ```

    And a `Source o m a` would be something that keeps on pumping out `o`s as
    much as we need, without requiring any upstream input.

    This is actually the essential behavior of the (true) list monad
    transformer, as esposed by the
    *[list-transformer](https://hackage.haskell.org/package/list-transformer)*
    package.

    In that package, `ListT` is defined as:

    ``` haskell
    newtype ListT m a = ListT { next :: m (Step m a) }

    data Step m a = Cons a (ListT m a) | Nil
    ```

    And it's a type that can yield out new `a`s on-demand, until exhausted.

    In fact, `Source o m ()` is equivalent to `ListT m o`. Write the functions
    to convert between them! :D

    ``` haskell
    -- source: https://github.com/mstksg/inCode/tree/master/code-samples/misc/streaming-combinators-free.hs#L171-L179

    toListT :: Monad m => Pipe () o m a -> L.ListT m o

    fromListT :: Monad m => L.ListT m o -> Pipe i o m ()
    ```

    Unfortunately we cannot use `iterT` because the last type parameter of each
    is different. But manual pattern matching (like how we wrote `(.|)`) isn't
    too bad!

    The semantics of `ListT` api is that `x <|> y` will "do" (and emit the
    result) `x` before moving on to what `y` would emit. And `empty` is the
    `ListT` that signals it is done producing. `<|>` and `pure` and `empty` for
    `ListT` are roughly analogous to `>>` and `yield` and `return` for `Source`,
    respectively.

## Special Thanks

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on
[patreon](https://www.patreon.com/justinle/overview), Josh Vera! :)

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

[^1]: "Universally quantified" here means that the pipe's type is left fully
    polymorphic (with no constraints) over `i`, the input.

[^2]: This came about when I was developing my numerical
    *[emd](https://hackage.haskell.org/package/emd)* library.

