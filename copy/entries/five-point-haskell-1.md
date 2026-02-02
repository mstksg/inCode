---
title: "\"Five-Point Haskell\": Total Depravity (and Defensive Typing)"
categories: Haskell
tags: functional programming, type safety
create-time: 2025/12/26 15:01:46
date: 2026/02/02 07:06:46
identifier: five-point-haskell-1
slug: five-point-haskell-part-1-total-depravity
series: Five-Point Haskell
---

I have thought about distilling the principles by which I program Haskell, and
how I've been able to steer long-lived projects over years of growth,
refactorings, and changes in demands. I find myself coming back to a few
distinct and helpful "points" ("doctrines", if you may allow me to say) that
have yet to lead me astray.

With a new age of software development coming, what does it even mean to write
good, robust, correct code? It is long overdue to clarify the mindset we use
to define "good" coding principles.

In this series, *[Five-Point Haskell][]*, I'll set out to establish a
five-point framework for typed functional programming (and Haskell-derived)
design that aims to produce code that is maintainable, correct, long-lasting,
extensible, and beautiful to write and work with. We'll reference real-world
case studies with actual examples when we can, and also attempt to dispel
thought-leader sound bites that have become all too popular on Twitter
("heresies", so to speak).

[Five-Point Haskell]: https://blog.jle.im/entries/series/+five-point-haskell.html

Let's jump right into point 1: the doctrine of **Total Depravity**, and why
Haskell is perfectly primed to make living with it as frictionless as possible.

> Total Depravity: If your code's correctness depends on keeping complicated
> interconnected structure in your head, a devastating incident is not a matter
> of _if_ but _when_.
>
> Therefore, delegate these concerns to tooling and a sufficiently powerful
> compiler, use types to guard against errors, and free yourself to only
> mentally track the actual important things.

Mix-ups Are Inevitable
----------------------

Think about the stereotype of a "brilliant programmer" that an inexperienced
programmer has in their mind --- someone who can hold every detail of a complex
system in their head, every intricate connection and relationship between each
component. There's the classic [Monkey User Comic][focus] that valorizes this
ideal.

[focus]: https://www.monkeyuser.com/2018/focus/

![Monkey User --- Focus](/img/entries/five-point-haskell/79-focus.png "Monkey User --- Focus"){style="width:50%;height:auto;"}

The 10x developer is one who can carry the state and interconnectedness
of an entire system in their brain, and the bigger the state they can carry,
the more 10x they are.

This is the myth of the hero programmer. Did you have a bug? Well, you just
need to upgrade your mental awareness and your context window. You just need to
be better and better at keeping more in your mind.

Actually _addressing_ these issues in most languages requires a lot of overhead
and clunkiness. But luckily we're in Haskell.

### Explicit Tags

The [2022 Atlassian Outage][atlassian], in part, was the result of passing
the wrong type of ID. The operators were intended to pass _App_ IDs, but
instead passed _Site_ IDs, and the errors cascaded from there. It goes without
saying that if you have a bunch of "naked" IDs, then mixing them up is
eventually going to backfire on you.

[atlassian]: https://www.atlassian.com/blog/atlassian-engineering/post-incident-review-april-2022-outage

```haskell
newtype Id = Id String

type SiteId = Id
type AppId = Id

getApps :: SiteId -> IO [AppId]
deleteSite :: SiteId -> IO ()
deleteApp :: AppId -> IO ()
```

This is convenient because you get functions for all IDs without any extra
work. Let's say you want to serialize/print or deserialize/read these IDs ---
it can be helpful to give them all the same type so that you can write this
logic in one place.

```haskell
instance ToJSON Id where
  toJSON (Id x) = object [ "id" .= x ]

instance FromJSON Id where
  parseJSON = withObject "Id" $ \v ->
    Id <$> (v .: "id")
```

Convenient and effective, as long as you never accidentally use a `SiteId` as
an `AppId` or vice versa. And this is a very easy delusion to fall into, if you
don't believe in total depravity. However, sooner or later (maybe in a week,
maybe in a year, maybe after you onboard that new team member)...someone is
going to accidentally pass a site ID where an app ID is expected.

```haskell
main :: IO ()
main = do
    let targetSites = [Id "abc", Id "def"]
    mapM_ deleteApp targetSites
```

And at that point it's all over.

Knowing this can happen, we can add a simple newtype wrapper so that
accidentally using the wrong ID is a compile error:

```haskell
newtype SiteId = SiteId String
newtype AppId = AppId String
```

And now such a mis-call will never compile! Congratulations!

We do have a downside now: we can no longer write code polymorphic over IDs
when we want to. In the untagged situation, we could _only_ write polymorphic
code, and in the new situation we can _only_ write code for one ID type.

```haskell
instance FromJSON SiteId where
  parseJSON = withObject "Id" $ \v -> do
    tag <- v .: "type"
    unless (tag == "Site") $
      fail "Parsed wrong type of ID!"
    SiteId <$> (v .: "id")

instance ToJSON SiteId where
  toJSON (SiteId x) = object [ "type" .= "Site", "id" .= x ]

instance FromJSON AppId where
  parseJSON = withObject "Id" $ \v -> do
    tag <- v .: "type"
    unless (tag == "App") $
      fail "Parsed wrong type of ID!"
    AppId <$> (v .: "id")

instance ToJSON AppId where
  toJSON (AppId x) = object [ "type" .= "App", "id" .= x ]
```

However, luckily, because we're in Haskell, it's easy to get the best of both
worlds with _phantom types_ (that don't refer to anything inside the actual
data representation):

```haskell
data Id a = Id { getId :: String }

data Site
data App

type SiteId = Id Site
type AppId = Id App

-- using Typeable for demonstration purposes
instance Typeable a => ToJSON (Id a) where
  toJSON (Id x) = object
    [ "type" .= show (typeRep @a)
    , "id" .= x
    ]

instance Typeable a => FromJSON (Id a) where
  parseJSON = withObject "Id" $ \v -> do
    tag <- v .: "type"
    unless (tag == show (typeRep @a)) $
      fail "Parsed wrong type of ID!"
    Id <$> (v .: "id")
```

Type safety doesn't necessarily mean inflexibility!

### Phantom Powers

Phantom types give us a _lot_ of low-hanging fruit for preventing inadvertent
misuse.

The [2017 DigitalOcean outage][digitalocean], for example, was partially about
the wrong environment credentials being used.

[digitalocean]: https://www.theregister.com/2017/04/11/database_deletion_downed_digital_ocean_last_week/

We could imagine a test harness that clears a test database using
*[postgresql-simple][]*:

[postgresql-simple]: https://hackage.haskell.org/package/postgresql-simple

```haskell
-- | Warning: do NOT call this outside of test environment!
clearTestEnv :: Connection -> IO ()
clearTestEnv conn = do
  putStrLn "Are you sure you read the warning on this function? Well, too late now!"
  _ <- execute_ conn "DROP TABLE IF EXISTS users CASCADE"
  putStrLn "Test data wiped."
```

However, somewhere down the line, someone is going to call `clearTestEnv`
_deep_ inside a function inside a function inside a function, which itself is
called against the prod database. I guarantee it.

To ensure this never happens, we can use closed phantom types using
`DataKinds`:

```haskell
data Env = Prod | Test

newtype DbConnection (a :: Env) = DbConnection Connection

runQuery :: DbConnection a -> Query -> IO Int64
runQuery (DbConnection c) q = execute_ c q

-- | Warning: Did you remember to charge your chromebook? Oh and this function
-- is safe by the way.
clearTestEnv :: DbConnection Test -> IO ()
clearTestEnv conn = do
  _ <- runQuery conn "DROP TABLE IF EXISTS users CASCADE"
  putStrLn "Test data wiped."

connectProd :: IO (DbConnection Prod)
connectProd = DbConnection <$> connectPostgreSQL "host=prod..."
```

Now, if you create a connection using `connectProd`, you can use `runQuery` on
it (because it can run any `DbConnection a`)...but if any sub-function of a
sub-function calls `clearTestEnv`, it will have to unify with `DbConnection
Test`, which is impossible for a prod connection.

This is somewhat similar to using "mocking-only" subclasses for dependency
injection, but with a closed universe. I discuss patterns like this in my
[Introduction to Singletons][singletons] series.

[singletons]: https://blog.jle.im/entries/series/+introduction-to-singletons.html

Correct Representations
-----------------------

### Semantic Phantoms

And sometimes, phantom types can do the work for you, not only preventing
mix-ups but also encoding business logic in their manipulation.

Take, for instance, the [Mars Climate Orbiter failure][mars], where the
software module provided by Lockheed Martin expected US Customary Units, and
another one developed by NASA expected SI units.

[mars]: https://en.wikipedia.org/wiki/Mars_Climate_Orbiter

If I had a function like:

```haskell
-- | In Newton-seconds
myMomentum :: Double
myMomentum = 20

-- | In Pounds-second
myImpulse :: Double
myImpulse = 4

-- | Make sure these are both the same units!
applyImpulse :: Double -> Double -> Double
applyImpulse currentMomentum impulse = currentMomentum + impulse
```

This is just _asking_ for someone to come along and provide newtons alongside
pounds. It isn't even clear from the types what is expected!

We can instead use the *[dimensional][]* library:

[dimensional]: https://hackage.haskell.org/package/dimensional

```haskell
import qualified Numeric.Units.Dimensional.Prelude as U
import Numeric.Units.Dimensional ((*~))
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.NonSI

myMomentum :: Momentum
myMomentum = 20 *~ (newton U.* seconds)

myImpulse :: Impulse
myImpulse = 4 *~ (poundForce U.* seconds)

-- Verify at compile-time that we can use '+' with Momentum and Impulse
applyImpulse :: Momentum -> Impulse -> Momentum
applyImpulse currentMomentum impulse = currentMomentum + impulse
```

Now as long as momentum and impulse are provided in the correct types at API
boundaries, no mix-up will happen. No need to send 300 million dollars down the
drain! Libraries will just need to provide a unified `Momentum` or `Impulse`
type, and everything will work out.

### The Billion-Dollar Mistake

Speaking of costly errors, there is one extremely egregious pattern that is so
pervasive, so alluring, and yet so inevitably devastating, it has been dubbed
the "Billion Dollar Mistake". It's the idea of a _sentinel value_, or in-band
signaling.

There are examples:

*   `String.indexOf()`, `str.find()`, etc. in many languages return -1 if
    the substring is not found
*   C's `fgetc()`, `getchar()`, return -1 for `EOF`. And if you cast to
    `char`, you basically can't distinguish EOF from `0xff` (`ÿ`).
*   `malloc()` returning the pointer 0 means not enough memory
*   Some languages have a special `NULL` pointer value as well --- or even a
    value `null` that can be passed in for any expected type or object or
    value.
*   JavaScript's `parseInt` returns not `null`, but rather `NaN` for a bad
    parse --- giving two distinct sentinel values
*   A lot of Unix scripting uses the empty string `""` for non-presence
*   Sensor firmware often reports values like `-999` for a bad reading...but
    sometimes `-999` might actually be a valid value!

It should be evident that these are just accidents and ticking time bombs
waiting to happen. Some caller just needs to forget to handle the sentinel
value, or to falsely assume that the sentinel value is impossible to occur in
any situation.

It's called the billion dollar mistake, but it's definitely arguable that the
cumulative damage has been much higher. High-profile incidents include
[sock_sendpage][] and the [2025 GCP outage][gcp], but if you're reading this
and you are honest with yourself, it's probably happened to you multiple times
and has been the source of many frustrating bug hunts.

[sock_sendpage]: https://www.rapid7.com/db/modules/exploit/linux/local/sock_sendpage/
[gcp]: https://www.thousandeyes.com/blog/google-cloud-outage-analysis-june-12-2025

There's also [CVE-2008-5077][], because [EVP_VerifyInit][] returns `0` for
false, `1` for true, and `-1` for error! So some OpenSSL code did a simple
if-then-else check (`result != 0`) and treated error and true the same way.
Whoops.

[CVE-2008-5077]: https://www.invicti.com/web-application-vulnerabilities/openssl-improper-input-validation-vulnerability-cve-2008-5077
[EVP_VerifyInit]: https://docs.openssl.org/1.1.1/man3/EVP_VerifyInit/

Why do we do this to ourselves? Because it is convenient. In the case of
`EVP_VerifyInit`, we can define an enum instead...

```haskell
data VerifyResult = Success | Failure | Error
```

However, it's not easy to make an "integer or not found" type in C or
JavaScript without some sort of side-channel. Imagine if JavaScript's
`String.indexOf()`
instead expected continuations on success and failure and became much less
usable as a result:

```haskell
unsafeIndexOf :: String -> String -> Int

-- vs.

-- takes a success continuation and a failure continuation
indexOf :: String -> String -> (Int -> r) -> (() -> r) -> r
```

All of this just to [fake having actual sum types][faking].

[faking]: https://blog.jle.im/entry/faking-adts-and-gadts.html

We don't really have an excuse in Haskell, since we can just return `Maybe`:

```haskell
-- from Data.Vector
elemIndex :: Eq a => a -> Vector a -> Maybe Int
```

Returning `Maybe` or `Option` forces the caller to handle:

```haskell
case elemIndex 3 myVec of
  Just i -> -- ..
  Nothing -> -- ..
```

and this handling is compiler-enforced. Provided, of course, you don't
[intentionally throw away your type-safety and compiler checks for no
reason][cloudflare-unwrap]. You can even return `Either` with an enum for
richer responses, and very easily [chain erroring operations using Functor and
Monad][ode]. In fact, with cheap ADTs, you can define your own rich result
type, like in *[unix][]*'s `ProcessStatus`:

[cloudflare-unwrap]: https://blog.cloudflare.com/18-november-2025-outage/
[ode]: https://blog.jle.im/entry/inside-my-world-ode-to-functor-and-monad.html
[unix]: https://hackage-content.haskell.org/package/unix

```haskell
data ProcessStatus
   = Exited ExitCode
   | Terminated Signal Bool
   | Stopped Signal
```

Imagine trying to cram all of that information into an `int`!

Unmarked Assumptions
--------------------

Assumptions kill, and a lot of times we arrive at implicit assumptions in our
code. Unfortunately, even if we add these assumptions in our documentation, it
only takes a minor refactor or lapse in memory for these to cause catastrophic
incidents.

There are the simple cases --- consider a `mean` function:

```haskell
-- | Warning: do not give an empty list!
mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)
```

But are you _really_ going to remember to check if your list is empty _every_
time you give it to `mean`? No, of course not. Instead, make it a
compiler-enforced constraint.

```haskell
mean :: NonEmpty Double -> Double
mean xs = sum xs / fromIntegral (length xs)
```

Now `mean` takes a `NonEmpty` list, which can only be created safely using
`nonEmpty :: [a] -> Maybe (NonEmpty a)` (where the caller has to explicitly
handle the empty list case, so they'll never forget) or from functions that
already return `NonEmpty` by default (like `some :: f a -> f (NonEmpty a)` or
`group :: Eq a => [a] -> [NonEmpty a]`), allowing you to beautifully chain
post-conditions directly into pre-conditions.

Accessing containers is, in general, very fraught...even things like indexing
lists can send us into a graveyard spiral. Sometimes the issue is more subtle.
This is our reminder to never let these implicit
assumptions go unnoticed.

### Separate Processed Data

"Shotgun parsing" involves mixing validated and unvalidated data at different
levels in your program. Oftentimes it is considered "fine" because you just
need to remember which inputs are validated and which aren't...right? In
truth, all it takes is a simple temporary lapse of mental model, a time delay
between working on code, or uncoordinated contributions before things fall
apart.

Consider a situation where we validate usernames only on write to the database.

```haskell
validUsername :: String -> Bool
validUsername s = all isAlphaNum s && all isLower s

-- | Returns 'Nothing' if username is invalid or insertion failed
saveUser :: Connection -> String -> IO (Maybe UUID)
saveUser conn s
  | validUsername s = do
      newId <- query conn "INSERT INTO users (username) VALUES (?) returning user_id" (Only s)
      pure $ case newId of
        [] -> Nothing
        Only i : _ -> Just i
  | otherwise = pure Nothing

getUser :: Connection -> UUID -> IO (Maybe String)
getUser conn uid = do
  unames <- query conn "SELECT username FROM users where user_id = ?" (Only uid)
  pure $ case unames of
    [] -> Nothing
    Only s : _ -> Just s
```

It _should_ be fine as long as you only ever use `saveUser` and `getUser`...and
nobody else has access to the database. But, if someone hooks up a custom
connector, or does some manual modifications, then the `users` table will now
have an invalid username, bypassing Haskell. And because of that, `getUser` can
return an invalid string!

Don't assume that these inconsequential slip-ups won't happen; assume that it's
only a matter of time.

Instead, we can bake the state of a validated string into the type itself:

```haskell
newtype Username = UnsafeUsername String
  deriving (Show, Eq)

-- | Our "Smart Constructor"
mkUsername :: String -> Maybe Username
mkUsername s
  | validUsername s = Just (UnsafeUsername s)
  | otherwise       = Nothing

-- | Access the raw string if needed
unUsername :: Username -> String
unUsername (UnsafeUsername s) = s
```

`Username` and `String` themselves are not structurally different --- instead,
`Username` is a compiler-enforced tag specifying it went through a specific
required validation function _within Haskell_, not just externally verified.

Now `saveUser` and `getUser` are safe at the boundaries:

```haskell
saveUser :: Connection -> Username -> IO (Maybe UUID)
saveUser conn s = do
  newId <- query conn "INSERT INTO users (username) VALUES (?) returning user_id" (Only (unUsername s))
  pure $ case newId of
    [] -> Nothing
    Only i : _ -> Just i

getUser :: Connection -> UUID -> IO (Maybe Username)
getUser conn uid = do
  unames <- query conn "SELECT username FROM users where user_id = ?" (Only uid)
  pure $ case unames of
    [] -> Nothing
    Only s : _ -> mkUsername s
```

(In real code, of course, we would use a more usable indication of failure
than `Maybe`)

We can even hook this into Haskell's typeclass system to make this even more
rigorous: `Username` could have its own `FromField` and `ToField` instances
that push the validation to the driver level.

```haskell
instance FromField Username where
  fromField f mdata = do
    s :: String <- fromField f mdata
    case mkUsername s of
      Just u  -> pure u
      Nothing -> returnError ConversionFailed f ("Invalid username format: " ++ s)

instance ToField Username where
  toField = toField . unUsername

saveUser :: Connection -> Username -> IO (Maybe UUID)
saveUser conn s = do
  newId <- query conn "INSERT INTO users (username) VALUES (?) returning user_id" (Only s)
  pure $ case newId of
    [] -> Nothing
    Only i : _ -> Just i

getUser :: Connection -> UUID -> IO (Maybe Username)
getUser conn uid = do
  unames <- query conn "SELECT username FROM users where user_id = ?" (Only uid)
  pure $ case unames of
    [] -> Nothing
    Only s : _ -> Just s
```

Pushing it to the driver level will also unify everything with the driver's
error-handling system.

### Boolean Blindness

At the heart of it, the previous examples' cardinal sin was "boolean
blindness". If we have a predicate like `validUsername :: String -> Bool`, we
will branch on that `Bool` once and throw it away. Instead, by having a
function like `mkUsername :: String -> Maybe Username`, we _keep_ the proof
alongside the value for the entire lifetime of the value. We basically pair
the string with its proof forever, making them inseparable.

There was another example of such a thing earlier: instead of using `null ::
[a] -> Bool` and gating a call to `mean` with `null`, we instead use
`nonEmpty :: [a] -> Maybe (NonEmpty a)`, and pass along the proof of
non-emptiness alongside the value itself. And, for the rest of that list's
life, it will always be paired with its non-emptiness proof.

Embracing total depravity means always keeping these proofs together, with the
witnesses bundled with the value itself, because if you don't, someone is going
to assume it exists when it doesn't, or drop it unnecessarily.

Boolean blindness also has another facet, which is where `Bool` itself is not a
semantically meaningful type. This is "semantic boolean blindness".

The classic example is `filter :: (a -> Bool) -> [a] -> [a]`. It might sound
silly until it happens to you, but it is pretty easy to mix up if `True` means
"keep" or "discard". After all, a "water filter" only lets water through, but a
"profanity filter" only rejects profanity. Instead, how about `mapMaybe :: (a
-> Maybe b) -> [a] -> [b]`? In that case, it is clear that `Just` results are
kept, and the `Nothing` results are discarded.

Sometimes, the boolean is ambiguous as to what it means. You can sort of interpret
the [1999 Mars Polar Lander][polar] crash this way. Its functions took a
boolean based on the state of the legs:

[polar]: https://en.wikipedia.org/wiki/Mars_Polar_Lander

```haskell
deployThrusters :: Bool -> IO ()
```

and `True` and `False` were misinterpreted. Instead, they could have considered
semantically meaningful types: (simplified)

```haskell
data LegState = Extended | Retracted

deployThrusters :: LegState -> IO ()
```

### Resource Cleanup

Clean-up of finite system resources is another area that is very easy to assume
you have a handle on before it gets out of hand and sneaks up on you.

```haskell
process :: Handle -> IO ()

doTheThing :: FilePath -> IO ()
doTheThing path = do
  h <- openFile path ReadMode
  process h
  hClose h
```

A bunch of things could go wrong ---

*   You might forget to always `hClose` a file handle, and if your files come
    at you dynamically, you're going to run out of file descriptors, or hold on
    to locks longer than you should
*   If `process` throws an exception, we never get to `hClose`, and the same
    issues happen
*   If another thread throws an asynchronous exception (like a thread
    cancellation), you have to make sure the close still happens!

The typical solution that other languages (like Python, modern Java) take is to
put everything inside a "block" where quitting the block guarantees the
closure. In Haskell we have the `bracket` pattern:

```haskell
-- strongly discourage using `openFile` and `hClose` directly
withFile :: FilePath -> (Handle -> IO r) -> IO r
withFile path = bracket (openFile path ReadMode) hClose

doTheThing :: FilePath -> IO ()
doTheThing path = withFile path $ \h -> do
  process h
```

If you never use `openFile` directly, and always use `withFile`, all file usage
is safe!

But, admittedly, continuations can be annoying to work with. For example, what
if you wanted to safely open a list of files?

```haskell
processAll :: [Handle] -> IO ()

doTheThings :: [FilePath] -> IO ()
doTheThings paths = -- uh...
```

All of a sudden, not so fun. And what if you had, for example, a Map of files,
like `Map Username FilePath`?

```haskell
processAll :: Map Username Handle -> IO ()

doTheThings :: Map Username FilePath -> IO ()
doTheThings paths = -- uh...
```

In another language, at this point, we might just give up
and resort to manual opening and closing of files.

But this is Haskell. We have a better solution: cleanup-tracking monads!

This is a classic usage of `ContT` to let you chain bracket-like
continuations:

```haskell
processTwo :: Handle -> Handle -> IO ()

doTwoThings :: FilePath -> FilePath -> IO ()
doTwoThings path1 path2 = evalContT $ do
    h1 <- ContT $ withFile path1
    h2 <- ContT $ withFile path2
    liftIO $ processTwo h1 h2

processAll :: Map Username Handle -> IO ()

doTheThings :: Map Username FilePath -> IO ()
doTheThings paths = evalContT $ do
    handles <- traverse (ContT . withFile) paths
    liftIO $ processAll handles
```

However, using `ContT` doesn't allow you to do things like early cleanups or
canceling cleanup events. It forces us into a last-in, first-out sort of
cleanup pattern. If you want to deviate, this might cause you to, for
convenience, go for manual resource management. However, we have tools for more
fine-grained control, we have things like *[resourcet][]* `ResourceT`, which
lets you manually control the order of clean-up events, with the guarantee
that all of them _eventually_ happen.

[resourcet]: https://hackage.haskell.org/package/resourcet

```haskell
import qualified Data.Map as M

-- | Returns set of usernames to close
processAll :: Map Username Handle -> IO (Set Username)

allocateFile :: FilePath -> ResourceT IO (ReleaseKey, Handle)
allocateFile fp = allocate (openFile fp ReadMode) hClose

-- Guarantees that all handles will eventually close, even if `go` crashes
doTheThings :: Map Username FilePath -> IO ()
doTheThings paths = runResourceT $ do
    releasersAndHandlers <- traverse allocateFile paths
    go releasersAndHandlers
  where
    -- normal operation: slowly releases handlers as we drop them
    go :: Map Username (ReleaseKey, Handle) -> ResourceT IO ()
    go currOpen = do
      toClose <- liftIO $ processAll (snd <$> currOpen)
      traverse_ (release . fst) (currOpen `M.restrictKeys` toClose)
      let newOpen = currOpen `M.withoutKeys` toClose
      unless (M.null newOpen) $
        go newOpen
```

Here we get the best of both worlds: the ability to manually close handlers
when they are no longer needed, but also the guarantee that they will
eventually be closed.

Embracing Total Depravity
-------------------------

Hopefully these examples, and similar situations, should feel relatable. We've
all experienced the biting pain of too much self-trust. Or, too much trust in
our ability to communicate with team members. Or, too much trust in ourselves 6
months from now. The traumas described here _should_ resonate with you if you
have programmed in any capacity for more than a couple of scripts.

The doctrine of total depravity does not mean that we don't recognize the
ability to write sloppy code that works, or that flow states can enable some
great feats. After all, we all program with a certain sense of _imago
machinae_. Instead, it means that all such states are _fundamentally
unstable_ in their nature and will always fail at some point. The "total"
doesn't mean we are totally cooked, it means this eventual reckoning applies to
_all_ such shortcuts.

The problem won't be solved by "get good". The problem is solved by utilizing
the tooling we are given, especially since Haskell makes them so accessible and
easy to pull in.

There's another layer here that comes as a result of embracing this mindset:
you'll find that you have more mental space to dedicate to things that actually
matter! Instead of worrying about inconsequential minutiae and details of your
flawed abstractions, you can actually think about your business logic, the flow
of your program, and architecting that castle of beauty I know you are capable
of.

### In the Age of Agentic Coding

Before we end, let's address the elephant in the room. We're writing this in
2026, in the middle of one of the biggest revolutions in software engineering
in the history of the field. A lot of people have claimed that types and safety
are now no longer important in the age of LLMs and agentic coding.

However, these claims seem to miss the fact that the fundamental issue being
addressed here exists both in LLMs and humans: the limited "context window" and
attention. Humans might be barely able to keep a dozen things in our heads,
LLMs might be able to keep a dozen dozen things, but it will still be
ultimately finite. So, the more we can move concerns out of our context window
(be it biological or mechanical), the less crowded our context windows will be,
and the more productive we will be.

Agentic coding is progressing quickly, and over the past few months I have been
exploring this a lot, using models hands-on. One conclusion I have found (and,
this agrees with everyone else I've asked who has been trying the same thing)
is that Haskell's types, in many ways, are the killer productivity secret of
agentic coding.

Many of my Haskell coding tasks for an LLM agent often involve:

1. How will the types change, or what should the types be?
2. Ralph Wiggum loop to death until the program typechecks, using `ghci` and
   `cabal`.

And, this isn't 100% effective, but from personal experience it is much more
effective than the similar situation without typed guardrails for fast
feedback, and without instant compiler feedback. The feedback loop is tighter,
the objectives clearer, the constraints more resilient, the tooling more
utilized.

I have noticed, also, that my LLM agents often check the types of the APIs
using `ghci :type`, and rarely the documentation of the functions
using `ghci :docs`. So, any "documentation-based contracts" are definitely much
more likely to explode in your face in this new world than type-based
contracts.

I'm not sure how quickly LLM-based agentic coding will progress, but I am sure
that the accidental "dropping" of concerns will continue to be a bottleneck.
All of the traits described in this post for humans will continue to be traits
of limited context windows for LLMs.

If anything, limited "brain space" might be _the_ bottleneck, for both humans
and LLMs. When we provide LLMs with properly "depravity-aware" typed code ---
or somehow encourage them to write it by giving them the right iterative
tooling --- I truly believe this might be the key to unlocking the full
potential of agentic coding.

And...not whatever [this tweet is][tweet].

[tweet]: https://x.com/rywalker/status/2003525268821188746

### The Next Step

Total depravity is all about using types to _prevent errors_. However, you can
only go so far with defensive programming and carefully picking the structure
of your types. Sometimes, it feels like you are expending a lot of energy and
human effort just picking the perfectly designed data type, only for things out
of your hand to ruin your typed castle.

In the next chapter, we'll see how a little-discussed aspect of Haskell's type
system gives you a powerful tool for opening your mind to new avenues of design
that were impossible before. At the same time, we'll see how we can leverage
universal properties of mathematics itself to help us analyze our code in
unexpected ways.

Let's explore this further in the next principle of [Five-Point
Haskell][Five-Point Haskell], **Unconditional Election**!

Special Thanks
--------------

I am very humbled to be supported by an amazing community, who make it possible
for me to devote time to researching and writing these posts. Very special
thanks to my supporter at the "Amazing" level on [patreon][], Josh Vera! :)

[patreon]: https://www.patreon.com/justinle/overview
