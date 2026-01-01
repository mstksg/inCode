---
title: "\"Five Point Haskell\" Part 1: Total Depravity"
categories: Haskell
tags: functional programming, type safety
create-time: 2025/12/26 15:01:46
identifier: five-point-haskell-1
slug: five-point-haskell-part-1-total-depravity
series: five-point-haskell
---

Times have changed. The entire discipline of software development and
engineering is under question, and over the past years has come under multiple
movements re-thinking what it even means to write good, robust, correct code.
Multiple language and framework wars have been fought, the zeitgeist constantly
shifts. It is long overdue to clarify exactly the mindset on which we approach
and define "good" coding principles. This series sets to establish a five-point
unified framework of the "Typed Functional Programming" (and Haskell-derived)
programming philosophy aimed to create code that is maintainable, correct,
long-lasting, extensible, and beautiful to write and work with, in terms of
dispelling "heresies" (thought leader sound-bytes that have become all too
popular on Twitter) and clarifying the tried and true refutations and
guiding rules. We'll go over actionable code examples that are guided by these
principles, and also real-life examples of these principles in action.

Let's jump right into point 1: Total Depravity.

The "Hero Programmer"
---------------------

Think about the stereotype of a "brilliant programmer" that an inexperienced
programmer has in their mind --- someone who can hold every detail of a complex
system in their head, every intricate connection and relationship between each
component. There's the classic [Monkey User Comic][focus] that valorizes this
ideal.

[focus]: https://www.monkeyuser.com/2018/focus/

![Monkey User --- Focus](/img/entries/five-point-haskell/79-focus.png "Monkey User --- Focus"){style="width:50%;height:auto;"}

The 10x developer is one who can carry the state and inter-connectedness
of an entire system in their brain, and the bigger the state they can carry,
the more 10x they are.

I've been programming long enough to know both that people like this do exist,
but also that it's...not exactly what makes a programmer great, or enables you
to write good code.

If you are a new programmer trying to soak in cultural values and shared
knowledge and ideals, it's natural to think: this is how things should be,
being a programmer is about mastering this art, this is a unique skill of the
distinguished developer, if I can master my mind palace I can become as cool as
Mr. Robot.

We see this mindset happening at all levels. C++ programmers often deride rust
programmers: memory mismanagement is a skill issue, we should be teaching
people to program without memory bugs. C programmers deride C++ programmers in
the same way, and assembly programmers deride C programmers all the same.
Being a good programmer is about getting to the point where you just...don't
write bad code!

This is the myth of the hero programmer. Did you have a bug? Well, you just
need to upgrade your mental awareness and your context window. You just need to
be better and better at keeping more in your mind.

Well, a big part of what you are struggling to keep in your mind are
_inconsequential minutia_ unrelated to the actual real, high-level problems you
are supposed to be solving. The more of your mental context window is wasted on
minutia like memory management and type safety, the less you have to actually
think about the _real_ high-level problems of the code you are trying to write.
The more you can outsource, the more you can care about what really matters.

We reject the heresy of the hero programmer! We acknowledge that the inevitable
conclusion of the "get good" mental process is achieving a perfection that is
not realistic or possible.

We declare the acceptance of **Total Depravity**

> Total Depravity: No programmer can indefinitely keep the entire state of the
> program and its interconnected structure in only their head. Firstly, it is
> always only a matter of time before a critical failure happens. Secondly, it
> drains productivity by wasting mind-space on the inconsequential.

The doctrine of total depravity does not mean that we don't recognize the
ability to write sloppy code that works, or that flow states can enable some
great feats. After all, we all program with a certain sense of _imago
machinae_. Instead, it means that that all such states are _fundamentally_
unstable in their nature and will always fail at some point.

Examples
--------

We're going to look at real public examples in real life that illustrate this
principle: how these mistakes were made because of the refusal to accept total
depravity, the costs, and how we might approach these systems differently if we
_were_ aware.

Of course, we aren't highlighting these examples to judge anyone personally,
but rather to illustrate the truth of the doctrine from real-world experiences.

### ID Mix-ups

The [2022 Atlassian Outage][atlassian], fundamentally, was the result of
passing the wrong type of ID. The operators were intended to pass _App_ IDs,
but instead passed _Site_ IDs, and the errors cascaded from there. It goes to
say that if you have a bunch of "naked" ID's, then mixing them up is eventually
going to backfire on you.

[atlassian]: https://www.atlassian.com/blog/atlassian-engineering/post-incident-review-april-2022-outage


```haskell
newtype Id = Id Int

type SiteId = Id
type AppId = Id

getApps :: SiteId -> IO [AppId]
deleteSite :: SiteId -> IO ()
deleteApp :: AppId -> IO ()
```

This is very convenient because you get functions for all ID's without any
extra work. Let's say you want to serialize/print or deserialize/read these ID's --- it
can be very convenient to give them all the same type so that you can write
this logic in one place.

```haskell
instance ToJSON Id where
  toJSON (Id x) = object [ "id" .= x ]

instance FromJSON Id where
  parseJSON = withObject "Id" $ \v ->
    Id <$> (v .: "id")
```

Convenient and effective...as long as you never accidentally use a `SiteId` as
a `AppId` or vice versa. And this is a very easy delusion to take, if you
don't believe in total depravity. However...sooner or later (maybe in a week,
maybe in a year, maybe after you onboard that new team member)...and someone is
going to accidentally pass a site id where an app id is expected.

```haskell
main :: IO ()
main = do
    let targetSites = ["abc", "def"]
    mapM_ deleteApp targetaSites
```

And at that point it's all over.

Knowing this can happen, we can add a simple newtype wrapper so that
accidentally using the wrong ID is a compile error:

```haskell
newtype SiteId = SiteId Id
newtype AppId = AppId Id
```

And now such a mis-call will never compile! Congratulations!

We did have a downside now: we can no longer write code polymorphic over Id's
when we want to. In the untyped situation, we could _only_ write polymorphic
code, and in the new situation we can _only_ write code for one Id type. In
some cases, we would like the ability to choose to do one or the other and get
the best of both worlds. We can get this by using _phantom types_, types that
don't refer to anything inside the actual data representation:

```haskell
data Id a = Id { getId :: Int }

data Site
data App

type SiteId = Id User
type AppId = Id Product

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

### More Phantoms

Phantom types gives us a _lot_ of low-hanging fruits to preventing inadvertent
bad usages.

The [2017 Digital Ocean outage][digitalocean] outage, for example,
fundamentally was about the wrong environment credentials being used.

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
_deep_ inside a function inside a function inside a function called inside a
call to the prod database. I guarantee it.

To ensure this never happens, we can used closed phantom types using
`DataKinds`:

```haskell
data Env = Prod | Test

newtype DbConnection (a :: Env) = DbConnection Connection

runQuery :: DbConnection a -> Query -> IO Int64
runQuery (DbConnection c) q = execute_ c q

-- | Warning: Did you remember to charge your chromebook? Oh and this function
-- is safe by way.
clearTestEnv :: DbConnection Test -> IO ()
clearTestEnv conn = do
  _ <- runQuery conn "DROP TABLE IF EXISTS users CASCADE"
  putStrLn "Test data wiped."

connectProd :: IO (DbConnection Prod)
connectProd = DbConnection <$> connectPostgreSQL "host=prod..."
```

Now, if you create a connection using `connectProd`, you can use `runQuery` on
it (because it can run any `DbConnection a`)...but if any sub-function of a
sub-function calls `clearTestEnv`, it will have to unite with `DbConnection
Test`, which is impossible for a prod connection.

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
applyThrust :: Double -> Double -> Double
applyThrust currentMomentum impulse = currentMomentum + impulse
```

This is just _asking_ for someone to come along and provide newtons alongside
pounds. It isn't even clear from the types what is expected!

We can instead use the *[dimensional][]* library:

[dimensional]: https://hackage.haskell.org/package/dimensional

```haskell
import qualified Numeric.Units.Dimensional.Prelude as P
import Numeric.Units.Dimensional ((*~))
import Numeric.Units.Dimensional.SIUnits
import Numeric.Units.Dimensional.NonSI

myMomentum :: Momentum
myMomentum = 20 *~ (newton P.* seconds)

myImpulse :: Impulse
myImpulse = 4 *~ (poundForce P.* seconds)

applyThrust :: Momentum -> Impulse -> Momentum
applyThrust currentMomentum impulse = currentMomentum + impulse
```

Now as long as momentum and impulse are provided in the correct types at API
boundaries, no mix-up will happen! Libaries just need to provide a unified
`Momenum` or `Impulse` type.

### The Million-Dollar Problem





















### Blurring the Boundaries

"Shotgun parsing" involves mixing validated and unvalidated data at different
levels in your program. Often times it is considered "fine" because you just
need to remember which inputs are validated and which aren't ... right? In
truth, all it takes is a simple temporary lapse of mental model, a time delay
between working on code, or uncoordinated contributions before things fall
apart.

Consider: let us only store valid usernames in the database.

```haskell
validUsername :: String -> Bool
validUsername s = all isAlphaNum s && all isLower s

-- | Returns 'Nothing' if username is invalid or insertion failed
saveUser :: Connection -> String -> IO (Maybe UUID)
saveUser conn s
  | validUsername s = do
      newId <- query conn "INSERT INTO users (username) VALUES (?) returning user_id" (Only s)
      case newId of
        [] -> pure Nothing
        Only i : _ -> pure (Just i)
  | otherwise = pure False

getUser :: Connection -> UUID -> IO (Maybe String)
getUser conn uid = do
  unames <- query conn "SELECT username FROM users where user_id = ?"
  case unames of
    [] -> pure Nothing
    Only s : _ -> pure (Just s)
```

It _should_ be fine as long as you only ever use `saveUser` and `getUser`...and
nobody else has access to the database. But, all it takes is for someone to
hook up a custom connector, or do some manual modifications, and the `users`
table will now have an invalid username, bypassing Haskell.  And because of
that, `getUser` can return an invalid string!

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
  case newId of
    [] -> pure Nothing
    Only i : _ -> pure (Just i)

getUser :: Connection -> UUID -> IO (Maybe Username)
getUser conn uid = do
  unames <- query conn "SELECT username FROM users where user_id = ?"
  case unames of
    [] -> pure Nothing
    Only s : _ -> pure (mkUsername s)
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
  case newId of
    [] -> pure Nothing
    Only i : _ -> pure (Just i)

getUser :: Connection -> UUID -> IO (Maybe Username)
getUser conn uid = do
  <- query conn "SELECT username FROM users where user_id = ?"
  case unames of
    [] -> pure Nothing
    Only s : _ -> pure s
```

Pushing it to the driver level will also unify everything with the driver's
error-handling system.



### Use After Free



<!-- ### 2. Ariane 5 (Integer Overflow) -->

<!-- **The Depravity:** Blindly casting a 64-bit float to a 16-bit int. -->
<!-- **The Salvation:** Returning `Maybe` forces the programmer to handle the `Nothing` (failure) case. -->

<!-- ```haskell -->
<!-- import Data.Int (Int16) -->

<!-- -- The Safe primitive -->
<!-- safeCast :: Double -> Maybe Int16 -->
<!-- safeCast x --> 
<!--   | x < fromIntegral (minBound :: Int16) = Nothing -->
<!--   | x > fromIntegral (maxBound :: Int16) = Nothing -->
<!--   | otherwise = Just (round x) -->

<!-- -- The compiler forces you to acknowledge the failure mode -->
<!-- guidanceLoop :: Double -> IO () -->
<!-- guidanceLoop input = case safeCast input of -->
<!--     Just val -> runInertialSystem val -->
<!--     Nothing  -> abortSafe "Horizontal Bias Overflow" -- The line Ariane missed -->

<!-- ``` -->

<!-- ### 3. Knight Capital (Dead Flags) -->

<!-- **The Depravity:** Reusing a generic flag (`int` or `bool`) where meanings are implicit. -->
<!-- **The Salvation:** Sum Types and Exhaustiveness Checking. When you remove a constructor, the compiler screams at every location that still tries to handle it. -->

<!-- ```haskell -->
<!-- -- Old Code: data Strategy = PowerPeg | MarketMaker | Liquidity -->

<!-- -- New Code: We deleted 'PowerPeg' from the type definition -->
<!-- data Strategy = MarketMaker | Liquidity -->

<!-- runStrategy :: Strategy -> IO () -->
<!-- runStrategy s = case s of -->
<!--     MarketMaker -> placeLimitOrders -->
<!--     Liquidity   -> provideLiquidity -->
<!--     -- Compiler Error: "Constructor not in scope: PowerPeg" -->
<!--     -- The legacy code on the 8th server simply wouldn't compile/build. -->
<!--     -- PowerPeg    -> buyHighSellLow -- Logic automatically flagged for removal -->

<!-- ``` -->

<!-- ### 4. Heartbleed (Buffer Over-read) -->

<!-- **The Depravity:** Separating the data pointer from the data length (C-style). -->
<!-- **The Salvation:** Data structures that intrinsically know their size (or Dependent Types). Even standard Haskell `ByteString` prevents this, but a "Parse" view is more illustrative of the logic error. -->

<!-- ```haskell -->
<!-- import qualified Data.ByteString as BS -->

<!-- data Heartbeat = Heartbeat { payload :: BS.ByteString } -->

<!-- -- The "Sinful" Request: "Here is 1 byte, but please send me back 64kb" -->
<!-- handleHeartbeat :: BS.ByteString -> Int -> BS.ByteString -->
<!-- handleHeartbeat input claimedLen = --> 
<!--     -- Standard Haskell generic functions (take) are safe by default. -->
<!--     -- If you ask for 64kb from a 1-byte string, you just get 1 byte. -->
<!--     -- You cannot read uninitialized memory. -->
<!--     BS.take claimedLen input --> 

<!-- ``` -->


<!-- These examples I think were the wrong direction because a lot of them apply to -->
<!-- the other principles too. In this case i will now focus explicitly on bugs. -->


<!-- ```haskell -->
<!-- -- Example 5: Finite State Machine Transitions -->
<!-- -- Failure Mode: Invalid Transition (Skipping steps, e.g., 'Checkout' before 'Selection'). -->

<!-- -- [1] Untyped: One big record with nullable fields populated over time. -->
<!-- data Checkout = Checkout { items :: [Item], address :: Maybe Address, payment :: Maybe Token } -->

<!-- pay :: Checkout -> IO () -->
<!-- pay c = case address c of -->
<!--   Nothing -> error "Forgot address!" -- Runtime crash -->
<!--   Just a  -> processPayment a (payment c) -->

<!-- -- [2] Typed: GADTs enforce linear progression of state. -->
<!-- data Selecting; data Addressing; data Paying; -->

<!-- data CheckoutState a where -->
<!--   InSelection  :: [Item] -> CheckoutState Selecting -->
<!--   InAddress    :: [Item] -> CheckoutState Addressing -->
<!--   ReadyToPay   :: [Item] -> Address -> CheckoutState Paying -->

<!-- -- 'pay' can ONLY be called if we are in the Paying state. -->
<!-- pay :: CheckoutState Paying -> IO () -->
<!-- pay (ReadyToPay items addr) = processPayment addr -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 6: Authorization & Capabilities -->
<!-- -- Failure Mode: Forgotten Authorization (Admin action performed by regular user). -->

<!-- -- [1] Untyped: Passing a User struct and checking a boolean flag manually. -->
<!-- data User = User { isAdmin :: Bool, ... } -->

<!-- deleteSystem :: User -> IO () -->
<!-- deleteSystem u = -->
<!--   if isAdmin u -->
<!--   then pure () -- do delete -->
<!--   else error "403 Forbidden" -- Easy to forget this check in complex logic -->

<!-- -- [2] Typed: Capability token (Witness) required to call the function. -->
<!-- data AdminToken = AdminToken -- Opaque, only mintable by auth layer -->

<!-- -- The function signature *proves* authorization was checked upstream. -->
<!-- deleteSystem :: AdminToken -> IO () -->
<!-- deleteSystem token = pure () -- do delete -->

<!-- authorize :: User -> Maybe AdminToken -->
<!-- authorize u = if isAdmin u then Just AdminToken else Nothing -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 7: Asynchronous State Complexity -->
<!-- -- Failure Mode: Ambiguous State (Is "Nothing" result "pending" or "failed"?). -->

<!-- -- [1] Untyped: Multiple flags to track async status. -->
<!-- data AsyncJob = AsyncJob { -->
<!--   isStarted :: Bool, -->
<!--   isFinished :: Bool, -->
<!--   result :: Maybe String, -->
<!--   errorMsg :: Maybe String -->
<!-- } -->
<!-- -- Bug: What does {isStarted=True, isFinished=True, result=Nothing, errorMsg=Nothing} mean? -->

<!-- -- [2] Typed: Explicit states covering all async possibilities. -->
<!-- data AsyncJob a -->
<!--   = Idle -->
<!--   | InProgress -->
<!--   | Failed String -->
<!--   | Succeeded a -->

<!-- render :: AsyncJob String -> String -->
<!-- render status = case status of -->
<!--   Idle       -> "Waiting..." -->
<!--   InProgress -> "Loading..." -->
<!--   Failed e   -> "Error: " ++ e -->
<!--   Succeeded s-> "Result: " ++ s -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 8: Structural Guarantees (Non-Empty Lists) -->
<!-- -- Failure Mode: Partial Function / unsafe head (Crashing on empty input). -->

<!-- -- [1] Untyped: Standard list, assuming the caller checked for empty. -->
<!-- computeAverage :: [Double] -> Double -->
<!-- computeAverage xs = sum xs / fromIntegral (length xs) -->
<!-- -- If xs is [], logic implies 0/0 (NaN) or length crashes depending on implementation. -->

<!-- -- [2] Typed: NonEmpty type enforces "at least one" element structurally. -->
<!-- import Data.List.NonEmpty (NonEmpty(..)) -->

<!-- computeAverage :: NonEmpty Double -> Double -->
<!-- computeAverage (x :| xs) = -->
<!--   let all = x : xs -->
<!--   in sum all / fromIntegral (length all) -->
<!-- -- We cannot accidentally call this with an empty list; it won't compile. -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 9: Unit Confusion -->
<!-- -- Failure Mode: Unit Mismatch (Treating seconds as milliseconds). -->

<!-- -- [1] Untyped: Primitives everywhere. -->
<!-- delay :: Int -> IO () -- Is this seconds? ms? µs? -->
<!-- delay n = threadDelay n -->

<!-- runConfig = delay 5 -- Developer meant 5 seconds, system does 5 microseconds. -->

<!-- -- [2] Typed: Newtypes or Units library prevents mixing. -->
<!-- newtype Seconds = Seconds Int -->
<!-- newtype Micros  = Micros Int -->

<!-- toMicros :: Seconds -> Micros -->
<!-- toMicros (Seconds s) = Micros (s * 1000000) -->

<!-- delay :: Micros -> IO () -->
<!-- delay (Micros us) = threadDelay us -->

<!-- runConfig = delay (Seconds 5) -- COMPILATION ERROR: Expected Micros, got Seconds. -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 10: Resource Lifecycle & Scope -->
<!-- -- Failure Mode: Resource Leak / Use-After-Free. -->

<!-- -- [1] Untyped: Manual open/close pairing. -->
<!-- useFile :: FilePath -> IO () -->
<!-- useFile path = do -->
<!--   h <- openFile path ReadMode -->
<!--   process h -->
<!--   -- if process throws exception, h is never closed (Leak). -->
<!--   -- if we close h and try to read again, we crash (Use-after-free). -->
<!--   hClose h -->

<!-- -- [2] Typed: "Bracket" pattern or Continuation Passing Style enforces scope. -->
<!-- withFile :: FilePath -> (Handle -> IO r) -> IO r -->
<!-- withFile path callback = bracket (openFile path ReadMode) hClose callback -->

<!-- useFile :: FilePath -> IO () -->
<!-- useFile path = withFile path $ \h -> do -->
<!--   process h -->
<!--   -- Handle is automatically closed here, even on error. -->
<!--   -- We cannot physically access 'h' outside this lambda. -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 1: The "Zombie Order" (Boolean Explosion) -->
<!-- -- Failure Mode: Business Logic Error (Shipping a cancelled item because you only checked 'isPaid'). -->

<!-- -- [1] Untyped: We add flags as features grow. The logic complexity is 2^n. -->
<!-- data Order = Order { isPaid :: Bool, isShipped :: Bool, isCancelled :: Bool } -->

<!-- shipBatch :: [Order] -> IO () -->
<!-- shipBatch orders = do -->
<!--   -- BUG: Developer checks 'isPaid' & '!isShipped', but forgets to check '!isCancelled'. -->
<!--   -- The cancelled order exists in a "valid" state for this specific if-statement. -->
<!--   let toShip = filter (\o -> isPaid o && not (isShipped o)) orders -->
<!--   mapM_ sendPackage toShip -->

<!-- -- [2] Typed: The sum type forces mutually exclusive states. -->
<!-- data OrderState = Unpaid | Paid | Cancelled | Shipped -->
<!-- data Order = Order { state :: OrderState, ... } -->

<!-- shipBatch :: [Order] -> IO () -->
<!-- shipBatch orders = do -->
<!--   -- The compiler forces us to destructure 'state'. -->
<!--   -- We literally cannot treat a 'Cancelled' order as 'Paid' because they are different constructors. -->
<!--   let toShip = [ o | o <- orders, case state o of { Paid -> True; _ -> False } ] -->
<!--   mapM_ sendPackage toShip -->
<!-- ``` -->


<!-- ```haskell -->
<!-- -- Example 7: The "Stale Data" UI (Async State) -->
<!-- -- Failure Mode: Race Condition/Glitch (Displaying old data while loading new data, or ignoring an error). -->

<!-- -- [1] Untyped: We keep data and metadata separate. -->
<!-- data Dashboard = Dashboard { isLoading :: Bool, data :: Maybe String, error :: Maybe String } -->

<!-- render :: Dashboard -> String -->
<!-- render d = -->
<!--   -- BUG: We accessed 'data' because it was Just "Old Value", failing to check 'isLoading'. -->
<!--   -- The user thinks they are looking at live results, but the refresh is still pending. -->
<!--   -- Or: 'isLoading' is False, but 'error' is Just "Fail", yet we still show stale 'data'. -->
<!--   case data d of -->
<!--     Just val -> "Current Price: " ++ val -->
<!--     Nothing  -> "No data" -->

<!-- -- [2] Typed: We model the lifecycle, making stale access impossible. -->
<!-- data RemoteData a = NotAsked | Loading | Failure String | Success a -->

<!-- render :: RemoteData String -> String -->
<!-- render state = case state of -->
<!--   Loading   -> "Spinner..." -- Impossible to access the string here. -->
<!--   Failure e -> "Error: " ++ e -->
<!--   Success s -> "Current Price: " ++ s -- The ONLY place string data exists. -->
<!--   NotAsked  -> "Click to load" -->
<!-- ``` -->

<!-- ```haskell -->
<!-- -- Example 8: The "Uninitialized" Crash (Temporal Coupling) -->
<!-- -- Failure Mode: Runtime Crash (Calling a method before 'init()'). -->

<!-- -- [1] Untyped: The object exists, but its internal fields are null/empty until a specific method is called. -->
<!-- data GameEngine = GameEngine { config :: Maybe Config, assets :: Maybe Assets } -->

<!-- initEngine :: GameEngine -> GameEngine -->
<!-- initEngine e = e { config = Just loadConfig } -- Populates the fields -->

<!-- startGame :: GameEngine -> IO () -->
<!-- startGame e = do -->
<!--   -- BUG: The compiler lets me pass a raw (un-inited) GameEngine here. -->
<!--   -- I have to "remember" that initEngine must be called first. -->
<!--   let fps = frameRate (fromJust $ config e) -- CRASH: "Maybe.fromJust: Nothing" -->
<!--   loop fps -->

<!-- -- [2] Typed: The 'Initialized' type proves the work was done. -->
<!-- data Configured -- Phantom tag or distinct type -->
<!-- data GameEngine state = GameEngine { ... } -->

<!-- initEngine :: GameEngine Unready -> IO (GameEngine Ready) -->
<!-- initEngine e = ... -- Performs the work and wraps in 'Ready' tag -->

<!-- startGame :: GameEngine Ready -> IO () -->
<!-- startGame e = ... -- Compiles ONLY if initEngine was called successfully. -->
<!-- ``` -->
