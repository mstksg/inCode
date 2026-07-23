Extreme Types: Dependently Typed Tic Tac Toe (Part 1)
=====================================================

> Originally posted by [Justin Le](https://blog.jle.im/).
> [Read online!](https://blog.jle.im/entry/extreme-types-dependently-typed-tic-tac-toe-1.html)

Like a good Haskeller, you always tried your best to "make illegal states
unrepresentable". "Parse, don't validate". Suddenly every "type-unsafety", every
possible illegal state, stands out to you like a repulsive scent. Yet you tell
yourself, "I could probably make that more type-safe, I could probably prevent
that bug...but, I have to be reasonable. There are trade-offs to being too
extreme. Just get good."

But some thought deep inside rises up to whisper to you, "Just do it. Indulge in
that extreme type safety. Build that perfect palace of dependently typed
perfectly type-safe code, no matter how much Haskell (or your coworkers) fight
back. After all, why do the Agda programmers have to have all the fun. What hurt
could it cause?"

Have you ever wondered what it would look like if you took that plunge? Yes, it
will be very painful. But...just how painful? Yes you can build your perfect
castle. But how far could it go? Let's scratch that itch and see today, and jump
into dependently-typed compiler-verified type-safe tic tac toe.

## Opening

TODO: set the vibe, state the goal, show the three illegal states we will erase.

## Make Illegal States Unrepresentable

TODO: quick refresher on the mantra and the exact promises we want.

## Safe Indexing

TODO: explain why lists are the wrong shape, and why Ix + Triple is the right
shape.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L43-L53

data Triple a = T a a a

data Ix = A | B | C

data Elem3 :: Triple a -> a -> Type where
    E3_A :: Elem3 ('T a b c) a
    E3_B :: Elem3 ('T a b c) b
    E3_C :: Elem3 ('T a b c) c
```

## Board and Players

TODO: introduce Player, Board, and EmptyBoard at the type level.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L32-L61

data Player = X | O
    deriving (Eq)

type Board = Triple (Triple (Maybe Player))

type EmptyBoard =
  'T
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
```

## Type-Level Board States

TODO: DataKinds + promoted constructors; show a concrete board type.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L57-L61

type EmptyBoard =
  'T
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
```

## Replace3: A Witness for Updates

TODO: explain Replace3 and how it proves a single-cell update.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L87-L90

data Replace3 :: Triple a -> Triple a -> Ix -> a -> a -> Type where
    RepA :: Replace3 ('T x b c) ('T y b c) 'A x y
    RepB :: Replace3 ('T a x c) ('T a y c) 'B x y
    RepC :: Replace3 ('T a b x) ('T a b y) 'C x y
```

## Play Witness

TODO: compose Replace3 into Play; show why occupied cells are impossible.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L32-L33

data Player = X | O
    deriving (Eq)
```

## NextPlayer and Game

TODO: show NextPlayer as an injective type family and the Game constructors.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L39-L116

type family NextPlayer (p :: Player) = (q :: Player) | q -> p where
    NextPlayer X = O
    NextPlayer O = X

data Game :: Board -> Player -> Type where
    Start :: Game EmptyBoard X
    AddMove :: !(Play p r c board board')
            -> !(NoWinner board)
            -> !(Game board p)
            -> Game board' (NextPlayer p)
```

## Singletons: The Runtime Bridge

TODO: explain SingKind/SingI and why we need to reflect values to types.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L27-L487

class SingKind k where
    data Sing (a :: k) :: Type
    fromSing :: Sing (a :: k) -> k
    withSing :: k -> (forall a. Sing (a :: k) -> r) -> r

class SingI a where
    sing :: Sing a
```

## Singleton Instances

TODO: Player/Ix/Maybe/Triple singletons, and the type aliases.

``` haskell
-- source: https://github.com/mstksg/inCode/tree/master/code-samples/ttt/TicTacToe.hs#L35-L516

instance SingKind Player where
    data Sing (p :: Player) where
        SX :: Sing 'X
        SO :: Sing 'O
    fromSing SX = X
    fromSing SO = O
    withSing X k = k SX
    withSing O k = k SO

instance SingKind Ix where
    data Sing (i :: Ix) where
        SA :: Sing 'A
        SB :: Sing 'B
        SC :: Sing 'C
    fromSing SA = A
    fromSing SB = B
    fromSing SC = C
    withSing A k = k SA
    withSing B k = k SB
    withSing C k = k SC

instance SingKind a => SingKind (Maybe a) where
    data Sing (m :: Maybe a) where
        SNothing :: Sing ('Nothing :: Maybe a)
        SJust :: Sing p -> Sing ('Just p)
    fromSing SNothing = Nothing
    fromSing (SJust sp) = Just (fromSing sp)
    withSing Nothing k = k SNothing
    withSing (Just a) k = withSing a \sa -> k (SJust sa)

instance SingKind a => SingKind (Triple a) where
    data Sing (t :: Triple a) where
        ST :: Sing a -> Sing b -> Sing c -> Sing ('T a b c)
    fromSing (ST a b c) =
        T (fromSing a) (fromSing b) (fromSing c)
    withSing (T a b c) k =
        withSing a \sa ->
            withSing b \sb ->
                withSing c \sc ->
                    k (ST sa sb sc)

type SPlayer = Sing :: Player -> Type

type SIx = Sing :: Ix -> Type

type SBoard = Sing :: Board -> Type
```

## Finale

TODO: tease the next step: decision functions to turn runtime moves into proofs.

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

