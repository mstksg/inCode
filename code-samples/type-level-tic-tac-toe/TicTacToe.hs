{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module TicTacToe where

import Data.Kind
import Data.Dependent.Sum (DSum((:=>)))
import Data.Void
import Data.Type.Equality

class SingKind k where
    data Sing (a :: k) :: Type
    fromSing :: Sing (a :: k) -> k
    withSing :: k -> (forall a. Sing (a :: k) -> r) -> r

data Player = X | O
    deriving (Eq)

type SPlayer = Sing :: Player -> Type

type SIx = Sing :: Ix -> Type

type family NextPlayer (p :: Player) = (q :: Player) | q -> p where
    NextPlayer X = O
    NextPlayer O = X

data Triple a = T a a a

data Prod3 :: (k -> Type) -> Triple k -> Type where
    P3 :: f a -> f b -> f c -> Prod3 f ('T a b c)

data Elem3 :: Triple a -> a -> Type where
    E3_A :: Elem3 ('T a b c) a
    E3_B :: Elem3 ('T a b c) b
    E3_C :: Elem3 ('T a b c) c

data Ix = A | B | C

type Board = Triple (Triple (Maybe Player))

type EmptyBoard =
  'T
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)

type family Transpose (b :: Triple (Triple a)) :: Triple (Triple a) where
    Transpose ('T ('T a b c) ('T d e f) ('T g h i)) =
        'T ('T a d g) ('T b e h) ('T c f i)

type family Reverse (b :: Triple (Triple a)) :: Triple (Triple a) where
    Reverse ('T r1 r2 r3) = 'T r3 r2 r1

data Diagonal :: Triple (Triple k) -> Triple k -> Type where
    Diagonal :: Diagonal
        ('T ('T a x y) ('T u b v) ('T w z c))
        ('T a b c)

newtype Horiz board l = Horiz (Elem3 board l)
newtype Vert board l = Vert (Elem3 (Transpose board) l)

data Diag :: Triple (Triple k) -> Triple k -> Type where
    Diag1 :: !(Diagonal board l) -> Diag board l
    Diag2 :: !(Diagonal (Reverse board) l) -> Diag board l

data Line :: Triple (Triple k) -> Triple k -> Type where
    LineHoriz :: !(Horiz board l) -> Line board l
    LineVert :: !(Vert board l) -> Line board l
    LineDiag :: !(Diag board l) -> Line board l

data Replace3 :: Triple a -> Triple a -> Ix -> a -> a -> Type where
    RepA :: Replace3 ('T x b c) ('T y b c) 'A x y
    RepB :: Replace3 ('T a x c) ('T a y c) 'B x y
    RepC :: Replace3 ('T a b x) ('T a b y) 'C x y

data Play :: Player -> Board -> Board -> Type where
    Play :: Replace3 row row' c 'Nothing ('Just p)
         -> Replace3 board board' r row row'
         -> Play p board board'

data Victory :: (Board -> Triple (Maybe Player) -> Type) -> Board -> Player -> Type where
    Victory :: !(line board ('T ('Just p) ('Just p) ('Just p))) -> Victory line board p

data AllSame :: Triple (Maybe Player) -> Player -> Type where
    AllSame :: AllSame ('T ('Just p) ('Just p) ('Just p)) p

data FullCell :: Maybe Player -> Type where
    FullCell :: FullCell ('Just p)

type Full = Prod3 (Prod3 FullCell)

type NoWinner board = DSum Sing (Victory Line board) -> Void

-- | Board, palyer
data Game :: Board -> Player -> Type where
    Start :: Game EmptyBoard X
    AddMove :: Play p board board' -> NoWinner board -> Game board p -> Game board' (NextPlayer p)

addMove
    :: forall board board' p.
       (SingI board, SingI board')
    => Play p board board'
    -> Game board p
    -> Either
        (DSum Sing (Victory Line board))
        (Either (DSum Sing (Victory Line board')) (Game board' (NextPlayer p)))
addMove p g = case decideVictorySing (sing @board) of
    Proved wv -> Left wv
    Disproved nw ->
        case decideVictorySing (sing @board') of
            Proved wv' -> Right (Left wv')
            Disproved _ -> Right (Right (AddMove p nw g))

data Decision a = Proved a | Disproved (a -> Void)

class SDecide k where
    (%~) :: Sing (a :: k) -> Sing (b :: k) -> Decision (a :~: b)

instance SDecide Player where
    SX %~ SX = Proved Refl
    SO %~ SO = Proved Refl
    _  %~ _  = Disproved \case {}

instance SDecide Ix where
    SA %~ SA = Proved Refl
    SB %~ SB = Proved Refl
    SC %~ SC = Proved Refl
    _  %~ _  = Disproved \case {}

instance SDecide a => SDecide (Maybe a) where
    SNothing %~ SNothing = Proved Refl
    SJust a %~ SJust b =
        case a %~ b of
            Proved Refl -> Proved Refl
            Disproved r -> Disproved \case Refl -> r Refl
    _ %~ _ = Disproved \case {}

instance SDecide a => SDecide (Triple a) where
    ST a1 b1 c1 %~ ST a2 b2 c2 =
        case a1 %~ a2 of
            Disproved r -> Disproved \case Refl -> r Refl
            Proved Refl ->
                case b1 %~ b2 of
                    Disproved r -> Disproved \case Refl -> r Refl
                    Proved Refl ->
                        case c1 %~ c2 of
                            Disproved r -> Disproved \case Refl -> r Refl
                            Proved Refl -> Proved Refl

data PlayAt (p :: Player) (r :: Ix) (c :: Ix) (board :: Board) (board' :: Board) where
    PlayAt :: Replace3 row row' c 'Nothing ('Just p)
           -> Replace3 board board' r row row'
           -> PlayAt p r c board board'

data RowReplace (row :: Triple (Maybe Player)) (p :: Player) (c :: Ix) where
    RowReplace :: Sing row' -> Replace3 row row' c 'Nothing ('Just p) -> RowReplace row p c

data WithElemDSum (p :: k -> j -> Type) (xs :: Triple k) where
    WithElemDSum :: Elem3 xs x -> DSum Sing (p x) -> WithElemDSum p xs

decideAny3
    :: (forall (x :: k). Sing x -> Decision (DSum Sing (p x)))
    -> Sing (xs :: Triple k)
    -> Decision (WithElemDSum p xs)
decideAny3 decideOne (ST a b c) =
    case decideOne a of
        Proved pa -> Proved (WithElemDSum E3_A pa)
        Disproved ra ->
            case decideOne b of
                Proved pb -> Proved (WithElemDSum E3_B pb)
                Disproved rb ->
                    case decideOne c of
                        Proved pc -> Proved (WithElemDSum E3_C pc)
                        Disproved rc ->
                            Disproved \case
                                WithElemDSum e pr -> case e of
                                    E3_A -> ra pr
                                    E3_B -> rb pr
                                    E3_C -> rc pr

decideAll3
    :: (forall (x :: k). Sing x -> Decision (p x))
    -> Sing (xs :: Triple k)
    -> Decision (Prod3 p xs)
decideAll3 decideOne (ST a b c) =
    case decideOne a of
        Disproved ra -> Disproved \case
            P3 pa _ _ -> ra pa
        Proved pa ->
            case decideOne b of
                Disproved rb -> Disproved \case
                    P3 _ pb _ -> rb pb
                Proved pb ->
                    case decideOne c of
                        Disproved rc -> Disproved \case
                            P3 _ _ pc -> rc pc
                        Proved pc -> Proved (P3 pa pb pc)

decideRowAllSame
    :: Sing row
    -> Decision (DSum Sing (AllSame row))
decideRowAllSame row@(ST a b c) =
    case a of
        SNothing ->
            Disproved \case
                _ :=> w -> case w of {}
        SJust sp ->
            case b %~ SJust sp of
                Disproved r ->
                    Disproved \case
                        sp' :=> w -> case sp %~ sp' of
                            Proved Refl -> case w of
                                AllSame -> r Refl
                            Disproved r' -> case w of
                                AllSame -> r' Refl
                Proved Refl ->
                    case c %~ SJust sp of
                        Disproved r ->
                            Disproved \case
                                sp' :=> w -> case sp %~ sp' of
                                    Proved Refl -> case w of
                                        AllSame -> r Refl
                                    Disproved r' -> case w of
                                        AllSame -> r' Refl
                        Proved Refl -> Proved (sp :=> AllSame)

decideFullCell :: Sing cell -> Decision (FullCell cell)
decideFullCell cell = case cell of
    SNothing -> Disproved \case {}
    SJust _ -> Proved FullCell

decideFullRow
    :: Sing row
    -> Decision (Prod3 FullCell row)
decideFullRow row =
    case decideAll3 decideFullCell row of
        Proved p3 -> Proved p3
        Disproved r -> Disproved \p3 -> r p3

decideFull
    :: SBoard board
    -> Decision (Full board)
decideFull board =
    case decideAll3 decideFullRow board of
        Proved p3 -> Proved p3
        Disproved r -> Disproved \p3 -> r p3

decideOutcome
    :: SBoard board
    -> Decision (Either (DSum Sing (Victory Line board)) (Full board))
decideOutcome board =
    case decideVictorySing board of
        Proved v -> Proved (Left v)
        Disproved nv ->
            case decideFull board of
                Proved f -> Proved (Right f)
                Disproved nf -> Disproved \case
                    Left v -> nv v
                    Right f -> nf f

playAt
    :: SPlayer p
    -> SBoard board
    -> SIx r
    -> SIx c
    -> Decision (DSum Sing (PlayAt p r c board))
playAt sp (ST r1 r2 r3) sr sc =
    case sr of
        SA ->
            case replaceRow sp r1 sc of
                Disproved r -> Disproved (\case
                    _ :=> PlayAt rep RepA ->
                        r (RowReplace (replaceRowSing sp r1 rep) rep))
                Proved (RowReplace r1' repRow) ->
                    let board' = ST r1' r2 r3
                    in Proved (board' :=> PlayAt repRow RepA)
        SB ->
            case replaceRow sp r2 sc of
                Disproved r -> Disproved (\case
                    _ :=> PlayAt rep RepB ->
                        r (RowReplace (replaceRowSing sp r2 rep) rep))
                Proved (RowReplace r2' repRow) ->
                    let board' = ST r1 r2' r3
                    in Proved (board' :=> PlayAt repRow RepB)
        SC ->
            case replaceRow sp r3 sc of
                Disproved r -> Disproved (\case
                    _ :=> PlayAt rep RepC ->
                        r (RowReplace (replaceRowSing sp r3 rep) rep))
                Proved (RowReplace r3' repRow) ->
                    let board' = ST r1 r2 r3'
                    in Proved (board' :=> PlayAt repRow RepC)

replaceRow
    :: SPlayer p
    -> Sing row
    -> SIx c
    -> Decision (RowReplace row p c)
replaceRow sp (ST a b c) sc =
    case sc of
        SA -> case a of
            SNothing -> Proved (RowReplace (ST (SJust sp) b c) RepA)
            SJust _ -> Disproved (\case RowReplace _ rep -> case rep of {})
        SB -> case b of
            SNothing -> Proved (RowReplace (ST a (SJust sp) c) RepB)
            SJust _ -> Disproved (\case RowReplace _ rep -> case rep of {})
        SC -> case c of
            SNothing -> Proved (RowReplace (ST a b (SJust sp)) RepC)
            SJust _ -> Disproved (\case RowReplace _ rep -> case rep of {})

replaceRowSing
    :: SPlayer p
    -> Sing row
    -> Replace3 row row' c 'Nothing ('Just p)
    -> Sing row'
replaceRowSing sp (ST _ b c) RepA = ST (SJust sp) b c
replaceRowSing sp (ST a _ c) RepB = ST a (SJust sp) c
replaceRowSing sp (ST a b _) RepC = ST a b (SJust sp)

transposeSing
    :: Sing b
    -> Sing (Transpose b)
transposeSing (ST (ST a b c) (ST d e f) (ST g h i)) =
    ST (ST a d g) (ST b e h) (ST c f i)

reverseSing
    :: Sing b
    -> Sing (Reverse b)
reverseSing (ST r1 r2 r3) = ST r3 r2 r1

decideHoriz
    :: SBoard board
    -> Decision (DSum Sing (Victory Horiz board))
decideHoriz board =
    case decideAny3 decideRowAllSame board of
        Proved (WithElemDSum e (sp :=> AllSame)) ->
            Proved (sp :=> Victory (Horiz e))
        Disproved r ->
            Disproved \case
                sp :=> Victory (Horiz e) ->
                    r (WithElemDSum e (sp :=> AllSame))

decideVert
    :: SBoard board
    -> Decision (DSum Sing (Victory Vert board))
decideVert board =
    case decideHoriz (transposeSing board) of
        Proved (sp :=> Victory (Horiz h)) -> Proved (sp :=> Victory (Vert h))
        Disproved no ->
            Disproved \case
                sp :=> Victory (Vert h) -> no (sp :=> Victory (Horiz h))

decideDiagonal
    :: SBoard board
    -> Decision (DSum Sing (Victory Diagonal board))
decideDiagonal (ST (ST a _ _) (ST _ b _) (ST _ _ c)) =
    case decideRowAllSame (ST a b c) of
        Proved (sp :=> AllSame) -> Proved (sp :=> Victory Diagonal)
        Disproved no ->
            Disproved \case
                sp :=> Victory Diagonal -> no (sp :=> AllSame)

decideDiag
    :: SBoard board
    -> Decision (DSum Sing (Victory Diag board))
decideDiag board =
    case decideDiagonal board of
        Proved (sp :=> Victory diag) -> Proved (sp :=> Victory (Diag1 diag))
        Disproved no1 ->
            case decideDiagonal (reverseSing board) of
                Proved (sp :=> Victory diag) -> Proved (sp :=> Victory (Diag2 diag))
                Disproved no2 ->
                    Disproved \case
                        sp :=> Victory diag -> case diag of
                            Diag1 d -> no1 (sp :=> Victory d)
                            Diag2 d -> no2 (sp :=> Victory d)

decideVictorySing
    :: SBoard board
    -> Decision (DSum Sing (Victory Line board))
decideVictorySing board =
    case decideHoriz board of
        Proved (sp :=> v) -> Proved (sp :=> liftHoriz v)
        Disproved nh ->
            case decideVert board of
                Proved (sp :=> v) -> Proved (sp :=> liftVert v)
                Disproved nv ->
                    case decideDiag board of
                        Proved (sp :=> v) -> Proved (sp :=> liftDiag v)
                        Disproved nd ->
                            Disproved \case
                                sp :=> Victory line -> case line of
                                    LineHoriz h -> nh (sp :=> Victory h)
                                    LineVert h -> nv (sp :=> Victory h)
                                    LineDiag h -> nd (sp :=> Victory h)

liftHoriz :: Victory Horiz board p -> Victory Line board p
liftHoriz (Victory h) = Victory (LineHoriz h)

liftVert :: Victory Vert board p -> Victory Line board p
liftVert (Victory h) = Victory (LineVert h)

liftDiag :: Victory Diag board p -> Victory Line board p
liftDiag (Victory h) = Victory (LineDiag h)

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

class SingI a where
    sing :: Sing a

instance SingI 'X where
    sing = SX

instance SingI 'O where
    sing = SO

instance SingI 'A where
    sing = SA

instance SingI 'B where
    sing = SB

instance SingI 'C where
    sing = SC

instance SingKind a => SingI ('Nothing :: Maybe a) where
    sing = SNothing

instance SingI p => SingI ('Just p) where
    sing = SJust sing

instance (SingI a, SingI b, SingI c) => SingI ('T a b c) where
    sing = ST sing sing sing

type SBoard = Sing :: Board -> Type
