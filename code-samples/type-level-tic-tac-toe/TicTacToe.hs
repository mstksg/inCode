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

data Player = X | O
    deriving (Eq)

data family Sing (a :: k)

type SPlayer = Sing

data instance Sing (p :: Player) where
    SX :: Sing 'X
    SO :: Sing 'O

data instance Sing (i :: Ix) where
    SA :: Sing 'A
    SB :: Sing 'B
    SC :: Sing 'C

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

data (:. ) (f :: t -> i -> m -> Type) (g :: m -> j -> a -> Type) :: t -> i -> j -> a -> Type where
    (:. ) :: f t i m -> g m j a -> (f :. g) t i j a

data Ix = A | B | C

data IProd3 :: (Ix -> k -> Type) -> Triple k -> Type where
    IP3 :: f 'A a -> f 'B b -> f 'C c -> IProd3 f ('T a b c)

data IElem3 :: Triple a -> Ix -> a -> Type where
    IE3_A :: IElem3 ('T a b c) 'A a
    IE3_B :: IElem3 ('T a b c) 'B b
    IE3_C :: IElem3 ('T a b c) 'C c

type Board = Triple (Triple (Maybe Player))

type EmptyBoard =
  'T
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)
    ('T 'Nothing 'Nothing 'Nothing)

-- newtype Flip f a b = Flip (f b a)

-- data Line = LHoriz Ix
--           | LVert Ix
--           | LDiag1
--           | LDiag2

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
newtype Diag1 board l = Diag1 (Diagonal board l)
newtype Diag2 board l = Diag2 (Diagonal (Reverse board) l)

data Line :: Triple (Triple k) -> Triple k -> Type where
    LineHoriz :: !(Horiz board l) -> Line board l
    LineVert :: !(Vert board l) -> Line board l
    LineDiag1 :: !(Diag1 board l) -> Line board l
    LineDiag2 :: !(Diag2 board l) -> Line board l

-- data Empty :: Maybe Player -> Type where
--     Empty :: Empty 'Nothing

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

-- | Board, palyer, winner
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

class SingI k where
    sing :: Sing k

class DecideVictory (board :: Board) (p :: Player) where
    decideVictory :: Either (Victory Line board p) (NoWinner board)

data Decision a = Proved a | Disproved (a -> Void)

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

decidePlayerEq :: SPlayer (p :: Player) -> SPlayer (q :: Player) -> Decision (p :~: q)
decidePlayerEq SX SX = Proved Refl
decidePlayerEq SO SO = Proved Refl
decidePlayerEq _  _  = Disproved \case {}

decideMaybeEq
    :: Sing (m :: Maybe Player)
    -> Sing (n :: Maybe Player)
    -> Decision (m :~: n)
decideMaybeEq SNothing SNothing = Proved Refl
decideMaybeEq (SJust p) (SJust q) =
    case decidePlayerEq p q of
        Proved Refl -> Proved Refl
        Disproved r -> Disproved \case Refl -> r Refl
decideMaybeEq _ _ = Disproved \case {}

decideTripleEq
    :: Sing (t1 :: Triple (Maybe Player))
    -> Sing (t2 :: Triple (Maybe Player))
    -> Decision (t1 :~: t2)
decideTripleEq (ST a1 b1 c1) (ST a2 b2 c2) =
    case decideMaybeEq a1 a2 of
        Disproved r -> Disproved \case Refl -> r Refl
        Proved Refl ->
            case decideMaybeEq b1 b2 of
                Disproved r -> Disproved \case Refl -> r Refl
                Proved Refl ->
                    case decideMaybeEq c1 c2 of
                        Disproved r -> Disproved \case Refl -> r Refl
                        Proved Refl -> Proved Refl

decideAllSameWith
    :: Sing (row :: Triple (Maybe Player))
    -> SPlayer p
    -> Decision (AllSame row p)
decideAllSameWith (ST a b c) sp =
    case decideMaybeEq a (SJust sp) of
        Disproved r -> Disproved \case AllSame -> r Refl
        Proved Refl ->
            case decideMaybeEq b (SJust sp) of
                Disproved r -> Disproved \case AllSame -> r Refl
                Proved Refl ->
                    case decideMaybeEq c (SJust sp) of
                        Disproved r -> Disproved \case AllSame -> r Refl
                        Proved Refl -> Proved AllSame

decideRowAllSame
    :: Sing (row :: Triple (Maybe Player))
    -> Decision (DSum Sing (AllSame row))
decideRowAllSame row@(ST a b c) =
    case a of
        SNothing ->
            Disproved \case
                _ :=> w -> case w of {}
        SJust sp ->
            case decideMaybeEq b (SJust sp) of
                Disproved r ->
                    Disproved \case
                        sp' :=> w -> case decidePlayerEq sp sp' of
                            Proved Refl -> case w of
                                AllSame -> r Refl
                            Disproved r' -> case w of
                                AllSame -> r' Refl
                Proved Refl ->
                    case decideMaybeEq c (SJust sp) of
                        Disproved r ->
                            Disproved \case
                                sp' :=> w -> case decidePlayerEq sp sp' of
                                    Proved Refl -> case w of
                                        AllSame -> r Refl
                                    Disproved r' -> case w of
                                        AllSame -> r' Refl
                        Proved Refl -> Proved (sp :=> AllSame)

decideFullCell :: Sing (cell :: Maybe Player) -> Decision (FullCell cell)
decideFullCell cell = case cell of
    SNothing -> Disproved \case {}
    SJust _ -> Proved FullCell

decideFullRow
    :: Sing (row :: Triple (Maybe Player))
    -> Decision (Prod3 FullCell row)
decideFullRow row =
    case decideAll3 decideFullCell row of
        Proved p3 -> Proved p3
        Disproved r -> Disproved \p3 -> r p3

decideFull
    :: Sing (board :: Board)
    -> Decision (Full board)
decideFull board =
    case decideAll3 decideFullRow board of
        Proved p3 -> Proved p3
        Disproved r -> Disproved \p3 -> r p3

decideOutcome
    :: Sing (board :: Board)
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
    :: SPlayer (p :: Player)
    -> Sing (board :: Board)
    -> Sing (r :: Ix)
    -> Sing (c :: Ix)
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
    :: SPlayer (p :: Player)
    -> Sing (row :: Triple (Maybe Player))
    -> Sing (c :: Ix)
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
    -> Sing (row :: Triple (Maybe Player))
    -> Replace3 row row' c 'Nothing ('Just p)
    -> Sing row'
replaceRowSing sp (ST _ b c) RepA = ST (SJust sp) b c
replaceRowSing sp (ST a _ c) RepB = ST a (SJust sp) c
replaceRowSing sp (ST a b _) RepC = ST a b (SJust sp)

transposeSing
    :: Sing (b :: Triple (Triple a))
    -> Sing (Transpose b)
transposeSing (ST (ST a b c) (ST d e f) (ST g h i)) =
    ST (ST a d g) (ST b e h) (ST c f i)

reverseSing
    :: Sing (b :: Triple (Triple a))
    -> Sing (Reverse b)
reverseSing (ST r1 r2 r3) = ST r3 r2 r1

decideHoriz
    :: Sing (board :: Board)
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
    :: Sing (board :: Board)
    -> Decision (DSum Sing (Victory Vert board))
decideVert board =
    case decideHoriz (transposeSing board) of
        Proved (sp :=> Victory (Horiz h)) ->
            Proved (sp :=> Victory (Vert h))
        Disproved no ->
            Disproved \case
                sp :=> Victory (Vert h) ->
                    no (sp :=> Victory (Horiz h))

decideDiag1
    :: Sing (board :: Board)
    -> Decision (DSum Sing (Victory Diag1 board))
decideDiag1 (ST (ST a _ _) (ST _ b _) (ST _ _ c)) =
    case decideRowAllSame (ST a b c) of
        Proved (sp :=> AllSame) ->
            Proved (sp :=> Victory (Diag1 Diagonal))
        Disproved no ->
            Disproved \case
                sp :=> Victory (Diag1 Diagonal) ->
                    no (sp :=> AllSame)

decideDiag2
    :: Sing (board :: Board)
    -> Decision (DSum Sing (Victory Diag2 board))
decideDiag2 board =
    case decideDiag1 (reverseSing board) of
        Proved (sp :=> Victory (Diag1 h)) ->
            Proved (sp :=> Victory (Diag2 h))
        Disproved no ->
            Disproved \case
                sp :=> Victory (Diag2 h) ->
                    no (sp :=> Victory (Diag1 h))

decideVictorySing
    :: Sing (board :: Board)
    -> Decision (DSum Sing (Victory Line board))
decideVictorySing board =
    case decideHoriz board of
        Proved (sp :=> v) -> Proved (sp :=> liftHoriz v)
        Disproved nh ->
            case decideVert board of
                Proved (sp :=> v) -> Proved (sp :=> liftVert v)
                Disproved nv ->
                    case decideDiag1 board of
                        Proved (sp :=> v) -> Proved (sp :=> liftDiag1 v)
                        Disproved nd1 ->
                            case decideDiag2 board of
                                Proved (sp :=> v) -> Proved (sp :=> liftDiag2 v)
                                Disproved nd2 ->
                                    Disproved \case
                                        sp :=> Victory line -> case line of
                                            LineHoriz h ->
                                                nh (sp :=> Victory h)
                                            LineVert h ->
                                                nv (sp :=> Victory h)
                                            LineDiag1 h ->
                                                nd1 (sp :=> Victory h)
                                            LineDiag2 h ->
                                                nd2 (sp :=> Victory h)

liftHoriz :: Victory Horiz board p -> Victory Line board p
liftHoriz (Victory h) = Victory (LineHoriz h)

liftVert :: Victory Vert board p -> Victory Line board p
liftVert (Victory h) = Victory (LineVert h)

liftDiag1 :: Victory Diag1 board p -> Victory Line board p
liftDiag1 (Victory h) = Victory (LineDiag1 h)

liftDiag2 :: Victory Diag2 board p -> Victory Line board p
liftDiag2 (Victory h) = Victory (LineDiag2 h)

data instance Sing (m :: Maybe a) where
    SNothing :: Sing ('Nothing :: Maybe a)
    SJust :: Sing p -> Sing ('Just p)

data instance Sing (t :: Triple a) where
    ST :: Sing a -> Sing b -> Sing c -> Sing ('T a b c)

instance SingI 'X where
    sing = SX

instance SingI 'O where
    sing = SO

instance SingI ('Nothing :: Maybe a) where
    sing = SNothing

instance SingI p => SingI ('Just p) where
    sing = SJust sing

instance (SingI a, SingI b, SingI c) => SingI ('T a b c) where
    sing = ST sing sing sing

instance SingI 'A where
    sing = SA

instance SingI 'B where
    sing = SB

instance SingI 'C where
    sing = SC

-- type Almost =
--     'T
--       ('T ('Just X) 'Nothing 'Nothing)
--       ('T 'Nothing ('Just O) 'Nothing)
--       ('T 'Nothing 'Nothing 'Nothing)

-- instance DecideVictory Almost p where
--     decideVictory = Right $ NoWinner \case {}






-- type family At3 (i :: I3) (xs :: V3 a) :: a where
--   At3 'I0 ('V3 x _ _) = x
--   At3 'I1 ('V3 _ x _) = x
--   At3 'I2 ('V3 _ _ x) = x


-- type family Set3 (i :: I3) (x :: a) (xs :: V3 a) :: V3 a where
--   Set3 'I0 x ('V3 _ y z) = 'V3 x y z
--   Set3 'I1 x ('V3 y _ z) = 'V3 y x z
--   Set3 'I2 x ('V3 y z _) = 'V3 y z x


-- type family At (r :: I3) (c :: I3) (b :: Board) :: Maybe Player where
--   At r c b = At3 c (At3 r b)


-- type family Set (r :: I3) (c :: I3) (x :: Maybe Player) (b :: Board) :: Board where
--   Set r c x b = Set3 r (Set3 c x (At3 r b)) b


-- type family Place (r :: I3) (c :: I3) (p :: Player) (b :: Board) :: Board where
--   Place r c p b = Place' (At r c b) r c p b


-- type family Place' (m :: Maybe Player) (r :: I3) (c :: I3) (p :: Player) (b :: Board) :: Board where
--   Place' 'Nothing r c p b = Set r c ('Just p) b


-- type family Next (p :: Player) :: Player where
--   Next 'X = 'O
--   Next 'O = 'X

-- data EmptyAt (r :: I3) (c :: I3) (b :: Board) where
--   Empty00 ::
--     EmptyAt 'I0 'I0 ('V3 ('V3 'Nothing a b) r1 r2)
--   Empty01 ::
--     EmptyAt 'I0 'I1 ('V3 ('V3 a 'Nothing b) r1 r2)
--   Empty02 ::
--     EmptyAt 'I0 'I2 ('V3 ('V3 a b 'Nothing) r1 r2)
--   Empty10 ::
--     EmptyAt 'I1 'I0 ('V3 r0 ('V3 'Nothing a b) r2)
--   Empty11 ::
--     EmptyAt 'I1 'I1 ('V3 r0 ('V3 a 'Nothing b) r2)
--   Empty12 ::
--     EmptyAt 'I1 'I2 ('V3 r0 ('V3 a b 'Nothing) r2)
--   Empty20 ::
--     EmptyAt 'I2 'I0 ('V3 r0 r1 ('V3 'Nothing a b))
--   Empty21 ::
--     EmptyAt 'I2 'I1 ('V3 r0 r1 ('V3 a 'Nothing b))
--   Empty22 ::
--     EmptyAt 'I2 'I2 ('V3 r0 r1 ('V3 a b 'Nothing))

-- data Game (p :: Player) (b :: Board) = Game

-- play :: Game p b -> EmptyAt r c b -> Game (Next p) (Place r c p b)
-- play Game _ = Game


-- type AfterX = Place 'I0 'I0 'X Start
-- -- type BadMove = Place 'I0 'I0 'O AfterX
