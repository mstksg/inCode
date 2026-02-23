{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Void

data Player = X | O

data family Sing (a :: k)

type SPlayer = Sing

data instance Sing (p :: Player) where
    SX :: Sing 'X
    SO :: Sing 'O

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

data Line :: Triple (Triple k) -> Triple k -> Type where
    Horiz :: !(Elem3 board l) -> Line board l
    Vert :: !(Elem3 (Transpose board) l) -> Line board l
    Diag1 :: !(Diagonal board l) -> Line board l
    Diag2 :: !(Diagonal (Reverse board) l) -> Line board l

-- data Empty :: Maybe Player -> Type where
--     Empty :: Empty 'Nothing

data Replace3 :: Triple a -> Triple a -> Ix -> a -> a -> Type where
    RepA :: Replace3 ('T x b c) ('T y b c) 'A x y
    RepB :: Replace3 ('T a x c) ('T a y c) 'B x y
    RepC :: Replace3 ('T a b x) ('T a b y) 'C x y

data Play :: Player -> Ix -> Ix -> Board -> Board -> Type where
    Play :: Replace3 row row' c 'Nothing ('Just p)
         -> Replace3 board board' r row row'
         -> Play p r c board board'

newtype Victory :: Board -> Player -> Type where
    Victory :: Line board (T (Just a) (Just a) (Just a)) -> Victory board a

newtype NoWinner board = NoWinner (forall p. SPlayer p -> Victory board p -> Void)

-- | Board, palyer, winner
data Game :: Board -> Player -> Maybe Player -> Type where
    Start :: Game EmptyBoard X Nothing
    Win :: Play p row col board board' -> Victory board' p -> Game board p 'Nothing -> Game board' (NextPlayer p) ('Just p)
    PlayOn :: Play p row col board board' -> NoWinner board' -> Game board p 'Nothing -> Game board' (NextPlayer p) 'Nothing

-- data Decision a = Proved a | Disproved (a -> Void)

class SingI k where
    sing :: Sing k

class DecideVictory (board :: Board) (p :: Player) where
    decideVictory :: Either (Victory board p) (NoWinner board)

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

-- decideVictorySing
--     :: Sing board
--     -> SPlayer p
--     -> Either (Victory board p) (NoWinner board)
-- decideVictorySing (ST (ST (SJust SX) (SJust SX) (SJust SX)) _ _) SX =
--     Left (Victory (Horiz E3_A))
-- decideVictorySing (ST _ (ST (SJust SX) (SJust SX) (SJust SX)) _) SX =
--     Left (Victory (Horiz E3_B))
-- decideVictorySing (ST _ _ (ST (SJust SX) (SJust SX) (SJust SX))) SX =
--     Left (Victory (Horiz E3_C))
-- decideVictorySing
--     (ST (ST (SJust SX) _ _) (ST (SJust SX) _ _) (ST (SJust SX) _ _)) SX =
--     Left (Victory (Vert E3_A))
-- decideVictorySing
--     (ST (ST _ (SJust SX) _) (ST _ (SJust SX) _) (ST _ (SJust SX) _)) SX =
--     Left (Victory (Vert E3_B))
-- decideVictorySing
--     (ST (ST _ _ (SJust SX)) (ST _ _ (SJust SX)) (ST _ _ (SJust SX))) SX =
--     Left (Victory (Vert E3_C))
-- decideVictorySing
--     (ST (ST (SJust SX) _ _) (ST _ (SJust SX) _) (ST _ _ (SJust SX))) SX =
--     Left (Victory (Diag1 Diagonal))
-- decideVictorySing
--     (ST (ST _ _ (SJust SX)) (ST _ (SJust SX) _) (ST (SJust SX) _ _)) SX =
--     Left (Victory (Diag2 Diagonal))
-- decideVictorySing (ST (ST (SJust SO) (SJust SO) (SJust SO)) _ _) SO =
--     Left (Victory (Horiz E3_A))
-- decideVictorySing (ST _ (ST (SJust SO) (SJust SO) (SJust SO)) _) SO =
--     Left (Victory (Horiz E3_B))
-- decideVictorySing (ST _ _ (ST (SJust SO) (SJust SO) (SJust SO))) SO =
--     Left (Victory (Horiz E3_C))
-- decideVictorySing
--     (ST (ST (SJust SO) _ _) (ST (SJust SO) _ _) (ST (SJust SO) _ _)) SO =
--     Left (Victory (Vert E3_A))
-- decideVictorySing
--     (ST (ST _ (SJust SO) _) (ST _ (SJust SO) _) (ST _ (SJust SO) _)) SO =
--     Left (Victory (Vert E3_B))
-- decideVictorySing
--     (ST (ST _ _ (SJust SO)) (ST _ _ (SJust SO)) (ST _ _ (SJust SO))) SO =
--     Left (Victory (Vert E3_C))
-- decideVictorySing
--     (ST (ST (SJust SO) _ _) (ST _ (SJust SO) _) (ST _ _ (SJust SO))) SO =
--     Left (Victory (Diag1 Diagonal))
-- decideVictorySing
--     (ST (ST _ _ (SJust SO)) (ST _ (SJust SO) _) (ST (SJust SO) _ _)) SO =
--     Left (Victory (Diag2 Diagonal))
-- decideVictorySing _ _ =
--     Right $ NoWinner \_ -> \case
--       Victory (Horiz p) -> case p of
--         E3_A -> _
--         E3_B -> _
--         E3_C -> _


instance (SingI board, SingI p) => DecideVictory board p where




-- instance DecideVictory
--     ('T ('T ('Just p) ('Just p) ('Just p)) r2 r3)
--     p where
--     decideVictory = Left (Victory (Horiz E3_A))

-- instance DecideVictory
--     ('T r1 ('T ('Just p) ('Just p) ('Just p)) r3)
--     p where
--     decideVictory = Left (Victory (Horiz E3_B))

-- instance DecideVictory
--     ('T r1 r2 ('T ('Just p) ('Just p) ('Just p)))
--     p where
--     decideVictory = Left (Victory (Horiz E3_C))

-- instance DecideVictory
--     ('T ('T ('Just p) b c) ('T ('Just p) e f) ('T ('Just p) h i))
--     p where
--     decideVictory = Left (Victory (Vert E3_A))

-- instance DecideVictory
--     ('T ('T a ('Just p) c) ('T d ('Just p) f) ('T g ('Just p) i))
--     p where
--     decideVictory = Left (Victory (Vert E3_B))

-- instance DecideVictory
--     ('T ('T a b ('Just p)) ('T d e ('Just p)) ('T g h ('Just p)))
--     p where
--     decideVictory = Left (Victory (Vert E3_C))

-- instance DecideVictory
--     ('T ('T ('Just p) b c) ('T d ('Just p) f) ('T g h ('Just p)))
--     p where
--     decideVictory = Left (Victory (Diag1 Diagonal))

-- instance DecideVictory
--     ('T ('T a b ('Just p)) ('T d ('Just p) f) ('T ('Just p) h i))
--     p where
--     decideVictory = Left (Victory (Diag2 Diagonal))

-- instance DecideVictory EmptyBoard p where
--     decideVictory = Right $ NoWinner \case {}


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
