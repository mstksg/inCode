module Gol where

import Control.Monad.Free        as Free
import Control.Monad.State
import Control.MonadZero         as MonadZero
import Data.Array                as A
import Data.Bifunctor
import Data.DateTime             as Date
import Data.DateTime.Instant     as Instant
import Data.Foldable
import Data.Function.Uncurried
import Data.Generic.Rep          as Generic
import Data.Generic.Rep.Show
import Data.Int                  as Int
import Data.Lazy
import Data.List.Lazy            (List)
import Data.List.Lazy            as List
import Data.List.Lazy.NonEmpty   as NEList
import Data.Map                  (Map)
import Data.Map                  as Map
import Data.Maybe
import Data.Newtype              as Newtype
import Data.NonEmpty             as NE
import Data.Set                  (Set)
import Data.Set                  as Set
import Data.Set.NonEmpty         (NonEmptySet)
import Data.Set.NonEmpty         as NESet
import Data.Traversable
import Data.Tuple
import Data.Unfoldable
import Effect                    (Effect, forE)
import Effect.Aff                (Aff)
import Effect.Aff                as Aff
import Effect.Class              (class MonadEffect, liftEffect)
import Effect.Class.Console      (log)
import Effect.Exception.Unsafe
import Effect.Now                as Now
import Effect.Ref                as Ref
import Matryoshka                as Rec
import Prelude
import Queue.One                 as Queue
import Web.DOM.Document          as Document
import Web.Event.Event           (Event, EventType)
import Web.Event.EventTarget     (EventTarget, addEventListener, eventListener)
import Web.HTML                  as Web
import Web.HTML.Event.EventTypes (readystatechange)
import Web.HTML.HTMLDocument     as HTMLDocument
import Web.HTML.Window           as Window

main :: Effect Unit
main = do
    doc  <- map HTMLDocument.toDocument <<< Window.document =<< Web.window
    ready doc do
      logMe 33

      g3D <- initGol3D "#gol3D"
      drawGol3D g3D {height:20, width:20} <<< A.fromFoldable <<< List.take 7 $
            (map <<< map) drawer3D (runner 1 initialPoints)

      g4D <- initGol4D "#gol4D"
      drawGol4D g4D {height:20, width:20} <<< A.fromFoldable <<< List.take 7 $
            (map <<< map) drawer4D (runner 2 initialPoints)

      gFlat <- initGolFlat "#golFlat"
      drawGolFlat gFlat {height:20, width:20} <<< A.fromFoldable <<< map A.fromFoldable $
        List.transpose <<< flip map (List.range 0 8) $ \d ->
           map (map drawerFlat) (List.take 7 (runner d initialPoints))

      drawGolSyms "#golSymsForward" false
      drawGolSyms "#golSymsReverse" true

      -- assignWindow "hierarchy" $ vecTreeHierarchy (vecRunNeighbsTree 4 12)
  where
    drawerFlat = map (\(Tuple (Tuple x y) pts) ->
                        { x: (x+8) `mod` 20
                        , y: (y+8) `mod` 20
                        , pts: A.fromFoldable pts
                        }
                 )
         <<< Map.toUnfoldableUnordered
    drawer3D = map (\(Tuple (Tuple x y) pts) ->
                        { x: (x+8) `mod` 20
                        , y: (y+8) `mod` 20
                        , zs: A.fromFoldable pts
                        }
                 )
         <<< Map.toUnfoldableUnordered
    drawer4D = map (\(Tuple (Tuple x y) pts) ->
                        { x: (x+8) `mod` 20
                        , y: (y+8) `mod` 20
                        , zws: A.fromFoldable pts
                        }
                 )
         <<< Map.toUnfoldableUnordered
    ready doc a = do
      a' <- doOnce a
      onE readystatechange
          (Document.toEventTarget doc)
          (\_ -> a')

runSteps
    :: Aff.Milliseconds         -- ^ time per step
    -> List (Effect Unit)
    -> Aff Unit
runSteps (Aff.Milliseconds dt) = go
  where
    go xs = do
      t0 <- liftEffect Now.now
      case List.step xs of
        List.Nil       -> pure unit
        List.Cons x ys -> do
          liftEffect x
          t1 <- liftEffect Now.now
          let fdt      = Newtype.unwrap (Instant.unInstant t1) - Newtype.unwrap (Instant.unInstant t0)
              leftover = dt - fdt
          liftEffect $ log (show leftover)
          when (leftover > 0.0) $
            Aff.delay (Aff.Milliseconds leftover)
          go ys

forIterate
    :: forall m a. Monad m
    => Int      -- ^ number of times
    -> (a -> a)
    -> (a -> m Unit)
    -> a
    -> m Unit
forIterate n f g = go 0
  where
    go i x | i < n = do
      g x
      go (i+1) (f x)
    go _ _ = pure unit


ixOrZero :: Array Int -> Int -> Int
ixOrZero xs i = case A.index xs i of
    Nothing -> 0
    Just x  -> x

type Point = Array Int

initialPoints :: Set Point2
initialPoints = Set.fromFoldable [ Tuple 0 2, Tuple 1 0, Tuple 1 2, Tuple 2 1, Tuple 2 2]

data LCount = LOne
            | LTwo
            | LThree

instance showLCount :: Show LCount where
    show = case _ of
      LOne   -> "One"
      LTwo   -> "Two"
      LThree -> "Three"

newtype NCount = NCount (Maybe LCount)

instance showNCount :: Show NCount where
    show = case _ of
      NCount (Just LOne) -> "One"
      NCount (Just LTwo) -> "Two"
      NCount (Just LThree) -> "Three"
      NCount Nothing -> "Many"

nCountInt :: NCount -> Int
nCountInt = case _ of
  NCount (Just LOne) -> 1
  NCount (Just LTwo) -> 2
  NCount (Just LThree) -> 3
  NCount Nothing -> 4

data Neighbs = Dead LCount
             | LiveAlone
             | Live LCount
             | Overload

instance showNeighbs :: Show Neighbs where
    show = case _ of
      Dead c -> "(Dead " <> show c <> ")"
      LiveAlone -> "LiveAlone"
      Live c -> "(Live " <> show c <> ")"
      Overload -> "Overload"

addLCount :: forall a. (LCount -> a) -> a -> LCount -> LCount -> a
addLCount f x = case _ of
    LOne   -> case _ of
      LOne -> f LTwo
      LTwo -> f LThree
      _    -> x
    LTwo   -> case _ of
      LOne -> f LThree
      _    -> x
    LThree -> const x

instance sgNCount :: Semigroup NCount where
    append (NCount (Just x)) (NCount (Just y)) = NCount (addLCount Just Nothing x y)
    append _ _ = NCount Nothing

instance sgNieghbs :: Semigroup Neighbs where
    append = case _ of
      Dead x -> case _ of
        Dead y    -> addLCount Dead Overload x y
        LiveAlone -> Live x
        Live y    -> addLCount Live Overload x y
        _         -> Overload
      LiveAlone -> case _ of
        Dead y    -> Live y
        LiveAlone -> LiveAlone
        Live y    -> Live y
        _         -> Overload
      Live x -> case _ of
        Dead y    -> addLCount Live Overload x y
        LiveAlone -> Live x
        Live y    -> addLCount Live Overload x y
        _         -> Overload
      Overload -> const Overload

validLiveCount :: Neighbs -> Boolean
validLiveCount = case _ of
    Dead LThree -> true
    Live LTwo   -> true
    Live LThree -> true
    _           -> false

neighbs2d :: Int -> Int -> List Int
neighbs2d n i = do
    dx <- 0 List.: (-1) List.: 1 List.: List.nil
    dy <- 0 List.: (-1) List.: 1 List.: List.nil
    pure (i + dx + n*dy)

type Point2 = Tuple Int Int

stepper
    :: forall a. Ord a => Show a
    => (a -> List a)
    -> (Int -> IntMap Neighbs)
    -> Map a (NonEmptySet Int)
    -> Map a (NonEmptySet Int)
stepper expand syms cs = Map.mapMaybe (NESet.fromFoldable <<< intMapKeys <<< filterIntMap validLiveCount <<< unionsIntMap) $
    foldl (\res (Tuple gIx ds) ->
          case Map.lookup ds prebaked of
            Nothing                 -> res
            Just (Tuple here there) -> flip (Map.unionWith append) res <<< Map.fromFoldable $
              List.zip (expand gIx) (List.singleton here List.: List.repeat (List.singleton there))
        )
      (Map.empty :: Map a (List (IntMap Neighbs)))
      (Map.toUnfoldableUnordered cs :: List (Tuple a (NonEmptySet Int)))
  where
    uniqueGroups :: Set (NonEmptySet Int)
    uniqueGroups = Set.fromFoldable cs
    prebaked :: Map (NonEmptySet Int) (Tuple (IntMap Neighbs) (IntMap Neighbs))
    prebaked = Map.fromFoldable (map (\gr -> Tuple gr (prebake gr)) (List.fromFoldable uniqueGroups))
    prebake :: NonEmptySet Int -> Tuple (IntMap Neighbs) (IntMap Neighbs)
    prebake = bimap unionsIntMap unionsIntMap <<< foldr (\pIx (Tuple here there) ->
              let pNeighbs = syms pIx
                  here'  = singletonIntMap pIx LiveAlone
                    List.: pNeighbs
                    List.: here
                  there' = singletonIntMap pIx (Dead LOne)
                    List.: pNeighbs
                    List.: there
              in  Tuple here' there'
          )
      (Tuple mempty mempty)

runner
    :: Int                          -- ^ extra dimensions
    -> Set Point2                   -- ^ points
    -> List (Lazy (Map Point2 (NonEmptySet Int)))  -- ^ steps
runner d = List.iterate ((pure <<< stepper lowNeighbs highNeighbs) =<< _)
       <<< pure
       <<< Map.fromFoldable
       <<< map (\x -> Tuple x (NESet.singleton 0))
       <<< List.fromFoldable
  where
    highNeighbs = memoInt (toIntMap <<< map (map toDead) <<< vecRunNeighbs d)
    -- highNeighbs = memoInt (map toDead <<< Map.fromFoldableWith append <<< vecRunNeighbs d)

lowNeighbs :: Point2 -> List Point2
lowNeighbs (Tuple x y) = do
  x' <- x List.: (x-1) List.: (x+1) List.: List.nil
  y' <- y List.: (y-1) List.: (y+1) List.: List.nil
  pure (Tuple x' y')

toDead :: NCount -> Neighbs
toDead (NCount (Just l)) = Dead l
toDead (NCount Nothing ) = Overload

neighbsSet :: Point -> List Point
neighbsSet = traverse (\x -> x List.: (x-1) List.: (x+1) List.: List.nil)

genVecRunIxPascal
    :: Int      -- ^ dimension
    -> Int      -- ^ maximum
    -> Int      -- ^ number
    -> List Int
genVecRunIxPascal n mx x = go x mx n
  where
    go :: Int -> Int -> Int -> List Int
    go q m k =
      let Tuple (Tuple j r) k' = chompPascal q m k
      in  if m == 1 then j List.: k' List.: List.nil
                    else j List.: go r (m-1) k'

mulNCount :: NCount -> NCount -> NCount
mulNCount (NCount (Just LOne)) y = y
mulNCount x (NCount (Just LOne)) = x
mulNCount _ _                    = NCount Nothing

toNCount :: Int -> NCount
toNCount 0 = unsafeThrow "0 ncount"
toNCount 1 = NCount (Just LOne)
toNCount 2 = NCount (Just LTwo)
toNCount 3 = NCount (Just LThree)
toNCount _ = NCount Nothing

-- | Streaming/constant space enumerate all neighbor and multiplicities
vecRunNeighbs
    :: Int      -- ^ dimension
    -> Int      -- ^ pascal index
    -> List (Tuple Int NCount)
vecRunNeighbs n orig = case List.step gens of
    List.Nil       -> List.nil
    List.Cons x xs -> go mx n 0 x true (NCount (Just LOne)) 0 x xs
  where
    mx   = maxBinom n orig + 1
    gens = genVecRunIxPascal n mx orig
    go  :: Int          -- ^ pascal i
        -> Int          -- ^ pascal j
        -> Int          -- ^ running total
        -> Int          -- ^ origina item
        -> Boolean      -- ^ currently all the same?
        -> NCount       -- ^ multiplicity
        -> Int          -- ^ item to the right
        -> Int          -- ^ current item
        -> List Int     -- ^ leftover items (right to left)
        -> List (Tuple Int NCount)
    go i j tot x0 allSame p r x ls0 = case List.step ls0 of
      List.Nil ->
        let res = r + x
            p'  = p `mulNCount` toNCount
                    ( (factorial res * (2 `Int.pow` r)) `div` factorial x )
            tot' = tot
        in  Tuple tot' p' <$ MonadZero.guard (not (allSame && x == x0))
      List.Cons l ls -> do
        xlContrib <- safeRange 0 (x+l)
        xContrib  <- safeRange (max 0 (xlContrib - l)) (min x xlContrib)
        let lContrib = xlContrib - xContrib
            res      = r + xlContrib
            l'       = l - lContrib
            x'       = x - xContrib
            p'       = p `mulNCount` toNCount
                        ( factorial res
                    `div` (factorial r * factorial xContrib * factorial lContrib)
                        )
            tot'     = tot + sum (map (\k -> binom (i+j-k) (i-1)) (safeRange 1 res))
            i'       = i - 1
            j'       = j - res
        go i' j' tot' l (allSame && xContrib == x0) p' x' l' ls

safeRange :: Int -> Int -> List Int
safeRange x y
    | y >= x    = List.range x y
    | otherwise = List.nil

data VecTree f a b = Node (f { val :: a, rest :: VecTree f a b })
                   | Leaf a b
data VecTreeF f a b r = NodeF (f { val :: a, rest :: r})
                      | LeafF a b
derive instance functorVecTreeF :: Functor f => Functor (VecTreeF f a b)
instance foldableVecTreeF :: Foldable f => Foldable (VecTreeF f a b) where
    foldr = foldrDefault
    foldl = foldlDefault
    foldMap f = case _ of
      NodeF xs -> foldMap (\{rest} -> f rest) xs
      LeafF _ _ -> mempty
instance traversableVecTreeF :: Traversable f => Traversable (VecTreeF f a b) where
    sequence = sequenceDefault
    traverse f = case _ of
      NodeF xs  -> NodeF <$> traverse (\{val,rest} -> (\r -> { val, rest: r }) <$> f rest) xs
      LeafF x y -> pure (LeafF x y)

derive instance genericVecTree :: Generic.Generic (VecTree f a b) _

instance vecTreeCorec :: Functor f => Rec.Corecursive (VecTree f a b) (VecTreeF f a b) where
    embed (NodeF xs) = Node xs
    embed (LeafF x y) = Leaf x y
instance vecTreeRec :: Functor f => Rec.Recursive (VecTree f a b) (VecTreeF f a b) where
    project (Node xs) = NodeF xs
    project (Leaf x y) = LeafF x y

instance showVecTree :: (Functor f, Foldable f, Show a, Show b) => Show (VecTree f a b) where
    show = Rec.cata $ case _ of
      NodeF xs ->
        let rshow {val,rest} = "{val: " <> show val <> ", rest: " <> rest <> "}"
            xshow = "[" <> intercalate "," (map rshow xs) <> "]"
        in  "(NodeF " <> xshow <> ")"
      LeafF x y -> "(Leaf " <> show x <> " " <> show y <> ")"

type VRState =
    { i :: Int
    , j :: Int
    , out :: List Int
    , x0 :: Int
    , allSame :: Boolean
    , p :: Int
    , r :: Int
    , x :: Int
    , ls0 :: List Int
    }


-- consNE :: forall a. a -> NEList.NonEmptyList a -> NEList.NonEmptyList a
-- consNE x (NEList.NonEmptyList xs) = NEList.NonEmptyList $
--     flip map xs $ case _ of
--       y NE.:| ys -> x NE.:| (y List.: ys)

-- mkNE :: forall a. a -> List a -> NEList.NonEmptyList a
-- mkNE x xs = NEList.NonEmptyList $ pure (x NE.:| xs)


type Contrib = { left :: Int, here :: Int, chosen :: Array Int, leftovers :: Array Int }
-- type TreeRes = { mult :: Int, res :: Array Int }

-- addLastTwo :: List Int -> List Int
-- addLastTwo xs = case List.step xs of
--     List.Nil -> List.nil
--     List.Cons y ys -> case List.step ys of
--       List.Nil -> List.singleton y
--       List.Cons z zs -> case List.step zs of
--         List.Nil -> List.singleton (y+z)
--         List.Cons a as -> y List.: addLastTwo ys

dropAlong :: forall a b. List a -> List b -> List b
dropAlong xs ys = case List.step xs of
    List.Nil -> ys
    List.Cons _ xs' -> case List.step ys of
      List.Nil -> List.nil
      List.Cons _ ys' -> dropAlong xs' ys'

vecRunNeighbsTree
    :: Int      -- ^ dimension
    -> Int      -- ^ pascal index
    -> VecTree List Contrib Int
vecRunNeighbsTree n orig = case List.step gens of
    List.Nil       -> undefined
    List.Cons x xs -> Rec.ana go
      { i: mx, j: n, out: List.nil, x0: x, allSame: true, p: 1, r: 0, x: x, ls0: xs}
  where
    mx   = maxBinom n orig + 1
    gens = genVecRunIxPascal n mx orig
    revGens = List.reverse gens
    go :: VRState -> VecTreeF List Contrib Int VRState
    go {i,j,out,x0,allSame,p,r,x,ls0} = case List.step ls0 of
      List.Nil ->
        let res = r + x
            p'  = p *
                    ( (factorial res * (2 `Int.pow` r)) `div` factorial x )
            out' = res List.: out
            leftovers = A.replicate (mx+1) 0
         in  LeafF { left: 0, here: x, chosen: A.fromFoldable out', leftovers } p'
      List.Cons l ls -> NodeF do
        xlContrib <- safeRange 0 (x+l)
        xContrib  <- safeRange (max 0 (xlContrib - l)) (min x xlContrib)
        let lContrib = xlContrib - xContrib
            res      = r + xlContrib
            l'       = l - lContrib
            x'       = x - xContrib
            p'       = p *
                        ( factorial res
                    `div` (factorial r * factorial xContrib * factorial lContrib)
                        )
            out'     = res List.: out
            i'       = i - 1
            j'       = j - res
            leftovers = A.fromFoldable <<< List.take (mx + 1) <<< (_ <> List.repeat 0) <<< List.reverse $
                            x' List.: l' List.: List.drop 2 (dropAlong out gens)
        pure { val: { left: lContrib, here: xContrib, chosen: A.fromFoldable out', leftovers }
             , rest: { i: i'
                     , j: j'
                     , out: out'
                     , x0: l
                     , allSame: allSame && xContrib == x0
                     , p: p'
                     , r: x'
                     , x: l'
                     , ls0: ls
                     }
             }

vecTreeHierarchy :: forall f a b. Foldable f => VecTree f a b -> Hierarchy (VecTree f a b)
vecTreeHierarchy xs = buildHierarchy xs $ case _ of
    Node xs  -> A.fromFoldable (foldMap (List.singleton <<< (_.rest)) xs)
    Leaf x y -> []

type Bazaar f a = forall r. (a -> f r) -> f Unit

type StopBazaar f a = (a -> f Boolean) -> f Unit

-- foreign import testPrint :: forall a. StopBazaar Effect a -> Effect Unit

-- // extractor :: c -> (a -> b -> r) -> r
-- // merger :: r -> r -> r
-- // bazaar :: (c -> f r) -> f r
-- exports._mergeMaps = function(extractor, merger, bazaar) {

foreign import data IntMap :: Type -> Type
foreign import _toIntMap :: forall b c r. Fn3 (c -> (Int -> b -> Effect r) -> Effect r) (r -> r -> r) (Bazaar Effect c) (IntMap r)

toIntMap :: forall f a. Foldable f => Semigroup a => f (Tuple Int a) -> IntMap a
toIntMap xs = runFn3 _toIntMap (\(Tuple x y) f -> f x y) append (for_ xs)

foreign import _unionsIntMap :: forall b c r. Fn2 (r -> r -> r) (Bazaar Effect (IntMap r)) (IntMap r)
unionsIntMap :: forall f a. Foldable f => Semigroup a => f (IntMap a) -> IntMap a
unionsIntMap xs = runFn2 _unionsIntMap append (for_ xs)

foreign import _filterIntMap :: forall a. Fn2 (a -> Boolean) (IntMap a) (IntMap a)
filterIntMap :: forall a. (a -> Boolean) -> IntMap a -> IntMap a
filterIntMap = runFn2 _filterIntMap

foreign import intMapKeys :: forall a. IntMap a -> Array Int

foreign import _singletonIntMap :: forall a. Fn2 Int a (IntMap a)
singletonIntMap :: forall a. Int -> a -> IntMap a
singletonIntMap = runFn2 _singletonIntMap

foreign import data Hierarchy :: Type -> Type
foreign import _buildHierarchy :: forall a. Fn2 a (a -> Array a) (Hierarchy a)
buildHierarchy :: forall a. a -> (a -> Array a) -> Hierarchy a
buildHierarchy = runFn2 _buildHierarchy

foreign import logMe :: forall a. a -> Effect Unit
foreign import trace :: forall a. a -> a

traceShow :: forall a. Show a => a -> a
traceShow x = let y = trace (show x) in x

foreign import data SVGFlat :: Type
foreign import initGolFlat :: String -> Effect SVGFlat
foreign import _drawGolFlat :: Fn3
    SVGFlat
    {height::Int,width::Int}
    (Array (Array (Lazy (Array {x :: Int, y :: Int, pts :: Array Int}))))
    (Effect Unit)

drawGolFlat
    :: SVGFlat
    -> {height :: Int, width :: Int}
    -> Array (Array (Lazy (Array {x :: Int, y :: Int, pts :: Array Int})))
    -> Effect Unit
drawGolFlat = runFn3 _drawGolFlat

foreign import data SVG3D :: Type
foreign import initGol3D :: String -> Effect SVG3D
foreign import _drawGol3D :: Fn3
    SVG3D
    {height::Int,width::Int}
    (Array (Lazy (Array {x :: Int, y :: Int, zs :: Array Int})))
    (Effect Unit)

drawGol3D
    :: SVG3D
    -> {height :: Int, width :: Int}
    -> Array (Lazy (Array {x :: Int, y :: Int, zs :: Array Int}))
    -> Effect Unit
drawGol3D = runFn3 _drawGol3D

foreign import data SVG4D :: Type
foreign import initGol4D :: String -> Effect SVG4D
foreign import _drawGol4D :: Fn3
    SVG4D
    {height::Int,width::Int}
    (Array (Lazy (Array {x :: Int, y :: Int, zws :: Array Int})))
    (Effect Unit)

drawGol4D
    :: SVG4D
    -> {height :: Int, width :: Int}
    -> Array (Lazy (Array {x :: Int, y :: Int, zws :: Array Int}))
    -> Effect Unit
drawGol4D = runFn3 _drawGol4D

foreign import _drawGolSyms :: Fn2 String Boolean (Effect Unit)
drawGolSyms :: String -> Boolean -> Effect Unit
drawGolSyms = runFn2 _drawGolSyms

foreign import _binom :: Fn2 Int Int Int

binom :: Int -> Int -> Int
binom = runFn2 _binom

foreign import _maxBinom :: Fn2 Int Int Int

-- use one less than mx
maxBinom :: Int -> Int -> Int
maxBinom = runFn2 _maxBinom

foreign import _ixPascal :: Fn2 Int Int (Array Int)
ixPascal :: Int -> Int -> Array Int
ixPascal = runFn2 _ixPascal

foreign import pascalIx :: Array Int -> Int

foreign import _chompPascal :: forall a. Fn4 Int Int Int (Int -> Int -> Int -> a) a

chompPascal :: Int -> Int -> Int -> Tuple (Tuple Int Int) Int
chompPascal q n k = runFn4 _chompPascal q n k \i' q' k' -> Tuple (Tuple i' q') k'

foreign import factorial :: Int -> Int
foreign import memoInt :: forall a. (Int -> a) -> Int -> a

onE :: EventType
    -> EventTarget
    -> (Event -> Effect Unit)
    -> Effect Unit
onE etype targ h = do
  listener <- eventListener h
  addEventListener etype listener false targ

doOnce
    :: Effect Unit
    -> Effect (Effect Unit)
doOnce a = do
    doneRef <- Ref.new false
    pure do
      done <- Ref.read doneRef
      unless done do
        a
        Ref.write true doneRef

foreign import undefined :: forall a. a
foreign import _assignWindow :: forall a. Fn2 String a (Effect Unit)
assignWindow :: forall a. String -> a -> Effect Unit
assignWindow = runFn2 _assignWindow
