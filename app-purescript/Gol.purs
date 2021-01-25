module Gol where

import Data.Array                as A
import Data.Foldable
import Data.Int as Int
import Control.MonadZero as MonadZero
import Data.Newtype as Newtype
import Data.Set.NonEmpty as NESet
import Data.Set.NonEmpty (NonEmptySet)
import Data.Function.Uncurried
import Data.List.Lazy            (List)
import Effect.Exception.Unsafe
import Data.List.Lazy            as List
import Data.List.Lazy.NonEmpty   as NEList
import Data.Map                  (Map)
import Data.Map                  as Map
import Data.Maybe
import Data.Set                  (Set)
import Data.Set                  as Set
import Data.Traversable
import Data.Tuple
import Effect                    (Effect, forE)
import Effect.Aff                (Aff)
import Effect.Aff                as Aff
import Effect.Class              (class MonadEffect, liftEffect)
import Effect.Class.Console      (log)
import Effect.Ref                as Ref
import Prelude
import Queue.One                 as Queue
import Web.DOM.Document          as Document
import Web.Event.Event           (Event, EventType)
import Web.Event.EventTarget     (EventTarget, addEventListener, eventListener)
import Web.HTML                  as Web
import Web.HTML.Event.EventTypes (readystatechange)
import Web.HTML.HTMLDocument     as HTMLDocument
import Data.Lazy
import Web.HTML.Window           as Window
import Data.DateTime as Date
import Data.DateTime.Instant as Instant
import Effect.Now as Now

main :: Effect Unit
main = do
    -- Aff.launchAff_ $
    --     runSteps
    --     (Aff.Milliseconds 1000.0)
    --     6
    --     stepper
    --     (log <<< show)
    --     initialPoints

    doc  <- map HTMLDocument.toDocument <<< Window.document =<< Web.window
    ready doc do
      logMe 17
      g1 <- initGol1
      Aff.launchAff_ $
        runSteps
        (Aff.Milliseconds 1000.0)
        (List.cycle <<< List.take 6 $
             drawGol1 g1 {height:20, width:20} <<< drawer <$> runner 4 initialPoints
        )
  where
    drawer = map (\(Tuple (Tuple x y) pts) ->
                        { x: (x+8) `mod` 20
                        , y: (y+8) `mod` 20
                        , val: NESet.size pts
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

stepper :: Set Point -> Set Point
stepper = Map.keys
      <<< Map.filter validLiveCount
      <<< Map.fromFoldableWith append
      <<< foldMap populate
  where
    populate = List.zipWith (flip Tuple) (LiveAlone List.: List.repeat (Dead LOne))
           <<< neighbsSet

neighbs2d :: Int -> Int -> List Int
neighbs2d n i = do
    dx <- 0 List.: (-1) List.: 1 List.: List.nil
    dy <- 0 List.: (-1) List.: 1 List.: List.nil
    pure (i + dx + n*dy)

type Point2 = Tuple Int Int

stepper2
    :: forall a. Ord a => Show a
    => (a -> List a)
    -> (Int -> Map Int Neighbs)
    -> Map a (NonEmptySet Int)
    -> Map a (NonEmptySet Int)
stepper2 expand syms cs = Map.mapMaybe (NESet.fromSet <<< Map.keys <<< Map.filter validLiveCount) $
    foldl (\res (Tuple gIx ds) ->
          case Map.lookup ds prebaked of
            Nothing                 -> res
            Just (Tuple here there) -> Map.unionWith (Map.unionWith append) res <<< Map.fromFoldable $
              List.zip (expand gIx) (here List.: List.repeat there)
        )
      Map.empty
      (Map.toUnfoldableUnordered cs :: List (Tuple a (NonEmptySet Int)))
  where
    uniqueGroups :: Set (NonEmptySet Int)
    uniqueGroups = Set.fromFoldable cs
    prebaked :: Map (NonEmptySet Int) (Tuple (Map Int Neighbs) (Map Int Neighbs))
    prebaked = Map.fromFoldable (map (\gr -> Tuple gr (prebake gr)) (List.fromFoldable uniqueGroups))
    prebake :: NonEmptySet Int -> Tuple (Map Int Neighbs) (Map Int Neighbs)
    prebake = foldl (\(Tuple here there) pIx ->
              let pNeighbs = syms pIx
                  here'  = Map.insertWith append pIx LiveAlone   pNeighbs
                  there' = Map.insertWith append pIx (Dead LOne) pNeighbs
              in  Tuple (Map.unionWith append here here') (Map.unionWith append there there')
          )
      (Tuple mempty mempty)

runner
    :: Int                          -- ^ extra dimensions
    -> Set Point2                   -- ^ points
    -> List (Map Point2 (NonEmptySet Int))  -- ^ steps
runner d = List.iterate (stepper2 lowNeighbs highNeighbs)
       <<< Map.fromFoldable
       <<< map (\x -> Tuple x (NESet.singleton 0))
       <<< List.fromFoldable
  where
    highNeighbs = memoInt (map toDead <<< Map.fromFoldableWith append <<< vecRunNeighbs d)

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

type Bazaar f a = forall r. (a -> f r) -> f Unit

type StopBazaar f a = (a -> f Boolean) -> f Unit

-- toStopBazaar :: forall f a. Monad f => List a -> StopBazaar f a
-- toStopBazaar xs0 f = go xs0
--   where
--     go xs = case List.step xs of
--       List.Nil -> pure unit
--       List.Cons x ys -> do
--         r <- f x
--         if r
--           then go ys
--           else pure unit

-- toFold :: forall f a. Foldable f => f a -> Fold a
-- toFold xs f r = foldr f r xs

-- foreign import testPrint :: forall a. StopBazaar Effect a -> Effect Unit

-- // extractor :: c -> (a -> b -> r) -> r
-- // merger :: r -> r -> r
-- // bazaar :: (c -> f r) -> f r
-- exports._mergeMaps = function(extractor, merger, bazaar) {

foreign import _mergeMaps :: forall b c r. Fn3 (c -> (Int -> b -> Effect r) -> Effect r) (r -> r -> r) (Bazaar Effect c) (Array r)

mergeMaps :: forall f a. Foldable f => Semigroup a => f (Tuple Int a) -> Array a
mergeMaps xs = runFn3 _mergeMaps (\(Tuple x y) f -> f x y) append (for_ xs)

    -- forall a. StopBazaar Effect a -> Effect Unit

foreign import logMe :: forall a. a -> Effect Unit
foreign import trace :: forall a. a -> a

traceShow :: forall a. Show a => a -> a
traceShow x = let y = trace (show x) in x

foreign import data SVG :: Type
foreign import initGol1 :: Effect SVG
foreign import _drawGol1 :: Fn3
    SVG
    {height::Int,width::Int}
    (Array {x :: Int, y :: Int, val :: Int })
    (Effect Unit)

drawGol1
    :: SVG
    -> {height :: Int, width :: Int}
    -> Array {x :: Int, y :: Int, val :: Int}
    -> Effect Unit
drawGol1 = runFn3 _drawGol1

foreign import _binom :: Fn2 Int Int Int

binom :: Int -> Int -> Int
binom = runFn2 _binom

foreign import _maxBinom :: Fn2 Int Int Int

-- use one less than mx
maxBinom :: Int -> Int -> Int
maxBinom = runFn2 _maxBinom

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

