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
import Data.Monoid.Additive
import Data.Newtype              as Newtype
import Data.NonEmpty             as NE
import Data.Nullable             (Nullable)
import Data.Nullable             as Nullable
import Data.Set                  (Set)
import Data.Set                  as Set
import Data.Set.NonEmpty         (NonEmptySet)
import Data.Set.NonEmpty         as NESet
import Data.Traversable
import Data.Tuple
import Data.Unfoldable hiding    (fromMaybe)
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
import Unsafe.Coerce             as Unsafe
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
      logMe 13

      draw3D <- setupGol3D "#gol3D" {height:20, width:20, maxT: 6}
      drawFlat <- setupGolFlat "#golFlat" {height:20, width:20, maxT: 6, maxDim: 8}
      -- g4D <- initGol4D "#gol4D"

      drawGolSyms4D "#golSymsForward" false
      drawGolSyms4D "#golSymsReverse" true

      drawTree "#golTreeForward" true
      drawTree "#golTreeReverse" false
      -- assignWindow "testtree" $ vecTreeHierarchy (vecRunNeighbsTree 4 22)

      drawGolSyms5D "#golSyms5D"

      drawer <- setupDrawer "#golDrawer" {height:8, width:8} $ \pts -> do
        let bumped = Set.fromFoldable (map (bump 6) pts)
        -- log $ show bumped
        -- let bumped' = initialPoints'

        draw3D bumped
        drawFlat bumped
        -- drawGol3D g3D {height:20, width:20} 7 bumped
        -- drawGol4D g4D {height:20, width:20} 7 bumped
        -- drawGolFlat gFlat {height:20, width:20} 8 7 bumped

      drawer $ map (bump 2) initialPoints

  where
    ready doc a = do
      a' <- doOnce a
      onE readystatechange
          (Document.toEventTarget doc)
          (\_ -> a')
    bump n {x,y} = {x:x+n, y:y+n}
    initialPoints' = Set.fromFoldable (map (bump 8) initialPoints)

type Point = Array Int

initialPoints :: Array Point2
initialPoints = [
      { x: 0, y: 2 }
    , { x: 1, y: 0 }
    , { x: 1, y: 2 }
    , { x: 2, y: 1 }
    , { x: 2, y: 2 }
    ]

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

type Point2 = { x :: Int, y :: Int }

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
    highNeighbs = memoInt $
            toIntMap
        <<< map (map toDead)
        <<< vecRunNeighbs (\n -> mulNCount n <<< toNCount) (NCount (Just LOne)) d

lowNeighbs :: Point2 -> List Point2
lowNeighbs {x, y} = do
  x' <- x List.: (x-1) List.: (x+1) List.: List.nil
  y' <- y List.: (y-1) List.: (y+1) List.: List.nil
  pure {x:x', y:y'}

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
    :: forall a.
       (a -> Int -> a)  -- ^ multiply with int
    -> a                -- ^ initial
    -> Int         -- ^ dimension
    -> Int         -- ^ pascal index
    -> List (Tuple Int a)
vecRunNeighbs update p0 n orig = case List.step gens of
    List.Nil       -> List.nil
    List.Cons x xs -> go mx n 0 x true p0 0 x xs
  where
    mx   = maxBinom n orig + 1
    gens = genVecRunIxPascal n mx orig
    go  :: Int          -- ^ pascal i
        -> Int          -- ^ pascal j
        -> Int          -- ^ running total
        -> Int          -- ^ origina item
        -> Boolean      -- ^ currently all the same?
        -> a            -- ^ multiplicity
        -> Int          -- ^ item to the right
        -> Int          -- ^ current item
        -> List Int     -- ^ leftover items (right to left)
        -> List (Tuple Int a)
    go i j tot x0 allSame p r x ls0 = case List.step ls0 of
      List.Nil ->
        let res = r + x
            p'  = update p $
                    (factorial res * (2 `Int.pow` r)) `div` (factorial x * factorial r)
            tot' = tot
        in  Tuple tot' p' <$ MonadZero.guard (not (allSame && x == x0))
      List.Cons l ls -> do
        xlContrib <- safeRange 0 (x+l)
        lContrib  <- safeRange (max 0 (xlContrib - x)) (min l xlContrib)
        let xContrib = xlContrib - lContrib
            res      = r + xlContrib
            l'       = l - lContrib
            x'       = x - xContrib
            p'       = update p $
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

data VecTree f a = Node a (f (VecTree f a))
data VecTreeF f a r = NodeF a (f r)
derive instance functorVecTree :: Functor f => Functor (VecTree f)
derive instance functorVecTreeF :: Functor f => Functor (VecTreeF f a)

instance vecTreeCorec :: Functor f => Rec.Corecursive (VecTree f a) (VecTreeF f a) where
    embed (NodeF x ys) = Node x ys
instance vecTreeRec :: Functor f => Rec.Recursive (VecTree f a) (VecTreeF f a) where
    project (Node x ys) = NodeF x ys

instance showVecTree :: (Functor f, Foldable f, Show a) => Show (VecTree f a) where
    show = Rec.cata $ \(NodeF x xs) ->
        let xshow = "[" <> intercalate "," xs <> "]"
        in  "(Node " <> show x <> " " <> xshow <> ")"
instance foldableVecTree :: Foldable f => Foldable (VecTree f) where
    foldr = foldrDefault
    foldl = foldlDefault
    foldMap f (Node x xs) = f x <> foldMap (foldMap f) xs
instance traversableVecTree :: Traversable f => Traversable (VecTree f) where
    traverse f (Node x ys) = Node <$> f x <*> traverse (traverse f) ys
    sequence = sequenceDefault

type Mult = { total :: Int, here :: String }

type Contrib = { chosen :: Array Int
               , leftovers :: Array Int
               , multP :: Lazy Mult
               , multQ :: Lazy Mult
               , allSame :: Boolean
               , parts :: { left :: Nullable Int, here :: Nullable Int, right :: Nullable Int }
               }

type VRState =
    { i :: Int
    , j :: Int
    , out :: List Int
    , x0 :: Int
    , allSame :: Boolean
    , p :: Lazy Int
    , q :: Lazy Int
    , r :: Maybe Int
    , x :: Int
    , ls0 :: List Int
    }

dropAlong :: forall a b. List a -> List b -> List b
dropAlong xs ys = case List.step xs of
    List.Nil -> ys
    List.Cons _ xs' -> case List.step ys of
      List.Nil -> List.nil
      List.Cons _ ys' -> dropAlong xs' ys'

vecRunNeighbsTree
    :: Int      -- ^ dimension
    -> Int      -- ^ pascal index
    -> VecTree List (Lazy Contrib)
vecRunNeighbsTree n orig = case List.step gens of
    List.Nil       -> undefined
    List.Cons x xs -> Rec.ana go $
      Tuple (Just { i: mx, j: n
                  , out: List.nil, x0: x, allSame: true
                  , p: defer \_ -> 1, q: initQ, r: Nothing, x: x, ls0: xs
                  }
            )
            (defer \_ ->
                let leftovers = A.fromFoldable (List.reverse gens)
                in  { chosen: []
                    , multP: defer \_ -> { total: 1, here: "=1" }
                    , multQ: flip map initQ \q -> { total: q, here: "=" <> show q }
                    , leftovers: A.fromFoldable (List.reverse gens)
                    , allSame: true
                    , parts: { left: Nullable.null, here: Nullable.null, right: Nullable.null }
                    }
            )
  where
    mx   = maxBinom n orig + 1
    gens = genVecRunIxPascal n mx orig
    initQ = defer \_ -> product (map factorial gens)
    go :: Tuple (Maybe VRState) (Lazy Contrib) -> VecTreeF List (Lazy Contrib) (Tuple (Maybe VRState) (Lazy Contrib))
    go (Tuple vrst lastContrib) = NodeF lastContrib case vrst of
      Nothing -> List.nil
      Just {i,j,out,x0,allSame,p,q,r,x,ls0} -> case List.step ls0 of
        List.Nil ->
          let contrib = defer \_ ->
                let r' = fromMaybe 0 r
                    res = r' + x
                    mulfact = defer \_ -> factorial x * factorial r'
                    multP = do
                      p_ <- p
                      m_ <- mulfact
                      let pPow = 2 `Int.pow` r'
                          here = factorial res `div` m_
                      pure { total: p_ * here * pPow
                           , here: "×" <> show here <> "×" <> show pPow
                           }
                    multQ = do
                      q_ <- q
                      m_ <- mulfact
                      let qPow = 2 `Int.pow` (x0 - x)
                      pure { total: (q_ * qPow) `div` m_
                           , here: "÷" <> show m_ <> "×" <> show qPow
                           }
                    out' = res List.: out
                    allSame' = allSame && (x == x0)
                    chosen = A.fromFoldable out'
                in  { chosen
                    , multP
                    , multQ
                    , allSame: allSame'
                    , leftovers: A.replicate (mx+1) 0
                    , parts: { left: Nullable.null, here: Nullable.notNull x, right: Nullable.toNullable r }
                    }
          in  List.singleton (Tuple Nothing contrib)
        List.Cons l ls -> do
          xlContrib <- safeRange 0 (x+l)
          lContrib  <- safeRange (max 0 (xlContrib - x)) (min l xlContrib)
          let r'       = fromMaybe 0 r
              xContrib = xlContrib - lContrib
              res      = r' + xlContrib
              l'       = l - lContrib
              x'       = x - xContrib
              mulfact  = defer \_ -> factorial r' * factorial xContrib * factorial lContrib
              multP = do
                 p_ <- p
                 m_ <- mulfact
                 let here = factorial res `div` m_
                 pure { total: p_ * here
                      , here: "×" <> show here
                      }
              multQ = do
                 q_ <- q
                 m_ <- mulfact
                 pure { total: q_ `div` m_
                      , here: "÷" <> show m_
                      }
              out'     = res List.: out
              i'       = i - 1
              j'       = j - res
              allSame' = allSame && xContrib == x0
              contrib = defer \_ ->
                  { chosen: A.fromFoldable out'
                  , multP
                  , multQ
                  , allSame: allSame'
                  , leftovers: A.fromFoldable <<< List.take (mx + 1) <<< (_ <> List.repeat 0) <<< List.reverse $
                      x' List.: l' List.: List.drop 2 (dropAlong out gens)
                  , parts: { left: Nullable.notNull lContrib
                           , here: Nullable.notNull xContrib
                           , right: Nullable.toNullable r
                           }
                  }
          pure $
            Tuple (Just { i: i'
                        , j: j'
                        , out: out'
                        , x0: l
                        , allSame: allSame'
                        , p: (_.total) <$> multP
                        , q: (_.total) <$> multQ
                        , r: Just x'
                        , x: l'
                        , ls0: ls
                        }
                  )
                  contrib

vecTreeHierarchy :: forall f a. Foldable f => VecTree f a -> Hierarchy (VecTree f a)
vecTreeHierarchy xs = buildHierarchy xs $ \(Node _ ys) -> A.fromFoldable ys

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
foreign import trace :: forall a b. a -> b -> b

-- traceShow :: forall a. Show a => a -> a
-- traceShow x = let y = trace (show x) in x

type Drawer = Array Point2 -> Effect Unit
foreign import _setupDrawer
    :: Fn3 String
           {height :: Int, width :: Int}
           (Array Point2 -> Effect Unit)
           (Effect Drawer)
setupDrawer
    :: String
    -> {height :: Int, width :: Int}
    -> (Array Point2 -> Effect Unit)
    -> Effect Drawer
setupDrawer = runFn3 _setupDrawer

type GolFlatCallback = Array (Array (Lazy (Array {x :: Int, y :: Int, pts :: Array Int}))) -> Effect Unit
foreign import _setupGolFlat :: Fn2
    String
    {height::Int,width::Int,maxT::Int,maxDim::Int}
    (Effect GolFlatCallback)

setupGolFlat
    :: String
    -> {height :: Int, width :: Int, maxT :: Int, maxDim :: Int}
    -> Effect (Set Point2 -> Effect Unit)
setupGolFlat sel size = (_ <<< preRun) <$> runFn2 _setupGolFlat sel size
  where
    -- preRun pts = A.fromFoldable <<< List.take (size.maxZ +1) <<< map (map drawer) <<< runner 1
    preRun pts = A.fromFoldable <<< map A.fromFoldable
      <<< List.transpose <<< flip map (List.range 0 size.maxDim) $ \d ->
            map (map drawer) (List.take (size.maxT+1) (runner d pts))
    drawer = map (\(Tuple {x, y} pts) ->
                    { x: x `mod` size.width
                    , y: y `mod` size.height
                    , pts: A.fromFoldable pts
                    }
             )
         <<< Map.toUnfoldableUnordered

type Gol3DCallback = Array (Lazy (Array {x :: Int, y :: Int, zs :: Array Int})) -> Effect Unit
foreign import _setupGol3D :: Fn2
    String
    {height::Int,width::Int,maxT::Int}
    (Effect Gol3DCallback)

setupGol3D
    :: String
    -> {height :: Int, width :: Int, maxT :: Int}
    -> Effect (Set Point2 -> Effect Unit)
setupGol3D sel size = (_ <<< preRun) <$> runFn2 _setupGol3D sel size
  where
    preRun = A.fromFoldable <<< List.take (size.maxT +1) <<< map (map drawer) <<< runner 1
    drawer = map (\(Tuple {x, y} pts) ->
                      { x: x `mod` size.width
                      , y: y `mod` size.height
                      , zs: A.fromFoldable pts
                      }
               )
       <<< Map.toUnfoldableUnordered


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
    -> Int
    -> Set Point2
    -> Effect Unit
drawGol4D sel size n pts = runFn3 _drawGol4D sel size $
      A.fromFoldable <<< List.take n $
        map (map drawer) (runner 2 pts)
  where
    drawer = map (\(Tuple {x, y} pts) ->
                      { x: x `mod` size.width
                      , y: y `mod` size.height
                      , zws: A.fromFoldable pts
                      }
               )
       <<< Map.toUnfoldableUnordered

foreign import _drawGolSyms4D :: Fn2 String Boolean (Effect Unit)
drawGolSyms4D :: String -> Boolean -> Effect Unit
drawGolSyms4D = runFn2 _drawGolSyms4D

foreign import _drawGolSyms5D
    :: Fn2 String (Int -> IntMap Int) (Effect Unit)
drawGolSyms5D
    :: String
    -> Effect Unit
drawGolSyms5D sel = runFn2 _drawGolSyms5D sel $
        Unsafe.unsafeCoerce
    <<< toIntMap
    <<< map (map Additive)
    <<< vecRunNeighbs (*) 1 3

foreign import data TreeNode :: Type
foreign import _drawTree :: forall f.
      Fn5 String
          Boolean
          (Int -> Int -> Array Int)
          (Int -> Int -> Hierarchy (VecTree f (Lazy Contrib)))
          (forall a. VecTree f a -> a)
          (Effect Unit)
drawTree
    :: forall f. String -> Boolean -> Effect Unit
drawTree sel fwd = runFn5 _drawTree sel fwd vecRun mkHier (\(Node x _) -> x)
  where
    vecRun d n = A.fromFoldable (List.reverse gens)
      where
        mx   = maxBinom d n + 1
        gens = genVecRunIxPascal d mx n
    mkHier d = vecTreeHierarchy <<< vecRunNeighbsTree d

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
