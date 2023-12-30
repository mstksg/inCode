module Main where

import Control.Monad.Maybe.Trans  (MaybeT(..), runMaybeT)
import Control.Alternative        as Alternative
import Data.Array                 as A
import Data.Bifunctor
import Data.Foldable
import Data.Function.Uncurried
import Data.Int                   as Int
import Data.Lazy
import Data.List.Lazy             (List)
import Data.List.Lazy             as List
import Data.Map                   (Map)
import Data.Map                   as Map
import Data.Maybe
import Data.Monoid.Additive
import Data.Monoid.Endo
import Data.Nullable              (Nullable)
import Data.Nullable              as Nullable
import Data.Set                   (Set)
import Data.Set                   as Set
import Data.Set.NonEmpty          (NonEmptySet)
import Data.Set.NonEmpty          as NESet
import Data.String                as String
import Data.String.CodeUnits      as StringCU
import Data.Traversable
import Data.Tuple
import Effect                     (Effect, forE)
import Effect.Class               (class MonadEffect, liftEffect)
import Effect.Exception.Unsafe
import Effect.Ref                 as Ref
import Foreign                    as Foreign
import Matryoshka                 as Rec
import Prelude
import Unsafe.Coerce              as Unsafe
import Web.DOM.Document           as Document
import Web.DOM.Element            as Element
import Web.DOM.Node               as Node
import Web.DOM.NodeList           as NodeList
import Web.DOM.ParentNode         as ParentNode
import Web.Event.Event            (Event, EventType)
import Web.Event.EventTarget      (EventTarget, addEventListener, eventListener)
import Web.HTML                   as Web
import Web.HTML.Event.EventTypes  (readystatechange, click)
import Web.HTML.HTMLDocument      as HTMLDocument
import Web.HTML.History           as History
import Web.HTML.Location          as Location
import Web.HTML.Window            as Window
import Web.URL.URLSearchParams    as USP
import Web.URL                    as URL

main :: Effect Unit
main = do
    doc  <- map HTMLDocument.toDocument <<< Window.document =<< Web.window
    ready doc do
      logMe 13
      startingPts <- fromMaybe initialSet <$> loadUri doc

      eMap <- buildElemMap doc
      makeElemLinks doc eMap

      draw2D   <- setupGolFlat "#gol2DCont"   {height:20, width:20, maxT: 6, maxDim: Nothing}
      draw3D   <- setupGol3D   "#gol3DCont"   {height:20, width:20, maxT: 6}
      draw4D   <- setupGol4D   "#gol4DCont"   {height:20, width:20, maxT: 6}
      drawFlat <- setupGolFlat "#golFlatCont" {height:20, width:20, maxT: 6, maxDim: Just 8}

      drawGolSyms3D "#golSyms3DForwardCont" 6 false
      drawGolSyms3D "#golSyms3DReverseCont" 6 true
      drawGolSyms4D "#golSyms4DForwardCont" 6 false
      drawGolSyms4D "#golSyms4DReverseCont" 6 true

      drawTree "#golTreeCont"

      drawGolSyms5D "#golSyms5DCont"

      drawer <- setupDrawer "#golDrawerCont" {height:8, width:8} $ \pts -> do
        let bumped = Set.fromFoldable (map (bump 6) pts)
        draw2D bumped
        draw3D bumped
        draw4D bumped
        drawFlat bumped

      drawer $ A.fromFoldable startingPts

      linkify doc (drawer <<< A.fromFoldable)

  where
    ready doc a = do
      a' <- doOnce a
      onE readystatechange
          (Document.toEventTarget doc)
          (\_ -> a')
    bump n {x,y} = {x:x+n, y:y+n}
    initialSet = Set.fromFoldable (map (bump 2) initialPoints)

elements :: Array String
elements = map fst elementGroups
elementGroupMap :: Map String Int
elementGroupMap = Map.fromFoldable elementGroups
elementGroups :: Array (Tuple String Int)
elementGroups =
    [ Tuple "#golDrawer" 0
    , Tuple "#gol2D" 0
    , Tuple "#gol3D" 0
    , Tuple "#golSyms3DForward" 1
    , Tuple "#golSyms3DReverse" 1
    , Tuple "#gol4D" 0
    , Tuple "#golSyms4DForward" 1
    , Tuple "#golSyms4DReverse" 1
    , Tuple "#golSyms5D" 1
    , Tuple "#golTree" 1
    , Tuple "#golFlat" 0
    ]

buildElemMap :: Document.Document -> Effect (Array (Tuple String String))
buildElemMap doc = do
    loc <- liftEffect $ Window.location =<< Web.window
    baseLoc <- append
        <$> Location.origin loc
        <*> Location.pathname loc
    map A.catMaybes <<< for elements $ \e -> runMaybeT do
      telem <- MaybeT $ ParentNode.querySelector
              (ParentNode.QuerySelector $ e <> " p")
              (Document.toParentNode doc)
      ttext <- liftEffect $
            String.drop 1 <<< String.dropWhile (_ /= String.codePointFromChar ':')
        <$> Node.textContent (Element.toNode telem)
      bnode <- map Element.toNode $ MaybeT $ ParentNode.querySelector
              (ParentNode.QuerySelector $ e <> " p strong")
              (Element.toParentNode telem)
      liftEffect $ void do
        anchor <- Document.createElement "a" doc
        Element.setAttribute "href" (baseLoc <> e) anchor
        Node.setTextContent "#" (Element.toNode anchor)
        Node.insertBefore (Element.toNode anchor) bnode (Element.toNode telem)
      pure $ Tuple e ttext

makeElemLinks :: Document.Document -> Array (Tuple String String) -> Effect Unit
makeElemLinks doc elemMap = for_ elemMap $ \(Tuple e _) -> do
    let groupId = Map.lookup e elementGroupMap
        linkString = append "Jump to: "
                 <<< intercalate " / "
                 <<< flip A.mapMaybe linkStrings $ \(Tuple e' (Tuple a b)) -> do
                       Alternative.guard $ Map.lookup e' elementGroupMap == groupId
                       pure $ if e' == e
                          then a
                          else b
    container <- ParentNode.querySelector
              (ParentNode.QuerySelector e)
              (Document.toParentNode doc)
    for_ container $ \c -> do
      listp <- Document.createElement "p" doc
      setInnerHTML listp linkString
      Node.appendChild (Element.toNode listp) (Element.toNode c)
  where
    linkStrings :: Array (Tuple String (Tuple String String))
    linkStrings = flip map elemMap \(Tuple e ename) ->
        Tuple e (Tuple ename ("<a href=\"" <> e <> "\">" <> ename <> "</a>"))

loadUri :: Document.Document -> Effect (Maybe (Set Point2))
loadUri doc = runMaybeT do
    loc <- liftEffect $ Window.location =<< Web.window
    usp0 <- liftEffect $ USP.fromString <$> Location.search loc
    str <- maybe Alternative.empty pure $ USP.get "points" usp0
    let res = blocksToPoint str
        usp1 = USP.toString $ USP.delete "points" usp0
    Alternative.guard (not (null res))
    origTitle <- MaybeT $
        traverse HTMLDocument.title (HTMLDocument.fromDocument doc)
    liftEffect $ do
      basicUrl <- fold <$> sequence
        [ Location.origin loc
        , Location.pathname loc
        , Location.hash loc
        , pure $
            if String.null usp1
              then usp1
              else "?" <> usp1
        ]
      hist <- Window.history =<< Web.window
      History.pushState
        (Foreign.unsafeToForeign basicUrl)
        (History.DocumentTitle origTitle)
        (History.URL basicUrl)
        hist
    pure res

linkify :: Document.Document -> (Set Point2 -> Effect Unit) -> Effect Unit
linkify doc cb = do
    linkElems <- ParentNode.querySelectorAll
            (ParentNode.QuerySelector "a.loadpoints")
            (Document.toParentNode doc)
    flip traverseNodeList_ linkElems \linkElem -> void $ runMaybeT do
      liftEffect $ Element.setAttribute "title" "Click to load" linkElem
      href <- MaybeT $ Element.getAttribute "href" linkElem
      usp <- URL.searchParams <$> maybe Alternative.empty pure (URL.fromAbsolute href)
      str  <- maybe Alternative.empty pure $ USP.get "points" usp
      let res = blocksToPoint str
      Alternative.guard (not (null res))
      liftEffect $ onE click (Element.toEventTarget linkElem) \e -> do
        preventDefault e
        cb res
      pure unit

type Point = Array Int

initialPoints :: Array Point2
initialPoints = [
      { x: 0, y: 2 }
    , { x: 1, y: 0 }
    , { x: 1, y: 2 }
    , { x: 2, y: 1 }
    , { x: 2, y: 2 }
    ]

validLiveCount :: Additive Int -> Boolean
validLiveCount (Additive x) = x >= 5 && x <= 7

neighbs2d :: Int -> Int -> List Int
neighbs2d n i = do
    dx <- 0 List.: (-1) List.: 1 List.: List.nil
    dy <- 0 List.: (-1) List.: 1 List.: List.nil
    pure (i + dx + n*dy)

type Point2 = { x :: Int, y :: Int }

type DList a = Endo (->) (List a)
dsingleton :: forall a. a -> DList a
dsingleton x = Endo (x List.: _)
fromDList :: forall a. DList a -> List a
fromDList (Endo f) = f List.nil

stepper
    :: forall a. Ord a => Show a
    => (a -> List a)
    -> (Int -> IntMap (Additive Int))
    -> Map a (NonEmptySet Int)
    -> Map a (NonEmptySet Int)
stepper expand syms cs = Map.mapMaybe (NESet.fromFoldable <<< intMapKeys <<< filterIntMap validLiveCount <<< unionsIntMap <<< fromDList) $
    foldl (\res (Tuple gIx ds) ->
          case Map.lookup ds prebaked of
            Nothing                 -> res
            Just (Tuple here there) -> flip (Map.unionWith append) res <<< Map.fromFoldable $
              List.zip (expand gIx) (dsingleton here List.: List.repeat (dsingleton there))
        )
      (Map.empty :: Map a (DList (IntMap (Additive Int))))
      (Map.toUnfoldableUnordered cs :: List (Tuple a (NonEmptySet Int)))
  where
    uniqueGroups :: Set (NonEmptySet Int)
    uniqueGroups = Set.fromFoldable cs
    prebaked :: Map (NonEmptySet Int) (Tuple (IntMap (Additive Int)) (IntMap (Additive Int)))
    prebaked = Map.fromFoldable (map (\gr -> Tuple gr (prebake gr)) (List.fromFoldable uniqueGroups))
    prebake :: NonEmptySet Int -> Tuple (IntMap (Additive Int)) (IntMap (Additive Int))
    prebake = bimap unionsIntMap unionsIntMap <<< foldr (\pIx (Tuple here there) ->
              let pNeighbs = syms pIx
                  here'  = singletonIntMap pIx (Additive 1)
                    List.: pNeighbs
                    List.: here
                  there' = singletonIntMap pIx (Additive 2)
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
        <<< map (map (\w -> Additive (w*2)))
        <<< vecRunNeighbs d

lowNeighbs :: Point2 -> List Point2
lowNeighbs {x, y} = do
  x' <- x List.: (x-1) List.: (x+1) List.: List.nil
  y' <- y List.: (y-1) List.: (y+1) List.: List.nil
  pure {x:x', y:y'}

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

-- | Streaming/constant space enumerate all neighbor and multiplicities
vecRunNeighbs
    :: Int         -- ^ dimension
    -> Int         -- ^ pascal index
    -> List (Tuple Int Int)
vecRunNeighbs n orig = case List.step gens of
    List.Nil       -> List.nil
    List.Cons x xs -> go mx n 0 x true 1 0 x xs
  where
    mx   = maxBinom n orig + 1
    gens = genVecRunIxPascal n mx orig
    go  :: Int          -- ^ pascal i
        -> Int          -- ^ pascal j
        -> Int          -- ^ running total
        -> Int          -- ^ origina item
        -> Boolean      -- ^ currently all the same?
        -> Int          -- ^ multiplicity
        -> Int          -- ^ item to the right
        -> Int          -- ^ current item
        -> List Int     -- ^ leftover items (right to left)
        -> List (Tuple Int Int)
    go i j tot x0 allSame p r x ls0 = case List.step ls0 of
      List.Nil ->
        let res = r + x
            p'  = p *
                    (factorial res * (2 `Int.pow` r)) `div` (factorial x * factorial r)
            tot' = tot
        in  Tuple tot' p' <$ Alternative.guard (not (allSame && x == x0))
      List.Cons l ls -> do
        xlContrib <- safeRange 0 (x+l)
        lContrib  <- safeRange (max 0 (xlContrib - x)) (min l xlContrib)
        let xContrib = xlContrib - lContrib
            res      = r + xlContrib
            l'       = l - lContrib
            x'       = x - xContrib
            p'       = p *
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
    List.Nil       -> unsafeThrow "vecRunNeighbsTree step gens"
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
    :: Fn4 String
           {height :: Int, width :: Int}
           (Array Point2 -> String)
           (Array Point2 -> Effect Unit)
           (Effect Drawer)
setupDrawer
    :: String
    -> {height :: Int, width :: Int}
    -> (Array Point2 -> Effect Unit)
    -> Effect Drawer
setupDrawer sel size = runFn4 _setupDrawer sel size $
    pointsToBlocks <<< Set.fromFoldable

type GolFlatCallback = Array (Array (Lazy (Array {x :: Int, y :: Int, pts :: Array Int}))) -> Effect Unit
foreign import _setupGolFlat :: Fn3
    String
    Boolean
    {height::Int,width::Int,maxT::Int,maxDim::Nullable Int}
    (Effect GolFlatCallback)

setupGolFlat
    :: String
    -> {height :: Int, width :: Int, maxT :: Int, maxDim :: Maybe Int}  -- if Nothing, hide the points too
    -> Effect (Set Point2 -> Effect Unit)
setupGolFlat sel size = (_ <<< preRun) <$> runFn3 _setupGolFlat sel (isJust size.maxDim) size'
  where
    size' = { height: size.height, width: size.width, maxT: size.maxT
            , maxDim: Nullable.toNullable size.maxDim
            }
    preRun pts = case size.maxDim of
      Nothing -> A.fromFoldable <<< map (A.singleton <<< map drawer) $ List.take (size.maxT+1) (runner 0 pts)
      Just md -> A.fromFoldable <<< map A.fromFoldable
        <<< List.transpose <<< flip map (List.range 0 md) $ \d ->
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

type Gol4DCallback = Array (Lazy (Array {x :: Int, y :: Int, zws :: Array Int})) -> Effect Unit
foreign import _setupGol4D :: Fn2
    String
    {height::Int,width::Int,maxT::Int}
    (Effect Gol4DCallback)

setupGol4D
    :: String
    -> {height :: Int, width :: Int, maxT :: Int}
    -> Effect (Set Point2 -> Effect Unit)
setupGol4D sel size = (_ <<< preRun) <$> runFn2 _setupGol4D sel size
  where
    preRun = A.fromFoldable <<< List.take (size.maxT +1) <<< map (map drawer) <<< runner 2
    drawer = map (\(Tuple {x, y} pts) ->
                      { x: x `mod` size.width
                      , y: y `mod` size.height
                      , zws: A.fromFoldable pts
                      }
               )
       <<< Map.toUnfoldableUnordered

type SymInfo = { dim :: Int
               , gridSize :: { width :: Int, height :: Int }
               , ptPos :: Array Int -> {x :: Int, y :: Int}
               }
foreign import _drawGolSyms
    :: Fn4 String
           Int
           SymInfo
           Boolean
           (Effect Unit)

drawGolSyms3D :: String -> Int -> Boolean -> Effect Unit
drawGolSyms3D sel maxZ = runFn4 _drawGolSyms sel maxZ
        { dim: 1
        , gridSize: {width: 3, height: 1}
        , ptPos: case _ of
            [x] -> {x, y: 0}
            _   -> unsafeThrow "drawGolSyms3D ptPos"
        }
drawGolSyms4D :: String -> Int -> Boolean -> Effect Unit
drawGolSyms4D sel maxZ = runFn4 _drawGolSyms sel maxZ
        { dim: 2
        , gridSize: {width: 3, height: 3}
        , ptPos: case _ of
            [x,y] -> {x, y}
            _     -> unsafeThrow "drawGolSyms4D ptPos"
        }

foreign import _drawGolSyms5D
    :: Fn2 String (Int -> IntMap Int) (Effect Unit)
drawGolSyms5D
    :: String
    -> Effect Unit
drawGolSyms5D sel = runFn2 _drawGolSyms5D sel $
        Unsafe.unsafeCoerce
    <<< toIntMap
    <<< map (map Additive)
    <<< vecRunNeighbs 3

foreign import data TreeNode :: Type
foreign import _drawTree :: forall f.
      Fn4 String
          (Int -> Int -> Array Int)
          (Int -> Int -> Hierarchy (VecTree f (Lazy Contrib)))
          (forall a. VecTree f a -> a)
          (Effect Unit)
drawTree
    :: forall f. String -> Effect Unit
drawTree sel = runFn4 _drawTree sel vecRun mkHier (\(Node x _) -> x)
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

foreign import _setInnerHTML :: Fn2 Element.Element String (Effect Unit)
setInnerHTML :: Element.Element -> String -> Effect Unit
setInnerHTML = runFn2 _setInnerHTML

foreign import preventDefault :: Event -> Effect Unit

blockTable :: Array Char
blockTable =
    [ '_', '▘', '▝', '▀', '▖', '▌', '▞', '▛'
    , '▗', '▚', '▐', '▜', '▄', '▙', '▟', '█'
    ]

newtype FourBit = FourBit { nw :: Boolean, ne :: Boolean, sw :: Boolean, se :: Boolean }

instance showFourBit :: Show FourBit where
    show (FourBit xs) = show xs

instance semigroupFourBit :: Semigroup FourBit where
    append (FourBit x) (FourBit y) = FourBit
        { nw: x.nw || y.nw, ne: x.ne || y.ne
        , sw: x.sw || y.sw, se: x.se || y.se
        }

instance monoidFourBit :: Monoid FourBit where
    mempty = FourBit { nw: false, ne: false, sw: false, se: false }

fourBitList :: FourBit -> List Boolean
fourBitList (FourBit {nw,ne,sw,se}) = nw List.: ne List.: sw List.: se List.: List.nil

fourBitToBlock :: FourBit -> Char
fourBitToBlock fb = fromMaybe ' ' $
    blockTable `A.index` sum (List.zipWith go (List.iterate (_+1) 0) (fourBitList fb))
  where
    go i b
      | b         = 2 `Int.pow` i
      | otherwise = 0

blockToFourBit :: Char -> Maybe FourBit
blockToFourBit c = flip map (A.elemIndex c blockTable) \n -> FourBit
    { nw: n `mod` 2 == 1
    , ne: (n `div` 2) `mod` 2 == 1
    , sw: (n `div` 4) `mod` 2 == 1
    , se: (n `div` 8) `mod` 2 == 1
    }

point2ToFourBit :: Point2 -> Tuple Point2 FourBit
point2ToFourBit {x,y} = Tuple {x:x', y:y'} $ FourBit
    { nw: lx == 0 && ly == 0
    , ne: lx == 1 && ly == 0
    , sw: lx == 0 && ly == 1
    , se: lx == 1 && ly == 1
    }
  where
    x' = x `div` 2
    y' = y `div` 2
    lx = x `mod` 2
    ly = y `mod` 2

fourBitToPoint2 :: FourBit -> List Point2
fourBitToPoint2 fb = List.mapMaybe (\(Tuple p b) -> if b then Just p else Nothing) $
    List.zip pts (fourBitList fb)
  where
    pts    = {x:0,y:0}
      List.: {x:1,y:0}
      List.: {x:0,y:1}
      List.: {x:1,y:1}
      List.: List.nil

pointsToBlocks :: Set Point2 -> String
pointsToBlocks pts = intercalate "." $ flip map (List.range 0 3) $ \y ->
    flip foldMap (List.range 0 3) $ \x ->
      maybe "_" (StringCU.singleton <<< fourBitToBlock) (Map.lookup {x,y} ptMap)
  where
    ptMap :: Map Point2 FourBit
    ptMap = Map.fromFoldableWith append $
                map point2ToFourBit (A.fromFoldable pts)

blocksToPoint :: String -> Set Point2
blocksToPoint str = Set.fromFoldable $ do
    Tuple y' xs <- List.zip (List.range 0 3) $
        List.fromFoldable (String.split (String.Pattern ".") str)
    Tuple x' b <- List.zip (List.range 0 3) $
        List.fromFoldable (StringCU.toCharArray xs)
    fb <- maybe List.nil List.singleton $ blockToFourBit b
    dp <- fourBitToPoint2 fb
    let x = 2*x' + dp.x
        y = 2*y' + dp.y
    pure {x, y}

traverseNodeList_
    :: forall m. MonadEffect m
    => (Element.Element -> m Unit)
    -> NodeList.NodeList
    -> m Unit
traverseNodeList_ f nl = do
    ns <- liftEffect $ NodeList.toArray nl
    traverse_ f (A.mapMaybe Element.fromNode ns)


