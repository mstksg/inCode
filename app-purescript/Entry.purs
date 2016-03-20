module Entry where

import Control.Alt
import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Control.Monad.Eff.Ref
import Control.Monad.Maybe.Trans
import Control.Monad.State.Trans
import Control.Monad.Writer.Trans
import Control.Plus
import DOM                              as D
import DOM.Event.EventTarget            as D
import DOM.Event.EventTypes             as D
import DOM.Event.Types                  as D
import DOM.HTML                         as D
import DOM.HTML.Types                   as D
import DOM.HTML.Window                  as D
import DOM.Node.DOMTokenList            as DDTL
import DOM.Node.Document                as D
import DOM.Node.Element                 as D
import DOM.Node.Node                    as D
import DOM.Node.NodeList                as DNL
import DOM.Node.NodeType                as D
import DOM.Node.ParentNode              as D
import DOM.Node.Types                   as D
import Data.Array                       as A
import Data.Bitraversable
import Data.Char.Unicode
import Data.Either
import Data.Foldable
import Data.Generic
import Data.Lens.Getter
import Data.List                        as L
import Data.Maybe
import Data.Monoid
import Data.Nullable
import Data.String
import Data.Traversable
import Data.Tuple
import Prelude
import Unsafe.Coerce                    as U

main
    :: forall e.
       Eff (console :: CONSOLE, dom :: D.DOM, ref :: REF | e) Unit
main = do
    wind <- D.window
    doc  <- D.htmlDocumentToDocument <$> D.document wind

    ready doc do
      log "Hello from PureScript!"
      appendTopLinks doc
      setupSourceLink doc
      setupAsides doc
      processCodeBlocks doc
      log "Goodbye!"

  where
    ready doc a = do
      a' <- doOnce a
      onE D.readystatechange
          (D.documentToEventTarget doc)
          (D.eventListener \_ -> a')


appendTopLinks
    :: forall e.
       D.Document
    -> Eff (dom :: D.DOM | e) Unit
appendTopLinks doc = do
    hs <- D.querySelectorAll headers $ D.documentToParentNode doc
    flip traverseNodeList_ hs \h -> do
      topLink <- D.createElement "a" doc
      let topLinkNode = D.elementToNode topLink
      D.setAttribute "href" "#title" topLink
      D.setClassName "top-link" topLink
      D.setTextContent "top" topLinkNode
      D.appendChild topLinkNode (D.elementToNode h)
      return unit
  where
    headers = intercalate ", " [ ".main-content h2"
                               , ".main-content h3"
                               , ".main-content h4"
                               , ".main-content h5"
                               ]


data SourceMode = SMHover
                | SMOn

setupSourceLink
    :: forall e.
       D.Document
    -> Eff (dom :: D.DOM, ref :: REF | e) Unit
setupSourceLink doc = void <<< runMaybeT $ do
    sourceInfo <- nMaybeT $ D.querySelector ".source-info" docPN
    header     <- nMaybeT $ D.querySelector ".article > header" docPN
    let headerET = D.elementToEventTarget header
    liftEff do
      cList    <- D.classList sourceInfo
      DDTL.add ["hide"] cList
      modeRef  <- newRef SMHover

      onE D.mouseenter headerET <<< D.eventListener $ \_ -> do
        mode <- readRef modeRef
        case mode of
          SMHover -> DDTL.remove ["hide"] cList
          SMOn    -> return unit

      onE D.mouseleave headerET <<< D.eventListener $ \_ -> do
        mode <- readRef modeRef
        case mode of
          SMHover -> DDTL.add ["hide"] cList
          SMOn    -> return unit

      onE D.click headerET <<< D.eventListener $ \_ -> do
        mode <- readRef modeRef
        case mode of
          SMHover -> do
            DDTL.remove ["hide"] cList
            writeRef modeRef SMOn
          SMOn    -> do
            DDTL.add ["hide"] cList
            writeRef modeRef SMHover
  where
    docPN = D.documentToParentNode doc


setupAsides
    :: forall e.
       D.Document
    -> Eff (dom :: D.DOM | e) Unit
setupAsides doc = do
    asides <- D.querySelectorAll ".main-content .note" $ D.documentToParentNode doc
    flip traverseNodeList_ asides \aside -> do
      blks <- D.childNodes (D.elementToNode aside)
      let toggleAll =
            flip (withIndex traverseNodeList_) blks $ \i blk ->
              when (i > 0) $
                void $ DDTL.toggle "hide" =<< D.classList blk
      flip (withIndex traverseNodeList_) blks \i blk -> do
        when (i == 0) do
          onE D.click (D.elementToEventTarget blk) <<< D.eventListener $ \_ ->
            toggleAll
          DDTL.add ["clickable", "aside-header"] =<< D.classList blk
          clickMe <- D.createElement "span" doc
          let clickMeNode = D.elementToNode clickMe
          DDTL.add ["clickme"] =<< D.classList clickMe
          D.setTextContent "(Click me!)" clickMeNode
          D.appendChild clickMeNode (D.elementToNode blk)
          return unit

      toggleAll

data LinkSpec = LS { source      :: Maybe String
                   , interactive :: Maybe String
                   }

derive instance genericLS :: Generic LinkSpec

instance showLS :: Show LinkSpec where
  show = gShow

instance semigroupLS :: Semigroup LinkSpec where
  append (LS s1) (LS s2)
    = LS { source:      s1.source      <|> s2.source
         , interactive: s1.interactive <|> s2.interactive
         }

instance monoidLS :: Monoid LinkSpec where
  mempty = LS { source: Nothing, interactive: Nothing }

-- assumption: only one <code> per <pre>
processCodeBlocks
    :: forall e.
       D.Document
    -> Eff (dom :: D.DOM, console :: CONSOLE | e) Unit
processCodeBlocks doc = do
    blks <- D.querySelectorAll ".main-content code.sourceCode"
              $ D.documentToParentNode doc
    flip traverseNodeList_ blks \blk -> do
      let blkN = D.elementToNode blk
      lines <- D.childNodes blkN
      lSpec <- execWriterT
           <<< ((<=< childNodes') <<< traverseNodeList_) (pullLinkSpec blkN)
             $ blkN
      chompWhitespace blkN
      genLinkBox lSpec blkN
      colorPrompt blkN
  where
    childNodes' = liftEff <<< D.childNodes
    pullLinkSpec
        :: D.Node
        -> D.Element
        -> WriterT LinkSpec (Eff (dom :: D.DOM, console :: CONSOLE | e)) Unit
    pullLinkSpec blk line = do
      let lineN = D.elementToNode line
      linec <- liftEff $ D.textContent lineN
      for_ prefHandlers $ \(Tuple pref handler) -> do
        for_ (pref `stripPrefix` linec) $ \stuff -> do
          liftEff $ do
            D.setNodeValue "" lineN
            D.removeChild lineN blk
          case handler stuff of
            Left s  -> tell $ LS { source: Just s , interactive: Nothing }
            Right s -> tell $ LS { source: Nothing, interactive: Just s  }
    prefHandlers = [ Tuple "-- source: "      Left
                   , Tuple "-- interactive: " Right
                   ]
    chompWhitespace :: forall e'. D.Node -> Eff (dom :: D.DOM | e') Unit
    chompWhitespace blk = go
      where
        go = do
          fc' <- toMaybe <$> D.firstChild blk
          for_ fc' \fc -> do
            isWhitespace <- all isSpace <<< toCharArray <$> D.textContent fc
            when isWhitespace do
              D.removeChild fc blk
              go
    genLinkBox :: forall e'. LinkSpec -> D.Node -> Eff (dom :: D.DOM | e') Unit
    genLinkBox (LS s) blk = void <<< runMaybeT $ do
      _   <- maybe empty return $ s.source <|> s.interactive
      pre <- nMaybeT $ D.parentNode blk
      liftEff $ do
        linkBox <- D.createElement "div" doc
        let linkBoxN = D.elementToNode linkBox
        D.setClassName "code-link-box" linkBox
        D.insertBefore linkBoxN blk pre
        let boxes = A.mapMaybe (\(Tuple u i) -> (_ `Tuple` i) <$> u)
                  $ [ Tuple s.source
                            (Tuple "code-source-link"      "View full source")
                    , Tuple s.interactive
                            (Tuple "code-interactive-link" "Interactive"     )
                    ]
        for_ boxes \(Tuple u (Tuple cls txt)) -> do
          link <- D.createElement "a" doc
          let linkN = D.elementToNode link
          D.setAttribute "href" u link
          D.setClassName cls link
          D.setTextContent txt linkN
          D.appendChild linkN linkBoxN
          return unit

        cList <- D.classList linkBox
        DDTL.add ["hide"] cList

        let preET' = D.elementToEventTarget <$> nodeToElement pre
        for_ preET' \preET -> do
          onE D.mouseenter preET <<< D.eventListener $ \_ ->
            DDTL.remove ["hide"] cList
          onE D.mouseleave preET <<< D.eventListener $ \_ ->
            DDTL.add ["hide"] cList
    colorPrompt :: D.Node -> Eff (dom :: D.DOM, console :: CONSOLE | e) Unit
    colorPrompt blk = void <<< runMaybeT $ do
      fc   <- nMaybeT $ D.firstChild blk
      pr   <- nMaybeT $ D.parentElement blk
      liftEff $ do
        tc <- D.textContent fc
        let isPrompt = A.mapMaybe (_ `stripPrefix` tc) ["λ", "ghci"]
        case L.toList isPrompt of
          L.Cons _ _ -> DDTL.add ["code-block-prompt"] =<< D.classList pr
          L.Nil      -> return unit



-- | Helpers

fromNodeList
    :: forall e.
       D.NodeList
    -> Eff (dom :: D.DOM | e) (L.List D.Element)
fromNodeList nl = do
    l <- DNL.length nl
    ns <- traverse (_ `DNL.item` nl) (0 L... (l - 1))
    return $ L.mapMaybe (nodeToElement <=< toMaybe) ns

traverseNodeList_
    :: forall e m. MonadEff (dom :: D.DOM | e) m
    => (D.Element -> m Unit)
    -> D.NodeList
    -> m Unit
traverseNodeList_ f nl = do
    ns <- liftEff $ fromNodeList nl
    traverse_ f ns


doOnce
    :: forall e.
       Eff (ref :: REF | e) Unit
    -> Eff (ref :: REF | e) (Eff (ref :: REF | e) Unit)
doOnce a = do
    doneRef <- newRef false
    return do
      done <- readRef doneRef
      unless done do
        a
        writeRef doneRef true

onE :: forall e.
       D.EventType
    -> D.EventTarget
    -> D.EventListener (dom :: D.DOM | e)
    -> Eff (dom :: D.DOM | e) Unit
onE etype targ h = D.addEventListener etype h true targ

nMaybeT
    :: forall f a. Functor f
    => f (Nullable a)
    -> MaybeT f a
nMaybeT = MaybeT <<< map toMaybe

withIndex
    :: forall s t a b f. Applicative f
    => ((a -> StateT Int f b) -> (s -> StateT Int f t))
    -> (Int -> a -> f b) -> (s -> f t)
withIndex t f = flip evalStateT 0 <<< t f'
  where
    f' :: a -> StateT Int f b
    f' y = StateT $ \i -> (_ `Tuple` i+1) <$> f i y

nodeToElement :: D.Node -> Maybe D.Element
nodeToElement n =
    case D.nodeType n of
      D.ElementNode -> Just (U.unsafeCoerce n)
      _             -> Nothing

