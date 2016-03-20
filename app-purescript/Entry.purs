module Entry where

import Control.Bind
import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Console
import Control.Monad.Eff.Console.Unsafe
import Control.Monad.Eff.Ref
import Control.Monad.Maybe.Trans
import Control.Monad.State.Trans
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
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Nullable
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



-- | Helpers

fromNodeList
    :: forall e.
       D.NodeList
    -> Eff (dom :: D.DOM | e) (List D.Element)
fromNodeList nl = do
    l <- DNL.length nl
    ns <- traverse (_ `DNL.item` nl) (0 .. (l - 1))
    return $ mapMaybe (nodeToElement <=< toMaybe) ns

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
