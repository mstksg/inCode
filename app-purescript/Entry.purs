module Entry where

import           Control.Monad
import           Control.Monad.Eff
import           Control.Monad.Eff.Console
import           Control.Monad.Eff.Console.Unsafe
import           Control.Monad.Eff.Ref
import           Data.Foldable
import           Data.List
import           Data.Nullable
import           Data.Traversable
import           Prelude
import qualified DOM                              as D
import qualified DOM.Event.EventTarget            as D
import qualified DOM.Event.EventTypes             as D
import qualified DOM.Event.Types                  as D
import qualified DOM.HTML                         as D
import qualified DOM.HTML.Types                   as D
import qualified DOM.HTML.Window                  as D
import qualified DOM.Node.Document                as D
import qualified DOM.Node.Element                 as D
import qualified DOM.Node.Node                    as D
import qualified DOM.Node.NodeList                as D
import qualified DOM.Node.ParentNode              as D
import qualified DOM.Node.Types                   as D

main
    :: forall e.
       Eff (console :: CONSOLE, dom :: D.DOM, ref :: REF | e) Unit
main = do
    wind <- D.window
    doc  <- D.htmlDocumentToDocument <$> D.document wind

    ready doc $ do
      log "Hello from PureScript!"
      appendTopLinks doc
      log "Goodbye!"
  where
    ready doc a = do
      a' <- doOnce a
      onE (D.documentToEventTarget doc)
           D.readystatechange
          (D.eventListener $ \_ -> a')


appendTopLinks
    :: forall e.
       D.Document
    -> Eff (dom :: D.DOM | e) Unit
appendTopLinks doc = do
    hs <- D.querySelectorAll headers (D.documentToParentNode doc)
    flip traverseNodeList_ hs $ \h -> do
      topLink <- D.createElement "a" doc
      let topLinkNode = D.elementToNode topLink
      D.setAttribute "href" "#title" topLink
      D.setClassName "top-link" topLink
      D.setTextContent "top" topLinkNode
      traverse_ (D.appendChild topLinkNode) (toMaybe h)
  where
    headers = intercalate ", " [ ".main-content h2"
                               , ".main-content h3"
                               , ".main-content h4"
                               , ".main-content h5"
                               ]

-- | Helpers

fromNodeList
    :: forall e.
       D.NodeList
    -> Eff (dom :: D.DOM | e) (List (Nullable D.Node))
fromNodeList nl = do
    l <- D.length nl
    traverse (`D.item` nl) (0 .. (l - 1))

traverseNodeList_
    :: forall e.
       (Nullable D.Node -> Eff (dom :: D.DOM | e) Unit)
    -> D.NodeList
    -> Eff (dom :: D.DOM | e) Unit
traverseNodeList_ f nl = do
    ns <- fromNodeList nl
    traverse_ f ns


doOnce
    :: forall e.
       Eff (ref :: REF | e) Unit
    -> Eff (ref :: REF | e) (Eff (ref :: REF | e) Unit)
doOnce a = do
    doneRef <- newRef false
    return $ do
      done <- readRef doneRef
      unless done $ do
        a
        writeRef doneRef true

onE :: forall e.
       D.EventTarget
    -> D.EventType
    -> D.EventListener (dom :: D.DOM | e)
    -> Eff (dom :: D.DOM | e) Unit
onE targ etype h = D.addEventListener etype h true targ
