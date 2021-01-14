module Entry where

import Control.Monad.Maybe.Trans         (MaybeT(..), runMaybeT)
import Control.Monad.State.Trans         (StateT(..), evalStateT)
import Control.Monad.Writer.Trans        (WriterT, execWriterT, tell)
import Control.Plus                      (empty, (<|>))
import Data.Array                        as A
import Data.Char.Unicode                 (isSpace)
import Data.Either                       (Either(..))
import Data.Maybe                        (Maybe(..), maybe)
import Data.String                       (Pattern(..))
import Data.String.CodeUnits             (stripPrefix, toCharArray)
import Data.Traversable                  (all, for_, intercalate, traverse_)
import Data.Tuple                        (Tuple(..))
import Effect                            (Effect)
import Effect.Class                      (class MonadEffect, liftEffect)
import Effect.Class.Console              (log)
import Effect.Ref                        as Ref
import Prelude
import Web.DOM.DOMTokenList              as DOMTokenList
import Web.DOM.Document                  as Document
import Web.DOM.Element                   as Element
import Web.DOM.Node                      as Node
import Web.DOM.NodeList                  as NodeList
import Web.DOM.ParentNode                as ParentNode
import Web.Event.Event                   (Event, EventType)
import Web.Event.EventTarget             (EventTarget, addEventListener, eventListener)
import Web.HTML                          as Web
import Web.HTML.Event.EventTypes         (readystatechange)
import Web.HTML.HTMLDocument             as HTMLDocument
import Web.HTML.Window                   as Window
import Web.HTML.Window                   as Window
import Web.UIEvent.MouseEvent.EventTypes as EventTypes

main :: Effect Unit
main = do
    doc  <- map HTMLDocument.toDocument <<< Window.document =<< Web.window

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
      onE readystatechange
          (Document.toEventTarget doc)
          (\_ -> a')


appendTopLinks :: Document.Document -> Effect Unit
appendTopLinks doc = do
    hs <- ParentNode.querySelectorAll
            (ParentNode.QuerySelector headers)
            (Document.toParentNode doc)
    flip traverseNodeList_ hs \h -> do
      topLink <- Document.createElement "a" doc
      let topLinkNode = Element.toNode topLink
      Element.setAttribute "href" "#title" topLink
      Element.setClassName "top-link" topLink
      Node.setTextContent "top" topLinkNode
      void $ Node.appendChild topLinkNode (Element.toNode h)
  where
    headers = intercalate ", " [ ".main-content h2"
                               , ".main-content h3"
                               , ".main-content h4"
                               , ".main-content h5"
                               ]


data SourceMode = SMHover
                | SMOn

setupSourceLink :: Document.Document -> Effect Unit
setupSourceLink doc = void <<< runMaybeT $ do
    sourceInfo <- MaybeT $ ParentNode.querySelector
      (ParentNode.QuerySelector ".source-info") docPN
    header     <- MaybeT $ ParentNode.querySelector
      (ParentNode.QuerySelector ".article > header") docPN
    let headerET = Element.toEventTarget header
    liftEffect do
      cList    <- Element.classList sourceInfo
      DOMTokenList.add cList "hide"
      modeRef  <- Ref.new SMHover

      onE EventTypes.mouseenter headerET \_ -> do
        mode <- Ref.read modeRef
        case mode of
          SMHover -> DOMTokenList.remove cList "hide"
          SMOn    -> pure unit

      onE EventTypes.mouseleave headerET \_ -> do
        mode <- Ref.read modeRef
        case mode of
          SMHover -> DOMTokenList.add cList "hide"
          SMOn    -> pure unit

      onE EventTypes.click headerET \_ -> do
        mode <- Ref.read modeRef
        case mode of
          SMHover -> do
            DOMTokenList.remove cList "hide"
            Ref.write SMOn modeRef
          SMOn    -> do
            DOMTokenList.add cList "hide"
            Ref.write SMHover modeRef
  where
    docPN = Document.toParentNode doc


setupAsides :: Document.Document -> Effect Unit
setupAsides doc = do
    asides <- ParentNode.querySelectorAll
      (ParentNode.QuerySelector ".main-content .note")
      (Document.toParentNode doc)
    flip traverseNodeList_ asides \aside -> do
      blks <- Node.childNodes (Element.toNode aside)
      let toggleAll =
            flip (withIndex traverseNodeList_) blks \i blk ->
              when (i > 0) $ do
                cList <- Element.classList blk
                void $ DOMTokenList.toggle cList "hide"
      flip (withIndex traverseNodeList_) blks \i blk -> do
        when (i == 0) do
          cList <- Element.classList blk
          onE EventTypes.click (Element.toEventTarget blk) \_ ->
            toggleAll
          DOMTokenList.add cList "clickable"
          DOMTokenList.add cList "aside-header"
          clickMe <- Document.createElement "span" doc
          let clickMeNode = Element.toNode clickMe
          cmList <- Element.classList clickMe
          DOMTokenList.add cmList "clickme"
          Node.setTextContent "(Click me!)" clickMeNode
          void $ Node.appendChild clickMeNode (Element.toNode blk)
      toggleAll

data LinkSpec = LS { source      :: Maybe String
                   , interactive :: Maybe String
                   }

instance semigroupLS :: Semigroup LinkSpec where
  append (LS s1) (LS s2)
    = LS { source:      s1.source      <|> s2.source
         , interactive: s1.interactive <|> s2.interactive
         }

instance monoidLS :: Monoid LinkSpec where
  mempty = LS { source: Nothing, interactive: Nothing }

-- assumption: only one <code> per <pre>
processCodeBlocks :: Document.Document -> Effect Unit
processCodeBlocks doc = do
    blks <- ParentNode.querySelectorAll
      (ParentNode.QuerySelector ".main-content code.sourceCode")
      (Document.toParentNode doc)
    flip traverseNodeList_ blks \blk -> do
      let blkN = Element.toNode blk
      lSpec <- execWriterT
           <<< ((_ <=< childNodes') <<< traverseNodeList_) (pullLinkSpec blkN)
             $ blkN
      pure unit
      chompWhitespace blkN
      genLinkBox lSpec blkN
      colorPrompt blkN
  where
    childNodes' = liftEffect <<< Node.childNodes
    pullLinkSpec
        :: Node.Node
        -> Element.Element
        -> WriterT LinkSpec Effect Unit
    pullLinkSpec blk line = do
      let lineN = Element.toNode line
      linec <- liftEffect $ Node.textContent lineN
      for_ prefHandlers \(Tuple pref handler) -> do
        for_ (Pattern pref `stripPrefix` linec) $ \stuff -> do
          void $ liftEffect do
            Node.setNodeValue "" lineN
            Node.removeChild lineN blk
          case handler stuff of
            Left s  -> tell $ LS { source: Just s , interactive: Nothing }
            Right s -> tell $ LS { source: Nothing, interactive: Just s  }
    prefHandlers = [ Tuple "-- source: "      Left
                   , Tuple "-- interactive: " Right
                   ]
    chompWhitespace :: Node.Node -> Effect Unit
    chompWhitespace blk = go
      where
        go = do
          fc' <- Node.firstChild blk
          for_ fc' \fc -> do
            isWhitespace <- all isSpace <<< toCharArray <$> Node.textContent fc
            when isWhitespace do
              void $ Node.removeChild fc blk
              go
    genLinkBox :: LinkSpec -> Node.Node -> Effect Unit
    genLinkBox (LS s) blk = void <<< runMaybeT $ do
      _   <- maybe empty pure $ s.source <|> s.interactive
      pre <- MaybeT $ Node.parentNode blk
      liftEffect $ do
        linkBox <- Document.createElement "div" doc
        let linkBoxN = Element.toNode linkBox
        Element.setClassName "code-link-box" linkBox
        void $ Node.insertBefore linkBoxN blk pre
        let boxes = A.mapMaybe (\(Tuple u i) -> (_ `Tuple` i) <$> u)
                  $ [ Tuple s.source
                            (Tuple "code-source-link"      "View full source")
                    , Tuple s.interactive
                            (Tuple "code-interactive-link" "Interactive"     )
                    ]
        for_ boxes \(Tuple u (Tuple cls txt)) -> do
          link <- Document.createElement "a" doc
          let linkN = Element.toNode link
          Element.setAttribute "href" u link
          Element.setClassName cls link
          Node.setTextContent txt linkN
          void $ Node.appendChild linkN linkBoxN

        cList <- Element.classList linkBox
        DOMTokenList.add cList "hide"

        let preET' = Element.toEventTarget <$> Element.fromNode pre
        for_ preET' \preET -> do
          onE EventTypes.mouseenter preET \_ ->
            DOMTokenList.remove cList "hide"
          onE EventTypes.mouseleave preET \_ ->
            DOMTokenList.add cList "hide"
    colorPrompt :: Node.Node -> Effect Unit
    colorPrompt blk = void <<< runMaybeT $ do
      fc   <- MaybeT $ Node.firstChild blk
      pr   <- MaybeT $ Node.parentElement blk
      liftEffect $ do
        tc <- Node.textContent fc
        cList <- Element.classList pr
        let isPrompt = A.mapMaybe (\p -> Pattern p `stripPrefix` tc)
                        ["λ", "ghci", "$", "irb", ">>>"]
        when (A.length isPrompt > 0) $
          DOMTokenList.add cList "code-block-prompt"



-- -- | Helpers

traverseNodeList_
    :: forall m. MonadEffect m
    => (Element.Element -> m Unit)
    -> NodeList.NodeList
    -> m Unit
traverseNodeList_ f nl = do
    ns <- liftEffect $ NodeList.toArray nl
    traverse_ f (A.mapMaybe Element.fromNode ns)


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

onE :: EventType
    -> EventTarget
    -> (Event -> Effect Unit)
    -> Effect Unit
onE etype targ h = do
  listener <- eventListener h
  addEventListener etype listener false targ

withIndex
    :: forall s t a b f. Applicative f
    => ((a -> StateT Int f b) -> (s -> StateT Int f t))
    -> (Int -> a -> f b) -> (s -> f t)
withIndex t f = flip evalStateT 0 <<< t f'
  where
    f' :: a -> StateT Int f b
    f' y = StateT \i -> (\z -> Tuple z (i+1)) <$> f i y

