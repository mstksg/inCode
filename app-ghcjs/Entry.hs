{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Maybe
import           GHCJS.DOM
import           GHCJS.DOM.Document          as DD
import           GHCJS.DOM.Element           as DE
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLAnchorElement
import           GHCJS.DOM.HTMLDivElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList
import qualified GHCJS.DOM.DOMTokenList      as DTL

fromNodeList :: MonadIO m => NodeList -> m [Node]
fromNodeList nl = do
    l <- getLength nl
    catMaybes <$> mapM (nl `item`) [0 .. (l - 1)]

fromNodeList' :: MonadIO m => Word -> NodeList -> m [Node]
fromNodeList' t nl = do
    l  <- getLength nl
    ns <- catMaybes <$> mapM (nl `item`) [0 .. (l - 1)]
    filterM (fmap (== t) . getNodeType) ns

mapMNodeList_
    :: MonadIO m
    => (Node -> m ())
    -> NodeList
    -> m ()
mapMNodeList_ f nl = do
    ns <- fromNodeList nl
    mapM_ f ns

mapMNodeList_'
    :: MonadIO m
    => Word
    -> (Node -> m ())
    -> NodeList
    -> m ()
mapMNodeList_' t f nl = do
    ns <- fromNodeList' t nl
    mapM_ f ns


withDTL
    :: MonadIO m
    => (DTL.DOMTokenList -> m ())
    -> Element
    -> m ()
withDTL f e = do
    dtl <- getClassList e
    mapM_ f dtl

to' :: Monad m
    => (a -> m (Maybe b))
    -> (b -> m ())
    -> a
    -> m ()
to' f g x = do
    y <- f x
    traverse_ g y


main :: IO ()
main = do
    putStrLn "Hello from the world of ghcjs!"

    runWebGUI $ \webView -> do
      enableInspector webView
      Just doc <- webViewGetDomDocument webView

      appendTopLinks doc
      setupSourceLink doc
      processCodeBlocks doc
      setupAsides doc

    putStrLn "Goodbye!"

appendTopLinks :: Document -> IO ()
appendTopLinks doc = do
    hds <- doc `DD.querySelectorAll`
             intercalate "," [".main-content h2"
                             ,".main-content h3"
                             ,".main-content h4"
                             ,".main-content h5"
                             ]
    flip (mapM_ . mapMNodeList_) hds $ \hd -> do
      topLink' <- fmap castToHTMLAnchorElement
                    <$> doc `createElement` Just "a"
      forM_ topLink' $ \topLink -> do
        topLink `setHref` "#title"
        topLink `setClassName` "top-link"
        topLink `setInnerHTML` Just "top"
        void $ hd `appendChild` Just topLink
        -- TODO: animate.  would this require jquery?

data SourceMode = SMHover
                | SMOn
  deriving (Show, Enum, Eq, Ord)

data MouseEvt = MouseOn
              | MouseOut
  deriving (Show, Enum, Eq, Ord)

-- setup source link visiblity behavior at the entry header.  When the
--    header is clicked, it should toggle between mostly-off and always-on.
--    In mostly-off mode, a hover reveals the link.  This is accomplished
--    using an IORef/FayRef...but maybe there is a better way.  Juggling
--    binds/event handlers is kind of the same thing anyway.
setupSourceLink :: Document -> IO ()
setupSourceLink doc = void . runMaybeT $ do
    sourceInfo <- MaybeT $ doc `DD.querySelector` ".source-info"

    liftIO $ withDTL (`DTL.add`    ["hide"]) sourceInfo

    sourceToggled <- liftIO $ newIORef False
    header <- MaybeT $ doc `DD.querySelector` ".article > header"

    void . liftIO $ do
      _ <- (header `on` DE.mouseEnter) . liftIO $ do
        toggled <- readIORef sourceToggled
        unless toggled $
          withDTL (`DTL.remove` ["hide"]) sourceInfo

      _ <- (header `on` DE.mouseLeave) . liftIO $ do
        toggled <- readIORef sourceToggled
        unless toggled $
          withDTL (`DTL.add`    ["hide"]) sourceInfo

      (header `on` DE.click) . liftIO $ do
        toggled <- readIORef sourceToggled
        if toggled
          then do
            withDTL (`DTL.add`    ["hide"]) sourceInfo
            writeIORef sourceToggled False
          else do
            withDTL (`DTL.remove` ["hide"]) sourceInfo
            writeIORef sourceToggled True

data LinkSpec = LS { lsSource      :: Maybe String
                   , lsInteractive :: Maybe String
                   }
  deriving (Show, Eq)

instance Monoid LinkSpec where
    mempty = LS Nothing Nothing
    LS s1 i1 `mappend` LS s2 i2
      = LS (s1 <|> s2) (i1 <|> i2)


-- assumption: only one <code> per <pre>
processCodeBlocks :: Document -> IO ()
processCodeBlocks doc = do
    blks <- doc `DD.querySelectorAll` ".main-content code.sourceCode"
    flip (mapM_ . mapMNodeList_) blks $ \blk -> do
      lSpec <- execWriterT
             . (to' getChildNodes . mapMNodeList_' ELEMENT_NODE) (pullLinkSpec blk)
             $ blk
      chompWhitespace blk
      blk `genLinkBox` lSpec
      colorPrompt blk
  where
    pullLinkSpec :: Node -> Node -> WriterT LinkSpec IO ()
    pullLinkSpec blk line = do
      linec' <- getTextContent line
      forM_ linec' $ \linec -> do
        forM_ [("-- source: "     , Left )
              ,("-- interactive: ", Right)
              ] $ \(pref,handler) -> do
          forM_ (pref `stripPrefix` linec) $ \stuff -> do
            line `setNodeValue` ("" <$ Nothing)
            _ <- blk `removeChild` Just line
            case handler stuff of
              Left s  -> tell $ LS (Just s) Nothing
              Right s -> tell $ LS Nothing  (Just s)
    chompWhitespace :: Node -> IO ()
    chompWhitespace blk = go
      where
        go = do
          fc' <- getFirstChild blk
          forM_ fc' $ \fc -> do
            t <- getTextContent fc
            let isWhitespace = maybe True (all isSpace) (t :: Maybe String)
            when isWhitespace $ do
              _ <- blk `removeChild` Just fc
              go
    genLinkBox :: Node -> LinkSpec -> IO ()
    genLinkBox blk LS{..} = void . runMaybeT $ do
      _       <- maybe mzero return $ lsSource <|> lsInteractive
      linkBox <- castToHTMLDivElement
             <$> MaybeT (doc `createElement` Just "div")
      pre     <- MaybeT (getParentNode blk)

      liftIO $ do
        linkBox `setClassName` "code-link-box"
        _ <- pre `insertBefore` Just linkBox $ Just blk

        forM_ [(lsSource     , "code-source-link"     , "View full source")
              ,(lsInteractive, "code-interactive-link", "Interactive"     )
              ] $ \(url', cls, txt) -> runMaybeT $ do
          url  <- maybe mzero return $ url'
          link <- castToHTMLAnchorElement
              <$> MaybeT (doc `createElement` Just "a")
          liftIO $ do
            link `setHref` url
            link `setClassName` cls
            link `setTarget` "_blank"
            link `setInnerHTML` Just txt
            linkBox `appendChild` Just link

        let preE     = castToElement pre
            linkBoxE = toElement linkBox
        withDTL (`DTL.add`    ["hide"]) linkBoxE

        _ <- (preE `on` DE.mouseEnter) . liftIO $
          withDTL (`DTL.remove` ["hide"]) linkBoxE

        _ <- (preE `on` DE.mouseLeave) . liftIO $
          withDTL (`DTL.add`    ["hide"]) linkBoxE

        return ()
    colorPrompt :: Node -> IO ()
    colorPrompt blk = void . runMaybeT $ do
      fc <- MaybeT $ getFirstChild  blk
      tc <- MaybeT $ getTextContent fc
      pr <- MaybeT $ getParentElement blk
      let isPrompt = any (`isPrefixOf` tc) ["λ", "ghci"]
      when isPrompt . liftIO $
        withDTL (`DTL.add` ["code-block-prompt"]) pr
      






setupAsides :: Document -> IO ()
setupAsides doc = do
    asides <- doc `DD.querySelectorAll` ".main-content .note"
    flip (mapM_ . mapMNodeList_ . to' getChildNodes) asides $ \blks -> do
      let flipAll =
            flip (withIndex (mapMNodeList_' ELEMENT_NODE)) blks $ \i blk -> do
              when (i > 0) $ do
                -- TODO: submit PR for toggle for ghcjs-dom
                flip withDTL (castToElement blk) $ \l -> do
                  hasHide <- l `DTL.contains` "hide"
                  if hasHide
                    then l `DTL.remove` ["hide"]
                    else l `DTL.add`    ["hide"]

      flip (withIndex (mapMNodeList_' ELEMENT_NODE)) blks $ \i blk -> do
        let blkE = castToElement blk
        when (i == 0) $ do
          _ <- (blkE `on` DE.click) $ liftIO flipAll
          withDTL (`DTL.add` ["clickable", "aside-header"]) blkE
          clickMeMaybe <- doc `createElement` Just "span"
          forM_ clickMeMaybe $ \clickMe -> do
            withDTL (`DTL.add` ["clickme"]) clickMe
            clickMe `setInnerHTML` Just "(Click me!)"
            void $ blk `appendChild` Just (toNode clickMe)

      flipAll

withIndex
    :: forall s t a b f. Applicative f
    => ((a -> StateT Integer f b) -> (s -> StateT Integer f t))
    -> (Integer -> a -> f b) -> (s -> f t)
withIndex t f = fmap fst . flip runStateT 0 . t f'
  where
    f' :: a -> StateT Integer f b
    f' y = StateT $ \i -> (, i+1) <$> f i y


