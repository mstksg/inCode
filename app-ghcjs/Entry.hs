{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Maybe
import           GHCJS.DOM
import           GHCJS.DOM.Document           as JD
import           GHCJS.DOM.Element            as JE
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLAnchorElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList
import qualified GHCJS.DOM.DOMTokenList       as DTL

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
    print "Hello from the world of ghcjs!"

    runWebGUI $ \webView -> do
      enableInspector webView
      Just doc <- webViewGetDomDocument webView
      -- Just body <- fmap castToHTMLBodyElement <$> getBody doc

      appendTopLinks doc
      setupSourceLink doc
      processCodeBlocks
      setupAsides doc



appendTopLinks :: Document -> IO ()
appendTopLinks doc = do
    hds <- doc `JD.querySelectorAll`
             intercalate "," [".main-content h2"
                             ,".main-content h3"
                             ,".main-content h4"
                             ,".main-content h5"
                             ]
    flip (mapM_ . mapMNodeList_) hds $ \hd -> do
      topLink' <- fmap castToHTMLAnchorElement
                    <$> doc `createElement` Just "a"
      forM_ topLink' $ \topLink -> do
        topLink `setHref `"#title"
        topLink `setClassName` "top-link"
        topLink `setInnerHTML` Just "top"
        void $ hd `appendChild` Just topLink
        -- TODO: animate.  would this require jquery?


-- setup source link visiblity behavior at the entry header.  When the
--    header is clicked, it should toggle between mostly-off and always-on.
--    In mostly-off mode, a hover reveals the link.  This is accomplished
--    using an IORef/FayRef...but maybe there is a better way.  Juggling
--    binds/event handlers is kind of the same thing anyway.
setupSourceLink :: Document -> IO ()
setupSourceLink doc = void . runMaybeT $ do
    sourceInfo <- MaybeT $ doc `JD.querySelector` ".source-info"

    liftIO $ withDTL (`DTL.add`    ["hide"]) sourceInfo

    sourceToggled <- liftIO $ newIORef False
    header <- MaybeT $ doc `JD.querySelector` ".article > header"

    void . liftIO $ do
      _ <- (header `on` JE.mouseEnter) . liftIO $ do
        toggled <- readIORef sourceToggled
        unless toggled $
          withDTL (`DTL.remove` ["hide"]) sourceInfo

      _ <- (header `on` JE.mouseLeave) . liftIO $ do
        toggled <- readIORef sourceToggled
        unless toggled $
          withDTL (`DTL.add`    ["hide"]) sourceInfo

      (header `on` JE.click) . liftIO $ do
        toggled <- readIORef sourceToggled
        if toggled
          then do
            withDTL (`DTL.add`    ["hide"]) sourceInfo
            writeIORef sourceToggled False
          else do
            withDTL (`DTL.remove` ["hide"]) sourceInfo
            writeIORef sourceToggled True



processCodeBlocks :: IO ()
processCodeBlocks = return ()

setupAsides :: Document -> IO ()
setupAsides doc = do
    asides <- doc `JD.querySelectorAll` ".main-content .note"
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
          _ <- (blkE `on` JE.click) $ liftIO flipAll
          withDTL (`DTL.add` ["clickable", "aside-header"]) blkE
          clickMeMaybe <- doc `createElement` Just "span"
          forM_ clickMeMaybe $ \clickMe -> do
            withDTL (`DTL.add` ["clickme"]) clickMe
            clickMe `setInnerHTML` Just "(Click me!)"
            void $ blk `appendChild` Just (toNode clickMe)

withIndex
    :: forall s t a b f. Applicative f
    => ((a -> StateT Integer f b) -> (s -> StateT Integer f t))
    -> (Integer -> a -> f b) -> (s -> f t)
withIndex t f = fmap fst . flip runStateT 0 . t f'
  where
    f' :: a -> StateT Integer f b
    f' y = StateT $ \i -> (, i+1) <$> f i y


