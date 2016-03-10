{-# LANGUAGE TupleSections #-}

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Functor
import           Data.IORef
import           Data.List
import           Data.Maybe
import           GHCJS.DOM
import           GHCJS.DOM.Document          as JD
import           GHCJS.DOM.Element           as JE
import           GHCJS.DOM.EventM
import           GHCJS.DOM.HTMLAnchorElement
import           GHCJS.DOM.HTMLBodyElement
import           GHCJS.DOM.HTMLElement
import           GHCJS.DOM.Node
import           GHCJS.DOM.NodeList
import qualified GHCJS.DOM.DOMTokenList      as DTL

fromNodeList :: NodeList -> IO [Node]
fromNodeList nl = do
    l <- getLength nl
    catMaybes <$> mapM (nl `item`) [0 .. (l - 1)]

fromNodeListIx :: NodeList -> IO [(Integer, Node)]
fromNodeListIx nl = do
    l <- getLength nl
    catMaybes <$> mapM (\i -> fmap (fromIntegral i,) <$> nl `item` i)
                       [0 .. (l - 1)]


mapMNodeList_ :: (Node -> IO ()) -> NodeList -> IO ()
mapMNodeList_ f nl = do
    ns <- fromNodeList nl
    mapM_ f ns

withDTL
    :: MonadIO m
    => (DTL.DOMTokenList -> m ())
    -> Element
    -> m ()
withDTL f e = do
    dtl <- getClassList e
    mapM_ f dtl

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
    return ()
    -- (mapM_ . mapMNodeList_) (flipAside True) asides
  -- where
    -- flipAside :: Bool -> Element -> IO ()
    -- flipAside setup aside = do
        


    

--   asides <- select ".main-content .note"
--   flip each asides $ \_ el -> do
--     flipAside True =<< select el
--     return True
--   return ()

-- flipAside :: Bool -> JQuery -> Fay ()
-- flipAside setup aside = do
--     blks <- children aside
--     flip each blks $ \i el -> do
--       elJ <- select el
--       if i == 0
--         then do
--           when setup $ do
--             flip click elJ $ \_ -> flipAside False aside
--             addClass "clickable aside-header" elJ
--             J.append clickMe elJ
--           return ()
--         else do
--           toggle Fast elJ
--           return ()
--       return True
--     return ()
--   where
--     clickMe = " <span class='clickme'>(Click me!)</span>"

