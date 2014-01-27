{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

#ifdef FAY
module Main where
#else
module Fay.Page.Entry where
#endif

#ifdef FAY
import Prelude
#else
import "fay-base" Prelude
#endif

import JQuery
import FFI
import Fay.Text as T
import FayRef
-- import DOM

jLength :: JQuery -> Fay Int
jLength = ffi "%1.length"

sHide :: JQuery -> Fay JQuery
sHide = ffi "%1['hide']()"

-- toJSON :: TocData -> Fay Text
-- toJSON = ffi "JSON.stringify(%1)"

main :: Fay ()
main = ready $ do
  print "Hello, from the world of Fay!"

  -- initToc
  appendTopLinks
  setupSourceLink
  processCodeBlocks

  return ()

-- initToc :: Fay ()
-- initToc = do
--   toc <- select "#toc"
--   -- tData <- toJSON tocData
--   -- print tData
--   initTocFFI toc tData
--   tocLength <- select "#toc li" >>= jLength
--   when (tocLength > 0) $
--     select ".contents-container" >>= unhide

-- data TocData = TocData  { selectors :: Text
--                         , container :: Text
--                         , smoothScrolling :: Bool
--                         , prefix :: Text
--                         , highlightOnScroll :: Bool
--                         , anchorName :: Int -> Text -> Text -> Text
--                         , headerText :: Int -> Text -> JQuery -> Text
--                         , itemClass :: Int -> Text -> JQuery -> Text -> Text
--                         }

-- tocData :: TocData
-- tocData = TocData { selectors = "h1,h2,h3,h4"
--                   , container = ".main-content"
--                   , smoothScrolling = True
--                   , prefix = "sec"
--                   , highlightOnScroll = False
--                   , anchorName = anchorNameFunc
--                   , headerText = headerTextFunc
--                   , itemClass = itemClassFunc
--                   }
--   where
--     anchorNameFunc :: Int -> Text -> Text -> Text
--     anchorNameFunc = ffi "(%3+%1)"
--     headerTextFunc :: Int -> Text -> JQuery -> Text
--     headerTextFunc = ffi "(%3.text())"
--     itemClassFunc :: Int -> Text -> JQuery -> Text -> Text
--     itemClassFunc = ffi "(%3[0].tagName.toLowerCase())"

-- initTocFFI :: JQuery -> Text -> Fay ()
-- initTocFFI = ffi "%1['toc'](%2)"

-- initTocFFI :: Fay ()
-- initTocFFI = ffi
--     -- $('#toc').toc({
--     --     'selectors': 'h1,h2,h3,h4', //elements to use as headings
--     --     'container': '.main-content', //element to find all selectors in
--     --     'smoothScrolling': true, //enable or disable smooth scrolling on click
--     --     'prefix': 'sec', //prefix for anchor tags and class names
--     --     // 'onHighlight': function(el) {}, //called when a new section is highlighted
--     --     'highlightOnScroll': false, //add class to heading that is currently in focus
--     --     // 'highlightOffset': 100, //offset to trigger the next headline
--     --     'anchorName': function(i, heading, prefix) { //custom function for anchor name
--     --         return prefix+i;
--     --     },
--     --     'headerText': function(i, heading, $heading) { //custom function building the header-item text
--     --         return $heading.text();
--     --     },
--     --     'itemClass': function(i, heading, $heading, prefix) { // custom function for item class
--     --         return $heading[0].tagName.toLowerCase();
--     --     }
--     -- });

-- tData :: Text
-- tData = "{'selectors': 'h1,h2,h3,h4', 'container': '.main-content', 'smoothScrolling': true, 'prefix': 'sec', 'highlightOnScroll': false, 'anchorName': function(i, heading, prefix) { return prefix+i; }, 'headerText': function(i, heading, $heading) { return $heading.text(); }, 'itemClass': function(i, heading, $heading, prefix) { return $heading[0].tagName.toLowerCase(); }}"

appendTopLinks :: Fay ()
appendTopLinks = do
  mainContent <- select ".main-content"
  headings <- "h2,h3,h4,h5" `childrenMatching` mainContent
  JQuery.append topLink headings
  topLinks <- select ".top-link"
  click scrollTo topLinks
  return ()
  where
    topLink = "<a href='#title' class='top-link'>top</a>"

scrollTo :: Event -> Fay ()
scrollTo e = do
  preventDefault e
  sTarg <- select =<< target e
  sTargHref <- "href" `getAttr` sTarg
  case sTargHref of
    Undefined     -> return ()
    Defined targ  -> animScroll targ 400 "swing"
  return ()

animScroll ::
       Text     -- target href
    -> Double   -- duration
    -> Text     -- easing ("swing","linear")
    -> Fay ()
animScroll targH dur easing = do
  targJ <- select targH
  animScrollFFI targJ dur easing targH
  where
    animScrollFFI :: JQuery -> Double -> Text -> Text -> Fay ()
    animScrollFFI = ffi "$('body,html').animate({ scrollTop: %1.offset().top }, %2, %3, function() { location.hash = %4 })"

setupSourceLink :: Fay ()
setupSourceLink = do
  sourceInfo <- select ".source-info"
  -- print sourceInfo
  sourceCount <- jLength sourceInfo
  let
    hasSource = sourceCount > 0

  when hasSource $ do
    sourceToggled <- newFayRef False
    header <- select ".article header"

    flip mouseenter header $ \_ -> do
      toggled <- readFayRef sourceToggled
      unless toggled (unhide sourceInfo)

    flip mouseleave header $ \_ -> do
      toggled <- readFayRef sourceToggled
      unless toggled (sHide sourceInfo)

    flip click header $ \_ -> do
      toggled <- readFayRef sourceToggled
      if toggled
        then sHide sourceInfo
        else unhide sourceInfo
      modifyFayRef' sourceToggled Prelude.not

-- sShow :: JQuery -> Fay ()
-- sShow = ffi "%1['show']()"

processCodeBlocks :: Fay ()
processCodeBlocks = do
  blocks <- select ".main-content pre.sourceCode"
  flip each blocks $ \_ el ->
    select el >>= processBlock >> return True
  return ()
  where
    processBlock :: JQuery -> Fay ()
    processBlock blk = do
      oldcode <- children blk

      newcode <- select "<code />"
      oldclasses <- fromDefined "" `mapFay` getAttr "class" oldcode
      setAttr "class" oldclasses newcode

      codecontents <- contents oldcode

      afterProcessed <- newFayRef False

      flip each codecontents $ \_ el -> do
        ap <- readFayRef afterProcessed
        if ap
          then writeFayRef afterProcessed False
          else do
            processed <- processComment el blk
            if processed
              then writeFayRef afterProcessed True
              else void (JQuery.append el newcode)
        return True

      replaceWithJQuery newcode oldcode

      linkBox <- childrenMatching ".code-link-box" blk

      flip mouseenter blk $ \_ -> do
        unhide linkBox
        return ()

      flip mouseleave blk $ \_ -> do
        sHide linkBox
        return ()

      return ()

    processComment :: Element -> JQuery -> Fay Bool
    processComment el blk = do
      elJ <- select el
      isComment <- hasClass "co" elJ
      if isComment
        then do
          coText <- getText elJ
          processes <-
            forM  [("-- source: "     , handleSource)
                  ,("-- interactive: ", handleInter )] $
              \(pref,handler) -> do
                let isPre = pref `isPrefixOfT` coText
                when isPre (handler blk coText)
                return isPre
          return (or processes)
        else return False

    handleSource :: JQuery -> Text -> Fay ()
    handleSource blk coText = do
      linkBox <- getLinkBox blk
      JQuery.append sourceLink linkBox
      return ()
      where
        u = dropT (T.length "-- source: ") coText
        sourceLink = T.concat ["<a href='", u, "' class='code-source-link' target='_blank'>Download source</a>"]
    handleInter :: JQuery -> Text -> Fay ()
    handleInter blk coText = do
      linkBox <- getLinkBox blk
      JQuery.append interactiveLink linkBox
      return ()
      where
        u = dropT (T.length "-- interactive: ") coText
        interactiveLink = T.concat ["<a href='", u, "' class='code-interactive-link' target='_blank'>Interactive</a>"]
    getLinkBox :: JQuery -> Fay JQuery
    getLinkBox blk = do
      already <- childrenMatching ".code-link-box" blk
      hasAlready <- (> 0) `mapFay` jLength already
      if hasAlready
        then return already
        else do
          linkBox <- select "<div />"
          addClass "code-link-box" linkBox
          prepend linkBox blk
          return linkBox


    -- processBlock :: JQuery -> Fay ()
    -- processBlock blk = do
    --   preblk <- children blk
    --   comments <- childrenMatching ".co" preblk
    --   flip each comments $ \i el ->
    --     select el >>= processComment blk >> return (i < 2)
    --   return ()
    -- processComment :: JQuery -> JQuery -> Fay ()
    -- processComment blk co = do
    --   coText <- getText co
    --   when ("-- source: " `isPrefixOfT` coText) $ handleSource blk co coText
    --   when ("-- interactive: " `isPrefixOfT` coText) $ handleInter blk co coText
    -- handleSource :: JQuery -> JQuery -> Text -> Fay ()
    -- handleSource blk co coText = do
    --   -- prepend sourceLink blk
    --   -- void $ sHide co
    --   -- void $ setHeight 0 co
    --   return ()
    --   where
    --     u = dropT (T.length "-- source: ") coText
    --     sourceLink = T.concat ["<a href='", u, "' class='code-source-link'>Download source</a>"]
    -- handleInter :: JQuery -> JQuery -> Text -> Fay ()
    -- handleInter blk co coText = do
    --   -- prepend interactiveLink blk
    --   -- void $ sHide co
    --   -- void $ setHeight 0 co
    --   return ()
    --   where
    --     u = dropT (T.length "-- interactive: ") coText
    --     interactiveLink = T.concat ["<a href='", u, "' class='code-interactive-link'>Interactive</a>"]


mapFay :: (a -> b) -> Fay a -> Fay b
mapFay f fay = do
  res <- fay
  return (f res)

fromDefined :: a -> Defined a -> a
fromDefined x Undefined = x
fromDefined _ (Defined x) = x

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isPrefixOfT :: Text -> Text -> Bool
isPrefixOfT s1 s2 =
  case (uncons s1, uncons s2) of
    (Nothing, _) -> True
    (_, Nothing) -> False
    (Just (x,xs),Just (y,ys)) -> x == y && isPrefixOfT xs ys

dropT :: Int -> Text -> Text
dropT i txt | i <= 0    = txt
            | otherwise =
                case uncons txt of
                  Nothing -> txt
                  Just (_,xs) -> dropT (i-1) xs
