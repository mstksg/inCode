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

-- toJSON :: TocData -> Fay Text
-- toJSON = ffi "JSON.stringify(%1)"

main :: Fay ()
main = ready $ do
  print "Hello, world!"

  -- initToc
  appendTopLinks
  setupSourceLink

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

-- if ($('.source-info').length > 0) {
--     $('.article header').hover(function() {
--         if (!source_toggled) {
--             $('.source-info').show();
--         }
--     }, function() {
--         if (!source_toggled) {
--             $('.source-info').hide();
--         }
--     }).click(function() {
--         if (source_toggled) {
--             $('.source-info').hide();
--         } else {
--             $('.source-info').show();
--         }
--         source_toggled = !source_toggled;
--     });
-- }

setupSourceLink :: Fay ()
setupSourceLink = do
  sourceInfo <- select ".source-info"
  print sourceInfo
  hasSource <- jLength sourceInfo >>= return . (> 0)

  when hasSource $ do
    sourceToggled <- newFayRef False
    header <- select ".article header"

    flip mouseenter header $ \_ -> do
      toggled <- readFayRef sourceToggled
      unless toggled (unhide sourceInfo)

    flip mouseleave header $ \_ -> do
      toggled <- readFayRef sourceToggled
      unless toggled (hide Instantly sourceInfo)

    flip click header $ \_ -> do
      toggled <- readFayRef sourceToggled
      if toggled
        then hide Instantly sourceInfo
        else unhide sourceInfo
      modifyFayRef sourceToggled Prelude.not

-- sShow :: JQuery -> Fay ()
-- sShow = ffi "%1['show']()"

-- sHide :: JQuery -> Fay ()
-- sHide = ffi "%1['hide]()"
