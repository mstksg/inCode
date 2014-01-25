{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RebindableSyntax #-}

#ifdef FAY
module Main where
#else
module Web.Blog.Fay.Entry where
#endif

#ifdef FAY
import Prelude
#else
import "fay-base" Prelude
#endif

import JQuery
import FFI
import Fay.Text
-- import DOM

main :: Fay ()
main = ready $ do
  putStrLn "Hello, world!"
  -- $('.main-content').children('h2,h3,h4,h5').append('<a href="#title" class="top-link">top</a>');
  headings <- select ("p" :: Text)
  -- headings <- select (".main-content" :: Text) >>=
  --               childrenMatching "h2,h3,h4,h5"
  print headings
  addClass "yolo" headings
  -- remove headings
  -- JQuery.append topLink headings
  return ()

topLink :: Text
topLink = "<a href='#title' class='top-link'>top</a>"

scrollTo :: Event -> Fay ()
scrollTo e = do
  preventDefault e
  sTarg <- select =<< target e
  sTargHref <- "href" `getAttr` sTarg
  case sTargHref of
    Undefined     -> return ()
    -- Defined targ  -> animScroll targ 400 "swing"
  return ()

-- animScroll t d e = putStrLn "scroll me"

-- animScroll ::
--        Text     -- target href
--     -> Double   -- duration
--     -> Text     -- easing ("swing","linear")
--     -> Fay ()
-- animScroll targH dur easing = do
--   targJ <- select targH
--   putStrLn $ "I'm scrolling to " ++ unpack targH
--   return ()
--   -- setAttr "scrollTop"
--   -- animScrollFFI targJ dur easing targH
--   -- undefined targJ dur easing targH
--   -- where
--     -- animScrollFFI = FFI.ffi
--     --   ("$('body,html').animate({ scrollTop: %1.offset().top }, %2, %3, function() { location.hash = %4 })" :: String)

-- var scrollTo = function(e) {
--     e.preventDefault();
--     var elScrollTo = $(e.target).attr('href');
--     var $el = $(elScrollTo);
    -- $('body,html').animate({ scrollTop: $el.offset().top }, 400, 'swing', function() {
    --     location.hash = elScrollTo;
    -- });
-- };
