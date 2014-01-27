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

import JQuery as J
import FFI
import Fay.Text as T
import FayRef


-- | Helper ffi's
jLength :: JQuery -> Fay Int
jLength = ffi "%1.length"

sHide :: JQuery -> Fay JQuery
sHide = ffi "%1['hide']()"

-- $(document).ready()
main :: Fay ()
main = ready $ do
  print "Hello, from the world of Fay!"

  appendTopLinks
  setupSourceLink
  processCodeBlocks

  return ()

-- | Startup subprocedures
--
-- add "top" links to all h2,h3,h4,h5
appendTopLinks :: Fay ()
appendTopLinks = do
  mainContent <- select ".main-content"
  headings <- "h2,h3,h4,h5" `childrenMatching` mainContent
  J.append topLink headings
  topLinks <- select ".top-link"
  click (scrollTo 400) topLinks
  return ()
  where
    topLink = "<a href='#title' class='top-link'>top</a>"

-- turn link into a scrollTo
scrollTo :: Double -> Event -> Fay ()
scrollTo duration e = do
  preventDefault e
  sTarg <- select =<< target e
  sTargHref <- getAttr "href" sTarg
  case sTargHref of
    Undefined     -> return ()
    Defined targ  -> animScroll targ duration "swing"
  return ()

-- animate scrolling
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

-- setup source link visiblity behavior at the entry header.  When the
--    header is clicked, it should toggle between mostly-off and always-on.
--    In mostly-off mode, a hover reveals the link.  This is accomplished
--    using an IORef/FayRef...but maybe there is a better way.  Juggling
--    binds/event handlers is kind of the same thing anyway.
setupSourceLink :: Fay ()
setupSourceLink = do
  sourceInfo <- select ".source-info"
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

-- process code blocks and parse "source" and "interactive" links
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
              else void (el `J.append` newcode)
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
      sourceLink `J.append` linkBox
      return ()
      where
        u = dropT (T.length "-- source: ") coText
        sourceLink = T.concat ["<a href='", u, "' class='code-source-link' target='_blank'>Download source</a>"]
    handleInter :: JQuery -> Text -> Fay ()
    handleInter blk coText = do
      linkBox <- getLinkBox blk
      interactiveLink `J.append` linkBox
      return ()
      where
        u = dropT (T.length "-- interactive: ") coText
        interactiveLink = T.concat  ["<a href='", u, "' class='code-interactive-link' target='_blank'>Interactive</a>"]
    getLinkBox :: JQuery -> Fay JQuery
    getLinkBox blk = do
      already <- childrenMatching ".code-link-box" blk
      hasAlready <- (> 0) `mapFay` jLength already
      if hasAlready
        then return already
        else do
          linkBox <- select "<div />"
          "code-link-box" `addClass` linkBox
          linkBox `prepend` blk
          return linkBox

-- | Util functions

mapFay :: (a -> b) -> Fay a -> Fay b
mapFay f fay = do
  res <- fay
  return (f res)

fromDefined :: a -> Defined a -> a
fromDefined x Undefined = x
fromDefined _ (Defined x) = x

isPrefixOfT :: Text -> Text -> Bool
isPrefixOfT s1 s2 =
  case (uncons s1, uncons s2) of
    (Nothing, _)              -> True
    (_, Nothing)              -> False
    (Just (x,xs),Just (y,ys)) -> x == y && isPrefixOfT xs ys

dropT :: Int -> Text -> Text
dropT i txt | i <= 0    = txt
            | otherwise =
                case uncons txt of
                  Nothing     -> txt
                  Just (_,xs) -> dropT (i-1) xs
