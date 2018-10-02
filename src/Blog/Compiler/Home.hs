{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Home where

import           Blog.Compiler.Entry
import           Blog.Compiler.Redirect
import           Blog.Types
import           Blog.Util
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Home
import           Data.Default
import           Hakyll
import           Hakyll.Web.Blaze
import qualified Data.Text              as T

homeCompiler
    :: (?config :: Config)
    => [Int]
    -> [(TagType, T.Text)]
    -> Int
    -> Pattern
    -> Compiler (Item String)
homeCompiler allPages allTags i p = do
    entries <- map itemBody <$> loadAllSnapshots p "entry"
    homeEntries <- mapM compileTE entries
    allTs <- mapM (uncurry fetchTag) allTags
    linksCopy  <- readPandocWith entryReaderOpts =<< load "copy/static/home-links.md"
    bannerCopy <- readPandocWith entryReaderOpts =<< load "copy/static/home-banner.md"
    patronList <- either (fail . show) pure
                . parsePatronList PLSupport
              =<< loadBody "config/patrons.yaml"
    let hi = HI { hiPageNum    = i
                , hiPrevPage   =
                    if | i <= 1
                           -> Nothing
                       | i == 2
                           -> Just "/"
                       | otherwise
                           -> Just $ "/home/" ++ show (i - 1) ++ ".html"
                , hiNextPage   =
                    if | i >= maximum allPages
                           -> Nothing
                       | otherwise
                           -> Just $ "/home/" ++ show (i + 1) ++ ".html"
                , hiEntries    = homeEntries
                , hiAllTags    = allTs
                , hiLinksCopy  = itemBody linksCopy
                , hiBannerCopy = itemBody bannerCopy
                , hiPatrons    = patronList
                }
        pd = def { pageDataTitle = if i == 1
                                     then Nothing
                                     else Just $ T.pack ("Home (Page "  ++ show i ++ ")")
                 , pageDataCanonical = if i == 1
                                         then Just $ renderUrl' "/"
                                         else Nothing
                 , pageDataCss   = ["/css/page/home.css"
                                   ,"/css/pygments.css"
                                   ]
                 , pageDataJs    = ["/js/disqus_count.js"]
                 }
    render <- blazeCompiler pd (viewHome hi)
    if i == 1
      then do
        _ <- saveSnapshot "index" render
        redirectCompiler (\_ -> renderUrl "/index.html")
      else
        return render

