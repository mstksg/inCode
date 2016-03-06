{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Blog.Compiler.Home where

import           Blog.Compiler.Entry
import           Blog.Types
import           Blog.Util.Tag
import           Blog.View
import           Blog.View.Home
import           Data.Default
import           Hakyll
import           Hakyll.Web.Blaze
import           Hakyll.Web.Redirect
import qualified Data.Text            as T

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
    linksCopy  <- loadBody "copy/static/home-links.md"
    bannerCopy <- loadBody "copy/static/home-banner.md"
    let hi = HI { hiPageNum    = i
                , hiPrevPage   =
                    if | i <= 1    -> Nothing
                       | i == 2    -> Just "/"
                       | otherwise -> Just $ "/home/" ++ show (i - 1)
                , hiNextPage   =
                    if | i >= maximum allPages -> Nothing
                       | otherwise             -> Just $ "/home/" ++ show (i + 1)
                , hiEntries    = homeEntries
                , hiAllTags    = allTs
                , hiLinksCopy  = linksCopy
                , hiBannerCopy = bannerCopy
                }
        pd = def { pageDataTitle = if i == 1
                                     then Nothing
                                     else Just $ T.pack ("Home ("  ++ show i ++ ")")
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
      else do
        return render

