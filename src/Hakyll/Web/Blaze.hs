{-# LANGUAGE ImplicitParams #-}

module Hakyll.Web.Blaze where

import           Blog.Types
import           Blog.Render
import           Hakyll
import qualified Text.Blaze.Html.Renderer.Pretty as HP
import qualified Text.Blaze.Html.Renderer.String as HS
import qualified Text.Blaze.Html5                as H

blazeCompiler
    :: (?config :: Config)
    => PageData
    -> H.Html
    -> Compiler (Item String)
blazeCompiler pd body = do
    r <- getRoute =<< getUnderlying
    let pd' = case pageDataCanonical pd of
                Nothing -> pd { pageDataCanonical = r }
                Just _  -> pd
        h   = renderLayout pd' body
    makeItem $ case confEnvType ?config of
                 ETDevelopment -> HP.renderHtml h
                 ETProduction  -> HS.renderHtml h
