module Hakyll.Web.Blaze where

import           Hakyll
import qualified Text.Blaze.Html.Renderer.Pretty as H
import qualified Text.Blaze.Html5            as H

blazeCompiler :: H.Html -> Compiler (Item String)
blazeCompiler = undefined
