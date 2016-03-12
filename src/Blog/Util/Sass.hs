{-# LANGUAGE ImplicitParams #-}

module Blog.Util.Sass where

import Blog.Types
import Blog.View
import Text.Sass

renderSassUrl
    :: (?config :: Config)
    => SassFunction
renderSassUrl = SassFunction "render-url($x)" $ \v -> return $
    case v of
      SassList [SassString l] _ -> SassString $ "url(\"" ++ renderRootUrl' l ++ "\")"
      _            -> v
