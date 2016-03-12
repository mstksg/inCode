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
      -- SassString s -> SassString (renderUrl' s)
      -- SassString s -> SassString "hey"
      -- _ -> SassString $ show v

      SassList [SassString l] _ -> SassString $ "url(\"" ++ renderUrl' l ++ "\")"
      _            -> v
