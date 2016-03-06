module Web.Blog.Models.Slug where

import "base" Prelude
import qualified Data.Text as T
import Web.Blog.Models.Models

slugPrettyLabel :: Slug -> T.Text
slugPrettyLabel s = T.append (slugSlugPrefix $ slugIsCurrent s) $ slugSlug s
  where
    slugSlugPrefix True = "!"
    slugSlugPrefix False = "`"
