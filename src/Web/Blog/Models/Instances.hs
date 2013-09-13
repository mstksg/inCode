{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Web.Blog.Models.Instances where

import Web.Blog.Models.Models
import Web.Blog.Models.Types
import qualified Data.Text as T

instance Show Entry where
  show (Entry t _ cA pA _ i) = concat
    [ show t
    , " ("
    , maybe "" ((++ ", ") . show) i
    , maybe "" ((++ ", ") . show) cA
    , maybe "no post date" (("posted " ++) . show) pA
    , ")"
    ]

instance Show Tag where
  show t = T.unpack $ T.append (tagTypePrefix $ tagType_ t) $ tagLabel t
