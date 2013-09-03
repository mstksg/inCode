{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Web.Blog.Models.Instances where

import Web.Blog.Models.Models

instance Show Entry where
  show (Entry t _ cA pA _ i) = concat
    [ show t
    , " ("
    , maybe "" ((++ ", ") . show) i
    , maybe "" ((++ ", ") . show) cA
    , maybe "no post date" (("posted " ++) . show) pA
    , ")"
    ]
