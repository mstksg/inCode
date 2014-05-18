{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Blog.Models.Instances where

import "base" Prelude
import Web.Blog.Models.Models
import Web.Blog.Models.Types
import qualified Data.Text as T

-- instance Show Entry where
--   -- show (Entry t _ _ _ cA pA _ i) = concat
--   show (Entry { entryTitle      = t
--               , entryCreatedAt  = cA
--               , entryPostedAt   = pA
--               , entryIdentifier = i
--               } ) = concat
--     [ show t
--     , " ("
--     , maybe "" ((++ ", ") . show) i
--     , maybe "" ((++ ", ") . show) cA
--     , maybe "no post date" (("posted " ++) . show) pA
--     , ")"
--     ]

-- instance Show Tag where
--   show t = T.unpack $ T.append (tagTypePrefix $ tagType_ t) $ tagLabel t

-- instance Indexable Entry where
--   empty = ixSet
--     [ ixGen (Proxy :: Proxy T.Text)
--     , ixGen (Proxy :: Proxy Content)
--     , ixGen (Proxy :: Proxy )
--     ]
