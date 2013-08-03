{-# LANGUAGE TemplateHaskell #-}

module Web.Blog.Models.Types where

import Database.Persist.TH

-- General: #
-- Category: @
-- Series: +
data TagType = GeneralTag | CategoryTag | SeriesTag
  deriving (Show, Read, Eq)

derivePersistField "TagType"

