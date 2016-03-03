{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View where

import           Blog.Types
import           Data.Monoid
import           System.FilePath
import qualified Data.Text           as T
import qualified Text.Blaze.Internal as H

mainSection :: H.Attribute
mainSection = H.customAttribute "role" "main"

renderUrl :: (?config :: Config) => T.Text -> T.Text
renderUrl u | hasP      = u
            | otherwise = T.pack $
                            T.unpack (urlBase ?config) </> T.unpack u
  where
    hasP = length (T.splitOn "//" u) > 1

renderUrl' :: (?config :: Config) => String -> String
renderUrl' = T.unpack . renderUrl . T.pack

urlBase :: Config -> T.Text
urlBase (confHostInfo->HostInfo{..})
    = "http://"
   <> hostBase
   <> maybe mempty (T.pack . (':':) . show) hostPort

