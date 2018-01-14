{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.View where

import           Blog.Types
import           Blog.Util
import           Data.List
import           Data.Monoid
import           Text.Blaze.Html5            ((!))
import qualified Data.Text                   as T
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Pandoc                 as P
import qualified Text.Pandoc.Builder         as P
import qualified Text.Pandoc.Shared          as P

mainSection :: H.Attribute
mainSection = H.customAttribute "role" "main"

renderUrl
    :: (?config :: Config)
    => T.Text
    -> T.Text
renderUrl u | hasP      = u
            | otherwise = urlBase </!> u
  where
    hasP = length (T.splitOn "//" u) > 1

renderUrl'
    :: (?config :: Config)
    => String -> String
renderUrl' = T.unpack . renderUrl . T.pack

renderSourceUrl
    :: (?config :: Config)
    => T.Text
    -> Maybe T.Text
renderSourceUrl u = flip fmap (sourceBlobs ?config) $ \b ->
                      b </!> u

renderRenderUrl
    :: (?config :: Config)
    => T.Text
    -> Maybe T.Text
renderRenderUrl u = flip fmap (renderBlobs ?config) $ \b ->
                      b </!> u

renderRootUrl
    :: (?config :: Config)
    => T.Text
    -> T.Text
renderRootUrl u | hasP      = u
                | otherwise = maybe mempty (T.cons '/') hostRoot
                           </!> u
  where
    HostInfo{..} = confHostInfo ?config
    hasP = length (T.splitOn "//" u) > 1

renderRootUrl'
    :: (?config :: Config)
    => String
    -> String
renderRootUrl' = T.unpack . renderRootUrl . T.pack

urlBase :: (?config :: Config) => T.Text
urlBase = protocol
       <> "://"
       <> hostBase
       <> maybe mempty (T.pack . (':':) . show) hostPort
       <> maybe mempty (T.cons '/') hostRoot
  where
    HostInfo{..} = confHostInfo ?config
    protocol | hostSecure = "https"
             | otherwise  = "http"

copyToHtml :: P.Pandoc -> H.Html
copyToHtml = either (error . show) id
           . P.runPure
           . P.writeHtml5 entryWriterOpts

copySection :: T.Text -> H.Html -> H.Html
copySection title copy = do
    H.header $
      H.h1 $
        H.toHtml title
    H.hr
    H.div ! A.class_ "copy-content" $
      copy
    H.div ! A.class_ "clear" $
      mempty

copyToHtmlString :: P.Pandoc -> String
copyToHtmlString = either (error . show) T.unpack
                 . P.runPure
                 . P.writeHtml5String entryWriterOpts

stripPandoc :: P.Pandoc -> T.Text
stripPandoc (P.Pandoc _ bs) = T.pack $ P.stringify inls
  where
    inls = concatMap grabInls . intersperse (P.Plain [P.Space]) $ bs
    grabInls :: P.Block -> [P.Inline]
    grabInls (P.Plain inls') = inls'
    grabInls (P.Para inls') = inls'
    grabInls (P.CodeBlock _ str) = P.toList $ P.text str
    grabInls (P.RawBlock _ str) = P.toList $ P.text str
    grabInls (P.BlockQuote bs') = concatMap grabInls bs'
    grabInls (P.OrderedList _ bss ) =
      concatMap ((++) (P.toList $ P.text " * ") . concatMap grabInls) bss
    grabInls (P.BulletList bss) =
      concatMap ((++) (P.toList $ P.text " * ") . concatMap grabInls) bss
    grabInls (P.DefinitionList ds) = concatMap mapDs ds
      where
        mapDs (inls', bss) =
          concat
            [ P.toList $ P.text " * "
            , inls'
            , P.toList $ P.text ": "
            , concatMap (concatMap grabInls) bss
            ]
    grabInls (P.Header _ _ inls') = inls'
    grabInls (P.HorizontalRule) = P.toList $ P.text "---"
    grabInls (P.Table cap _ _ _ _) = cap
    grabInls _ = []

