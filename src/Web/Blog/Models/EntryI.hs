module Web.Blog.Models.EntryI where

-- import Control.Applicative
import Data.Time
import Web.Blog.Models.Models
import Web.Blog.Types
import qualified Data.Foldable as F
import qualified Data.Map      as M
import qualified Data.Text     as T

postedEntriesI :: UTCTime -> SiteDatabase -> KeyMap Entry
postedEntriesI now db =
    M.filter (F.any (< now) . entryPostedAt) $ siteDatabaseEntries db

postedEntryCountI :: UTCTime -> SiteDatabase -> Int
postedEntryCountI = (M.size .) . postedEntriesI


getEntryData :: KeyMapKey Entry -> SiteDatabase -> (T.Text,[Tag])
getEntryData k db = undefined
  -- p <- getUrlPath e
  -- ts  <- getTags e []
  -- return (p,ts)

wrapEntryData :: KeyMapKey Entry -> SiteDatabase -> (KeyMapPair Entry, (T.Text, [Tag]))
wrapEntryData k db = undefined
  -- d <- getEntryData e
  -- return (e,d)

getTagsByEntityKey :: KeyMapKey Entry -> SiteDatabase -> [Tag]
getTagsByEntityKey k filters = undefined
  -- ets <- D.selectList [ EntryTagEntryId D.==. k ] []
  -- let
  --   tagKeys = map (entryTagTagId . D.entityVal) ets
  --   selectTags tt = D.selectList
  --                     ([ TagId D.<-. tagKeys, TagType_ D.==. tt ] ++ filters)
  --                     [ D.Asc TagLabel ]
  -- tags <- concat <$> mapM selectTags [GeneralTag ..]

  -- return $ map D.entityVal tags

getUrlPathI :: KeyMapKey Entry -> SiteDatabase -> T.Text
getUrlPathI k db = undefined
  -- slug <- getCurrentSlug entry
  -- case slug of
  --   Just (D.Entity _ slug') ->
  --     return $ T.append "/entry/" (slugSlug slug')
  --   Nothing               -> do
  --     let
  --       D.Entity eKey _ = entry
  --     return $ T.append "/entry/id/" (T.pack $ show eKey)

getCurrentSlugI :: KeyMapKey Entry -> SiteDatabase -> Maybe (KeyMapPair Slug)
getCurrentSlugI k db = undefined
    -- current <- D.selectFirst [ SlugEntryId   D.==. eKey
    --                          , SlugIsCurrent D.==. True ]
    --                          [ D.Desc SlugId ]
    -- case current of
    --   Just _ -> return current
    --   Nothing -> D.selectFirst [ SlugEntryId D.==. eKey ]
    --                            [ D.Desc SlugId ]
  -- where
    -- D.Entity eKey _ = entry
