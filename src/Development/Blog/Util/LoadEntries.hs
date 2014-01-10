
module Development.Blog.Util.LoadEntries (loadEntries) where

import Control.Applicative                    ((<$>), pure)
import Control.Monad
import Control.Monad.IO.Class
import Data.List                              (isPrefixOf)
import Data.Maybe                             (fromJust, listToMaybe)
import Data.Monoid
import Data.Time
import System.Directory                       (getDirectoryContents)
import System.FilePath                        ((</>))
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import Web.Blog.Models.Util
import qualified Data.Map                     as M
import qualified Data.Text                    as T
import qualified Database.Persist.Postgresql  as D
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.Builder          as P
import qualified Text.Pandoc.Readers.Markdown as PM

data MetaKey = MetaKeyTags
             | MetaKeyCategories
             | MetaKeySeries
             | MetaKeyCreateTime
             | MetaKeyPostDate
             | MetaKeyModifiedTime
             | MetaKeyIdentifier
             | MetaKeyPreviousTitles
             deriving ( Show, Eq, Read, Ord )
data MetaValue = MetaValueTime UTCTime
               | MetaValueTags [D.Entity Tag]
               | MetaValueText T.Text
               | MetaValueTexts [T.Text]
               | MetaValueNull
               deriving ( Show, Eq, Read )

loadEntries :: FilePath -> IO ()
loadEntries entriesDir = do
  runDB blogMigrate

  entryFiles <-
    map (entriesDir </>) . filter (not . isPrefixOf ".") <$>
      getDirectoryContents entriesDir

  runDB $ do
    eKeys <- mapM processEntryFile entryFiles
    removeOrphanEntries eKeys


-- TODO: emit warnings, wrap in writer monad.
processEntryFile :: FilePath -> D.SqlPersistM (D.Key Entry)
processEntryFile entryFile = do
    tzone <- liftIO getCurrentTimeZone

    (P.Pandoc _ contents, _) <- liftIO $
      readMarkdown <$> readFile entryFile

    let
      writeMarkdownBlocks bs = writeMarkdown $ P.doc $ P.fromList bs
      (P.Header _ _ headerBlocks, metaBody) = stripAndTake isHeader contents
      (metaBlock, body) = stripAndTake isDefList metaBody

      titleRaw = writeMarkdownBlocks $ pure $ P.Plain headerBlocks
      titleSmart = P.readMarkdown smartReaderOptions titleRaw
      title = T.pack $ writeMarkdown titleSmart
      entryMarkdown = T.pack $ writeMarkdownBlocks body

    metas <- fmap M.fromList . mapM (processMeta tzone) $ defListList metaBlock

    (D.Entity eKey _) <- do
      entryMaybe <- findExistingEntry title metas
      case entryMaybe of
        Just e -> return e
        Nothing -> do
          let
            newEntry = Entry
                         title
                         entryMarkdown
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
                         Nothing
          k <- fromJust <$> insertEntry newEntry
          return $ D.Entity k newEntry

    D.update eKey [ EntryContent    D.=. entryMarkdown
                  , EntryImage      D.=. Nothing
                  , EntrySourceFile D.=. Just entryFile
                  , EntryCreatedAt  D.=. Nothing
                  , EntryPostedAt   D.=. Nothing
                  , EntryModifiedAt D.=. Nothing
                  , EntryIdentifier D.=. Nothing
                  ]
    -- D.deleteWhere [ EntryTagEntryId D.==. eKey ]

    void $ M.traverseWithKey (applyMetas eKey) metas

    updateEntryTitle eKey title

    eVal <- D.getJust eKey

    liftIO $ print eVal

    return eKey
  where
    readMarkdown = PM.readMarkdownWithWarnings (P.def P.ReaderOptions)
    writeMarkdown = P.writeMarkdown basicWriterOptions
    stripAndTake p l = (taken, rest)
      where
        stripped = dropWhile (not . p) l
        taken = head stripped
        rest = tail stripped
    isHeader (P.Header 1 _ _)         = True
    isHeader _                        = False
    isDefList (P.DefinitionList _)    = True
    isDefList _                       = False
    defListList (P.DefinitionList ds) = ds
    defListList _                     = mempty

findExistingEntry :: T.Text -> M.Map MetaKey MetaValue -> D.SqlPersistM (Maybe (D.Entity Entry))
findExistingEntry title metas = foldl go (return Nothing) attempts
  where
    go a x = do
      acc <- a
      case acc of
        Just _ -> return acc
        Nothing -> x
    attempts :: [D.SqlPersistM (Maybe (D.Entity Entry))]
    attempts = concat
      [ [ progressReport "Looking up entry by title..."
        , D.getBy $ UniqueEntryTitle title
        , progressReport "Looking up entry by identifier..."
        , case M.lookup MetaKeyIdentifier metas of
            Just (MetaValueText i) ->
              listToMaybe <$>
                D.selectList
                  [ EntryIdentifier D.==. Just i ]
                  [ D.Asc EntryCreatedAt ]
            _ -> return Nothing
        , progressReport "Looking up entry by previous titles..."
        ]
      , prevTitlesSearch
      , [ progressReport "No entry found ... creating new entry"
        ]
      ]
      where
        prevTitlesSearch :: [D.SqlPersistM (Maybe (D.Entity Entry))]
        prevTitlesSearch =
          case M.lookup MetaKeyPreviousTitles metas of
            Just (MetaValueTexts ts) ->
              map (D.getBy . UniqueEntryTitle) ts
            _ -> []
        progressReport t = liftIO (putStrLn t) >> return Nothing


applyMetas :: D.Key Entry -> MetaKey -> MetaValue -> D.SqlPersistM ()
applyMetas _ _ MetaValueNull = return ()
applyMetas entryKey MetaKeyCreateTime (MetaValueTime t) =
  void $ D.update entryKey [EntryCreatedAt D.=. Just t]
applyMetas entryKey MetaKeyPostDate (MetaValueTime t) =
  void $ D.update entryKey [EntryPostedAt D.=. Just t]
applyMetas entryKey MetaKeyModifiedTime (MetaValueTime t) =
  void $ D.update entryKey [EntryModifiedAt D.=. Just t]
applyMetas entryKey MetaKeyIdentifier (MetaValueText i) =
  void $ D.update entryKey [EntryIdentifier D.=. Just i]
applyMetas entryKey mk (MetaValueTags ts) = do
  let
    tagIds = map D.entityKey ts
    tt = case mk of
      MetaKeyTags       -> GeneralTag
      MetaKeyCategories -> CategoryTag
      MetaKeySeries     -> SeriesTag
      _                 -> error $ "Not a tag type meta key: " ++ show mk

  tagsOfType <- map D.entityKey <$> D.selectList [ TagType_ D.==. tt ] []
  D.deleteWhere
    [ EntryTagEntryId D.==. entryKey
    , EntryTagTagId D./<-. tagIds
    , EntryTagTagId D.<-. tagsOfType ]
  forM_ tagIds $ \tKey -> do
    -- liftIO $ print tKey
    res <- D.insertUnique $ EntryTag entryKey tKey
    liftIO $ print res
    return res
applyMetas _ MetaKeyPreviousTitles _ = return ()
applyMetas _ k v = error $ "Weird meta key/value: " ++ show (k,v)

processMeta :: TimeZone -> ([P.Inline], [[P.Block]]) -> D.SqlPersistM (MetaKey, MetaValue)
processMeta tzone (keyBlocks, valBlockss) = do
    metaValue <- case metaKey of
      MetaKeyPostDate -> readTimeBlockss valBlockss
      MetaKeyModifiedTime -> readTimeBlockss valBlockss
      MetaKeyCreateTime -> readTimeBlockss valBlockss
      MetaKeyPreviousTitles -> return $ MetaValueTexts $
        map (T.pack . renderBlocks) valBlockss
      MetaKeyIdentifier -> return $ MetaValueText $
        T.pack $ renderBlocks $ head valBlockss
      MetaKeyCategories -> generateTags CategoryTag valBlockss
      MetaKeyTags -> generateTags GeneralTag valBlockss
      MetaKeySeries -> generateTags SeriesTag valBlockss

    return (metaKey, metaValue)
  where
    metaKey = read $ (++) "MetaKey" $ renderBlocks $ pure $ P.Plain keyBlocks
    readTime' = parseTime defaultTimeLocale "%Y/%m/%d %X %z"
    readTimeBlockss bss =
      case readTime' . (++ timeZoneOffsetString tzone) . renderBlocks $ head bss of
        Just t -> return $ MetaValueTime t
        Nothing -> return MetaValueNull
    generateTags :: TagType -> [[P.Block]] -> D.SqlPersistM MetaValue
    generateTags tt bss =
      MetaValueTags <$>
        mapM (generateTag . T.pack . renderBlocks) bss
        where
          generateTag :: T.Text -> D.SqlPersistM (D.Entity Tag)
          generateTag label = do
            tag <- D.getBy $ UniqueLabelType label tt
            case tag of
              Just t -> return t
              Nothing -> fmap fromJust $ insertTag' $ PreTag label tt Nothing
    renderBlocks :: [P.Block] -> String
    renderBlocks bs =
      P.writeMarkdown basicWriterOptions $ P.doc $ P.fromList bs

basicWriterOptions :: P.WriterOptions
basicWriterOptions = (P.def P.WriterOptions)
                       { P.writerReferenceLinks = True
                       }

smartReaderOptions :: P.ReaderOptions
smartReaderOptions = (P.def P.ReaderOptions)
                       { P.readerSmart = True
                       }

removeOrphanEntries :: [D.Key Entry] -> D.SqlPersistM ()
removeOrphanEntries eKeys = do
  orphans <- D.selectList [ EntryId D./<-. eKeys ] []
  mapM_ removeEntry orphans
