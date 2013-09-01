
import Control.Monad
-- import Data.Time.Format
-- import Debug.Trace
import Control.Applicative                    ((<$>), pure)
import Control.Monad.IO.Class
import Data.Maybe                             (fromJust)
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
import qualified Text.Pandoc.Readers.Markdown as PM

entriesDir :: FilePath
entriesDir = "copy/entries"

data MetaKey = MetaKeyTags
             | MetaKeyCategories
             | MetaKeySeries
             | MetaKeyCreateTime
             | MetaKeyPostDate
             | MetaKeyModifiedTime
             | MetaKeyOriginalTitle
             deriving ( Show, Eq, Read, Ord )
data MetaValue = MetaValueTime UTCTime
               | MetaValueTags [D.Entity Tag]
               | MetaValueString String
               deriving ( Show, Eq, Read )

type EntryMeta = M.Map MetaKey MetaValue

main :: IO ()
main = do
  -- runDB blogMigrate
  entryFiles <-
    map (entriesDir </>) . filter (`notElem` [".",".."]) <$>
      getDirectoryContents "copy/entries"
  runDB $ mapM_ processEntryFile entryFiles

-- TODO: emit warnings, wrap in writer monad.
processEntryFile :: FilePath -> D.SqlPersistM ()
processEntryFile entryFile = do
    (P.Pandoc pandocMeta contents, _) <- liftIO $
      readMarkdown <$> readFile entryFile
    let
      writeMarkdownBlocks bs = writeMarkdown $ P.Pandoc pandocMeta bs
      (P.Header _ _ headerBlocks, metaBody) = stripAndTake isHeader contents
      (metaBlock, body) = stripAndTake isDefList metaBody

      title = T.pack $ writeMarkdownBlocks $ pure $ P.Plain headerBlocks
      entryMarkdown = writeMarkdownBlocks body

    metas <- fmap M.fromList $ mapM processMeta $ defListList metaBlock

    -- liftIO $ print title
    -- liftIO $ print metas
    -- liftIO $ putStrLn entryMarkdown
    --
    -- now <- liftIO getCurrentTime

    entryEntity <- do
      entryMaybe <- D.getBy $ UniqueEntryTitle title
      case entryMaybe of
        Just e -> return e
        Nothing -> do
          let
            newEntry = Entry
                         title
                         (T.pack entryMarkdown)
                         Nothing
                         Nothing
                         Nothing
          k <- D.insert newEntry
          return $ D.Entity k newEntry

    void $ M.traverseWithKey (applyMetas entryEntity) metas

    liftIO $ print $ D.entityVal entryEntity

    return ()
  where
    readMarkdown = PM.readMarkdownWithWarnings (P.def P.ReaderOptions)
    writeMarkdown = P.writeMarkdown (P.def P.WriterOptions)
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
    applyMetas (D.Entity entryKey _) MetaKeyCreateTime (MetaValueTime t) =
      void $ D.update entryKey [EntryCreatedAt D.=. Just t]
    applyMetas (D.Entity entryKey _) MetaKeyPostDate (MetaValueTime t) =
      void $ D.update entryKey [EntryPostedAt D.=. Just t]
    applyMetas (D.Entity entryKey _) MetaKeyModifiedTime (MetaValueTime t) =
      void $ D.update entryKey [EntryModifiedAt D.=. Just t]
    applyMetas (D.Entity entryKey _) _ (MetaValueTags ts) =
      forM_ ts $ \(D.Entity tKey _) ->
        void $ D.insertUnique $ EntryTag entryKey tKey

processMeta :: ([P.Inline], [[P.Block]]) -> D.SqlPersistM (MetaKey, MetaValue)
processMeta (keyBlocks, valBlockss) = do
    metaValue <- case metaKey of
      MetaKeyPostDate -> readTimeBlockss valBlockss
      MetaKeyModifiedTime -> readTimeBlockss valBlockss
      MetaKeyCreateTime -> readTimeBlockss valBlockss
      MetaKeyOriginalTitle -> return $ MetaValueString $ renderBlocks $ head valBlockss
      MetaKeyCategories -> generateTags CategoryTag valBlockss
      MetaKeyTags -> generateTags GeneralTag valBlockss
      MetaKeySeries -> generateTags SeriesTag valBlockss

    return (metaKey, metaValue)
  where
    metaKey = read $ (++) "MetaKey" $ renderBlocks $ pure $ P.Plain keyBlocks
    readTime' = readTime defaultTimeLocale "%Y/%m/%d %X"
    readTimeBlockss bss = return $ MetaValueTime $ readTime' $ renderBlocks $ head bss
    generateTags :: TagType -> [[P.Block]] -> D.SqlPersistM MetaValue
    generateTags tt bss = MetaValueTags <$>
      mapM (generateTag . T.pack . renderBlocks) bss
      where
        generateTag :: T.Text -> D.SqlPersistM (D.Entity Tag)
        generateTag label = do
          tag <- D.getBy $ UniqueLabelType label tt
          case tag of
            Just t -> return t
            Nothing -> fmap fromJust $ insertTag' $ PreTag label tt Nothing


renderBlocks :: [P.Block] -> String
renderBlocks bs = P.writeMarkdown (P.def P.WriterOptions) $ P.Pandoc emptyMeta bs
  where
    emptyMeta = P.Meta mempty mempty mempty
