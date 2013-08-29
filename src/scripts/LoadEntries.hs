
-- import Debug.Trace
import Control.Applicative                    ((<$>), pure)
import Control.Monad.IO.Class
import Data.Monoid
import Data.Time
-- import Data.Time.Format
import System.Directory                       (getDirectoryContents)
import System.FilePath                        ((</>))
import System.Locale
import Web.Blog.Database
import Web.Blog.Models
import qualified Data.Map                     as M
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
             deriving ( Show, Eq, Read )
data MetaValue = MetaValueTime UTCTime
               | MetaValueTags [Tag]
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

      title = writeMarkdownBlocks $ pure $ P.Plain headerBlocks
      entryMarkdown = writeMarkdownBlocks body

    metas <- mapM processMeta $ defListList metaBlock

    liftIO $ print title
    liftIO $ print metas
    liftIO $ putStrLn entryMarkdown
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

processMeta :: ([P.Inline], [[P.Block]]) -> D.SqlPersistM (MetaKey, MetaValue)
processMeta (keyBlocks, valBlockss) = do
    metaValue <- case metaKey of
      MetaKeyPostDate -> readTimeBlockss valBlockss
      MetaKeyModifiedTime -> readTimeBlockss valBlockss
      MetaKeyCreateTime -> readTimeBlockss valBlockss
      MetaKeyOriginalTitle -> return $ MetaValueString $ renderBlocks $ head valBlockss
      _ -> return $ MetaValueString "hey"

    return (metaKey, metaValue)
  where
    metaKey = read $ (++) "MetaKey" $ renderBlocks $ pure $ P.Plain keyBlocks
    readTime' = readTime defaultTimeLocale "%Y/%m/%d %X"
    readTimeBlockss bss = return $ MetaValueTime $ readTime' $ renderBlocks $ head bss


renderBlocks :: [P.Block] -> String
renderBlocks bs = P.writeMarkdown (P.def P.WriterOptions) $ P.Pandoc emptyMeta bs
  where
    emptyMeta = P.Meta mempty mempty mempty
