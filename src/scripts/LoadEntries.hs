
import Control.Applicative                    ((<$>))
import Control.Monad.IO.Class
import System.Directory                       (getDirectoryContents)
import System.FilePath                        ((</>))
import Web.Blog.Database
import qualified Database.Persist.Postgresql  as D
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.Readers.Markdown as PM

entriesDir :: FilePath
entriesDir = "copy/entries"

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
    (P.Pandoc pandocMeta entryContents, _) <- liftIO $
      readMarkdown <$> readFile entryFile
    let
      (entryTitleBlock, entryMetaBody) = stripAndTake isHeader entryContents
      (entryMetaBlock, entryBody) = stripAndTake isDefList entryMetaBody
      entryBodyMarkdown = writeMarkdown $ P.Pandoc pandocMeta entryBody
    liftIO $ print entryTitleBlock
    liftIO $ print entryMetaBlock
    liftIO $ print entryBodyMarkdown
    return ()
  where
    readMarkdown = PM.readMarkdownWithWarnings (P.def P.ReaderOptions)
    writeMarkdown = P.writeMarkdown (P.def P.WriterOptions)
    stripAndTake p l = (taken, rest)
      where
        stripped = dropWhile (not . p) l
        taken = head stripped
        rest = tail stripped
    isHeader b =
      case b of
        P.Header n _ _ -> n == 1
        _              -> False
    isDefList b =
      case b of
        P.DefinitionList _ -> True
        _                  -> False



        -- copyPandoc = P.readMarkdown (P.def P.ReaderOptions) copyMarkdown

