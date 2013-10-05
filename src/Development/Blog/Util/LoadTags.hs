
module Development.Blog.Util.LoadTags (loadTags) where

-- import Control.Monad.IO.Class
-- import qualified Data.Foldable             as Fo
import Control.Applicative                    ((<$>))
import Control.Monad
import Data.List                              (isPrefixOf)
import Data.Maybe
import System.Directory                       (getDirectoryContents, doesDirectoryExist)
import System.FilePath                        ((</>), takeBaseName)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import qualified Data.Text                    as T
import qualified Data.Traversable             as Tr
import qualified Database.Persist.Postgresql  as D
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.Builder          as P
import qualified Text.Pandoc.Readers.Markdown as PM

loadTags :: FilePath -> IO ()
loadTags tagsDir = do
  runDB blogMigrate

  tagFiless <-
    forM ["tags","categories","series"] $ \dirName -> do
      let
        dir = tagsDir </> dirName

      dirExists <- doesDirectoryExist dir

      if dirExists
        then
          map (dir </>) . filter (not . isPrefixOf ".") <$>
            getDirectoryContents dir
        else
          return []

  foundTags <- forM (zip tagFiless [GeneralTag ..]) $ \(tagFiles, tagType) ->
    mapM (processTagFile tagType) tagFiles

  runDB $ clearOrphanTags $ catMaybes $ join foundTags

processTagFile :: TagType -> FilePath -> IO (Maybe (D.Key Tag))
processTagFile tagType tagFile = do
    (P.Pandoc _ rawContents, _) <- readMarkdown <$> readFile tagFile
    
    let
      contents = T.pack $ renderBlocks $
        case rawContents of
          P.Header 1 _ _:bs -> bs
          _ -> rawContents

    let
      slug = T.pack $ takeBaseName tagFile

    tag <- runDB $ D.getBy $ UniqueSlugType slug tagType

    Tr.forM tag $ \(D.Entity tagKey tagVal) -> do
      print tagVal
      runDB $ D.update tagKey [ TagDescription D.=. Just contents ]
      return tagKey

  where
    readMarkdown :: String -> (P.Pandoc, [String])
    readMarkdown = PM.readMarkdownWithWarnings (P.def P.ReaderOptions)
    renderBlocks bs =
      P.writeMarkdown basicWriterOptions $ P.doc $ P.fromList bs

basicWriterOptions :: P.WriterOptions
basicWriterOptions = (P.def P.WriterOptions)
                       { P.writerReferenceLinks = True
                       }

clearOrphanTags :: [D.Key Tag] -> D.SqlPersistM ()
clearOrphanTags tagKeys =
    D.updateWhere
      [ TagId D./<-. tagKeys ]
      [ TagDescription D.=. Nothing ]
