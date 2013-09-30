
module Development.Blog.Util.LoadTags (loadTags) where

import Control.Applicative                    ((<$>))
import Control.Monad
import Control.Monad.IO.Class
import Data.List                              (isPrefixOf)
import System.Directory                       (getDirectoryContents)
import System.FilePath                        ((</>))
import Web.Blog.Database
import Web.Blog.Models.Types
import qualified Database.Persist.Postgresql  as D
import qualified Text.Pandoc                  as P
import qualified Text.Pandoc.Readers.Markdown as PM

loadTags :: FilePath -> IO ()
loadTags tagsDir = do
  runDB blogMigrate

  -- let
  --   generalsDir = tagsDir </> "tags"
  --   categoriesDir = tagsDir </> "categories"
  --   seriesDir = tagsDir </> "series"

  tagFiless <-
    forM ["tags","categories","series"] $ \dirName -> do
      let
        dir = tagsDir </> dirName
      map (dir </>) . filter (not . isPrefixOf ".") <$>
        getDirectoryContents dir

  forM_ (zip tagFiless [GeneralTag ..]) $ \(tagFiles, tagType) ->
    mapM_ (processTagFile tagType) tagFiles

processTagFile :: TagType -> FilePath -> IO ()
processTagFile tagType tagFile = do
    (P.Pandoc _ contents, _) <- liftIO $
      readMarkdown <$> readFile tagFile

    
    return ()
  where
    readMarkdown = PM.readMarkdownWithWarnings (P.def P.ReaderOptions)
