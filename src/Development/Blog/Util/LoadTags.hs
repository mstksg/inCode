
module Development.Blog.Util.LoadTags (loadTags) where

-- import Control.Monad.IO.Class
-- import qualified Text.Pandoc                  as P
-- import qualified Text.Pandoc.Readers.Markdown as PM
import Control.Applicative                       ((<$>))
import Control.Monad
import Data.List                                 (isPrefixOf)
import System.Directory                          (getDirectoryContents, doesDirectoryExist)
import System.FilePath                           ((</>), takeBaseName)
import Web.Blog.Database
import Web.Blog.Models
import Web.Blog.Models.Types
import qualified Data.Foldable                   as Fo
import qualified Data.Text                       as T
import qualified Database.Persist.Postgresql     as D

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

  forM_ (zip tagFiless [GeneralTag ..]) $ \(tagFiles, tagType) ->
    mapM_ (processTagFile tagType) tagFiles

processTagFile :: TagType -> FilePath -> IO ()
processTagFile tagType tagFile = do
    contents <- T.pack <$> readFile tagFile

    let
      slug = T.pack $ takeBaseName tagFile

    tag <- runDB $ D.getBy $ UniqueSlugType slug tagType

    Fo.forM_ tag $ \(D.Entity tagKey tagVal) -> do
      print tagVal
      runDB $ D.update tagKey [ TagDescription D.=. Just contents ]
