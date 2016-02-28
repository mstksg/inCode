{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import           Cases
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Default
import           Data.Foldable
import           Data.List
import           Data.Maybe
import           Options.Applicative
import           System.Directory
import           System.FilePath
import           System.IO.Error
import           Text.Pandoc
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector               as V

data EntryPieces = EP { epTitle :: String
                      , epMeta  :: String
                      , epBody  :: String
                      }
  deriving (Show)

data Opts = O { oSource :: FilePath
              , oSlugs  :: FilePath
              , oTarget :: FilePath
              , oForce  :: Bool
              }

parseOpts :: Parser Opts
parseOpts = O <$> strOption ( long "input"
                           <> short 'i'
                           <> metavar "SOURCE"
                           <> help "Source of old-style entries"
                           <> value "copy/entries-old"
                           <> showDefaultWith id
                            )
              <*> strOption ( long "slugs"
                           <> short 's'
                           <> metavar "SLUGS"
                           <> help "JSON file containing list of identifiers and slugs"
                           <> value "config/old-slugs.json"
                           <> showDefaultWith id
                            )
              <*> strOption ( long "output"
                           <> short 'o'
                           <> metavar "TARGET"
                           <> help "Target folder for new-style entries"
                           <> value "copy/entries"
                           <> showDefaultWith id
                            )
              <*> switch    ( long "force"
                           <> short 'f'
                           <> help "Force overwrite if file already exists at target folder"
                            )

main :: IO ()
main = do
    O{..} <- execParser $ info (helper <*> parseOpts)
                            ( fullDesc
                           <> progDesc "Migrate entry files from old-style to new-style"
                           <> header "blog-migrate-entries - blog entry migration system"
                            )

    createDirectoryIfMissing True oTarget

    slugMap <- (maybe M.empty parseSlugs . A.decode' <$> BL.readFile oSlugs)
                 `catch` \e -> if isDoesNotExistError e
                                 then return M.empty
                                 else throwIO e
    putStrLn $ "Loaded slugs for " ++ show (M.size slugMap) ++ " entries."

    fns <- getDirectoryContents oSource
    forM_ (filter (not . ("." `isPrefixOf`)) fns) $ \fn -> do
      let oldFn = oSource </> fn
          newFn = oTarget </> fn
      isFile <- doesFileExist oldFn
      when isFile $ do
        putStrLn $ "Processing " ++ oldFn
        contents <- lines <$> readFile oldFn
        let EP{..} = flip evalState contents $ do
                       t <- concat . take 1 <$> state (break null)
                       modify (drop 1)
                       m <- intercalate "\n" <$> state (break null)
                       modify (drop 1)
                       b <- intercalate "\n" <$> state (,[])
                       return $ EP t m b
            metas = case readMarkdown opts epMeta of
                      Right (Pandoc _ m) -> do
                        DefinitionList ds <- m
                        (kIs, vIss) <- ds
                        let k = T.unpack
                              . spinalize
                              . T.pack
                              . writePlain def
                              $ Pandoc mempty [Plain kIs]
                            v = map (writePlain def . Pandoc mempty) vIss
                        return (k, v)
                      Left _ -> []
            slugs = fromMaybe [] $ do
              [ident]   <- lookup "identifier" metas
              Slugs{..} <- M.lookup ident slugMap
              let currSlug = (("slug",) . (:[])) <$> slugsCurr
                  oldSlugs = ("old-slugs",) . S.toList  $ slugsOld
              return $ toList currSlug <> [oldSlugs]
            out = unlines
                    [ "---"
                    , "title: " ++ epTitle
                    , intercalate "\n"
                        . map (\(k, vs) -> k ++ ": " ++ intercalate ", " vs)
                        $ (metas ++ slugs)
                    , "---"
                    , ""
                    , epBody
                    ]

        hasFile <- doesFileExist newFn

        if hasFile
          then  if oForce
                  then do
                    putStrLn $ newFn ++ " already exists; overwriting."
                    writeFile newFn out
                  else
                    putStrLn $ newFn ++ " already exists."
          else
            writeFile newFn out

  where
    opts = def { readerExtensions = S.insert Ext_compact_definition_lists pandocExtensions }

data Slugs = Slugs { slugsCurr :: Maybe String
                   , slugsOld  :: S.Set String
                   }
  deriving (Show)

instance Monoid Slugs where
    mempty = Slugs Nothing S.empty
    mappend s1 s2 =
      case slugsCurr s2 of
        Just c2 ->
          Slugs (Just c2) $
            case slugsCurr s1 of
              Just c1 -> S.insert c1 olds
              Nothing -> olds
        Nothing ->
          Slugs (slugsCurr s1) olds
      where
        olds = slugsOld s1 <> slugsOld s2

parseSlugs :: V.Vector A.Array -> M.Map String Slugs
parseSlugs = M.fromListWith (<>)
           . mapMaybe f
           . V.toList
  where
    f :: V.Vector A.Value -> Maybe (String, Slugs)
    f v = do
      A.String ident <- v V.!? 0
      A.String slug  <- v V.!? 1
      let curr = case v V.!? 2 of
                   Just (A.Bool c) -> c
                   _              -> False

      let slugStr = T.unpack slug
      return . (T.unpack ident,) $
        if curr
          then Slugs (Just slugStr) S.empty
          else Slugs Nothing (S.singleton slugStr)



-- | SQL for getting the right format:
--
-- select
--     entry.identifier, slug.slug, slug.is_current
--   from entry
--   join slug
--     on slug.entry_id = entry.id
--   order by
--     entry.identifier, slug.id;
