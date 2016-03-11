{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImplicitParams #-}

module Blog.Util.Preprocessor where

import           Blog.Types
import           Control.Arrow    ((&&&))
import           Control.Monad
import           Data.Maybe       (fromMaybe, listToMaybe)
import           Hakyll
import           System.FilePath  ((</>))
import           Text.Parsec
import           Text.Parsec.Text
import qualified Data.Text        as T

data SampleSpec = SampleSpec  { sFile      :: FilePath
                              , sLive      :: Maybe String
                              , sKeywords  :: [(String,Maybe Int)]
                              , sLink      :: Maybe String
                              } deriving (Show)

preprocessEntry
    :: (?config :: Config)
    => T.Text
    -> Compiler T.Text
preprocessEntry t =
    fmap T.unlines . forM (T.lines t) $ \line ->
      if | "!!!" `T.isPrefixOf` line      ->
              insertSample . T.strip . T.dropWhile (== '!')     $ line
         | otherwise                      ->
              return line

insertSample
    :: (?config :: Config)
    => T.Text
    -> Compiler T.Text
insertSample sampline =
    case T.unpack <$> confCodeSamples ?config of
      Just samplesDir -> do
        case spec' of
          Left err    ->
            return $ T.pack (show err)
          Right spec  -> do
            rawSamp <- loadBody . fromFilePath $ samplesDir </> sFile spec
            return $ processSample spec (T.pack rawSamp)
      Nothing -> return "No registered code sample directory."
  where
    spec' = runP sampleSpec () (T.unpack sampline) sampline


processSample :: (?config :: Config) => SampleSpec -> T.Text -> T.Text
processSample SampleSpec{..} rawSamp = processed
  where
    rawLines = T.lines rawSamp
    zipped = zip rawLines [1..]
    blocks =
      if null sKeywords
        then
          return
            ( (snd . head &&& snd . last) zipped
            , T.unlines (map fst zipped)        )
        else
          map (uncurry (grabBlock zipped)) sKeywords
    startLine = minimum . map (fst . fst) $ blocks
    endLine   = maximum . map (snd . fst) $ blocks
    sampCode  = T.unlines . map snd $ blocks
    toHeading key val = T.pack . concat $ ["-- ", key, ": ", val, "\n"]
    sourceUrl = do
      blob  <- T.unpack <$> confBlobs       ?config
      samps <- T.unpack <$> confCodeSamples ?config
      let
        suffix  = concat ["#L",show startLine,"-",show endLine]
        suffix' = if null sKeywords then "" else suffix
      return (blob </> samps </> sFile ++ suffix')
    sourceHeading   = maybe "" (toHeading "source") sourceUrl
    interHeading =
      let maybeHeading = do
            inter <- T.unpack <$> confInteractive ?config
            live <- sLive
            return $ toHeading "interactive" (inter </> live)
      in fromMaybe "" maybeHeading
    processed =
      case sLink of
        Nothing ->
          T.concat [sourceHeading, interHeading, sampCode]
        Just l ->
          T.pack $ concat ["[",l,"]: ",fromMaybe "/not-found" sourceUrl]


grabBlock :: [(T.Text,Int)] -> String -> Maybe Int -> ((Int,Int), T.Text)
grabBlock zipped key limit = fromMaybe notFound grabbed
  where
    zDropped =
      dropWhile (not . (T.pack key `T.isInfixOf`) . fst) zipped
    -- initialIndent = T.length . T.takeWhile isSpace . fst . head $ zDropped
    (zHead,zRest) =
      span (\(l,_) -> not (T.null l)) zDropped
    zBlock        =
      takeWhile (\(l,_) -> T.null l || " " `T.isPrefixOf` l) zRest
    zBlock'       =
      reverse . dropWhile (T.null . fst) . reverse $ zBlock
    zAll =
      case limit of
        Just lim -> take lim zDropped
        Nothing  -> zHead ++ zBlock'
    startLine     = snd <$> listToMaybe zAll
    endLine       = snd <$> listToMaybe (reverse zAll)
    sampCode      = T.unlines . map fst $ zAll
    grabbed       = do
      strtl <- startLine
      endl  <- endLine
      return ((strtl, endl), sampCode)
    notFound      = ((0,0), T.pack ("Key not found: " ++ key))

sampleSpec :: Parser SampleSpec
sampleSpec = do
    link <- try . optionMaybe $ between (char '[') (char ']') (many1 (noneOf "]")) <* char ':'
    filePath <- noSpaces <?> "sample filePath"
    spaces
    keywords <- many $ do
      keyword <- char '"' *> manyTill anyChar (char '"') <?> "keyword"
      keylimit <- optionMaybe (read <$> many1 digit <?> "keyword limit")
      spaces
      return (keyword,keylimit)

    live <- optionMaybe noSpaces <?> "live url"
    let
      live' = mfilter (not . null) live

    return $ SampleSpec filePath live' keywords link
  where
    noSpaces = manyTill anyChar (space <|> ' ' <$ eof)

