{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Blog.Util.Preprocessor where

import           Blog.Types
import           Blog.Util
import           Control.Applicative.Lift
import           Control.Arrow            ((&&&))
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Data.Traversable
import           Hakyll
import           System.FilePath          ((</>))
import           Text.Parsec
import           Text.Parsec.Text
import qualified Data.Text                as T

data SampleSpec = SampleSpec  { sFile      :: FilePath
                              , sLive      :: Maybe String
                              , sKeywords  :: [(String,Maybe Int)]
                              , sLink      :: Maybe String
                              } deriving (Show)

preprocessEntry
    :: (?config :: Config)
    => T.Text
    -> Compiler T.Text
preprocessEntry t = fmap (T.intercalate "\n") . forM (T.lines t) $ \line ->
    let (pref,line') = T.span isSpace line
        samp = insertSample . T.strip . T.dropWhile (== '!') $ line'
    in  if "!!!" `T.isPrefixOf` line'
          then T.intercalate "\n" . map (pref <>) . T.lines <$> samp
          else return line

insertSample
    :: (?config :: Config)
    => T.Text
    -> Compiler T.Text
insertSample sampline =
    case T.unpack <$> confCodeSamples ?config of
      Just samplesDir ->
        case spec' of
          Left err    ->
            fail $ show err
          Right spec  -> do
            rawSamp <- loadBody . fromFilePath $ samplesDir </> sFile spec
            case processSample spec (T.pack rawSamp) of
              Left errs -> fail $ "Key(s) not found: " ++ intercalate ", " errs
              Right res -> return res
      Nothing -> fail "No registered code sample directory."
  where
    spec' = runP sampleSpec () (T.unpack sampline) sampline

processSample :: (?config :: Config) => SampleSpec -> T.Text -> Either [String] T.Text
processSample SampleSpec{..} rawSamp = do
    blocks <- if null sKeywords
      then Right [( (snd . head &&& snd . last) zipped
                  , T.intercalate "\n" (map fst zipped)        )
                 ]
      else
        runErrors . for sKeywords $ \(k,l) ->
          maybe (failure [k]) pure $ grabBlock zipped k l
    let startLine = minimum . map (fst . fst) $ blocks
        endLine   = maximum . map (snd . fst) $ blocks
        sampCode  = T.intercalate "\n\n" . map snd $ blocks
        sourceUrl = do
          blob  <- T.unpack <$> sourceBlobs     ?config
          samps <- T.unpack <$> confCodeSamples ?config
          let suffix  = concat ["#L",show startLine,"-L",show endLine]
              suffix' = if null sKeywords then "" else suffix
          return (blob </> samps </> sFile ++ suffix')
        sourceHeading   = maybe "" (toHeading "source") sourceUrl
    return $ case sLink of
      Nothing ->
        T.concat [sourceHeading, interHeading, "\n", sampCode]
      Just l ->
        T.pack $ concat ["[",l,"]: ",fromMaybe "/not-found" sourceUrl]
  where
    rawLines = T.lines rawSamp
    zipped = zip rawLines [1..]
    toHeading key val = T.pack . concat $ ["-- ", key, ": ", val, "\n"]
    interHeading = fromMaybe "" $ do
      inter <- T.unpack <$> confInteractive ?config
      live <- sLive
      return $ toHeading "interactive" (inter </> live)


grabBlock :: [(T.Text,Int)] -> String -> Maybe Int -> Maybe ((Int,Int), T.Text)
grabBlock zipped key limit = do
    strtl <- startLine
    endl  <- endLine
    return ((strtl, endl), sampCode)
  where
    zDropped =
      dropWhile (not . (T.pack key `T.isPrefixOf`) . T.strip . fst) zipped
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
    sampCode      = T.intercalate "\n" . map fst $ zAll

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

