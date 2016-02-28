
main :: IO ()
main = do
    fns <- getDirectoryContents "copy/entries-old"
    forM_ (filter (`notElem` [".",".."]) fns) $ \fn -> do
      isFile <- liftIO $ doesFileExist fullFn
      when isFile $ do
        putStrLn fn
