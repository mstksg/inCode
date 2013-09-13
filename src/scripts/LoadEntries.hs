
import Development.Blog.Util.LoadEntries

entriesDir :: FilePath
entriesDir = "copy/entries"

main :: IO ()
main = loadEntries entriesDir
