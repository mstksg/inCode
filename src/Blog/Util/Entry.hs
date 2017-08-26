
module Blog.Util.Entry (
    entryContentsText
  , entryLedeText
  ) where

import           Blog.Types
import           Blog.Util
import qualified Data.Text   as T
import qualified Text.Pandoc as P


entryContentsText :: Entry -> T.Text
entryContentsText = T.pack . P.writeMarkdown entryWriterOpts . entryContents

entryLedeText :: Entry -> T.Text
entryLedeText = T.pack . P.writeMarkdown entryWriterOpts . entryLede


