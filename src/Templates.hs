module Templates where

import RIO
import RIO.Directory (listDirectory)
import RIO.FilePath (takeBaseName, (</>))
import RIO.Text (pack)
import Types

loadTemplates :: (MonadIO m) => m [MessageTemplate]
loadTemplates = do
  filenames <- listDirectory $ ".invoker" </> "templates"
  forM filenames $ \filename -> do
    contents <- readFileUtf8 $ ".invoker" </> "templates" </> filename
    pure $ MessageTemplate (filename & takeBaseName & pack) contents
