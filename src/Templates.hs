module Templates where

import RIO
import RIO.Directory (listDirectory)
import RIO.FilePath (takeBaseName, (</>))
import RIO.Text (pack)
import Types

loadTemplates :: IO [MessageTemplate]
loadTemplates = do
  filenames <- listDirectory $ ".invoker" </> "templates"
  forM filenames $ \filename -> do
    contents <- readFileUtf8 $ ".invoker" </> "templates" </> filename
    return $ MessageTemplate (filename & takeBaseName & pack) contents
