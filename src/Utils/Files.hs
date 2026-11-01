-- | The files of the folders of a project.
module Utils.Files (
    filesUnder,
    isHidden
) where

import Control.Monad
import Data.List (isPrefixOf, sort)
import System.Directory
import System.FilePath

-- | The files under a folder, as paths relative to it, sorted. Hidden files
-- and folders are left out, and a folder that does not exist has none.
filesUnder :: FilePath -> IO [FilePath]
filesUnder root = do
    exists <- doesDirectoryExist root
    if exists then sort <$> go "" else return []

  where

    go rel = do
        entries <- filter (not . isHidden) <$> listDirectory (root </> rel)
        concat <$> forM entries (\entry -> do
            let relEntry = if null rel then entry else rel </> entry
            isDir <- doesDirectoryExist (root </> relEntry)
            if isDir then go relEntry else return [relEntry])

-- | Whether a path is hidden: its name, or the name of one of its folders,
-- starts with a dot. The build leaves hidden files out.
isHidden :: FilePath -> Bool
isHidden = any (\part -> "." `isPrefixOf` part && part `notElem` [".", ".."]) . splitDirectories
