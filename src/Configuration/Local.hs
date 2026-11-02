-- | The libraries that a project takes from local folders, its path
-- dependencies: read from disk and checked as a fetched package would be,
-- following their own path dependencies.
module Configuration.Local (
    localFolderOf,
    readLocalPackages,
    reachedLocals
) where

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath

import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest
import Configuration.Package
import Configuration.Validate (checkPackage)

-- | The folder that a path dependency names, written in the manifest of a
-- folder: relative to that folder, without the steps that go down and up.
localFolderOf :: FilePath -> FilePath -> FilePath
localFolderOf parent path = tidy (splitDirectories (normalise (parent </> path)))

  where

    tidy = joinPath . reverse . foldl step []

    step (x : xs) ".." | x /= ".." = xs
    step acc "." = acc
    step acc segment = segment : acc

-- | The folders of the path dependencies of a local package.
pathFolders :: Package -> [FilePath]
pathFolders local = [localFolderOf (packageFolder local) path | DepPath path <- manifestDependencies (packageManifest local)]

-- | The packages of a map that the path dependencies of some folders reach,
-- directly or through other packages of the map.
reachedLocals :: M.Map FilePath Package -> [(FilePath, FilePath)] -> M.Map FilePath Package
reachedLocals locals starts = go M.empty [localFolderOf parent path | (parent, path) <- starts]

  where

    go found [] = found
    go found (folder : rest)
        | M.member folder found = go found rest
        | otherwise = case M.lookup folder locals of
            Nothing -> go found rest
            Just local -> go (M.insert folder local found) (rest ++ pathFolders local)

-- | The local packages that the path dependencies of some folders reach,
-- each one with the dependencies by path of its own manifest, by folder.
readLocalPackages :: [(FilePath, FilePath)] -> IO (Either [String] (M.Map FilePath Package))
readLocalPackages starts = go M.empty [localFolderOf parent path | (parent, path) <- starts]

  where

    go found [] = return (Right found)
    go found (folder : rest)
        | M.member folder found = go found rest
        | otherwise = do
            read' <- readLocal folder
            case read' of
                Left errs -> return (Left errs)
                Right local -> go (M.insert folder local found) (rest ++ pathFolders local)

    readLocal folder = do
        exists <- doesDirectoryExist folder
        if not exists
            then return (Left ["the dependency " ++ folder ++ " does not exist. In a container, the folder has to be mounted too."])
            else do
                manifest <- readManifest folder
                files <- concat <$> mapM (tree folder) ["src", "extern"]
                let inFolder = map ((folder ++ ": ") ++)
                return $ case packageFromFiles (FromFolder folder) folder (manifest ++ files) of
                    Left err -> Left (inFolder [err])
                    Right package -> case checkPackage package of
                        [] -> Right package
                        errs -> Left (inFolder errs)

    readManifest folder = do
        exists <- doesFileExist (folder </> "termina.yaml")
        if exists then (\bytes -> [FetchedFile "termina.yaml" "100644" bytes]) <$> BS.readFile (folder </> "termina.yaml") else return []

    -- | The files under a folder of the package, a symbolic link as git
    -- would give it.
    tree folder top = do
        exists <- doesDirectoryExist (folder </> top)
        if not exists then return [] else walk folder [top]

    walk folder segments = do
        let path = intercalate "/" segments
            full = folder </> joinPath segments
        isLink <- pathIsSymbolicLink full
        isDir <- doesDirectoryExist full
        if isLink
            then return [FetchedFile path "120000" BS.empty]
            else if isDir
                then do
                    entries <- listDirectory full
                    concat <$> forM entries (\e -> walk folder (segments ++ [e]))
                else (\bytes -> [FetchedFile path "100644" bytes]) <$> BS.readFile full
