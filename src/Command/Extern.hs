-- | The C of the project: the sources and the precompiled libraries that the
-- manifest lists in its "extern" section, all of them under the "extern"
-- folder of the project. They are checked against the folder, copied to the
-- output folder, and linked into a single relocatable object in which only
-- the extern functions stay global.
module Command.Extern (
    ExternBuild(..),
    externFolder,
    prepareExtern,
    externSymbols
) where

import Configuration.Configuration
import Configuration.Platform
import Command.Types
import Command.Utils
import Core.AST
import Modules.Modules

import Control.Monad
import qualified Data.ByteString as BS
import Data.List (nub, (\\))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.FilePath

-- | What the build of the C of the project needs: the sources and the
-- libraries of the selected platform, as paths relative to the "extern"
-- folder.
data ExternBuild = ExternBuild {
    buildSources :: [FilePath],
    buildLibraries :: [FilePath]
  } deriving (Show)

externFolder :: FilePath
externFolder = "extern"

-- | Check each "extern" section of the manifest against its folder, the one
-- of the project and, in a test, the one of the test, and copy the files that
-- the selected platform needs to the output folder. A file that both folders
-- have would overwrite the other in the output folder, so it is an error. A
-- deployment with no C returns Nothing.
prepareExtern :: TerminaConfig -> [(FilePath, ExternConfig)] -> IO (Maybe ExternBuild)
prepareExtern config folders = do
  checked <- mapM (\(folder, extern) -> map (folder,) <$> checkExtern config folder extern) folders
  let files = concat checked
      relFiles = map snd files
  forM_ (relFiles \\ nub relFiles) $ \file ->
    die . errorMessage $ "The extern file \"" ++ file ++ "\" is in more than one extern folder of the deployment"
  copyExtern (outputFolder config </> externFolder) files
  let srcs = filter ((== ".c") . takeExtension) relFiles
      libs = filter ((== ".a") . takeExtension) relFiles
  if null relFiles
    then return Nothing
    else return . Just $ ExternBuild srcs libs

-- | Check the "extern" section against its folder, and give the files that
-- the selected platform needs, relative to the folder.
checkExtern :: TerminaConfig -> FilePath -> ExternConfig -> IO [FilePath]
checkExtern config externFolderPath (ExternConfig srcs libs) = do
  folderExists <- doesDirectoryExist externFolderPath
  presentSources <- if folderExists then listCSources externFolderPath else return []
  -- | Every source of the folder is listed, so that no C is left uncompiled
  forM_ presentSources $ \src ->
    unless (src `elem` srcs) $
      die . errorMessage $ "The C source \"" ++ externFolderPath </> src ++ "\" is not listed in its \"extern\" section of \"termina.yaml\""
  forM_ srcs $ \src -> do
    unless (takeExtension src == ".c") $
      die . errorMessage $ "The extern source \"" ++ src ++ "\" is not a C source file"
    exists <- doesFileExist (externFolderPath </> src)
    unless exists $
      die . errorMessage $ "The extern source \"" ++ externFolderPath </> src ++ "\" does not exist"
  forM_ (M.toList libs) $ \(pltName, pltLibs) -> do
    when (checkPlatform (T.unpack pltName) == Nothing) $
      die . errorMessage $ "Unknown platform \"" ++ T.unpack pltName ++ "\" in the extern libraries"
    forM_ pltLibs $ \lib -> do
      unless (takeExtension lib == ".a") $
        die . errorMessage $ "The extern library \"" ++ lib ++ "\" is not a static library"
      exists <- doesFileExist (externFolderPath </> lib)
      unless exists $
        die . errorMessage $ "The extern library \"" ++ externFolderPath </> lib ++ "\" does not exist"
  -- | The libraries of a project are tied to the platforms they were compiled
  -- for, so a project that has some has to have them for the selected one
  pltLibs <- if M.null libs
    then return []
    else case M.lookup (platform config) libs of
      Just ls -> return ls
      Nothing -> die . errorMessage $ "The folder \"" ++ externFolderPath ++ "\" provides no extern libraries for the platform \"" ++ T.unpack (platform config) ++ "\""
  return (srcs ++ pltLibs)

-- | The C sources under a folder, as paths relative to it.
listCSources :: FilePath -> IO [FilePath]
listCSources root = go ""

  where

    go rel = do
      entries <- listDirectory (root </> rel)
      concat <$> forM entries (\entry -> do
        let relEntry = if null rel then entry else rel </> entry
        isDir <- doesDirectoryExist (root </> relEntry)
        if isDir
          then go relEntry
          else return [relEntry | takeExtension entry == ".c"])

-- | Copy the files, each one given by its folder and its path relative to it,
-- to the output folder. A file is written only when its content changed, so
-- that its date only moves when it has to, and the files of the output folder
-- that are no longer listed are removed.
copyExtern :: FilePath -> [(FilePath, FilePath)] -> IO ()
copyExtern destination folderFiles = do
  let files = map snd folderFiles
  forM_ folderFiles $ \(folder, file) -> do
    let source = folder </> file
        target = destination </> file
    content <- BS.readFile source
    targetExists <- doesFileExist target
    same <- if targetExists then (== content) <$> BS.readFile target else return False
    unless same $ do
      createDirectoryIfMissing True (takeDirectory target)
      BS.writeFile target content
  destinationExists <- doesDirectoryExist destination
  when destinationExists $ do
    present <- listFiles destination
    let listed = S.fromList files
    forM_ present $ \file ->
      unless (file `S.member` listed) $ removeFile (destination </> file)

  where

    listFiles root = go root ""

    go root rel = do
      entries <- listDirectory (root </> rel)
      concat <$> forM entries (\entry -> do
        let relEntry = if null rel then entry else rel </> entry
        isDir <- doesDirectoryExist (root </> relEntry)
        if isDir then go root relEntry else return [relEntry])

-- | The names of the extern functions of the project, which are the only
-- global symbols that its C keeps.
externSymbols :: LoweredProject -> [Identifier]
externSymbols project =
  S.toList . S.fromList $
    [ ident
    | m <- M.elems project
    , ExternFunction ident _ _ _ _ _ <- loweredAST (metadata m) ]
