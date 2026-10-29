-- | The C of the project: the sources, their headers and the precompiled
-- libraries that the manifest lists in its "extern" section, under the
-- "extern" folder of the project. They are checked against the folder, copied
-- to the output folder, and linked into a single relocatable object in which
-- only the extern functions stay global.
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
    dieWithError $ "The extern file \"" ++ file ++ "\" is in more than one extern folder of the deployment"
  copyExtern (outputFolder config </> externFolder) files
  let srcs = filter ((== ".c") . takeExtension) relFiles
      libs = filter ((== ".a") . takeExtension) relFiles
  if null relFiles
    then return Nothing
    else return . Just $ ExternBuild srcs libs

-- | Check the "extern" section against its folder, and give the files that
-- the selected platform needs, relative to the folder.
checkExtern :: TerminaConfig -> FilePath -> ExternConfig -> IO [FilePath]
checkExtern config externFolderPath (ExternConfig moduleEntries) = do
  folderExists <- doesDirectoryExist externFolderPath
  present <- if folderExists then listFiles externFolderPath else return []
  -- | The sources, the headers and the libraries of each platform have a folder each
  forM_ present $ \file ->
    unless (inItsFolder file) $
      dieWithError $ "The file \"" ++ externFolderPath </> file ++ "\" is not in its folder: "
        ++ "\"extern\" holds the C sources in \"src\", their headers in \"include\" and the libraries of each platform in \"lib/<platform>\""
  -- | Each listed path names a file, and no file twice in the same list
  listed <- fmap concat . forM (M.toList moduleEntries) $ \(modName, ExternEntry srcs hdrs libs) ->
    fmap concat . forM (listsOf srcs hdrs libs) $ \entries -> do
      let named = [ (entry, listedFiles present entry) | entry <- entries ]
      forM_ named $ \(entry, files) ->
        when (null files) $
          dieWithError $ "The extern entry of " ++ T.unpack modName ++ " lists \"" ++ externFolderPath </> entry ++ "\", which "
            ++ (if '*' `elem` entry then "matches no file" else "does not exist")
      let files = concatMap snd named
      forM_ (nub (files \\ nub files)) $ \file ->
        dieWithError $ "The extern entry of " ++ T.unpack modName ++ " names \"" ++ externFolderPath </> file ++ "\" twice"
      return files
  -- | Every file of the folder is listed, so that no C is left uncompiled and
  -- a package carries every header and library
  forM_ present $ \file ->
    unless (file `elem` listed) $
      dieWithError $ "The file \"" ++ externFolderPath </> file ++ "\" is not listed under any module in its \"extern\" section of \"termina.yaml\""
  -- | The libraries of a module are tied to the platforms they were compiled
  -- for, so a module that has some has to have them for the selected one
  forM_ (M.toList moduleEntries) $ \(modName, entry) ->
    unless (M.null (entryLibraries entry) || M.member (platform config) (entryLibraries entry)) $
      dieWithError $ "The extern entry of " ++ T.unpack modName ++ " has no libraries for the platform \"" ++ T.unpack (platform config) ++ "\""
  return . nub $ concat
    [ concatMap (listedFiles present . (externSourceFolder </>)) srcs
      ++ concatMap (listedFiles present . (externHeaderFolder </>)) hdrs
      ++ [ externLibraryFolder </> T.unpack (platform config) </> lib | lib <- M.findWithDefault [] (platform config) libs ]
    | ExternEntry srcs hdrs libs <- M.elems moduleEntries ]

  where

    listsOf srcs hdrs libs =
      [map (externSourceFolder </>) srcs, map (externHeaderFolder </>) hdrs]
      ++ [ map ((externLibraryFolder </> T.unpack plt) </>) pltLibs | (plt, pltLibs) <- M.toList libs ]

    inItsFolder file =
      case splitDirectories file of
        (folder : _ : _) | folder == externSourceFolder -> takeExtension file == ".c"
        (folder : _ : _) | folder == externHeaderFolder -> takeExtension file == ".h"
        (folder : plt : _ : _) | folder == externLibraryFolder -> checkPlatform plt /= Nothing && takeExtension file == ".a"
        _ -> False

-- | The files under a folder, as paths relative to it.
listFiles :: FilePath -> IO [FilePath]
listFiles root = go ""

  where

    go rel = do
      entries <- listDirectory (root </> rel)
      concat <$> forM entries (\entry -> do
        let relEntry = if null rel then entry else rel </> entry
        isDir <- doesDirectoryExist (root </> relEntry)
        if isDir then go relEntry else return [relEntry])

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

-- | The names of the extern functions of the project, which are the only
-- global symbols that its C keeps.
externSymbols :: LoweredProject -> [Identifier]
externSymbols project =
  S.toList . S.fromList $
    [ ident
    | m <- M.elems project
    , ExternFunction ident _ _ _ _ _ <- loweredAST (metadata m) ]
