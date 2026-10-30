-- | The C of the project: the sources, their headers and the precompiled
-- libraries that the manifest lists in its "extern" section, under the
-- "extern" folder of the project. They are checked against the folder, copied
-- to the output folder, and linked into a single relocatable object in which
-- only the extern functions stay global.
module Command.Extern (
    ExternBuild(..),
    externFolder,
    prepareExtern,
    checkExternModules,
    externSymbols
) where

import Configuration.Configuration
import Configuration.Platform
import Command.Common (parseTerminaFile)
import Command.Types
import Command.Utils
import Configuration.Namespace (locationLine)
import Core.AST
import Modules.Modules
import Modules.Utils (qualifiedToModuleName)

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
-- the modules of the build need on the selected platform to the output
-- folder. A file that both folders have would overwrite the other in the
-- output folder, so it is an error. A deployment with no C returns Nothing.
prepareExtern :: TerminaConfig -> ParsedProject -> [(FilePath, ExternConfig)] -> IO (Maybe ExternBuild)
prepareExtern config project folders = do
  checked <- mapM (\(folder, extern) -> map (folder,) <$> checkExtern config (loadedFrom project folder) folder extern) folders
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
-- the loaded modules need on the selected platform, relative to the folder.
checkExtern :: TerminaConfig -> S.Set T.Text -> FilePath -> ExternConfig -> IO [FilePath]
checkExtern config loaded externFolderPath (ExternConfig moduleEntries) = do
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
      dieWithError $ "The file \"" ++ externFolderPath </> file ++ "\" is not listed under any module in " ++ sectionOf externFolderPath
  -- | The libraries of a module are tied to the platforms they were compiled
  -- for, so a loaded module that has some has to have them for the selected one
  forM_ (M.toList loadedEntries) $ \(modName, entry) ->
    unless (M.null (entryLibraries entry) || M.member (platform config) (entryLibraries entry)) $
      dieWithError $ "The extern entry of " ++ T.unpack modName ++ " has no libraries for the platform \"" ++ T.unpack (platform config) ++ "\""
  return . nub $ concat
    [ concatMap (listedFiles present . (externSourceFolder </>)) srcs
      ++ concatMap (listedFiles present . (externHeaderFolder </>)) hdrs
      ++ [ externLibraryFolder </> T.unpack (platform config) </> lib | lib <- M.findWithDefault [] (platform config) libs ]
    | ExternEntry srcs hdrs libs <- M.elems loadedEntries ]

  where

    loadedEntries = M.filterWithKey (\modName _ -> S.member modName loaded) moduleEntries

    listsOf srcs hdrs libs =
      [map (externSourceFolder </>) srcs, map (externHeaderFolder </>) hdrs]
      ++ [ map ((externLibraryFolder </> T.unpack plt) </>) pltLibs | (plt, pltLibs) <- M.toList libs ]

    inItsFolder file =
      case splitDirectories file of
        (folder : _ : _) | folder == externSourceFolder -> takeExtension file == ".c"
        (folder : _ : _) | folder == externHeaderFolder -> takeExtension file == ".h"
        (folder : plt : _ : _) | folder == externLibraryFolder -> checkPlatform plt /= Nothing && takeExtension file == ".a"
        _ -> False

-- | Check each "extern" section against the modules of its source folder:
-- each entry names a module of the folder that declares some extern function,
-- and each module of the folder that the build loads with an extern function
-- has its entry. A module that the build does not load is only parsed. The
-- main module has no entry, so it declares no extern function.
checkExternModules :: ParsedModule -> FilePath -> ParsedProject -> [(FilePath, ExternConfig)] -> IO ()
checkExternModules mainModule mainSection project sections = do
  forM_ (externFunctions (parsedAST (metadata mainModule))) $ \(ident, ann) ->
    dieWithError $ fullPath mainModule ++ ":" ++ show (locationLine ann) ++ ": " ++ takeFileName (fullPath mainModule)
      ++ " cannot declare the extern function " ++ ident ++ ". Declare it in a module of " ++ sourceFolderOf mainSection
      ++ "/ and list that module in " ++ sectionOf mainSection
  -- | An entry may be in the section of another folder, also of a test that this build leaves out
  hasTests <- doesDirectoryExist "tests"
  tests <- if hasTests then listDirectory "tests" else return []
  let folders = nub (map fst sections ++ [ "tests" </> t </> externFolder | t <- tests ])
  forM_ sections $ \(folder, ExternConfig moduleEntries) -> do
    let srcFolder = sourceFolderOf folder
    forM_ (M.keys moduleEntries) $ \modName -> do
      let qname = joinPath (map T.unpack (T.splitOn (T.pack ".") modName))
          path = srcFolder </> qname <.> "fin"
          listing = "The extern section of " ++ ownerOf folder ++ " lists " ++ T.unpack modName
      isModule <- doesFileExist path
      unless isModule $ do
        isFolder <- doesDirectoryExist (srcFolder </> qname)
        when isFolder $
          dieWithError $ listing ++ ", which is a folder (" ++ srcFolder </> qname ++ ") and not a module"
        others <- filterM (\other -> doesFileExist (sourceFolderOf other </> qname <.> "fin")) (folders \\ [folder])
        forM_ others $ \other ->
          dieWithError $ listing ++ ", which is a module of " ++ ownerOf other ++ " (" ++ sourceFolderOf other </> qname <.> "fin"
            ++ "). Move its entry to " ++ sectionOf other
        dieWithError $ listing ++ ", which is not a module (" ++ path ++ " does not exist)"
      elements <- case M.lookup qname project of
        Just loaded | fullPath loaded == path -> return (parsedAST (metadata loaded))
        _ -> frags . snd <$> parseTerminaFile qname path
      when (null (externFunctions elements)) $
        dieWithError $ listing ++ ", which declares no extern function (" ++ path ++ ")"
    forM_ (filter (isFrom folder) (M.elems project)) $ \loaded ->
      case externFunctions (parsedAST (metadata loaded)) of
        (ident, _) : _ | not (M.member (T.pack (qualifiedToModuleName (qualifiedName loaded))) moduleEntries) ->
          dieWithError $ "The module " ++ qualifiedToModuleName (qualifiedName loaded) ++ " declares the extern function "
            ++ ident ++ " and is not listed in " ++ sectionOf folder
        _ -> return ()

  where

    externFunctions elements = [ (ident, ann) | ExternFunction ident _ _ _ _ ann <- elements ]

-- | The source folder next to an "extern" folder.
sourceFolderOf :: FilePath -> FilePath
sourceFolderOf folder = normalise (takeDirectory folder </> "src")

-- | Whether a module of the build is loaded from the source folder next to an
-- "extern" folder.
isFrom :: FilePath -> ParsedModule -> Bool
isFrom folder m = fullPath m == sourceFolderOf folder </> qualifiedName m <.> "fin"

-- | The modules of the build loaded from the source folder next to an
-- "extern" folder, as its section names them.
loadedFrom :: ParsedProject -> FilePath -> S.Set T.Text
loadedFrom project folder = S.fromList
  [ T.pack (qualifiedToModuleName (qualifiedName m)) | m <- M.elems project, isFrom folder m ]

-- | Who an "extern" folder belongs to, as the messages name it.
ownerOf :: FilePath -> String
ownerOf folder =
  case splitDirectories folder of
    ["tests", testName, _] -> "the test " ++ testName
    _ -> "the project"

sectionOf :: FilePath -> String
sectionOf folder = "the extern section of " ++ ownerOf folder

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
