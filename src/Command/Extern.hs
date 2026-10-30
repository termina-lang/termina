-- | The C of the project: the sources, their headers and the precompiled
-- libraries that the manifest lists in its "extern" section, under the
-- "extern" folder of the project. The files that the build uses are copied
-- to the output folder and linked into a single relocatable object in which
-- only the extern functions stay global. The sections are checked when the
-- project is loaded (Configuration.ExternCheck); what is left here depends on
-- the build.
module Command.Extern (
    ExternGroup(..),
    groupObject,
    groupObjects,
    prepareExtern,
    checkLoadedExterns,
    parsedExterns
) where

import Configuration.Configuration
import Configuration.ExternCheck (ModuleExterns(..), moduleErrors, ownerOfFolder, listFiles)
import Command.Types
import Command.Utils
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

-- | The C of one manifest that a deployment uses, linked into an object of
-- its own in which only its extern functions stay global.
data ExternGroup = ExternGroup {
    -- | The name of its object: the one of the binary for the project
    groupName :: String,
    -- | The prefix of the variable of its objects in the Makefile, empty for
    -- the project
    groupVariable :: String,
    -- | Its folder in the output, relative to it
    groupFolder :: FilePath,
    -- | The sources and the libraries of the selected platform, relative to
    -- its folder
    groupSources :: [FilePath],
    groupLibraries :: [FilePath],
    -- | Its extern functions
    groupSymbols :: [Identifier]
  } deriving (Show)

-- | The object of a group, relative to the folder of the objects.
groupObject :: ExternGroup -> FilePath
groupObject g = normalise (takeDirectory (groupFolder g) </> groupName g <.> "ext.o")

-- | The variable that holds the objects of the sources of a group.
groupObjects :: ExternGroup -> String
groupObjects g = if null (groupVariable g) then "EXTERN_OBJS" else groupVariable g ++ "_EXTERN_OBJS"

-- | Copy the files of each "extern" section, the one of the project and, in
-- a test, the one of the test, that the modules of the build need on the
-- selected platform to the output folder, and give the group of the project,
-- if it has C. A file that both folders have would overwrite the other in the
-- output folder, so it is an error.
prepareExtern :: TerminaConfig -> ParsedProject -> [(FilePath, ExternConfig)] -> IO [ExternGroup]
prepareExtern config project folders = do
  checked <- mapM (\(folder, extern) -> map (folder,) <$> usedFiles config (loadedFrom project folder) folder extern) folders
  let files = concat checked
      relFiles = map snd files
  forM_ (relFiles \\ nub relFiles) $ \file ->
    case [ folder | (folder, rel) <- files, rel == file ] of
      first : second : _ ->
        dieWithError $ "Both " ++ addTrailingPathSeparator first ++ " and " ++ addTrailingPathSeparator second ++ " have the file "
          ++ file ++ ", and the two would go to the same place in the output folder"
      _ -> return ()
  copyExtern (outputFolder config </> externFolder) files
  let symbols = S.toList (S.fromList [ ident | (folder, _) <- folders, m <- M.elems project, isFrom folder m
                                             , (ident, _) <- externDeclared (parsedExterns m) ])
  return [ ExternGroup (T.unpack (name config)) "" externFolder
             (filter ((== ".c") . takeExtension) relFiles) (filter ((== ".a") . takeExtension) relFiles) symbols
         | not (null relFiles) ]

-- | The files of an "extern" section that the loaded modules need on the
-- selected platform, relative to its folder. A loaded module that has
-- libraries has to have them for the selected platform, since they are tied to
-- the platforms they were compiled for.
usedFiles :: TerminaConfig -> S.Set T.Text -> FilePath -> ExternConfig -> IO [FilePath]
usedFiles config loaded externFolderPath (ExternConfig moduleEntries) = do
  folderExists <- doesDirectoryExist externFolderPath
  present <- if folderExists then listFiles externFolderPath else return []
  forM_ (M.toList loadedEntries) $ \(modName, entry) ->
    unless (M.null (entryLibraries entry) || M.member (platform config) (entryLibraries entry)) $
      dieWithError $ "The extern section of " ++ ownerOfFolder externFolderPath ++ " lists libraries of " ++ T.unpack modName
        ++ " for other platforms, and none for " ++ T.unpack (platform config)
  return . nub $ concat
    [ concatMap (listedFiles present . (externSourceFolder </>)) srcs
      ++ concatMap (listedFiles present . (externHeaderFolder </>)) hdrs
      ++ [ externLibraryFolder </> T.unpack (platform config) </> lib | lib <- M.findWithDefault [] (platform config) libs ]
    | ExternEntry srcs hdrs libs <- M.elems loadedEntries ]

  where

    loadedEntries = M.filterWithKey (\modName _ -> S.member modName loaded) moduleEntries

-- | Check that each module that the deployment loads from the source folder
-- of a section, and that declares an extern function, has its entry, and that
-- the module that wires the deployment, whose section is given, declares none.
checkLoadedExterns :: ParsedModule -> FilePath -> ParsedProject -> [(FilePath, ExternConfig)] -> IO ()
checkLoadedExterns mainModule mainSection project sections =
  case concat [ moduleErrors (ownerOfFolder folder) (sourceFolderOf folder) config
                  (if folder == mainSection then Just (parsedExterns mainModule) else Nothing)
                  (map parsedExterns (filter (isFrom folder) (M.elems project)))
              | (folder, config) <- sections ] of
    [] -> return ()
    errs -> dieWithFirstError errs

-- | The extern functions of a module that the build has loaded.
parsedExterns :: ParsedModule -> ModuleExterns
parsedExterns m = ModuleExterns (splitDirectories (qualifiedName m)) (fullPath m)
  [ (ident, ann) | ExternFunction ident _ _ _ _ ann <- parsedAST (metadata m) ]

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
