-- | The C of a deployment: the sources, their headers and the precompiled
-- libraries that the manifest of the project and those of its dependencies
-- list in their "extern" sections. The files that the build uses are copied
-- to the output folder, and the C of each manifest is linked into a
-- relocatable object of its own in which only its extern functions stay
-- global. The sections are checked when the project is loaded and when a
-- dependency is admitted (Configuration.ExternCheck); this module does the
-- part that depends on the deployment.
module Command.Extern (
    ExternGroup(..),
    groupObject,
    groupObjects,
    prepareExtern,
    verifyLoadedExterns,
    parsedExterns
) where

import Configuration.Configuration
import Configuration.ExternCheck (ModuleExterns(..), checkExternModules, ownerOfFolder)
import Configuration.Fetch (FetchedFile(..))
import Configuration.Manifest (Manifest(..), ProjectInfo(..))
import Configuration.Package (packageFiles, packageManifest)
import Configuration.Vendor (exportedExtern)
import Command.Sources (Owner(..), SourceFolder(..), Sources)
import Command.Types
import Command.Utils
import Core.AST
import Modules.Modules
import Modules.Utils (qualifiedToModuleName)
import Utils.Files (filesUnder)

import Control.Monad
import qualified Data.ByteString as BS
import Data.Char (toUpper)
import Data.Function (on)
import Data.List (groupBy, isPrefixOf, nub, sortOn, stripPrefix, (\\))
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

-- | Copy the C that the modules of the build need on the selected platform
-- to each of the output folders given, and give its groups: one for the
-- project, with its C and, in a test, the C of the test, and one for each
-- dependency whose loaded modules have C. A file that
-- both sections of the project have would overwrite the other in the output
-- folder, so it is an error, and so are two dependencies whose C would go to
-- the same place.
prepareExtern :: TerminaConfig -> [FilePath] -> ParsedProject -> Sources -> [(FilePath, ExternConfig)] -> IO [ExternGroup]
prepareExtern config outputs project sources folders = do
  checked <- forM folders $ \(folder, extern) -> do
    present <- filesUnder folder
    map (folder,) <$> usedFiles config "termina.yaml" (ownerOfFolder folder) (loadedFrom project folder) present extern
  let files = concat checked
      relFiles = map snd files
  forM_ (relFiles \\ nub relFiles) $ \file ->
    case [ folder | (folder, rel) <- files, rel == file ] of
      first : second : _ ->
        dieWithError $ "both " ++ addTrailingPathSeparator first ++ " and " ++ addTrailingPathSeparator second ++ " have the file "
          ++ file ++ ", and the two would go to the same place in the output folder"
      _ -> return ()
  let projectGroups = [ groupOf (T.unpack (name config)) "" externFolder relFiles (map fst folders) | not (null relFiles) ]
  -- | The C of each dependency, from the section of its manifest, with only what it exports
  dependencies <- fmap concat . forM [ (f, p) | f <- sources, isDependency (folderOwner f), Just p <- [folderPackage f] ] $ \(f, p) -> do
    let root = takeDirectory (folderPath f)
        extern = root </> externFolder
        manifest = packageManifest p
        library = T.unpack (projectName (manifestProject manifest))
        present = [ rel | FetchedFile path _ _ <- packageFiles p, Just rel <- [stripPrefix (externFolder ++ "/") path] ]
    used <- usedFiles config (root </> "termina.yaml") ("the dependency " ++ library) (loadedFrom project extern) present (exportedExtern manifest)
    return [ (f, p, used, groupOf library (variableOf f) (destinationOf f) used [extern]) | not (null used) ]
  -- | Two dependencies whose objects would have one variable, or whose C would go to one folder
  forM_ (groupBy ((==) `on` (\(_, _, _, g) -> groupVariable g)) (sortOn (\(_, _, _, g) -> groupVariable g) dependencies)) $ \case
    (f, _, _, g) : (other, _, _, _) : _ -> dieWithError $ case (folderOwner f, folderOwner other) of
      (LocalPackage library _, LocalPackage _ _) -> "the local folders " ++ takeDirectory (folderPath f) ++ " and " ++ takeDirectory (folderPath other)
        ++ " are both the library " ++ library ++ ", and their C would go to the same place in the output folder"
      _ -> takeDirectory (makeRelative "vendor" (folderPath f)) ++ " and " ++ takeDirectory (makeRelative "vendor" (folderPath other))
        ++ " give the same name, " ++ groupObjects g ++ ", to the objects of their C in the Makefile"
    _ -> return ()
  projectFiles <- mapM (\(folder, rel) -> (rel,) <$> BS.readFile (folder </> rel)) files
  forM_ outputs $ \out -> do
    copyExtern (out </> externFolder) projectFiles
    forM_ dependencies $ \(_, p, used, g) ->
      let contents = M.fromList [ (path, bytes) | FetchedFile path _ bytes <- packageFiles p ]
      in copyExtern (out </> groupFolder g)
           [ (file, bytes) | file <- used, Just bytes <- [M.lookup (externFolder ++ "/" ++ file) contents] ]
    removeStaleGroups out (map (\(_, _, _, g) -> groupFolder g) dependencies)
  return (projectGroups ++ [ g | (_, _, _, g) <- dependencies ])

  where

    -- | A group with its files and the extern functions of the modules
    -- loaded from the source folders next to its extern folders
    groupOf groupName' variable folder used externs = ExternGroup groupName' variable folder
      (filter ((== ".c") . takeExtension) used) (filter ((== ".a") . takeExtension) used)
      (S.toList (S.fromList [ ident | extern <- externs, m <- M.elems project, isFrom extern m
                                    , (ident, _) <- externDeclared (parsedExterns m) ]))

    isDependency owner = case owner of
      RemotePackage {} -> True
      LocalPackage {} -> True
      _ -> False

    -- | A package goes under its normalized URL, as in vendor/, and a local
    -- folder under the name of its library
    destinationOf f = case folderOwner f of
      LocalPackage library _ -> "local" </> library </> externFolder
      _ -> takeDirectory (folderPath f) </> externFolder

    variableOf f = case folderOwner f of
      LocalPackage library _ -> "LOCAL_" ++ map variableChar library
      _ -> map variableChar (takeDirectory (makeRelative "vendor" (folderPath f)))

    variableChar c = if c `elem` ("./" :: String) then '_' else toUpper c

-- | Remove from the output folder the C of the dependencies that the build no
-- longer has, given the folders of those that it has.
removeStaleGroups :: FilePath -> [FilePath] -> IO ()
removeStaleGroups out kept =
  forM_ ["vendor", "local"] $ \top -> do
    present <- filesUnder (out </> top)
    forM_ present $ \file ->
      unless (any (\folder -> addTrailingPathSeparator folder `isPrefixOf` (top </> file)) kept) $
        removeFile (out </> top </> file)

-- | The files of an "extern" section that the loaded modules need on the
-- selected platform, relative to its folder, given the manifest that declares
-- the section, its owner and the files of the folder. A loaded module with
-- libraries has them for the selected platform.
usedFiles :: TerminaConfig -> FilePath -> String -> S.Set T.Text -> [FilePath] -> ExternConfig -> IO [FilePath]
usedFiles config manifestPath owner loaded present (ExternConfig moduleEntries) = do
  forM_ (M.toList loadedEntries) $ \(modName, entry) ->
    unless (M.null (entryLibraries entry) || M.member (platform config) (entryLibraries entry)) $
      dieWithError $ manifestPath ++ ": the extern section of " ++ owner ++ " lists libraries of " ++ T.unpack modName
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
verifyLoadedExterns :: ParsedModule -> FilePath -> ParsedProject -> [(FilePath, ExternConfig)] -> IO ()
verifyLoadedExterns mainModule mainSection project sections =
  case concat [ checkExternModules (ownerOfFolder folder) (sourceFolderOf folder) config
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

-- | Write the files, each one given by its path relative to the output folder
-- and its content, to the output folder. A file is written only when its
-- content changes, and the files of the output folder that are no longer
-- listed are removed.
copyExtern :: FilePath -> [(FilePath, BS.ByteString)] -> IO ()
copyExtern destination contents = do
  let files = map fst contents
  forM_ contents $ \(file, content) -> do
    let target = destination </> file
    targetExists <- doesFileExist target
    same <- if targetExists then (== content) <$> BS.readFile target else return False
    unless same $ do
      createDirectoryIfMissing True (takeDirectory target)
      BS.writeFile target content
  present <- filesUnder destination
  let listed = S.fromList files
  forM_ present $ \file ->
    unless (file `S.member` listed) $ removeFile (destination </> file)
