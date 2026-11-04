module Command.Common where

import Command.Pipeline (typeProject)
import Command.Utils
import Command.Sources
import Command.Types
import Configuration.Configuration
import Configuration.Manifest
import Configuration.Namespace (exportedModules)
import Configuration.Package (Package(..))
import Configuration.Parsed
import Configuration.Platform
import Configuration.Prefix (manifestPrefixes, ownsModule)
import Generator.Environment (getPlatformInitialGlobalEnv)
import Semantic.Environment (makeInitialGlobalEnv)
import qualified Data.Map.Lazy as ML
import Data.List (find, isPrefixOf, nub)
import Modules.Utils (buildModuleName, qualifiedToModuleName)

import qualified Data.Set as S
import qualified Data.Text as T

import System.FilePath
import Parser.AST (TerminaModule)
import Parser.Types (ParserAnn)
import Parser.Errors
import qualified Data.Map.Strict as M
import Modules.Modules
import Core.AST
    ( TerminaModule'(frags, modules), ModuleImport'(..) )
import Utils.Annotations
import Text.Parsec.Error
import Text.Parsec.Pos (initialPos)
import qualified Data.ByteString as BS


-- | Load a Termina module, taken from the modules of its source folder or,
-- when the folder does not have it, read and parsed from its file. It gives
-- the first error of the file or of its imports.
loadTerminaModule ::
  Sources
  -- | Source folder of the module
  -> FilePath
  -- | Path of the file to load
  -> FilePath
  -> IO (Either CheckFailure ParsedModule)
loadTerminaModule sources root filePath = do
  let fullP = root </> filePath <.> "fin"
      inFolder = find ((== root) . folderPath) sources >>= ML.lookup (normalise fullP) . folderModules
  parsedFile <- maybe (readTerminaFile filePath fullP) return inFolder
  return $ do
    (src_code, term) <- parsedModule filePath fullP parsedFile
    imports <- mapM importOf (modules term)
    Right (TerminaModuleData filePath fullP imports [] src_code (ParsingData . frags $ term))

  where

    importerOwner = maybe Project folderOwner (find ((== root) . folderPath) sources)

    -- | The folders that the folder of the module sees
    seen = maybe [root] folderSees (find ((== root) . folderPath) sources)

    -- | An import, looked up in the folders that the folder of the module sees.
    importOf (ModuleImport modName ann) =
      case buildModuleName ann modName of
        Left err -> Left (checkFailure err)
        Right qname ->
          case lookupModule sources root qname of
            Found _ -> Right (ModuleDependency qname ann)
            Ambiguous a b -> Left (loadFailure ann (EModuleNameCollision (T.pack (collisionMessage sources qname a b))))
            Hidden folder -> Left (loadFailure ann
              (EImportedModuleHidden (T.pack (hiddenMessage folder importerOwner (qualifiedToModuleName qname)))))
            NotExported folder -> Left (loadFailure ann
              (EImportedModuleNotExported (T.pack (notExportedMessage folder (qualifiedToModuleName qname)))))
            Missing -> Left (loadFailure ann (EImportedModuleNotFound qname (root </> filePath <.> "fin") (nub seen)))

-- | Read a Termina file and parse it, without its imports.
readTerminaFile ::
  -- | Qualified name of the module
  FilePath
  -- | Path of the file
  -> FilePath
  -> IO ParsedFile
readTerminaFile filePath fullP = parseFile filePath fullP <$> BS.readFile fullP

-- | Read and parse a Termina file, without its imports.
parseTerminaFile ::
  -- | Qualified name of the module
  FilePath
  -- | Path of the file
  -> FilePath
  -> IO (T.Text, TerminaModule ParserAnn)
parseTerminaFile filePath fullP = readTerminaFile filePath fullP >>= parsedOrDie filePath fullP

-- | The text and the tree of a parsed module file, given its qualified name
-- and its path, or the error of the parser.
parsedModule :: QualifiedName -> FilePath -> ParsedFile -> Either CheckFailure (T.Text, TerminaModule ParserAnn)
parsedModule filePath fullP parsedFile =
  case parsedFile of
    Left NotUtf8 -> Left (loadFailure (Position fullP (initialPos fullP) (initialPos fullP)) EInvalidUtf8)
    Left (InvalidSyntax _ err) -> Left (loadFailure (Position filePath (errorPos err) (errorPos err)) (EParseError err))
    Right parsed -> Right parsed

-- | The text and the tree of a parsed module file, given its qualified name
-- and its path, or stop with the error of the parser.
parsedOrDie :: QualifiedName -> FilePath -> ParsedFile -> IO (T.Text, TerminaModule ParserAnn)
parsedOrDie filePath fullP parsedFile = orDieWithFailure files (parsedModule filePath fullP parsedFile)

  where

    files = case parsedFile of
      Left (InvalidSyntax src_code _) -> M.singleton fullP src_code
      _ -> M.empty

-- | Load the modules that the modules of a folder import, and everything they
-- import in turn, each one from the folder where the importer finds it. One
-- name found in two folders of the build is an error. It gives the first error.
loadModules
  :: Sources
  -> FilePath
  -> [ModuleDependency]
  -> IO (Either CheckFailure ParsedProject)
loadModules sources importer imported = loadModulesFrom sources [(dep, importer) | dep <- imported]

-- | Load modules, each one looked up from the folder given with it, and
-- everything they import in turn. It gives the first error.
loadModulesFrom
  :: Sources
  -> [(ModuleDependency, FilePath)]
  -> IO (Either CheckFailure ParsedProject)
loadModulesFrom sources = loadModules' M.empty

  where

  loadModules' :: ParsedProject -> [(ModuleDependency, FilePath)] -> IO (Either CheckFailure ParsedProject)
  loadModules' fsLoaded [] = return (Right fsLoaded)
  loadModules' fsLoaded ((ModuleDependency qname ann, from) : fss) =
    case lookupModule sources from qname of
      Found folder ->
        case M.lookup qname fsLoaded of
          Just loaded
            | fullPath loaded == folder </> qname <.> "fin" -> loadModules' fsLoaded fss
            | otherwise -> return (Left (loadFailure ann
                (EModuleNameCollision (T.pack (collisionMessage sources qname (moduleFolder qname (fullPath loaded)) folder)))))
          Nothing -> do
            loaded <- loadTerminaModule sources folder qname
            case loaded of
              Left failure -> return (Left failure)
              Right loadedModule -> loadModules' (M.insert qname loadedModule fsLoaded)
                (fss ++ [(dep, folder) | dep <- importedModules loadedModule])
      Ambiguous a b -> return (Left (loadFailure ann (EModuleNameCollision (T.pack (collisionMessage sources qname a b)))))
      _ -> return (Left (loadFailure ann (EImportedModuleNotFound qname (addTrailingPathSeparator from)
              (nub (maybe [from] folderSees (find ((== from) . folderPath) sources))))))

  -- | The source folder of a module, from its file and its qualified name.
  moduleFolder qname path =
    let parts = splitDirectories path
    in joinPath (take (length parts - length (splitDirectories qname)) parts)

-- | Load the main module of a build, from its folder and its qualified name,
-- and the modules it imports. It gives the first error.
loadMainModule :: Sources -> FilePath -> QualifiedName -> IO (Either CheckFailure (ParsedModule, ParsedProject))
loadMainModule sources mainFolder mainModule = do
  loadedMain <- loadTerminaModule sources mainFolder mainModule
  case loadedMain of
    Left failure -> return (Left failure)
    Right parsedMain -> do
      imported <- loadModules sources mainFolder (importedModules parsedMain)
      return ((\parsedModules -> (parsedMain, M.insert (qualifiedName parsedMain) parsedMain parsedModules)) <$> imported)

-- | Load the exported modules of a library, the exported modules of every
-- package that its dependencies reach, and the modules they import, in the
-- order in which they are type checked. It gives the first error.
loadExportedModules :: LoadedProject -> IO (Either CheckFailure (ParsedProject, [QualifiedName]))
loadExportedModules project = do
  let root = loadedRoot project
      src = normalise (root </> "src")
      sources = buildSources project Nothing src
  projectRoots <- exportedModules root (loadedManifest project)
  loaded <- loadModulesFrom sources
    ([(ModuleDependency qname Internal, src) | qname <- projectRoots] ++ dependencyRoots sources)
  return (loaded >>= \parsedModules -> (,) parsedModules <$> orderedModules parsedModules)

  where

    -- | The exported modules of each package of the build, each one with the
    -- folder of its package. A build without a test has only the packages that
    -- the dependencies of the project reach.
    dependencyRoots sources =
      [ (ModuleDependency qname Internal, folderPath folder)
      | folder <- sources
      , Just package <- [folderPackage folder]
      , file <- ML.keys (folderModules folder)
      , addTrailingPathSeparator (folderPath folder) `isPrefixOf` file
      , takeExtension file == ".fin"
      , let qname = dropExtension (makeRelative (folderPath folder) file)
      , any (`ownsModule` splitDirectories qname) (manifestPrefixes (packageManifest package)) ]

-- | Type check the modules of a library on a platform, as those of a test
-- build. It gives the modules type checked up to the first error, and the
-- error.
typeLibrary :: Manifest -> Platform -> S.Set QualifiedName -> ParsedProject -> [QualifiedName] -> (TypedProject, Maybe CheckFailure)
typeLibrary manifest plt deps = typeProject deps (makeInitialGlobalEnv (Just config) plt (getPlatformInitialGlobalEnv config plt))

  where

    config = (defaultConfig (T.unpack (projectName (manifestProject manifest))) plt) {
        sysPrintOutputBufferSize = manifestPrintBufSize manifest,
        sysReadInputBufferSize = manifestReadBufSize manifest,
        deploymentKind = TestBuild (T.pack "library")
      }

-- | The platforms on which a library is type checked: those of its tests, or
-- the test platform when it has none.
libraryPlatforms :: Manifest -> [Platform]
libraryPlatforms manifest =
  case foldr (\plt acc -> if plt `elem` acc then acc else plt : acc) [] (concatMap testPlatforms (M.elems (manifestTests manifest))) of
    [] -> [TestPlatform]
    plts -> plts

-- | The modules in the order in which they are type checked, or the cycle of
-- imports between them.
orderedModules :: ParsedProject -> Either CheckFailure [QualifiedName]
orderedModules parsedProject =
  either (Left . loadFailure Internal . EImportedFilesLoop) Right
    (sortProjectDepsOrLoop (M.map importedModules parsedProject))

-- | The failure of the loading of a module at a location.
loadFailure :: Location -> Error -> CheckFailure
loadFailure ann err = checkFailure (annotateError ann err :: ParsingErrors)

-- | The text of every module of the source folders of a build, by the path of
-- its file, which is what the error printer quotes from.
sourceTexts :: Sources -> M.Map FilePath T.Text
sourceTexts sources = M.fromList
  [ (file, text)
  | folder <- sources
  , (file, parsed) <- ML.toList (folderModules folder)
  , Just text <- [either failedText (Just . fst) parsed] ]

  where

    failedText (InvalidSyntax text _) = Just text
    failedText NotUtf8 = Nothing
