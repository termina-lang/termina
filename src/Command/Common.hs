module Command.Common where

import Command.Utils
import Command.Sources
import Command.Types
import Configuration.Namespace (locationLine)
import Configuration.Parsed
import qualified Data.Map.Lazy as ML
import Data.List (find)
import Modules.Utils (buildModuleName, qualifiedToModuleName)

import qualified Data.Text as T

import System.FilePath
import System.Directory
import Parser.AST (TerminaModule)
import Parser.Types (ParserAnn)
import Parser.Errors
import qualified Data.Map.Strict as M
import Modules.Modules
import Core.AST
    ( TerminaModule'(frags, modules), ModuleImport'(..) )
import Utils.Errors
import Utils.Annotations
import Text.Parsec.Error
import qualified Data.ByteString as BS


-- | Load a Termina module, taken from the modules of its source folder or,
-- when the folder does not have it, read and parsed from its file.
loadTerminaModule ::
  Sources
  -- | Source folder of the module
  -> FilePath
  -- | Path of the file to load
  -> FilePath
  -> IO ParsedModule
loadTerminaModule sources root filePath = do
  let fullP = root </> filePath <.> "fin"
      inFolder = find ((== root) . folderPath) sources >>= ML.lookup (normalise fullP) . folderModules
  (src_code, term) <- maybe (parseTerminaFile filePath fullP) (parsedOrDie filePath fullP) inFolder
  mod_time <- getModificationTime fullP
  imports <- mapM (importOf (M.singleton fullP src_code) fullP) (modules term)
  return $ TerminaModuleData filePath fullP mod_time imports [] src_code (ParsingData . frags $ term)

  where

    importerOwner = maybe Project folderOwner (find ((== root) . folderPath) sources)

    -- | An import, looked up in the folders that the folder of the module sees.
    importOf fileMap fullP (ModuleImport modName ann) =
      case buildModuleName ann modName of
        Left err -> dieWithReport (toText err fileMap)
        Right qname ->
          case lookupModule sources root qname of
            Found _ -> return (ModuleDependency qname ann)
            Ambiguous a b -> dieWithError (collisionMessage sources qname a b)
            Hidden folder -> dieWithError $ fullP ++ ":" ++ show (locationLine ann) ++ ": "
              ++ hiddenMessage folder importerOwner (qualifiedToModuleName qname)
            NotExported folder -> dieWithError $ fullP ++ ":" ++ show (locationLine ann) ++ ": "
              ++ notExportedMessage folder (qualifiedToModuleName qname)
            Missing -> dieWithReport (toText (annotateError ann (EImportedFileNotFound (root </> qname <.> "fin"))) fileMap)

-- | Read and parse a Termina file, without its imports.
parseTerminaFile ::
  -- | Qualified name of the module
  FilePath
  -- | Path of the file
  -> FilePath
  -> IO (T.Text, TerminaModule ParserAnn)
parseTerminaFile filePath fullP = BS.readFile fullP >>= parsedOrDie filePath fullP . parseFile filePath fullP

-- | The text and the tree of a parsed module file, given its qualified name
-- and its path, or stop with the error of the parser.
parsedOrDie :: QualifiedName -> FilePath -> ParsedFile -> IO (T.Text, TerminaModule ParserAnn)
parsedOrDie filePath fullP parsedFile =
  case parsedFile of
    Left NotUtf8 -> dieWithError $ fullP ++ ": the file is not valid UTF-8"
    Left (InvalidSyntax src_code err) ->
      let pErr = annotateError (Position filePath (errorPos err) (errorPos err)) (EParseError err)
          fileMap = M.singleton fullP src_code
      in
      dieWithReport (toText pErr fileMap)
    Right parsedModule -> return parsedModule

-- | Load the modules that the modules of a folder import, and everything they
-- import in turn, each one from the folder where the importer finds it. One
-- name found in two folders of the build is an error.
loadModules
  :: Sources
  -> FilePath
  -> [ModuleDependency]
  -> IO ParsedProject
loadModules sources importer imported = loadModules' M.empty [(dep, importer) | dep <- imported]

  where

  loadModules' :: ParsedProject -> [(ModuleDependency, FilePath)] -> IO ParsedProject
  loadModules' fsLoaded [] = pure fsLoaded
  loadModules' fsLoaded ((ModuleDependency qname _, from) : fss) = do
    folder <- case lookupModule sources from qname of
      Found folder -> return folder
      Ambiguous a b -> dieWithError (collisionMessage sources qname a b)
      _ -> dieWithError $ "Module " ++ qname ++ " not found"
    case M.lookup qname fsLoaded of
      Just loaded
        | fullPath loaded == folder </> qname <.> "fin" -> loadModules' fsLoaded fss
        | otherwise -> dieWithError (collisionMessage sources qname (moduleFolder qname (fullPath loaded)) folder)
      Nothing -> do
        loadedModule <- loadTerminaModule sources folder qname
        loadModules' (M.insert qname loadedModule fsLoaded)
          (fss ++ [(dep, folder) | dep <- importedModules loadedModule])

  -- | The source folder of a module, from its file and its qualified name.
  moduleFolder qname path =
    let parts = splitDirectories path
    in joinPath (take (length parts - length (splitDirectories qname)) parts)
